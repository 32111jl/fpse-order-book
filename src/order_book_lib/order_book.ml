open Order_types
open Ob_utils

let create_order_book (security : string) = {
  security = security;
  orders = [];
  best_bid = None;
  best_ask = None;
}

let add_order_to_memory (book : order_book) (order : db_order) =
  book.orders <- order :: book.orders;
  match order.order_type, order.buy_sell with
  | Limit { price; _ }, Buy | Margin price, Buy ->
    (match book.best_bid with
    | Some curr when price > curr -> book.best_bid <- Some price
    | None -> book.best_bid <- Some price
    | _ -> ())
  | Limit { price; _ }, Sell | Margin price, Sell ->
    (match book.best_ask with
    | Some curr when price < curr -> book.best_ask <- Some price
    | None -> book.best_ask <- Some price
    | _ -> ())
  | _ -> ()

let remove_order_from_memory (book : order_book) (order_id : int) =
  book.orders <- List.filter (fun order -> order.id <> Some order_id) book.orders;
  (* recompute best_bid/ask *)
  let buys = List.filter (fun order -> order.buy_sell = Buy &&
                            (match order.order_type with
                            | Limit _ | Margin _ -> true
                            | _ -> false))
                          book.orders in
  let sells = List.filter (fun order -> order.buy_sell = Sell &&
                            (match order.order_type with
                            | Limit _ | Margin _ -> true
                            | _ -> false))
                          book.orders in
  (* best_bid = highest buy price *)
  book.best_bid <- (match buys with
  | [] -> None
  | lst -> Some (List.fold_left (fun acc order ->
    let price = match order.order_type with
      | Limit { price; _ } -> price
      | Margin price -> price
      | Market -> 0.0
    in
    if price > acc then price else acc
  ) 0.0 lst));
  (* best_ask = lowest sell price *)
  book.best_ask <- (match sells with
  | [] -> None
  | lst -> Some (List.fold_left (fun acc order ->
    let price = match order.order_type with
      | Limit { price; _ } -> price
      | Margin price -> price
      | Market -> max_float in
    if price < acc then price else acc
  ) max_float lst))

let get_price (order : db_order) : float option = 
  match order.order_type with
  | Market -> None (* should be unreachable *)
  | Limit { price; _ } -> Some price
  | Margin price -> Some price

let virtual_price (order : db_order) : float option =
  match order.order_type, order.buy_sell with
  | Market, Buy -> Some max_float
  | Market, Sell -> Some (-.max_float)
  | _ -> get_price order

let get_best_bid (book : order_book) : float option = book.best_bid
let get_best_ask (book : order_book) : float option = book.best_ask

let get_bids (book : order_book) : db_order list =
  List.filter (fun order -> order.buy_sell = Buy) book.orders
  |> List.sort (fun order1 order2 -> 
    let price_cmp = compare_price_options (virtual_price order2) (virtual_price order1) in
    (* if price ties, sort by order id (lowest id = placed earlier = higher priority) *)
    if price_cmp = 0 then match order1.id, order2.id with
      | Some id1, Some id2 -> compare id1 id2
      | _ -> 0
    else price_cmp
  )

let get_asks (book : order_book) : db_order list =
  List.filter (fun order -> order.buy_sell = Sell) book.orders
  |> List.sort (fun order1 order2 -> 
    let price_cmp = compare_price_options (get_price order1) (get_price order2) in
    if price_cmp = 0 then match order1.id, order2.id with
      | Some id1, Some id2 -> compare id1 id2
      | _ -> 0
    else price_cmp
  )

let print_orders (book : order_book) =
  let bids = List.filter (fun order -> order.buy_sell = Buy) book.orders in
  let asks = List.filter (fun order -> order.buy_sell = Sell) book.orders in

  Printf.printf "Bids:\n";
  List.iter (fun order ->
    let price = match order.order_type with
      | Market -> "MARKET" (* should be unreachable *)
      | Limit { price; _ } -> Printf.sprintf "%.2f" price
      | Margin p -> Printf.sprintf "%.2f (Margin)" p
    in
    Printf.printf "Price: $%s, Qty: %.2f\n" price order.qty
  ) bids;

  Printf.printf "\nAsks:\n";
  List.iter (fun order ->
    let price = match order.order_type with
      | Market -> "MARKET" (* should be unreachable *)
      | Limit { price; _ } -> Printf.sprintf "%.2f" price
      | Margin p -> Printf.sprintf "%.2f (Margin)" p
    in
    Printf.printf "Price: $%s, Qty: %.2f\n" price order.qty
  ) asks

let print_trade (trade : trade) (security : string) =
  Printf.printf "Trade executed: %.2f units of %s between orders %d and %d at $%.2f.\n" 
                trade.qty security trade.buy_order_id trade.sell_order_id trade.price