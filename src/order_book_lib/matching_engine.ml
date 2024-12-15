open Order_book
open Order_types
open Price

let can_match (buy_order : db_order) (sell_order : db_order) : bool =
  match buy_order.order_type, sell_order.order_type with
  | Market, _ | _, Market -> true
  | Limit { price = bid_price; _ }, Limit { price = ask_price; _ }
  | Limit { price = bid_price; _ }, Margin ask_price
  | Margin bid_price, Limit { price = ask_price; _ }
  | Margin bid_price, Margin ask_price ->
    compare_price bid_price ask_price >= 0

let get_trade_price (buy_order : db_order) (sell_order : db_order) : price =
  match sell_order.order_type with
  | Market -> (match get_price buy_order with
    | Some price -> price
    | None -> failwith "Both orders cannot be market orders.")
  | Limit { price; _ } | Margin price -> price

let create_trade (order : db_order) (best_match : db_order) (trade_qty : float) (trade_price : price) : trade =
  {
    buy_order_id = Option.get (if order.buy_sell = Buy then order.id else best_match.id);
    sell_order_id = Option.get (if order.buy_sell = Buy then best_match.id else order.id);
    security = order.security;
    qty = trade_qty;
    price = trade_price;
  }

let update_order_book (order_book : order_book) (order : db_order) (best_match : db_order) 
    (trade_qty : float) (potential_matches : db_order list) : unit =
  let updated_matches =
    if Float.equal best_match.qty trade_qty then List.filter (fun o -> o != best_match) potential_matches
    else
      let updated_match = { best_match with qty = best_match.qty -. trade_qty } in
      updated_match :: (List.filter (fun o -> o != best_match) potential_matches)
  in
  order_book.orders <- (match order.buy_sell with
    | Buy -> (get_bids order_book) @ updated_matches
    | Sell -> updated_matches @ (get_asks order_book));
  remove_order_from_memory order_book (-1)

let process_match (order_book : order_book) (order : db_order) (best_match : db_order) (remaining_qty : float) (potential_matches : db_order list) : trade option * float =
  let can_match = match order.buy_sell with
    | Buy -> can_match order best_match
    | Sell -> can_match best_match order
  in
  if can_match then
    let trade_price = match order.buy_sell with
      | Buy -> get_trade_price order best_match
      | Sell -> get_trade_price best_match order
    in
    let trade_qty = Float.min remaining_qty best_match.qty in
    let trade = create_trade order best_match trade_qty trade_price in
    update_order_book order_book order best_match trade_qty potential_matches;
    Some trade, remaining_qty -. trade_qty
  else
    None, remaining_qty

let rec match_aux acc remaining_qty order order_book =
  if remaining_qty <= 0.0 then acc
  else
    let potential_matches = match order.buy_sell with
      | Buy -> get_asks order_book
      | Sell -> get_bids order_book
    in
    match potential_matches with
    | [] -> acc
    | best_match :: _ ->
      let trade_opt, new_remaining_qty = 
        process_match order_book order best_match remaining_qty potential_matches
      in
      match trade_opt with
      | Some trade -> match_aux (trade :: acc) new_remaining_qty order order_book
      | None -> acc

(* private helper to attempt to match orders - returns a list of trades *)
let try_match_orders order_book =
  let bids = get_bids order_book in
  let asks = get_asks order_book in
  match bids, asks with
  | [], _ | _, [] -> []
  | best_bid :: _, best_ask :: _ ->
    if can_match best_bid best_ask then
      match_aux [] best_bid.qty best_bid order_book
    else []

let match_orders (order_book : order_book) (_market_conditions : market_conditions) : trade list =
  let rec match_all_possible acc =
    let new_trades = try_match_orders order_book in
    if new_trades = [] then acc
    else match_all_possible (new_trades @ acc)
  in
  List.rev (match_all_possible [])


let match_all_books (order_books : order_book list) (market_conditions : market_conditions) : trade list =
  List.concat (List.map (fun ob -> match_orders ob market_conditions) order_books)