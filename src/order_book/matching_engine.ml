open Order
open Order_book
open Market_conditions
open User

type trade = {
  buy_order_id : int;
  sell_order_id : int;
  trade_qty : float;
  buy_qty_after : float;
  sell_qty_after : float;
  trade_price : float;
}

let check_spread (order_book : order_book) (market_conditions : market_conditions) : bool =
  match get_best_bid order_book, get_best_ask order_book with
  | Some best_bid, Some best_ask -> 
    (match get_bids order_book, get_asks order_book with
    | (bid :: _), (ask :: _) ->
      (match bid.order_type, ask.order_type with
      | Market, _ | _, Market -> true
      | _ -> best_bid >= best_ask && check_spread_conditions market_conditions best_bid best_ask)
    | _ -> false)
  | _ -> false

let get_trade_price buy_order sell_order =
  match sell_order.order_type with
  | Market -> (match get_price buy_order with
    | Some p -> p
    | None -> failwith "Both orders cannot be market orders")
  | Limit { price; _ } | Margin price -> price

(* remove???? *)
let execute_trade (buy_order : Order.order) (sell_order : Order.order) : float * float =
  let trade_qty = Float.min buy_order.qty sell_order.qty in
  let trade_price = get_trade_price buy_order sell_order in
  let total_cost = trade_price *. trade_qty in
  
  buy_order.qty <- buy_order.qty -. trade_qty;
  sell_order.qty <- sell_order.qty -. trade_qty;
  
  update_balance buy_order.user_id (-. total_cost);
  update_balance sell_order.user_id total_cost;
  update_position buy_order.user_id buy_order.security trade_qty;
  update_position sell_order.user_id sell_order.security (-. trade_qty);
  
  (trade_qty, trade_price)

(* let match_orders (order_book : order_book) (market_conditions : market_conditions) : trade list =
  let rec match_aux acc = 
    if check_spread order_book market_conditions then
      match get_bids order_book, get_asks order_book with
      | [], _ | _, [] -> acc
      | best_bid :: _, best_ask :: _ ->
          let (trade_qty, trade_price) = execute_trade best_bid best_ask in
          let trade = {
            buy_order_id = best_bid.id;
            sell_order_id = best_ask.id;
            trade_qty;
            buy_qty_after = best_bid.qty;
            sell_qty_after = best_ask.qty;
            trade_price;
          } in
          if best_bid.qty <= 0.0 then remove_order order_book best_bid.id;
          if best_ask.qty <= 0.0 then remove_order order_book best_ask.id;
          match_aux (trade :: acc)
    else acc
  in
  List.rev (match_aux []) *)

let match_orders (order_book : order_book) (market_conditions : market_conditions) : trade list =
  let _unused = market_conditions.bid_ask_spread in
  let rec match_aux acc =
    match get_bids order_book, get_asks order_book with
    | [], _ | _, [] -> acc
    | best_bid :: _, best_ask :: _ -> (
      match best_bid.order_type, best_ask.order_type with
      | Market, _ ->
        let trade_qty = Float.min best_bid.qty best_ask.qty in
        let trade_price = get_price_helper best_ask in
        let total_cost = trade_qty *. trade_price in
        Printf.printf "Executing trade: %f * %f = %f\n" trade_qty trade_price total_cost;
        update_balance best_bid.user_id (-. total_cost);
        Printf.printf "Updated balance for user %d: %f\n" best_bid.user_id (-. total_cost);
        update_position best_bid.user_id best_bid.security trade_qty;
        Printf.printf "Updated position for user %d: %f\n" best_bid.user_id trade_qty;
        update_balance best_ask.user_id total_cost;
        Printf.printf "Updated balance for user %d: %f\n" best_ask.user_id total_cost;
        update_position best_ask.user_id best_ask.security (-. trade_qty);
        Printf.printf "Updated position for user %d: %f\n" best_ask.user_id (-. trade_qty);
        best_bid.qty <- best_bid.qty -. trade_qty;
        best_ask.qty <- best_ask.qty -. trade_qty;
        if best_bid.qty <= 0.0 then remove_order order_book best_bid.id;
        if best_ask.qty <= 0.0 then remove_order order_book best_ask.id;
        let trade = {
          buy_order_id = best_bid.id;
          sell_order_id = best_ask.id;
          trade_qty;
          buy_qty_after = best_bid.qty;
          sell_qty_after = best_ask.qty;
          trade_price;
        } in
        match_aux (trade :: acc)
      | _, Market ->
        Printf.printf "Sell market order\n";
        (* do later *)
        match_aux acc
      | _, _ -> acc)
  in
  List.rev (match_aux [])
  

let match_all_books (order_books : order_book list) (market_conditions : market_conditions) : trade list =
  List.concat (List.map (fun ob -> match_orders ob market_conditions) order_books)