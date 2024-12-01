open Order
open Order_book
open Market_conditions

type trade = {
  buy_order_id : int;
  sell_order_id : int;
  trade_qty : float;
  buy_qty_after : float;
  sell_qty_after : float;
}

let get_price_helper (order : Order.order) : float =
  match Order_book.get_price order with
  | None -> failwith "Expected order to have a price."
  | Some price -> price

let check_spread (order_book : order_book) (market_conditions : Market_conditions.t) : bool =
  let best_bid = get_best_bid order_book in
  let best_ask = get_best_ask order_book in
  match best_bid, best_ask with
  | Some best_bid, Some best_ask -> 
    let bid_price = get_price_helper best_bid in
    let ask_price = get_price_helper best_ask in
    check_spread_conditions market_conditions bid_price ask_price
  | _ -> false

let execute_trade (buy_order : Order.order) (sell_order : Order.order) : float =
  let trade_qty = min buy_order.qty sell_order.qty in
  buy_order.qty <- buy_order.qty -. trade_qty;
  sell_order.qty <- sell_order.qty -. trade_qty;
  trade_qty


let match_orders (order_book : order_book) (market_conditions : Market_conditions.t) : trade list =
  let rec match_aux matched_orders = 
    if check_spread order_book market_conditions then
      match (Order_book.get_best_bid order_book, Order_book.get_best_ask order_book) with
      | Some best_bid, Some best_ask ->
        let trade_qty = execute_trade best_bid best_ask in
        let trade = {
          buy_order_id = best_bid.id;
          sell_order_id = best_ask.id;
          trade_qty = trade_qty;
          buy_qty_after = best_bid.qty;
          sell_qty_after = best_ask.qty;
        } in
        (* add trade to list of matched orders *)
        if best_bid.qty = 0.0 then Order_book.remove_order order_book best_bid.id;
        if best_ask.qty = 0.0 then Order_book.remove_order order_book best_ask.id;
        match_aux (trade :: matched_orders)
        (* note that trades is reversed *)
      | _ -> matched_orders     (* no bid/ask to match *)
    else matched_orders
  in
  match_aux []

let match_all_books (books : order_book list) (market_conditions : Market_conditions.t) : trade list =
  List.fold_left (fun acc book -> acc @ match_orders book market_conditions) [] books