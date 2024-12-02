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

let check_spread (order_book : order_book) (market_conditions : Market_conditions.t) : bool =
  match get_best_bid order_book, get_best_ask order_book with
  | Some best_bid, Some best_ask -> check_spread_conditions market_conditions best_bid best_ask
  | _ -> false

let execute_trade (buy_order : Order.order) (sell_order : Order.order) : float =
  let trade_qty = Float.min buy_order.qty sell_order.qty in
  buy_order.qty <- buy_order.qty -. trade_qty;
  sell_order.qty <- sell_order.qty -. trade_qty;
  trade_qty

let match_orders (order_book : order_book) (market_conditions : Market_conditions.t) : trade list =
  let rec match_aux acc = 
    if check_spread order_book market_conditions then
      match get_bids order_book, get_asks order_book with
      | [], _ | _, [] -> acc
      | best_bid :: _, best_ask :: _ ->
          let trade_qty = execute_trade best_bid best_ask in
          let trade = {
            buy_order_id = best_bid.id;
            sell_order_id = best_ask.id;
            trade_qty = trade_qty;
            buy_qty_after = best_bid.qty;
            sell_qty_after = best_ask.qty;
          } in
          if best_bid.qty <= 0.0 then remove_order order_book best_bid.id;
          if best_ask.qty <= 0.0 then remove_order order_book best_ask.id;
          match_aux (trade :: acc)
    else acc
  in
  List.rev (match_aux [])

let match_all_books (books : order_book list) (market_conditions : Market_conditions.t) : trade list =
  List.fold_left (fun acc book -> 
    List.rev_append (match_orders book market_conditions) acc
  ) [] books |> List.rev