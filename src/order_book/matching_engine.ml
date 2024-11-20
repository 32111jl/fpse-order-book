open Order
open Order_book
open Market_conditions

let check_spread (order_book : Order_book) (market_conditions : Market_conditions) : bool =
  let best_bid = Order_book.get_best_bid order_book in
  let best_ask = Order_book.get_best_ask order_book in
  Market_conditions.check_spread_conditions market_conditions best_bid best_ask

let execute_trade (buy_order : Order) (sell_order : Order) : (Order * Order) option =
  if buy_order.qty > sell_order.qty then
    let new_buy_order = { buy_order with qty = buy_order.qty -. sell_order.qty } in
    Some (new_buy_order, sell_order)

  else if buy_order.qty < sell_order.qty then
    let new_sell_order = { sell_order with qty = sell_order.qty -. buy_order.qty } in
    Some (buy_order, new_sell_order)

  else
    Some (buy_order, sell_order)


let match_orders (order_book : Order_book) (market_conditions : Market_conditions) : (Order * Order) list =
  let rec match_aux matched_orders = 
    if check_spread order_book market_conditions then
      match (get_best_bid order_book, get_best_ask order_book) with
      | Some best_bid, Some best_ask ->
        begin
          match execute_trade best_bid best_ask with
          | Some (buy_order, sell_order) ->
            if buy_order.qty = 0.0 then remove_order order_book best_bid.id;
            if sell_order.qty = 0.0 then remove_order order_book best_ask.id;

            if buy_order.qty > 0.0 then add_order order_book buy_order;
            if sell_order.qty > 0.0 then add_order order_book sell_order;

            match_aux ((buy_order, sell_order) :: matched_orders)
          | None -> matched_orders
        end
      | _ -> matched_orders     (* no bid/ask to match *)
    else matched_orders
  in
  match_aux []



(** Matches buy/sell orders based on price-time priority. *)
open order
open order_book
open market_conditions

val match_orders : order_book -> market_conditions -> (order * order) list
(** [match_orders order_book market_conditions] matches buy/sell orders based on price-time priority. *)

val match_all_books : order_book list -> market_conditions -> float -> (order * order) list
(** [match_all_books books market_conditions curr_time] matches buy/sell orders for the given order books. *)

val check_spread : order_book -> market_conditions -> bool
(** [check_spread order_book market_conditions] checks if the spread is within the market conditions. *)

val execute_trade : order -> order -> (order * order) option
(** [execute_trade buy_order sell_order] executes the buy and sell orders. *)