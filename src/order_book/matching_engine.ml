open Order
open Order_book
open Market_conditions

let check_spread (order_book : order_book) (market_conditions : Market_conditions.t) : bool =
  let best_bid = get_best_bid order_book in
  let best_ask = get_best_ask order_book in
  match best_bid, best_ask with
  | Some best_bid, Some best_ask -> 
    check_spread_conditions market_conditions best_bid.price best_ask.price
  | _ -> false

let execute_trade (buy_order : Order.order) (sell_order : Order.order) : (Order.order * Order.order) option =
  if buy_order.qty > sell_order.qty then
    let new_buy_order = { buy_order with qty = buy_order.qty -. sell_order.qty } in
    Some (new_buy_order, sell_order)

  else if buy_order.qty < sell_order.qty then
    let new_sell_order = { sell_order with qty = sell_order.qty -. buy_order.qty } in
    Some (buy_order, new_sell_order)

  else
    Some (buy_order, sell_order)


let match_orders (order_book : order_book) (market_conditions : Market_conditions.t) : (Order.order * Order.order) list =
  let rec match_aux matched_orders = 
    if check_spread order_book market_conditions then
      match (Order_book.get_best_bid order_book, Order_book.get_best_ask order_book) with
      | Some best_bid, Some best_ask ->
        begin
          match execute_trade best_bid best_ask with
          | Some (buy_order, sell_order) ->
            if buy_order.qty = 0.0 then Order_book.remove_order order_book best_bid.id;
            if sell_order.qty = 0.0 then Order_book.remove_order order_book best_ask.id;

            if buy_order.qty > 0.0 then Order_book.add_order order_book buy_order;
            if sell_order.qty > 0.0 then Order_book.add_order order_book sell_order;

            match_aux ((buy_order, sell_order) :: matched_orders)
          | None -> matched_orders
        end
      | _ -> matched_orders     (* no bid/ask to match *)
    else matched_orders
  in
  match_aux []

let match_all_books (books : order_book list) (market_conditions : Market_conditions.t) : (Order.order * Order.order) list =
  let match_for_book book = 
    if check_spread book market_conditions then
      match_orders book market_conditions
    else
      []
  in
  List.fold_left (fun acc book -> acc @ match_for_book book) [] books