open Order_book
open Order_types
open Price

let get_trade_price (buy_order : db_order) (sell_order : db_order) : price =
  match sell_order.order_type with
  | Market -> (match get_price buy_order with
    | Some price -> price
    | None -> failwith "Both orders cannot be market orders.")
  | Limit { price; _ } | Margin price -> price

(* looks at order book to find matching pairs of bids/asks *)
let match_orders (order_book : order_book) (_market_conditions : market_conditions) : trade list =
  let rec match_aux acc =
    let bids = get_bids order_book in
    let asks = get_asks order_book in
    match bids, asks with
    | [], _ | _, [] -> acc
    | best_bid :: _, best_ask :: _ ->
      let can_match = match best_bid.order_type, best_ask.order_type with
      | Market, _ | _, Market -> true
      | _ -> 
        match order_book.best_bid, order_book.best_ask with
        | Some bid_price, Some ask_price -> compare_price bid_price ask_price >= 0
        | _ -> false
      in
      if can_match then
        let trade_price = get_trade_price best_bid best_ask in
        let trade_qty = Float.min best_bid.qty best_ask.qty in
        let trade = {
          buy_order_id = (Option.get best_bid.id);
          sell_order_id = (Option.get best_ask.id);
          security = best_bid.security;
          qty = trade_qty;
          price = trade_price;
        } in
        let new_bids = 
          if Float.equal best_bid.qty trade_qty then List.filter (fun o -> o != best_bid) bids
          else 
            let updated_bid = { best_bid with qty = best_bid.qty -. trade_qty } in
            updated_bid :: (List.filter (fun o -> o != best_bid) bids)
        in
        let new_asks = 
          if Float.equal best_ask.qty trade_qty then List.filter (fun o -> o != best_ask) asks
          else
            let updated_ask = { best_ask with qty = best_ask.qty -. trade_qty } in
            updated_ask :: (List.filter (fun o -> o != best_ask) asks)
        in
        order_book.orders <- new_bids @ new_asks; 
        (* recalculate best_bid and best_ask *)
        remove_order_from_memory order_book (-1);
        match_aux (trade :: acc)
      else acc
  in
  List.rev (match_aux [])

let match_all_books (order_books : order_book list) (market_conditions : market_conditions) : trade list =
  List.concat (List.map (fun ob -> match_orders ob market_conditions) order_books)