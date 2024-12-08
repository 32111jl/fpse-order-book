open Order_book
open Market_conditions
open Database.Db
open Utils
open Utils.Order_types

let check_spread (book : order_book) (market_conditions : market_conditions) : bool =
  (* match book.best_bid, book.best_ask with
  | Some best_bid, Some best_ask -> 
    (match get_bids book, get_asks book with
    | (bid :: _), (ask :: _) ->
      (match bid.order_type, ask.order_type with
      | Market, _ | _, Market ->
        Printf.printf "Market order spread check\n";
        true
      | _ ->
        Printf.printf "Limit order spread check\n";
        (best_bid < best_ask) && (best_ask -. best_bid <= market_conditions.bid_ask_spread))
    | _ -> false)
  | _ ->
    Printf.printf "No best bid/ask\n";
    false *)
  match get_bids book, get_asks book with
  | (bid :: _), (ask :: _) ->
    (match bid.order_type, ask.order_type with
    | Market, _ | _, Market -> true
    | _ -> 
      let best_bid = Option.get book.best_bid in
      let best_ask = Option.get book.best_ask in
      best_bid >= best_ask || (best_ask -. best_bid <= market_conditions.bid_ask_spread))
  | _ -> false

let get_trade_price (buy_order : Order_types.db_order) (sell_order : Order_types.db_order) : float =
  match sell_order.order_type with
  | Market -> (match get_price buy_order with
    | Some p -> p
    | None -> failwith "Both orders cannot be market orders")
  | Limit { price; _ } | Margin price -> price

(* executes trade between two orders, updates database accordingly *)
let execute_trade (buy_order : Order_types.db_order) (sell_order : Order_types.db_order) : (float * float, string) result =
  let buy_id = unwrap_id buy_order.id in
  let sell_id = unwrap_id sell_order.id in
  let trade_qty = Float.min buy_order.qty sell_order.qty in
  let trade_price = get_trade_price buy_order sell_order in
  let total_cost = trade_price *. trade_qty in
    
  with_transaction (fun _conn ->
  let _ = record_trade ~buy_order_id:buy_id ~sell_order_id:sell_id ~security:buy_order.security ~qty:trade_qty ~price:trade_price in
    
    let buy_qty_remaining = buy_order.qty -. trade_qty in
    let sell_qty_remaining = sell_order.qty -. trade_qty in

    let _ = update_order_qty buy_id buy_qty_remaining in
    let _ = update_order_qty sell_id sell_qty_remaining in
    
    let _ = if buy_qty_remaining <= 0.0 then fill_order buy_id
            else update_order_status buy_id "PARTIAL" in
    let _ = if sell_qty_remaining <= 0.0 then fill_order sell_id
            else update_order_status sell_id "PARTIAL" in
    
    let _ = update_position buy_order.user_id buy_order.security trade_qty in
    let _ = update_position sell_order.user_id sell_order.security (-. trade_qty) in
    
    let _ = update_user_balance buy_order.user_id (-. total_cost) in
    let _ = update_user_balance sell_order.user_id total_cost in
    (trade_qty, trade_price)
  )

(* looks at order book to find matching pairs of bids/asks *)
let match_orders (order_book : order_book) (_market_conditions : market_conditions) : Order_types.trade list =
  let rec match_aux acc =
    match get_bids order_book, get_asks order_book with
    | [], _ | _, [] -> acc
    | best_bid :: _, best_ask :: _ ->
      let can_match = match best_bid.order_type, best_ask.order_type with
      | Market, _ | _, Market -> true
      | _ -> 
        match order_book.best_bid, order_book.best_ask with
        | Some bid_price, Some ask_price -> bid_price >= ask_price
        | _ -> false
      (* | Limit { price = bid_price; _ }, Limit { price = ask_price; _ } -> bid_price >= ask_price
      | Margin bid_price, Limit { price = ask_price; _ } 
      | Limit { price = bid_price; _ }, Margin ask_price 
      | Margin bid_price, Margin ask_price ->
        bid_price >= ask_price *)
      in
      if can_match then
        (match execute_trade best_bid best_ask with
        | Ok (trade_qty, trade_price) ->
          let trade = {
            buy_order_id = unwrap_id best_bid.id;
            sell_order_id = unwrap_id best_ask.id;
            security = best_bid.security;
            qty = trade_qty;
            price = trade_price;
          } in
          order_book.best_bid <- None;
          order_book.best_ask <- None;
          if get_bids order_book <> [] && get_asks order_book <> [] then
            match_aux (trade :: acc)
          else trade :: acc
        | Error _ -> match_aux acc)
      else acc (* no match is possible right now *)
  in
  List.rev (match_aux [])

let match_all_books (order_books : order_book list) (market_conditions : market_conditions) : Order_types.trade list =
  List.concat (List.map (fun ob -> match_orders ob market_conditions) order_books)