open Order_book_lib.Order_types
open Order_book_lib.Matching_engine
open Order_book_lib.Ob_utils
open Order_book_lib.Price
open Database.Db

let execute_trade (buy_order : db_order) (sell_order : db_order) : (float * price, string) result =
  let buy_id = unwrap_id buy_order.id in
  let sell_id = unwrap_id sell_order.id in
  let trade_qty = Float.min buy_order.qty sell_order.qty in
  let trade_price = get_trade_price buy_order sell_order in
  let trade_price_float = price_to_float trade_price in
  let total_cost = trade_price_float *. trade_qty in
    
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

let print_trade (trade : trade) (security : string) =
  Printf.printf "Trade executed: %.2f units of %s between orders %d and %d at $%s.\n" 
                trade.qty security trade.buy_order_id trade.sell_order_id (price_to_string trade.price)