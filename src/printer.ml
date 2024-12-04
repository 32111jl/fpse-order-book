open Order_book_lib.Order
open Order_book_lib.Order_book
open Order_book_lib.Matching_engine

let print_orders ob =
  Printf.printf "\nOrder book for %s:\n" (get_security ob);
  Printf.printf "Bids:\n";
  let bids = get_bids ob in
  if bids = [] then
    Printf.printf "No bids\n"
  else
    List.iter (fun order -> 
      Printf.printf "Price: $%.2f, Qty: %.2f\n" (get_price_helper order) order.qty
    ) bids;
  Printf.printf "\nAsks:\n";
  let asks = get_asks ob in
  if asks = [] then Printf.printf "No asks\n"
  else
    List.iter (fun order -> 
      Printf.printf "Price: $%.2f, Qty: %.2f\n" (get_price_helper order) order.qty
    ) asks;
  Printf.printf "------------------------\n"

let print_market_prices order_books =
  Printf.printf "\nCurrent Market Prices:\n";
  Hashtbl.iter (fun security ob ->
    let best_bid = match get_best_bid ob with 
      | Some price -> Printf.sprintf "%.2f" price
      | None -> "None" in
    let best_ask = match get_best_ask ob with
      | Some price -> Printf.sprintf "%.2f" price
      | None -> "None" in
    Printf.printf "%s: Bid: %s, Ask: %s\n" security best_bid best_ask
  ) order_books;
  Printf.printf "------------------------\n"

let print_trade trade security =
  Printf.printf "Trade executed: %.2f units of %s between orders %d and %d.\n" 
                  trade.trade_qty security trade.buy_order_id trade.sell_order_id