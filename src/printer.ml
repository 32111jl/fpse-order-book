open Order_book_lib.Order_book
open Order_types
open Database.Db

let print_orders (ob : order_book) =
  Printf.printf "Bids:\n";
  let security = get_security ob in
  match get_active_orders_given_security security with
  | Ok result ->
      let bids = ref [] in
      for i = 0 to result#ntuples - 1 do
        if result#getvalue i 4 = "BUY" then
          bids := (result#getvalue i 6, result#getvalue i 5) :: !bids
      done;
      if !bids = [] then Printf.printf "No bids\n"
      else
        List.iter (fun (price, qty) -> 
          Printf.printf "Price: $%s, Qty: %s\n" price qty
        ) (List.sort (fun (p1, _) (p2, _) -> 
          compare (float_of_string p2) (float_of_string p1)) !bids);
  | Error _ -> Printf.printf "Error fetching bids\n";
  Printf.printf "\nAsks:\n";
  match get_active_orders_given_security security with
  | Ok result ->
      let asks = ref [] in
      for i = 0 to result#ntuples - 1 do
        if result#getvalue i 4 = "SELL" then
          asks := (result#getvalue i 6, result#getvalue i 5) :: !asks
      done;
      if !asks = [] then Printf.printf "No asks\n"
      else
        List.iter (fun (price, qty) -> 
          Printf.printf "Price: $%s, Qty: %s\n" price qty
        ) (List.sort (fun (p1, _) (p2, _) -> 
          compare (float_of_string p1) (float_of_string p2)) !asks);
  | Error _ -> Printf.printf "Error fetching asks\n";
  Printf.printf "------------------------\n"

let print_user_orders user_id =
  match get_orders_by_user user_id with
  | Ok result ->
    if result#ntuples = 0 then Printf.printf "You have no active orders!\n"
    else begin
      Printf.printf "\nYour Active Orders:\n";
      Printf.printf "------------------------\n";
      for i = 0 to result#ntuples - 1 do
        Printf.printf "ID: %s, %s: %s order for %s shares at $%s\n"
          (result#getvalue i 0)  (* id *)
          (result#getvalue i 2)  (* security *)
          (result#getvalue i 4)  (* buy_sell *)
          (result#getvalue i 5)  (* quantity *)
          (result#getvalue i 6)  (* price *)
      done;
      Printf.printf "------------------------\n"
    end
  | Error _ -> Printf.printf "Error fetching orders\n"

let print_market_prices securities =
  Printf.printf "\nCurrent Market Prices:\n";
  List.iter (fun security ->
    let ob = create_order_book security in
    let best_bid = match get_best_bid ob with 
      | Some price -> Printf.sprintf "%.2f" price
      | None -> "None" in
    let best_ask = match get_best_ask ob with
      | Some price -> Printf.sprintf "%.2f" price
      | None -> "None" in
    Printf.printf "%s: Bid: %s, Ask: %s\n" security best_bid best_ask
  ) securities;
  Printf.printf "------------------------\n"

let print_trade trade security =
  Printf.printf "Trade executed: %.2f units of %s between orders %d and %d at $%.2f.\n" 
                trade.qty security trade.buy_order_id trade.sell_order_id trade.price

let print_query_result result =
  match result with
  | Ok res -> 
    for i = 0 to res#ntuples - 1 do
      for j = 0 to res#nfields - 1 do
        Printf.printf "%s\t" (res#getvalue i j)
      done;
      Printf.printf "\n"
    done
  | Error msg -> Printf.printf "Error: %s\n" msg

let print_security_info security =
  match get_security_info security with
  | Ok result ->
    if result#ntuples > 0 then
      Printf.printf "Symbol: %s\nPrice: $%s\nStatus: %s\n"
        (result#getvalue 0 0)  (* symbol *)
        (result#getvalue 0 1)  (* price *)
        (result#getvalue 0 2)  (* status *)
    else Printf.printf "Security %s not found\n" security
  | Error msg -> Printf.printf "Error: %s\n" msg

let print_recent_trades security =
  match get_trades_by_security security with
  | Ok result ->
    if result#ntuples = 0 then
      Printf.printf "No trades for %s\n" security
    else begin
      Printf.printf "\nRecent Trades for %s:\n" security;
      Printf.printf "ID\tQty\tPrice\n";
      for i = 0 to result#ntuples - 1 do
        Printf.printf "%s\t%s\t$%s\n"
          (result#getvalue i 0)  (* id *)
          (result#getvalue i 4)  (* quantity *)
          (result#getvalue i 5)  (* price *)
      done
    end
  | Error msg -> Printf.printf "Error: %s\n" msg