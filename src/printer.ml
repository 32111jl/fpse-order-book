open Order_book_lib.Order_book
open Utils.Order_types
open Database.Db

let print_orders (ob : order_book) =
  Printf.printf "Bids:\n";
  let security = get_security ob in
  (match get_active_orders_given_security security with
  | Ok result ->
    let bids = ref [] in
    for i = 0 to result#ntuples - 1 do
      if result#getvalue i 4 = "BUY" then
        bids := (result#getvalue i 6, result#getvalue i 5) :: !bids
    done;
    if !bids = [] then Printf.printf "No bids.\n"
    else
      List.iter (fun (price, qty) -> 
        Printf.printf "Price: $%.2f, Qty: %.2f\n" (float_of_string price) (float_of_string qty)
      ) (List.sort (fun (p1, _) (p2, _) -> 
        compare (float_of_string p2) (float_of_string p1)) !bids);
  | Error _ -> Printf.printf "Error fetching bids.\n");

  Printf.printf "\nAsks:\n";
  (match get_active_orders_given_security security with
  | Ok result ->
    let asks = ref [] in
    for i = 0 to result#ntuples - 1 do
      if result#getvalue i 4 = "SELL" then
        asks := (result#getvalue i 6, result#getvalue i 5) :: !asks
    done;
    if !asks = [] then Printf.printf "No asks.\n"
    else
      List.iter (fun (price, qty) -> 
        Printf.printf "Price: $%.2f, Qty: %.2f\n" (float_of_string price) (float_of_string qty)
      ) (List.sort (fun (p1, _) (p2, _) -> 
        compare (float_of_string p1) (float_of_string p2)) !asks);
  | Error _ -> Printf.printf "Error fetching asks.\n");
  Printf.printf "------------------------\n"

let print_user_orders (user_id : int) =
  match get_active_orders_given_user user_id with
  | Ok result ->
    if result#ntuples = 0 then (Printf.printf "You have no active orders!\n"; false)
    else begin
      Printf.printf "\nYour Active Orders:\n";
      Printf.printf "------------------------\n";
      for i = 0 to result#ntuples - 1 do
        Printf.printf "ID: %s, %s: %s order for %.2f shares at $%s\n"
          (result#getvalue i 0)  (* id *)
          (result#getvalue i 2)  (* security *)
          (result#getvalue i 4)  (* buy_sell *)
          (float_of_string (result#getvalue i 5))  (* quantity *)
          (result#getvalue i 6)  (* price *)
      done;
      Printf.printf "------------------------\n";
      true
    end
  | Error _ ->
    Printf.printf "Error fetching orders\n";
    false

let print_market_prices (securities : string list) =
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

let print_trade (trade : trade) (security : string) =
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


let print_available_securities ?(active_only=false) (securities : string list) =
  if active_only then
    let active_securities = List.filter (fun security -> 
      match get_active_orders_given_security security with
      | Ok result -> result#ntuples > 0
      | Error _ -> false
    ) securities in
    if active_securities = [] then Printf.printf "No securities with active orders.\n"
    else begin
      Printf.printf "Securities with active orders:\n";
      List.iter (fun security -> Printf.printf "%s " security) active_securities;
      Printf.printf "\n"
    end
  else begin
    Printf.printf "All available securities:\n";
    List.iter (fun security -> Printf.printf "%s " security) securities;
    Printf.printf "\n"
  end

let print_security_info (security : string) =
  match get_security_info security with
  | Ok result ->
    if result#ntuples > 0 then begin
      Printf.printf "\n=== Information for %s ===\n" security;
      Printf.printf "Current Price: $%s\n" (result#getvalue 0 1);
      match get_active_orders_given_security security with
      | Ok orders ->
        let num_orders = orders#ntuples in
        if num_orders > 0 then Printf.printf "Active Orders: %d\n" num_orders
        else Printf.printf "No active orders\n";
        (* print recent trades *)
        (match get_trades_by_security security with
        | Ok trades when trades#ntuples > 0 ->
          Printf.printf "Recent Trades: %d\n" trades#ntuples;
          Printf.printf "Last Trade Price: $%s\n" (trades#getvalue 0 5)
        | _ -> Printf.printf "No recent trades.\n")
      | Error _ -> Printf.printf "Error fetching trades.\n"
    end
  | Error msg -> Printf.printf "Error: %s\n" msg

let print_recent_trades (security : string) =
  match get_trades_by_security security with
  | Ok result ->
    if result#ntuples > 0 then begin
      Printf.printf "\n=== Recent Trades for %s ===\n" security;
      Printf.printf "Trade ID\tQuantity\tPrice\n";
      for i = 0 to result#ntuples - 1 do
        Printf.printf "%s\t\t%s\t\t$%s\n"
          (result#getvalue i 0)  (* id *)
          (result#getvalue i 4)  (* quantity *)
          (result#getvalue i 5)  (* price *)
      done
    end
  | Error msg -> Printf.printf "Error: %s\n" msg