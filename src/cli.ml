(* open Order_book_lib.Order *)
open Order_book_lib.Order_book
open Order_book_lib.Market_conditions
open Order_book_lib.Matching_engine
(* open Order_book_lib.User *)
open Database.Db
open Printer
open Utils
open Utils.Order_types

let order_books = Hashtbl.create 16              (* in-memory list of order books *)
(* I think in theory this is faster for lookups, but I'm not sure vs just the database. Need to *)
let curr_user_id = ref None                      (* current user ID, shouldn't change after setting it *)
let user_order_counter = ref 1000000             (* counter for user-placed orders *)
let computer_order_counter = ref (-1000000)      (* counter for computer-generated orders *)
(* list of currently-available securities *)
let available_securities = [
  "AAPL"; "MSFT"; "GOOGL"; "AMZN"; "TSLA"; 
  "META"; "NVDA"; "RKLB"; "RIVN"; "PLTR"
]


let get_base_price (security : string) : float = match security with
  | "AAPL" -> 150.0 | "MSFT" -> 330.0 | "GOOGL" -> 140.0
  | "AMZN" -> 180.0 | "TSLA" -> 300.0 | "META" -> 300.0
  | "NVDA" -> 400.0 | "RKLB" -> 20.0 | "RIVN" -> 15.0
  | "PLTR" -> 53.0 | _ -> 100.0

let with_user_id f = match !curr_user_id with
  | Some user_id -> f user_id
  | None -> Printf.printf "Please set your user ID first.\n"

let create_dynamic_market_conditions (security : string) : market_conditions =
  let base_price = get_base_price security in
  let spread = base_price *. 0.10 in (* 10% of base price, can be changed *)
  create_market_conditions spread 0.5

let get_or_create_order_book security =
  match Hashtbl.find_opt order_books security with
  | Some ob -> ob
  | None ->
    let ob = create_order_book security in
    Hashtbl.add order_books security ob;
    ob

let place_order security order_type buy_sell qty user_id =
  let order_book = get_or_create_order_book security in
  match order_type with
    | Market ->
      (* double-check that market orders can be placed *)
      (match buy_sell with
      | Buy ->
        (match get_best_ask order_book with
        | None -> Printf.printf "Cannot place market buy order: No asks available for %s\n" security
        | Some best_ask ->
          let price = best_ask in
          let order_id = !user_order_counter in
          user_order_counter := order_id + 1;
          match create_order order_id user_id security order_type buy_sell qty price with
          | Ok _ -> (* match here or no? need to wait some time before the position shows up if not, otherwise will have duplicated share *)
            Printf.printf "Market BUY order placed for %.2f shares of %s.\n" qty security
          | Error e -> Printf.printf "Error placing market buy order: %s\n" e)
      | Sell ->
        (match get_best_bid order_book with
        | None -> Printf.printf "Cannot place market sell order: No bids available for %s\n" security
        | Some best_bid ->
          let price = best_bid in
          let order_id = !user_order_counter in
          user_order_counter := order_id + 1;
          match create_order order_id user_id security order_type buy_sell qty price with
          | Ok _ -> (* match here or no? need to wait some time before the position shows up if not, otherwise will have duplicated share *)
            Printf.printf "Market SELL order placed for %.2f shares of %s.\n" qty security
          | Error e -> Printf.printf "Error placing market sell order: %s\n" e))
    | Limit { price; _ } | Margin price ->
      let order_id = !user_order_counter in
      user_order_counter := order_id + 1;
      match create_order order_id user_id security order_type buy_sell qty price with
      | Ok _ -> (* match here or no? *)
        Printf.printf "%s %s order placed: %.2f shares at $%.2f.\n"
          (match order_type with Limit _ -> "Limit" | Margin _ -> "Margin" | _ -> "")
          (if buy_sell = Buy then "BUY" else "SELL")
          qty price
      | Error e -> Printf.printf "Error placing limit/margin order: %s\n" e

let rec get_security () =
  Printf.printf "Enter the security (eg. AAPL): ";
  let security = String.uppercase_ascii (read_line ()) in
  if not (List.mem security available_securities) then begin
    Printf.printf "Invalid security. Please choose from the list above.\n";
    get_security ()
  end else security

let rec get_order_direction () =
  Printf.printf "Enter the order direction (Buy/Sell): ";
  match read_line () with
  | "Buy" -> Buy
  | "Sell" -> Sell
  | _ -> 
    Printf.printf "Invalid order direction. Please enter either Buy or Sell.\n";
    get_order_direction ()

let rec get_order_type () =
  Printf.printf "Enter the order type (Market/Limit/Margin): ";
  match String.lowercase_ascii (read_line ()) with
  | "market" -> Market
  | "limit" ->
    Printf.printf "Enter the price: ";
    let curr_price = float_of_string (read_line ()) in
    Limit { price = curr_price; expiration = Some (current_time () +. 3600.0) }
  | "margin" ->
    Printf.printf "Enter the price: ";
    Margin (float_of_string (read_line ()))
  | _ -> 
    Printf.printf "Invalid order type. Please enter Market, Limit, or Margin.\n";
    get_order_type ()

let rec get_quantity () =
  Printf.printf "Enter the quantity: ";
  try 
    let qty = float_of_string (read_line ()) in
    if qty <= 0.0 then begin
      Printf.printf "Quantity must be positive.\n";
      get_quantity ()
    end else qty
  with Failure _ -> 
    Printf.printf "Invalid quantity. Please enter a valid number.\n";
    get_quantity ()

let has_active_orders security =
  match get_active_orders_given_security security with
  | Ok result -> result#ntuples > 0
  | Error _ -> false

let set_user_id () = 
  match !curr_user_id with
  | Some id -> Printf.printf "User ID already set to %d\n" id
  | None ->
    Printf.printf "Enter your user ID: ";
    let user_id = int_of_string (read_line ()) in
    curr_user_id := Some user_id;
    match get_user_balance user_id with
    | None -> 
      ignore (create_user_in_db user_id "" 1000.0);
      Printf.printf "New user created with initial balance of $1000.00\n"
    | Some _ -> 
      Printf.printf "Welcome back!\n"

let can_place_order user_id security buy_sell order_type qty : (unit, string) result =
  match get_user_balance user_id with
  | None -> Error "User not found."
  | Some balance ->
    (* buy order: check if we can afford the order *)
    let cost = match order_type with
      | Market ->
        let ob = get_or_create_order_book security in
        if buy_sell = Buy then
          match get_best_ask ob with
          | Some price -> price *. qty
          | None -> 0.0
        else 0.0
      | Limit { price; _ } -> if buy_sell = Buy then price *. qty else 0.0
      | Margin price -> price *. 0.5
    in

    if buy_sell = Buy && cost > balance then
      Error (Printf.sprintf "Insufficient funds. Required: $%.2f, Available: $%.2f." cost balance)
    else if buy_sell = Sell then
      (* sell order: check if we have enough shares to sell *)
      match get_positions_by_user user_id with
      | Ok result ->
        let rec find_security i =
          if i >= result#ntuples then None
          else 
            let sec = result#getvalue i 1 in
            if String.uppercase_ascii sec = security then Some (float_of_string (result#getvalue i 2))
            else find_security (i + 1)
        in
        (match find_security 0 with
        | Some shares when shares >= qty -> Ok ()   (* user has enough shares *)
        | Some shares -> Error (Printf.sprintf "Insufficient shares. Required: %.2f, Available: %.2f." qty shares)
        | None -> Error (Printf.sprintf "You don't own any shares of %s!" security))
      | Error _ -> Error "Error fetching positions.\n"
    else Ok ()

let place_order_interactive () =
  with_user_id (fun user_id ->
    print_available_securities available_securities;
    print_available_securities ~active_only:true available_securities;
    let security = get_security () in
    let buy_sell = get_order_direction () in
    let order_type = get_order_type () in

    (* for market orders, check if there are any orders in the order book *)
    (* should_continue is false if we can't place the order, and will exit the function *)
    let should_continue = match order_type with
    | Market -> 
      let ob = get_or_create_order_book security in
      (match buy_sell with
      | Buy ->
        if get_best_ask ob = None then begin
          Printf.printf "Cannot place market buy order: No asks available for %s\n" security;
          false
        end else true
      | Sell ->
        if get_best_bid ob = None then begin
          Printf.printf "Cannot place market sell order: No bids available for %s\n" security;
          false
        end else true)
    | _ -> true
    in
    if should_continue then begin
      let qty = get_quantity () in
      match can_place_order user_id security buy_sell order_type qty with
      | Error e -> Printf.printf "%s\n" e
      | Ok _ ->
        place_order security order_type buy_sell qty user_id;
        Printf.printf "Order placed successfully!\n"
    end
  )

let cancel_order () = 
  with_user_id (fun user_id ->
    print_user_orders user_id;
    Printf.printf "Enter the order ID to cancel (or -1 to go back): ";
    let order_id = int_of_string (read_line ()) in
    if order_id = -1 then Printf.printf "Cancellation aborted.\n"
    else ignore (cancel_order order_id)
  )

let view_book () = 
  print_available_securities ~active_only:true available_securities;
  Printf.printf "\nEnter the security (or 'ALL' to view all): ";
  match String.uppercase_ascii (read_line ()) with  
  | "ALL" -> 
    List.iter (fun security ->
      let ob = get_or_create_order_book security in
      if has_active_orders security then begin
      Printf.printf "\n === Order Book for %s ===\n" security;
        print_orders ob
      end
    ) available_securities
  | security ->
    if List.mem security available_securities then
      let ob = get_or_create_order_book security in
      Printf.printf "\n === Order Book for %s ===\n" security;
      print_orders ob
    else Printf.printf "Invalid security. Please choose from the list above.\n"

let view_my_orders () = 
  with_user_id (fun user_id ->
    print_user_orders user_id
  )

let view_bal () = 
  with_user_id (fun user_id ->
    match get_user_balance user_id with
    | Some balance -> 
      Printf.printf "Cash balance: $%.2f\n" balance;
      Printf.printf "\nPositions:\n";
      (match get_positions_by_user user_id with
      | Ok result ->
        for i = 0 to result#ntuples - 1 do
          Printf.printf "%s: %s shares\n"
            (result#getvalue i 1)  (* security *)
            (result#getvalue i 2)  (* quantity *)
        done
      | Error _ -> Printf.printf "Error fetching positions\n")
    | None -> Printf.printf "User not found\n"
  )

let view_security_info () =
  print_available_securities available_securities;
  Printf.printf "Enter the security (or 'ALL' to view all): ";
  match String.uppercase_ascii (read_line ()) with
  | "ALL" -> List.iter (fun security -> print_security_info security) available_securities
  | security -> print_security_info security

let view_recent_trades () =
  print_available_securities available_securities;
  Printf.printf "Enter the security (or 'ALL' to view all): ";
  match String.uppercase_ascii (read_line ()) with
  | "ALL" -> 
    let has_trades = ref false in
    List.iter (fun security ->
      match get_trades_by_security security with
      | Ok result when result#ntuples > 0 ->
        has_trades := true;
        print_recent_trades security
      | _ -> ()
    ) available_securities;
    if not !has_trades then Printf.printf "No trades found for any security.\n"
  | security -> print_recent_trades security

let load_orders_from_db () =
  List.iter (fun security ->
    let order_book = get_or_create_order_book security in
    match get_active_orders_given_security security with
    | Ok result ->
      for i = 0 to result#ntuples - 1 do
        let order = {
          id = int_of_string (result#getvalue i 0);
          user_id = int_of_string (result#getvalue i 1);
          security = security;
          order_type = string_to_order_type (result#getvalue i 3) (float_of_string (result#getvalue i 6));
          buy_sell = string_to_buy_sell (result#getvalue i 4);
          qty = float_of_string (result#getvalue i 5);
        } in
        match add_order order_book order with
        | Ok _ -> ()
        | Error e -> Printf.printf "Error adding order to order book: %s\n" e
      done
    | Error _ -> ()
  ) available_securities

(* continuously match orders as long as there are any *)
let continuous_matching_thread () =
  while true do
    load_orders_from_db ();
    List.iter (fun security ->
      let ob = get_or_create_order_book security in
      let market_conditions = create_dynamic_market_conditions security in
      let trades = match_orders ob market_conditions in
      List.iter (fun trade -> print_trade trade security) trades
    ) available_securities;
    Unix.sleepf 0.001
  done

(* create some orders for a given security (id's will be negative to indicate they're computer-generated) *)
let initialize_random_orders (security : string) =
  let base_price = get_base_price security in
  let market_conditions = create_dynamic_market_conditions security in
  let create_random_order i =
    let order_id = !computer_order_counter in
    computer_order_counter := order_id - 1;
    let is_buy = i mod 2 = 0 in        (* alternate between buy/sell for computer-generated orders*)
    let computer_user_id = -1000000 - (Random.int 5 + 1) in       (* select from the 5 pre-defined computer uses in test_data.sql *)
    let price = random_price (if is_buy then base_price else base_price +. 1.0) market_conditions.bid_ask_spread in
    let buy_sell = if is_buy then Order_types.Buy else Order_types.Sell in
    let qty = round_quantity (random_float_between 20.0 100.0) in (* a random qty btwn 20 and 100 shares *)
    let order_type = Order_types.Limit {
      price = price;
      expiration = Some (current_time () +. 3600.0)
    } in
    match create_order order_id computer_user_id security order_type buy_sell qty price with
    | Ok _ -> Printf.printf "Created computer order: %d %s %s %.2f shares at $%.2f\n" order_id security (buy_sell_to_string buy_sell) qty price
    | Error e -> Printf.printf "Error creating computer order: %s\n" e
  in
  for i = 1 to 10 do create_random_order i done

let initialize_system () =
  Random.self_init ();
  (* initialize all securities in the database *)
  List.iter (fun security ->
    ignore (create_security security (get_base_price security))
  ) available_securities;
  (* choose 2-3 random securities to trade *)
  let num_securities = 2 + Random.int 2 in
  let selected = List.sort_uniq String.compare (
    List.init num_securities (fun _ -> 
      List.nth available_securities (Random.int (List.length available_securities))
    )) in
  (* have some random initial orders, then match trades *)
  List.iter (fun security ->
    initialize_random_orders security;
    Printf.printf "Market opened for %s\n" security
  ) selected;
  load_orders_from_db ()

let run_cli () = 
  initialize_system ();
  let _ = Thread.create continuous_matching_thread () in (* is this necessary?? to-do *)
  let rec loop () =
    Printf.printf "\n------------------------\n";
    Printf.printf "Select an option:\n";
    Printf.printf "1. Set user ID\n";
    Printf.printf "2. Place order\n";
    Printf.printf "3. Cancel order\n";
    Printf.printf "4. View order book\n";
    Printf.printf "5. View open orders\n";
    Printf.printf "6. View my balance\n";
    Printf.printf "7. View security info\n";
    Printf.printf "8. View recent trades\n";
    Printf.printf "9. Exit\n";
    Printf.printf "------------------------\n";
    match read_line () with
    | "1" -> set_user_id (); loop ()
    | "2" -> place_order_interactive (); loop ()
    | "3" -> cancel_order (); loop ()
    | "4" -> view_book (); loop ()
    | "5" -> view_my_orders (); loop ()
    | "6" -> view_bal (); loop ()
    | "7" -> view_security_info (); loop ()
    | "8" -> view_recent_trades (); loop ()
    | "9" -> Printf.printf "Goodbye! Thanks for trading!\n"
    | _ -> Printf.printf "Invalid option. Please type a valid number.\n"; loop ()
  in loop ()

(* run as executable *)
let () = run_cli ()