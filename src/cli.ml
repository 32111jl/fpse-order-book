open Order_book_lib.Order_book
open Order_book_lib.Market_conditions
open Order_book_lib.Matching_engine
open Database.Db
open Printer
open Utils
open Utils.Order_types
open Utils.Order_sync

let order_books = Hashtbl.create 16              (* in-memory list of order books, key: security, value: order_book *)
(* I think in theory this is faster for lookups, but I'm not sure vs just the database. Need to *)
(* let order_book_mutexes = Hashtbl.create 16       in-memory list of order book mutexes, key: security, value: mutex *)
let curr_user_id = ref None                      (* current user ID, shouldn't change after setting it *)

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

(* let get_or_create_mutex (security : string) : Mutex.t =
  match Hashtbl.find_opt order_book_mutexes security with
  | Some mutex -> mutex
  | None ->
    let mutex = Mutex.create () in
    Hashtbl.add order_book_mutexes security mutex;
    mutex *)

let with_user_id f = match !curr_user_id with
  | Some user_id -> f user_id
  | None -> Printf.printf "Please set your user ID first.\n"

let create_dynamic_market_conditions (security : string) : market_conditions =
  let base_price = get_base_price security in
  let spread = base_price *. 0.10 in (* 10% of base price, can be changed *)
  create_market_conditions spread 0.5

let get_or_create_order_book (security : string) : order_book =
  match Hashtbl.find_opt order_books security with
  | Some ob -> ob
  | None ->
    let ob = create_order_book security in
    Hashtbl.add order_books security ob;
    ob

let place_order (security : string) (order_type : order_type) (buy_sell : buy_sell) (qty : float) (user_id : int) =
  sync_ob_operation (fun () ->
    let book = get_or_create_order_book security in
    let order = { id = None; user_id; security; order_type; buy_sell; qty } in
    match add_order book order with
    | Ok result ->
      let order_id = int_of_string (result#getvalue 0 0) in
      (match order_type with
      | Market -> Printf.printf "Market %s order placed for %.2f shares of %s (order ID: %d)\n"
          (if buy_sell = Buy then "BUY" else "SELL") qty security order_id
      | Limit { price; _ } ->Printf.printf "Limit %s order placed: %.2f shares at $%.2f (order ID: %d).\n"
          (if buy_sell = Buy then "BUY" else "SELL") qty price order_id
      | Margin price -> Printf.printf "Margin %s order placed: %.2f shares at $%.2f (order ID: %d).\n"
          (if buy_sell = Buy then "BUY" else "SELL") qty price order_id)
    | Error e -> Printf.printf "Error placing order: %s\n" e
  )

let rec get_security () =
  Printf.printf "Enter the security (eg. AAPL): ";
  let security = String.uppercase_ascii (String.trim (read_line ())) in
  if not (List.mem security available_securities) then begin
    Printf.printf "Invalid security. Please choose from the list above.\n";
    get_security ()
  end else security

let rec get_order_direction () =
  Printf.printf "Enter the order direction (Buy/Sell): ";
  match String.lowercase_ascii (String.trim (read_line ())) with
  | "buy" -> Buy
  | "sell" -> Sell
  | _ -> 
    Printf.printf "Invalid order direction. Please enter either Buy or Sell.\n";
    get_order_direction ()

let rec get_order_type () =
  Printf.printf "Enter the order type (Market/Limit/Margin): ";
  match String.lowercase_ascii (String.trim (read_line ())) with
  | "market" -> Market
  | "limit" ->
    Printf.printf "Enter the price: ";
    let curr_price = float_of_string (String.trim (read_line ())) in
    Limit { price = curr_price; expiration = Some (current_time () +. 3600.0) }
  | "margin" ->
    Printf.printf "Enter the price: ";
    Margin (float_of_string (String.trim (read_line ())))
  | _ -> 
    Printf.printf "Invalid order type. Please enter Market, Limit, or Margin.\n";
    get_order_type ()

let rec get_quantity () =
  Printf.printf "Enter the quantity: ";
  try 
    let qty = float_of_string (String.trim (read_line ())) in
    if qty <= 0.0 then begin
      Printf.printf "Quantity must be positive.\n";
      get_quantity ()
    end else qty
  with Failure _ -> 
    Printf.printf "Invalid quantity. Please enter a valid number.\n";
    get_quantity ()

let has_active_orders (security : string) : bool =
  match get_active_orders_given_security security with
  | Ok result -> result#ntuples > 0
  | Error _ -> false

let place_order_interactive () =
  with_user_id (fun user_id ->
    print_available_securities available_securities;
    print_available_securities ~active_only:true available_securities;
    let security = get_security () in
    let buy_sell = get_order_direction () in
    let order_type = get_order_type () in
    let qty = get_quantity () in
    let book = get_or_create_order_book security in
    match validate_order book user_id security buy_sell order_type qty with
    | Valid ->
      place_order security order_type buy_sell qty user_id;
      Printf.printf "Order placed successfully!\n"
    | InvalidMarket msg ->
      Printf.printf "%s\n" msg
    | InvalidFunds (req, avail) ->
      Printf.printf "Insufficient funds. Required: $%.2f, Available: $%.2f.\n" req avail
    | InvalidShares (req, avail) ->
      Printf.printf "Insufficient shares. Required: %.2f, Available: %.2f.\n" req avail
    | NoPosition sec ->
      Printf.printf "You don't own any shares of %s!\n" sec
    | InvalidUser ->
      Printf.printf "Invalid user.\n"
    | DatabaseError ->
      Printf.printf "A database error occurred. Please try again.\n"
  )

let cancel_order () = 
  with_user_id (fun user_id ->
    let has_orders = print_user_orders user_id in
    (* if user has no orders, it'll print "you have no active orders" and not prompt the user any further *)
    if not has_orders then ()
    else begin
      Printf.printf "Enter the order ID to cancel (or -1 to go back): ";
      let order_id = int_of_string (String.trim (read_line ())) in
      if order_id = -1 then Printf.printf "Cancellation aborted.\n"
      else
        match cancel_order order_id with
        | Ok _ -> Printf.printf "Order %d cancelled successfully!\n" order_id
        | Error e -> Printf.printf "Error cancelling order %d: %s. Try again.\n" order_id e
    end
  )

let view_book () = 
  print_available_securities ~active_only:true available_securities;
  Printf.printf "\nEnter the security (or 'ALL' to view all): ";
  match String.uppercase_ascii (String.trim (read_line ())) with  
  | "ALL" -> 
    List.iter (fun security ->
      sync_ob_operation (fun () ->
        let ob = get_or_create_order_book security in
        if has_active_orders security then begin
        Printf.printf "\n === Order Book for %s ===\n" security;
          print_orders ob
        end
      )
    ) available_securities
  | security ->
    if List.mem security available_securities then
      let ob = get_or_create_order_book security in
      Printf.printf "\n === Order Book for %s ===\n" security;
      print_orders ob
    else Printf.printf "Invalid security. Please choose from the list above.\n"

let view_my_orders () = 
  with_user_id (fun user_id ->
    ignore (print_user_orders user_id)
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

(* let view_security_info () =
  print_available_securities available_securities;
  Printf.printf "Enter the security (or 'ALL' to view all): ";
  match String.uppercase_ascii (read_line ()) with
  | "ALL" -> List.iter (fun security -> print_security_info security) available_securities
  | security -> print_security_info security *)

(* let view_recent_trades () =
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
  | security -> print_recent_trades security *)

let load_orders_from_db () =
  List.iter (fun security ->
    let order_book = get_or_create_order_book security in
    match get_active_orders_given_security security with
    | Ok result ->
      for i = 0 to result#ntuples - 1 do
        let order = {
          id = Some (int_of_string (result#getvalue i 0));
          user_id = int_of_string (result#getvalue i 1);
          security = security;
          order_type = string_to_order_type (result#getvalue i 3) (float_of_string (result#getvalue i 6));
          buy_sell = string_to_buy_sell (result#getvalue i 4);
          qty = float_of_string (result#getvalue i 5);
        } in
        add_order_to_memory order_book order
      done
    | Error _ -> ()
  ) available_securities

(* continuously match orders as long as there are any *)
let continuous_matching_thread () =
  while true do
    (* load_orders_from_db (); *)
    List.iter (fun security ->
      sync_ob_operation (fun () ->
        let ob = get_or_create_order_book security in
        let market_conditions = create_dynamic_market_conditions security in
        let trades = match_orders ob market_conditions in
        List.iter (fun trade -> print_trade trade security) trades
      )
    ) available_securities;
    Unix.sleepf 0.01
  done

let initialize_system () =
  Random.self_init ();
  (* initialize all securities in the database *)
  List.iter (fun security ->
    ignore (create_security security (get_base_price security))
  ) available_securities;
  load_orders_from_db ()

let login () =
  Printf.printf "Enter your user ID: ";
  try
    let user_id = int_of_string (String.trim (read_line ())) in
    match get_user_name user_id with
    | None -> 
      Printf.printf "User ID not found. Please try again.\n";
      None
    | Some name -> 
      curr_user_id := Some user_id;
      Printf.printf "Welcome back, %s!\n" name;
      Some (user_id, name)
  with Failure _ -> 
    Printf.printf "Invalid input. Please enter a valid number.\n";
    None

let create_account () =
  Printf.printf "Enter your name: ";
  let name = String.trim (read_line ()) in (* removes whitespace from beginning/end, but not internal whitespace *)
  if String.length name = 0 then begin
    Printf.printf "Name cannot be empty.\n";
    None
  end else
    try
      let user_id = create_user_in_db name 1000.0 in
      curr_user_id := Some user_id;
      Printf.printf "Welcome, %s! Your user ID is %d. You have a starting balance of $1000.00.\n" name user_id;
      Some (user_id, name)
    with Failure _ -> 
      Printf.printf "Error creating account. Please try again.\n";
      None

let rec login_menu () =
  Printf.printf "\n------------------------\n";
  Printf.printf "Welcome to the Trading System!\n";
  Printf.printf "1. Log In\n";
  Printf.printf "2. Create New Account\n";
  Printf.printf "3. Exit\n";
  Printf.printf "------------------------\n";
  match String.trim (read_line ()) with
  | "1" -> 
    (match login () with
    | Some (user_id, name) -> trading_menu name user_id
    | None -> login_menu ())
  | "2" ->
    (match create_account () with
    | Some (user_id, name) -> trading_menu name user_id
    | None -> login_menu ())
  | "3" -> Printf.printf "Goodbye!\n"
  | _ -> 
    Printf.printf "Invalid option. Please type a valid number.\n";
    login_menu ()

and trading_menu user_name user_id =
  Printf.printf "\n------------------------\n";
  Printf.printf "Welcome, %s (User ID: %d)!\n" user_name user_id;
  Printf.printf "1. Place Order\n";
  Printf.printf "2. View Active Orders\n";
  Printf.printf "3. View Account Balance and Positions\n";
  Printf.printf "4. Cancel Order\n";
  Printf.printf "5. View Order Book\n";
  Printf.printf "6. Log Out\n";
  Printf.printf "7. Exit\n";
  Printf.printf "------------------------\n";
  match String.trim (read_line ()) with
  | "1" -> place_order_interactive (); trading_menu user_name user_id
  | "2" -> view_my_orders (); trading_menu user_name user_id
  | "3" -> view_bal (); trading_menu user_name user_id
  | "4" -> cancel_order (); trading_menu user_name user_id
  | "5" -> view_book (); trading_menu user_name user_id
  | "6" -> curr_user_id := None; login_menu ()
  | "7" -> Printf.printf "Goodbye! Thanks for trading!\n"
  | _ -> 
    Printf.printf "Invalid option. Please type a valid number.\n";
    trading_menu user_name user_id

let run_cli () = 
  initialize_system ();
  let _ = Thread.create continuous_matching_thread () in
  login_menu ()

(* run as executable *)
let () = run_cli ()