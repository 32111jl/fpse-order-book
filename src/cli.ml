open Cli_helpers
open Order_book_lib.Order_book
open Order_book_lib.Order_types
open Order_book_lib.Ob_utils
open Order_book_lib.Price
let curr_user_id = ref None                      (* current user ID, shouldn't change after setting it *)

(* list of currently-available securities *)
let available_securities = [
  "AAPL"; "MSFT"; "GOOGL"; "AMZN"; "TSLA"; 
  "META"; "NVDA"; "RKLB"; "RIVN"; "PLTR"
]

let with_user_id f = match !curr_user_id with
  | Some user_id -> f user_id
  | None -> Printf.printf "Please set your user ID first.\n"; ()

let get_or_create_order_book (security : string) : order_book =
  match Hashtbl.find_opt order_books security with
  | Some ob -> ob
  | None ->
    let ob = create_order_book security in
    Hashtbl.add order_books security ob;
    ob

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
    Limit { price = float_to_price curr_price; expiration = Some (current_time () +. 3600.0) }
  | "margin" ->
    Printf.printf "Enter the price: ";
    Margin (float_to_price (float_of_string (String.trim (read_line ()))))
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
      place_order_in_db_and_memory security order_type buy_sell qty user_id;
      Printf.printf "Order placed successfully!\n"
    | InvalidMarket msg ->
      Printf.printf "%s\n" msg
    | InvalidFunds (req, avail) ->
      Printf.printf "Insufficient funds. Required: $%.2f, Available: $%.2f.\n" (price_to_float req) (price_to_float avail)
    | InvalidShares (req, avail) ->
      Printf.printf "Insufficient shares. Required: %.2f, Available: %.2f.\n" req avail
    | InvalidPrice msg ->
      Printf.printf "You cannot place an order at this price: %s.\n" msg
    | NoPosition sec ->
      Printf.printf "You don't own any shares of %s!\n" sec
    | InvalidUser ->
      Printf.printf "Invalid user.\n"
    | DatabaseError ->
      Printf.printf "A database error occurred. Please try again.\n"
  )

let view_my_orders () = 
  with_user_id (fun user_id ->
    ignore (print_user_orders user_id)
  )

let view_balance () = 
  with_user_id (fun user_id ->
    view_bal user_id
  )

let cancel_order_interactive () = 
  with_user_id (fun user_id ->
    let has_orders = print_user_orders user_id in
    (* if user has no orders, it'll print "you have no active orders" and not prompt the user any further *)
    if not has_orders then ()
    else begin
      Printf.printf "Enter the order ID to cancel (or -1 to go back): ";
      let order_id = int_of_string (String.trim (read_line ())) in
      if order_id = -1 then Printf.printf "Cancellation aborted.\n"
      else cancel_order order_id
    end
  )

let view_book () = 
  print_available_securities ~active_only:true available_securities;
  Printf.printf "\nEnter the security (or 'ALL' to view all): ";
  match String.uppercase_ascii (String.trim (read_line ())) with  
  | "ALL" -> view_order_book_all ()
  | security ->
    if List.mem security available_securities then
      view_order_book_security security
    else Printf.printf "Invalid security. Please choose from the list above.\n"

let login () =
  Printf.printf "Enter your user ID: ";
  try
    let user_id = int_of_string (String.trim (read_line ())) in
    retrieve_user curr_user_id user_id
  with Failure _ -> 
    Printf.printf "Invalid input. Please enter a valid number.\n";
    None

let create_account () =
  Printf.printf "Enter your name: ";
  let name = String.trim (read_line ()) in (* removes whitespace from beginning/end, but not internal whitespace *)
  if String.length name = 0 then begin
    Printf.printf "Name cannot be empty.\n";
    None
  end else create_user curr_user_id name

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
  Printf.printf "2. View Your Active Orders\n";
  Printf.printf "3. View Your Account Balance and Positions\n";
  Printf.printf "4. Cancel Order\n";
  Printf.printf "5. View Order Book\n";
  Printf.printf "6. Log Out\n";
  Printf.printf "7. Exit\n";
  Printf.printf "------------------------\n";
  match String.trim (read_line ()) with
  | "1" -> place_order_interactive (); trading_menu user_name user_id
  | "2" -> view_my_orders (); trading_menu user_name user_id
  | "3" -> view_balance (); trading_menu user_name user_id
  | "4" -> cancel_order_interactive (); trading_menu user_name user_id
  | "5" -> view_book (); trading_menu user_name user_id
  | "6" -> curr_user_id := None; login_menu ()
  | "7" -> Printf.printf "Goodbye! Thanks for trading!\n"
  | _ -> 
    Printf.printf "Invalid option. Please type a valid number.\n";
    trading_menu user_name user_id

let run_cli () =
  Printf.printf "Loading Trading System...\n%!";
  initialize_system ();
  
  (* Start the matching thread *)
  let _ = Thread.create continuous_matching_thread () in
  
  (* Wait until initial processing is done *)
  Mutex.lock init_mutex;
  while not !initial_processing_done do
    Condition.wait init_cond init_mutex
  done;
  Mutex.unlock init_mutex;
  
  (* Now, start the CLI interaction *)
  login_menu ()

(* run as executable *)
let () = run_cli ()