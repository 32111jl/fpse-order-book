(* open Order_book_lib.Order *)
open Order_book_lib.Order_book
open Order_types
open Order_book_lib.Market_conditions
open Order_book_lib.Matching_engine
(* open Order_book_lib.User *)
open Database.Db
open Printer

(* global variables to store the order books and current user ID *)
(* let order_books : (string, order_book) Hashtbl.t = Hashtbl.create 16
let curr_user_id = ref None
let available_securities = [
  "AAPL"; "MSFT"; "GOOGL"; "AMZN"; "TSLA"; 
  "META"; "NVDA"; "RKLB"; "RIVN"; "PLTR"
]

let current_time () = Unix.gettimeofday ()

let set_user_id () = 
  match !curr_user_id with
  | Some id -> Printf.printf "User ID already set to %d. You can't change your user ID once set.\n" id
  | None ->
    Printf.printf "Enter your user ID: ";
    let user_id = int_of_string (read_line ()) in
    curr_user_id := Some user_id;
    (* create user if they don't exist yet *)
    if get_balance user_id = 0.0 then begin
      ignore (create_user user_id 1000.0);
      Printf.printf "New user created with initial balance of $1000.00.\n"
    end

(* base prices for a security *)
let get_base_price (security : string) : float = match security with
  | "AAPL" -> 150.0 | "MSFT" -> 330.0 | "GOOGL" -> 140.0
  | "AMZN" -> 180.0 | "TSLA" -> 300.0 | "META" -> 300.0
  | "NVDA" -> 400.0 | "RKLB" -> 20.0 | "RIVN" -> 15.0
  | "PLTR" -> 53.0 | _ -> 100.0

let create_dynamic_market_conditions (security : string) : market_conditions =
  let base_price = get_base_price security in
  let spread = base_price *. 0.10 in (* 10% of base price, can be changed *)
  create_market_conditions spread 0.5

let get_or_create_order_book (security : string) : order_book =
  match Hashtbl.find_opt order_books security with
  | Some ob -> ob
  | None ->
    let new_ob = create_order_book security in
    Hashtbl.add order_books security new_ob;
    new_ob

let execute_trade (order_book : order_book) (order1 : order) (order2 : order) =
  (* calculate trade quantities and price *)
  let trade_qty = min order1.qty order2.qty in
  let trade_price = get_price_helper order2 in
  let total_cost = trade_price *. trade_qty in
  (* determine buyer and seller based on order type *)
  let (buyer, seller) = match order1.buy_sell with
    | Buy -> (order1, order2)
    | Sell -> (order2, order1)
  in
  (* update balances and positions for both buyer and seller*)
  update_balance buyer.user_id (-.total_cost);
  update_position buyer.user_id buyer.security trade_qty;
  update_balance seller.user_id total_cost;
  update_position seller.user_id seller.security (-.trade_qty);

  (* update order quantities *)
  buyer.qty <- buyer.qty -. trade_qty;
  seller.qty <- seller.qty -. trade_qty;

  (* remove order if it's filled *)
  if buyer.qty <= 0.0 then remove_order order_book buyer.id;
  if seller.qty <= 0.0 then remove_order order_book seller.id;
  (trade_qty, trade_price)

let handle_market_order order_book order =
  match order.buy_sell with
  (* buy order: match with best ask *)
  | Buy -> (
    match get_best_ask order_book with
    | Some price ->
      let best_ask_order = List.find (fun order -> get_price order = Some price) (get_asks order_book) in
      ignore (execute_trade order_book order best_ask_order)
    | None -> Printf.printf "No asks to match market buy.\n"
  )
  (* sell order: match with best bid *)
  | Sell -> (
    match get_best_bid order_book with
    | Some price ->
      let best_bid_order = List.find (fun order -> get_price order = Some price) (get_bids order_book) in
      ignore (execute_trade order_book order best_bid_order)
    | None -> Printf.printf "No bids to match market sell.\n"
  )

let handle_other_order order_book order =
  match order.buy_sell with
  (* buy order: match with best ask *)
  | Buy -> (
    match get_best_ask order_book with
    | Some ask_price ->
      let our_price = get_price_helper order in
      if our_price < ask_price then add_order order_book order
      else
        let best_ask_order = List.find (fun order -> get_price order = Some ask_price) (get_asks order_book) in
        ignore (execute_trade order_book order best_ask_order)
    | None -> add_order order_book order
  )
  (* sell order: match with best bid *)
  | Sell -> (
    match get_best_bid order_book with
    | Some bid_price ->
      let our_price = get_price_helper order in
      if our_price > bid_price then add_order order_book order
      else
        let best_bid_order = List.find (fun order -> get_price order = Some bid_price) (get_bids order_book) in
        ignore (execute_trade order_book order best_bid_order)
    | None -> add_order order_book order
  )

let place_order (security : string) (order_type : order_type) (buy_sell : buy_sell) (qty : float) (user_id : int) =
  let order_book = get_or_create_order_book security in
  let order = create_order (generate_order_id order_book) security order_type buy_sell qty user_id in
  (* match market orders immediately, otherwise wait for a match *)
  match order_type with
  | Market -> handle_market_order order_book order
  | Limit _ | Margin _ -> handle_other_order order_book order

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

let rec place_order_interactive () =
  match !curr_user_id with
  | None -> Printf.printf "Please set your user ID first.\n"
  | Some user_id ->
    (* show market prices for all securities *)
    print_market_prices order_books;
    Printf.printf "Enter the security (eg. AAPL): ";
    let security = read_line () in
    (* show only specific security's order book after it's been selected*)
    (match Hashtbl.find_opt order_books security with
    | Some ob -> print_orders ob
    | None -> (
      Printf.printf "No orders yet for %s. Try typing a different security with orders.\n" security);
      place_order_interactive ()
      );
    (* get order details *)
    let buy_sell = get_order_direction () in
    let order_type = get_order_type () in
    let qty = get_quantity () in
    
    (* check if we can afford the order *)
    let estimated_cost = match order_type with
    | Market -> 
      (* use best ask if buying, best bid if selling *)
      let ob = get_or_create_order_book security in
      if buy_sell = Buy then
        match get_best_ask ob with
        | Some price -> price *. qty
        | None -> 0.0
      else 0.0
    | Limit { price; _ } -> if buy_sell = Buy then price *. qty else 0.0
    | Margin price -> price *. 0.5 (* 50% margin requirement to buy *)
    in
    let curr_balance = get_balance user_id in
    if buy_sell = Buy && estimated_cost > curr_balance then
      Printf.printf "Insufficient funds. Required: $%.2f, Available: $%.2f\n" estimated_cost curr_balance
    else place_order security order_type buy_sell qty user_id

let print_user_orders (user_id : int) =
  (* get all orders for the user *)
  let user_orders = ref [] in
  Hashtbl.iter (fun security ob ->
    let orders = List.filter (fun order -> order.user_id = user_id) (get_bids ob @ get_asks ob) in
    List.iter (fun order -> 
      user_orders := (security, order) :: !user_orders
    ) orders
  ) order_books;
  (* print orders if there are any *)
  if !user_orders = [] then
    Printf.printf "You have no active orders!\n"
  else begin
    Printf.printf "\nYour Active Orders:\n";
    Printf.printf "------------------------\n";
    List.iter (fun (security, (order : order)) -> 
      Printf.printf "ID: %d, %s: %s order for %.2f shares at $%.2f\n" 
        order.id 
        security
        (match order.buy_sell with Buy -> "Buy" | Sell -> "Sell")
        order.qty
        (get_price_helper order)
    ) !user_orders;
    Printf.printf "------------------------\n"
  end

let cancel_order () = 
  match !curr_user_id with
  | None -> Printf.printf "Please set your user ID first!\n"
  | Some user_id ->
    (* print user's orders to choose from *)
    print_user_orders user_id;
    Printf.printf "Enter the order ID to cancel (or 0 to go back): ";
    let order_id = int_of_string (read_line ()) in
    if order_id = 0 then
      Printf.printf "Cancellation aborted.\n"
    else
      (* find the order to cancel *)
      let order_book_and_order_opt = 
        Hashtbl.fold (fun _ ob acc ->
          match acc with
          | Some _ -> acc
          | None -> 
            let orders = get_bids ob @ get_asks ob in
            match List.find_opt (fun (order : order) -> 
              order.id = order_id && order.user_id = user_id) orders with
            | Some order -> Some (ob, order)
            | None -> None
        ) order_books None
      in
      (* cancel the order if it exists *)
      match order_book_and_order_opt with
      | None -> Printf.printf "Order with ID %d not found or doesn't belong to you!\n" order_id
      | Some (ob, _) ->
        remove_order ob order_id;
        Printf.printf "Order cancelled.\n"

let get_price_helper order = 
  match get_price order with
  | None -> 0.0  (* or another suitable default for market orders *)
  | Some price -> price

let view_book () = 
  Printf.printf "Enter the security you want to view, or 'ALL' to view all securities: ";
  match read_line () with
  | "ALL" -> Hashtbl.iter (fun _ ob -> print_orders ob) order_books
  | security ->
      match Hashtbl.find_opt order_books security with
      | None -> Printf.printf "No order book found for %s.\n" security
      | Some ob -> print_orders ob

let view_my_orders () = 
  match !curr_user_id with
  | None -> Printf.printf "Please set your user ID first.\n"
  | Some user_id ->
    let user_orders = ref false in
    (* get all orders, print them *)
    Hashtbl.iter (fun _ ob ->
      let orders = List.filter (fun order -> order.user_id = user_id) 
                    (get_bids ob @ get_asks ob) in
      if orders <> [] then begin
        user_orders := true;
        Printf.printf "Orders in %s:\n" (get_security ob);
        List.iter (fun (order : order) -> 
          Printf.printf "ID: %d, Type: %s, Price: %.2f, Qty: %.2f\n" 
            order.id 
            (match order.order_type with
              | Market -> "Market"
              | Limit _ -> "Limit"
              | Margin _ -> "Margin")
            (get_price_helper order) 
            order.qty
        ) orders
      end
    ) order_books;
    if not !user_orders then Printf.printf "You have no active orders!\n"

let view_bal () = 
  match !curr_user_id with
  | None -> Printf.printf "Please set your user ID first.\n"
  | Some user_id ->
    Printf.printf "Cash balance: $%.2f\n" (get_balance user_id);
    Printf.printf "\nPositions:\n";
    (* if user holds any positions, print them too *)
    Hashtbl.iter (fun security _ ->
      let pos = get_position user_id security in
      if pos <> 0.0 then
        Printf.printf "%s: %.2f shares\n" security pos
    ) order_books

let continuous_matching_thread () =
  (* continuously match orders as long as there are any *)
  while true do
    let all_books = Hashtbl.to_seq_values order_books |> List.of_seq in
    List.iter (fun ob ->
      let security = get_security ob in
      let market_conditions = create_dynamic_market_conditions security in
      let trades = match_orders ob market_conditions in
      List.iter (fun trade -> print_trade trade security) trades
    ) all_books;
    Unix.sleepf 0.001
  done

let random_price base = 
  base +. (Random.float 10.0) -. 5.0

let initialize_random_orders security =
  let ob = create_order_book security in
  let base_price = get_base_price security in
  let create_random_order id is_buy =
    let price = random_price (if is_buy then base_price else base_price +. 1.0) in
    let order_id = if is_buy then id else id + 100 in
    create_order order_id security 
    (Limit { price = price; expiration = Some (current_time () +. 3600.0) })
    (if is_buy then Buy else Sell)
    (Random.float 100.0) 
    (-id) (* negative id to indicate fake user *)
  in
  for i = 1 to 5 do
    add_order ob (create_random_order i true);  (* buy order *)
    add_order ob (create_random_order i false)  (* sell order *)
  done;
  ob

let initialize_system () =
  Random.self_init ();
  (* choose 2-3 random securities to trade *)
  let num_securities = 2 + Random.int 2 in
  let selected = List.sort_uniq String.compare (
    List.init num_securities (fun _ -> 
      List.nth available_securities (Random.int (List.length available_securities))
    )) in
  (* have some random initial orders, then match trades *)
  List.iter (fun security ->
    let ob = initialize_random_orders security in
    Hashtbl.add order_books security ob;
    Printf.printf "Market opened for %s\n" security
  ) selected;
  ignore (create_user 1 1000.0);
  Printf.printf "Initial balance set to $1000.00\n"

let run_cli () = 
  initialize_system ();
  let _ = Thread.create continuous_matching_thread () in
  let rec loop () =
    Printf.printf "\n------------------------\n";
    Printf.printf "Select an option:\n";
    Printf.printf "1. Set user ID\n";
    Printf.printf "2. Place order\n";
    Printf.printf "3. Cancel order\n";
    Printf.printf "4. View order book\n";
    Printf.printf "5. View open orders\n";
    Printf.printf "6. View my balance\n";
    Printf.printf "7. Exit\n";
    Printf.printf "------------------------\n";
    match read_line () with
    | "1" -> set_user_id (); loop ()
    | "2" -> place_order_interactive (); loop ()
    | "3" -> cancel_order (); loop ()
    | "4" -> view_book (); loop ()
    | "5" -> view_my_orders (); loop ()
    | "6" -> view_bal (); loop ()
    | "7" -> Printf.printf "Goodbye! Thanks for trading!\n"
    | _ -> Printf.printf "Invalid option. Please type a valid number.\n"; loop ()
  in loop ()

(* run as executable *)
let () = run_cli () *)

let curr_user_id = ref None
let available_securities = [
  "AAPL"; "MSFT"; "GOOGL"; "AMZN"; "TSLA"; 
  "META"; "NVDA"; "RKLB"; "RIVN"; "PLTR"
]

let current_time () = Unix.gettimeofday ()

let get_base_price (security : string) : float = match security with
  | "AAPL" -> 150.0 | "MSFT" -> 330.0 | "GOOGL" -> 140.0
  | "AMZN" -> 180.0 | "TSLA" -> 300.0 | "META" -> 300.0
  | "NVDA" -> 400.0 | "RKLB" -> 20.0 | "RIVN" -> 15.0
  | "PLTR" -> 53.0 | _ -> 100.0

let create_dynamic_market_conditions (security : string) : market_conditions =
  let base_price = get_base_price security in
  let spread = base_price *. 0.10 in (* 10% of base price, can be changed *)
  create_market_conditions spread 0.5

let get_or_create_order_book security =
  create_order_book security

let place_order security order_type buy_sell qty user_id =
  let _order_book = get_or_create_order_book security in
  let price = match order_type with
    | Market -> 0.0
    | Limit { price; _ } -> price
    | Margin price -> price
  in
  ignore (create_order ~id:0 ~user_id ~security ~order_type ~buy_sell ~qty ~price)

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

let set_user_id () = 
  match !curr_user_id with
  | Some id -> Printf.printf "User ID already set to %d\n" id
  | None ->
    Printf.printf "Enter your user ID: ";
    let user_id = int_of_string (read_line ()) in
    curr_user_id := Some user_id;
    match get_user_balance user_id with
    | None -> 
      ignore (create_user_in_db ~id:user_id ~name:"" ~balance:1000.0);
      Printf.printf "New user created with initial balance of $1000.00\n"
    | Some _ -> 
      Printf.printf "Welcome back!\n"

let place_order_interactive () =
  match !curr_user_id with
  | None -> Printf.printf "Please set your user ID first.\n"
  | Some user_id ->
    Printf.printf "Enter the security (eg. AAPL): ";
    let security = read_line () in
    let buy_sell = get_order_direction () in
    let order_type = get_order_type () in
    let qty = get_quantity () in
    
    (* check if we can afford it *)
    let estimated_cost = match order_type with
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
    match get_user_balance user_id with
    | Some balance when buy_sell = Buy && estimated_cost > balance ->
      Printf.printf "Insufficient funds. Required: $%.2f, Available: $%.2f\n"  estimated_cost balance
    | Some _ -> place_order security order_type buy_sell qty user_id
    | None -> Printf.printf "Can't place order, user not found.\n"

let cancel_order () = 
  match !curr_user_id with
  | None -> Printf.printf "Please set your user ID first!\n"
  | Some user_id ->
    print_user_orders user_id;
    Printf.printf "Enter the order ID to cancel (or 0 to go back): ";
    let order_id = int_of_string (read_line ()) in
    if order_id = 0 then Printf.printf "Cancellation aborted.\n"
    else ignore (cancel_order order_id)

let view_book () = 
  Printf.printf "Enter the security: ";
  let security = read_line () in
  let ob = get_or_create_order_book security in
  print_orders ob

let view_my_orders () = 
  match !curr_user_id with
  | None -> Printf.printf "Please set your user ID first.\n"
  | Some user_id -> print_user_orders user_id

let view_bal () = 
  match !curr_user_id with
  | None -> Printf.printf "Please set your user ID first.\n"
  | Some user_id ->
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

let view_security_info () =
  Printf.printf "Enter the security: ";
  let security = read_line () in
  print_security_info security

let view_recent_trades () =
  Printf.printf "Enter the security: ";
  let security = read_line () in
  print_recent_trades security

let continuous_matching_thread () =
  (* continuously match orders as long as there are any *)
  while true do
    List.iter (fun security ->
      let ob = get_or_create_order_book security in
      let market_conditions = create_dynamic_market_conditions security in
      let trades = match_orders ob market_conditions in
      List.iter (fun trade -> print_trade trade security) trades
    ) available_securities;
    Unix.sleepf 0.001
  done

let random_price base = base +. (Random.float 10.0) -. 5.0

let initialize_random_orders security =
  let base_price = get_base_price security in
  let create_random_order id is_buy =
    let price = random_price (if is_buy then base_price else base_price +. 1.0) in
    let order_id = if is_buy then id else id + 100 in
    let buy_sell = if is_buy then Buy else Sell in
    let qty = Random.float 100.0 in
    let order_type = Limit { price = price; expiration = Some (current_time () +. 3600.0) } in
    ignore (create_order ~id:order_id ~user_id:(-id) ~security ~order_type ~buy_sell ~qty ~price)
  in
  for i = 1 to 5 do
    create_random_order i true;  (* buy order *)
    create_random_order i false  (* sell order *)
  done

let initialize_system () =
  Random.self_init ();
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
  match create_user_in_db ~id:1 ~name:"" ~balance:1000.0 with
  | Ok _ -> Printf.printf "Initial balance set to $1000.00\n"
  | Error e -> Printf.printf "Error creating initial user: %s\n" e

let run_cli () = 
  initialize_system ();
  let _ = Thread.create continuous_matching_thread () in
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