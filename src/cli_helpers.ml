open Order_book_lib.Order_book
open Order_book_lib.Order_types
open Order_book_lib.Matching_engine
open Order_book_lib.Utils
open Database.Db
open Order_sync

(* This module acts as a bridge between the CLI (user interaction) and underlying order book logic.
   This logic includes database operations (fetching, validation, updating the in-memory order book, etc.). *)

let order_books = Hashtbl.create 16              (* in-memory list of order books, key: security, value: order_book *)

let available_securities = [
  "AAPL"; "MSFT"; "GOOGL"; "AMZN"; "TSLA"; 
  "META"; "NVDA"; "RKLB"; "RIVN"; "PLTR"
]

let get_base_price (security : string) : float = match security with
  | "AAPL" -> 150.0 | "MSFT" -> 330.0 | "GOOGL" -> 140.0
  | "AMZN" -> 180.0 | "TSLA" -> 300.0 | "META" -> 300.0
  | "NVDA" -> 400.0 | "RKLB" -> 20.0 | "RIVN" -> 15.0
  | "PLTR" -> 53.0 | _ -> 100.0

let get_or_create_order_book (security : string) : order_book =
  match Hashtbl.find_opt order_books security with
  | Some ob -> ob
  | None ->
    let ob = create_order_book security in
    Hashtbl.add order_books security ob;
    ob

let has_active_orders (security : string) : bool =
  match get_active_orders_given_security security with
  | Ok result -> result#ntuples > 0
  | Error _ -> false

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

let validate_market_liquidity (book : order_book) (buy_sell : buy_sell) : (unit, string) result =
  match buy_sell with
  (* for market orders, check if there are any orders in the order book *)
  | Buy ->
    if get_best_ask book = None then Error (Printf.sprintf "Cannot place market buy order: No asks available for %s" book.security)
    else Ok ()
  | Sell ->
    if get_best_bid book = None then Error (Printf.sprintf "Cannot place market sell order: No bids available for %s" book.security)
    else Ok ()
    
let validate_funds_and_shares (order_book : order_book) (user_id : int) (security : string) (buy_sell : buy_sell) (order_type : order_type) (qty : float) : validation_result =
  match get_user_balance user_id with
  | None -> InvalidUser
  | Some balance ->
    let cost = match order_type with
      | Market ->
        if buy_sell = Buy then
          match get_best_ask order_book with
          | Some price -> price *. qty
          | None -> 0.0
        else 0.0
      | Limit { price; _ } -> if buy_sell = Buy then price *. qty else 0.0
      | Margin price -> if buy_sell = Buy then price *. 0.5 else 0.0
    in
    (* check if user has enough money to place the order *)
    if buy_sell = Buy && cost > balance then
      InvalidFunds (cost, balance)
    else if buy_sell = Sell then
      (* check if user has enough shares to place the order *)
      match get_positions_by_user user_id with
      | Error _ -> DatabaseError
      | Ok result ->
        let rec find_security i =
          if i >= result#ntuples then None
          else 
            let sec = result#getvalue i 1 in
            if String.uppercase_ascii sec = security then Some (float_of_string (result#getvalue i 2))
            else find_security (i + 1)
        in
        (match find_security 0 with
        | None -> NoPosition security
        | Some shares -> if shares < qty then InvalidShares (qty, shares) else Valid)
    else Valid

let validate_order (order_book : order_book) (user_id : int) (security : string) (buy_sell : buy_sell) (order_type : order_type) (qty : float) : validation_result =
  match order_type with
  | Market ->
    (match validate_market_liquidity order_book buy_sell with
    | Error msg -> InvalidMarket msg
    | Ok () -> validate_funds_and_shares order_book user_id security buy_sell order_type qty)
  | Limit _ | Margin _ -> validate_funds_and_shares order_book user_id security buy_sell order_type qty

let execute_trade (buy_order : db_order) (sell_order : db_order) : (float * float, string) result =
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

let add_order (book : order_book) (order : db_order) : (Postgresql.result, string) result =
  match create_order_in_db order.user_id order.security order.order_type order.buy_sell order.qty (match get_price order with Some p -> p | None -> 0.0) with
  | Ok result ->
    add_order_to_memory book order;
    Ok result
  | Error e -> Error e

let cancel_order (order_id : int) =
  match cancel_order order_id with
  | Ok _ ->
    List.iter (fun security ->
      let ob = get_or_create_order_book security in
      remove_order_from_memory ob order_id
    ) available_securities;
    Printf.printf "Order %d cancelled successfully!\n" order_id
  | Error e -> Printf.printf "Error cancelling order %d: %s. Try again.\n" order_id e

let place_order_in_db_and_memory (security : string) (order_type : order_type) (buy_sell : buy_sell) (qty : float) (user_id : int) =
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

let remove_order_id_from_memory order_id =
  List.iter (fun sec ->
    let ob = get_or_create_order_book sec in
    remove_order_from_memory ob order_id
  ) available_securities

let refresh_and_print_book_for_security (security : string) =
  let ob = get_or_create_order_book security in
  ob.orders <- [];
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
      add_order_to_memory ob order
    done;
    if ob.orders <> [] then begin
      Printf.printf "\n === Order Book for %s ===\n" security;
      print_orders ob
    end else
      Printf.printf "\nNo active orders for %s.\n" security
  | Error _ -> Printf.printf "Error loading orders for %s\n" security

let load_orders_from_db_into_memory () =
  List.iter (fun security ->
    let ob = get_or_create_order_book security in
    ob.orders <- [];
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
        add_order_to_memory ob order
      done
    | Error _ -> ()
  ) available_securities

let view_bal (user_id : int) = 
  match get_user_balance user_id with
  | Some balance -> 
    Printf.printf "Cash balance: $%.2f\n" balance;
    Printf.printf "\nPositions:\n";
    (match get_positions_by_user user_id with
    | Ok result ->
      if result#ntuples = 0 then Printf.printf "No positions found.\n"
      else
      for i = 0 to result#ntuples - 1 do
        Printf.printf "%s: %s shares\n"
          (result#getvalue i 1)  (* security *)
          (result#getvalue i 2)  (* quantity *)
      done
    | Error _ -> Printf.printf "Error fetching positions\n")
  | None -> Printf.printf "User not found\n"

let view_order_book_all () =
  List.iter (fun security ->
    sync_ob_operation (fun () ->
      refresh_and_print_book_for_security security
    )
  ) available_securities

let view_order_book_security security =
  sync_ob_operation (fun () ->
    refresh_and_print_book_for_security security
  )

let initialize_system () =
  Random.self_init ();
  (* initialize all securities in the database *)
  List.iter (fun security ->
    ignore (create_security security (get_base_price security))
  ) available_securities;
  load_orders_from_db_into_memory ()

let retrieve_user (curr_user_id : int option ref) (user_id : int) =
  match get_user_name user_id with
  | None -> 
    Printf.printf "User ID not found. Please try again.\n";
    None
  | Some name -> 
    curr_user_id := Some user_id;
    Printf.printf "Welcome back, %s!\n" name;
    Some (user_id, name)

let create_user (curr_user_id : int option ref) (name : string) =
  try
    let user_id = create_user_in_db name 1000.0 in
    curr_user_id := Some user_id;
    Printf.printf "Welcome, %s! Your user ID is %d. You have a starting balance of $1000.00.\n" name user_id;
    Some (user_id, name)
  with Failure _ -> 
    Printf.printf "Error creating account. Please try again.\n";
    None