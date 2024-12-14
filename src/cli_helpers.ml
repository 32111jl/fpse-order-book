open Order_book_lib.Order_book
open Order_book_lib.Order_types
open Order_book_lib.Matching_engine
open Order_book_lib.Market_conditions
open Order_book_lib.Ob_utils
open Order_book_lib.Price
open Database.Db
open Utils.Order_sync
open Utils.Securities
open Trade

(* This module acts as a bridge between the CLI (user interaction) and underlying order book logic.
   This logic includes database operations (fetching, validation, updating the in-memory order book, etc.). *)

let order_books = Hashtbl.create 16              (* in-memory list of order books, key: security, value: order_book *)

let create_dynamic_market_conditions (security : string) : market_conditions =
  let base_price = get_base_price security in
  let max_spread_deviation = 0.25 *. base_price in (* 25% of base price, can be changed *)
  create_market_conditions max_spread_deviation 0.5

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

let validate_price_spread (security : string) (order_type : order_type) : validation_result =
  match order_type with
  | Market -> Valid (* no need to verify spread for market orders *)
  | Limit { price; _ } | Margin price ->
    let base_price = float_to_price (get_base_price security) in
    let market_conditions = create_dynamic_market_conditions security in
    match check_spread market_conditions base_price price with
    | ValidPrice -> Valid
    | PriceTooHigh (price, max) -> 
      InvalidPrice (Printf.sprintf "Price $%s too high. Maximum allowed: $%s" 
        (price_to_string price) (price_to_string max))
    | PriceTooLow (price, min) ->
      InvalidPrice (Printf.sprintf "Price $%s too low. Minimum allowed: $%s"
        (price_to_string price) (price_to_string min))

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
  | Some balance_float ->
    let balance = float_to_price balance_float in
    let cost = match order_type with
      | Market ->
        if buy_sell = Buy then
          match get_best_ask order_book with
          | Some price -> price * int_of_float qty
          | None -> 0
        else 0
      | Limit { price; _ } -> if buy_sell = Buy then price * int_of_float qty else 0
      | Margin price -> if buy_sell = Buy then price * int_of_float (0.5 *. qty) else 0
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
        | Some shares -> 
          match get_active_orders_given_user_and_security user_id security with
          | Error _ -> DatabaseError
          | Ok orders ->
            (* find if there are other orders with shares up for sale *)
            let allocated_shares = 
              let rec sum_sells i acc =
                if i >= orders#ntuples then acc
                else if orders#getvalue i 4 = "SELL" then
                  sum_sells (i + 1) (acc +. float_of_string (orders#getvalue i 5))
                else
                  sum_sells (i + 1) acc
              in
              sum_sells 0 0.0
            in
            if shares < (allocated_shares +. qty) then begin
              Printf.printf "You already have %.2f shares of %s allocated to other orders. You cannot place another sell order until your other orders are filled or you cancel them.\n" shares security;
              InvalidShares (qty, shares)
            end else Valid)
    else Valid

let validate_order (order_book : order_book) (user_id : int) (security : string) 
  (buy_sell : buy_sell) (order_type : order_type) (qty : float) : validation_result =
  match validate_price_spread security order_type with
  | Valid ->
    (match order_type with
    | Market ->
      (match validate_market_liquidity order_book buy_sell with
      | Error msg -> InvalidMarket msg
      | Ok () -> validate_funds_and_shares order_book user_id security buy_sell order_type qty)
    | Limit _ | Margin _ -> validate_funds_and_shares order_book user_id security buy_sell order_type qty)
  | InvalidPrice _ | InvalidUser | DatabaseError | InvalidMarket _ | InvalidFunds (_, _) | InvalidShares (_, _) | NoPosition _ as e -> e

let cancel_order (order_id : int) =
  match cancel_order order_id with
  | Ok _ ->
    List.iter (fun security ->
      let ob = get_or_create_order_book security in
      remove_order_from_memory ob order_id
    ) available_securities;
    Printf.printf "Order %d cancelled successfully!\n" order_id
  | Error e -> Printf.printf "Error cancelling order %d: %s. Try again.\n" order_id e


let db_result_to_order (result : Postgresql.result) (security : string) : db_order =
  let order_id = int_of_string (result#getvalue 0 0) in
  let user_id = int_of_string (result#getvalue 0 1) in
  let order_type = string_to_order_type (result#getvalue 0 3) (int_of_string (result#getvalue 0 6)) in
  let buy_sell = string_to_buy_sell (result#getvalue 0 4) in
  let qty = float_of_string (result#getvalue 0 5) in
  { id = Some order_id; user_id; security; order_type; buy_sell; qty }

let place_order_in_db_and_memory (security : string) (order_type : order_type) (buy_sell : buy_sell) (qty : float) (user_id : int) =
  sync_ob_operation (fun () ->
    let book = get_or_create_order_book security in
    let order = { id = None; user_id; security; order_type; buy_sell; qty } in
    match create_order_in_db user_id security order_type buy_sell qty (match get_price order with Some p -> p | None -> 0) with
    | Ok result ->
      let order_id = int_of_string (result#getvalue 0 0) in
      (* insert id before adding to memory *)
      let order_with_id = { order with id = Some order_id } in
      (match order_type with
      | Market -> (* execute trades immediately for market orders *)
        add_order_to_memory book order_with_id;
        let trades = match_orders book (create_dynamic_market_conditions security) in
        
        List.iter (fun trade ->
          match (get_order trade.buy_order_id, get_order trade.sell_order_id) with
          | Ok buy_res, Ok sell_res ->
            let buy_order = db_result_to_order buy_res security in
            let sell_order = db_result_to_order sell_res security in
            (match execute_trade buy_order sell_order with
            | Ok (_qty, _price) ->
              print_trade trade security;
            | Error e -> Printf.printf "Trade execution error: %s\n" e)
            | _ -> Printf.printf "Error fetching orders for trade.\n"
        ) trades;
        let still_active = List.exists (fun o -> o.id = Some order_id) book.orders in
        if still_active then begin
          ignore (cancel_order order_id);
          Printf.printf "Market order could not be fully matched and is now cancelled.\n"
        end else
          Printf.printf "Market %s order placed and executed immediately (order ID: %d).\n"
            (if buy_sell = Buy then "BUY" else "SELL") order_id
      | Limit { price = _; _ } | Margin _ ->
        add_order_to_memory book order_with_id;
        Printf.printf "%s %s order placed: %.2f shares at $%.2f (order ID: %d).\n"
          (match order_type with Limit _ -> "Limit" | Margin _ -> "Margin" | _ -> "")
          (if buy_sell = Buy then "BUY" else "SELL") qty 
          (match get_price order_with_id with Some p -> price_to_float p | None -> 0.0) 
          order_id)
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
        order_type = string_to_order_type (result#getvalue i 3) (int_of_string (result#getvalue i 6));
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
          order_type = string_to_order_type (result#getvalue i 3) (int_of_string (result#getvalue i 6));
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

(* continuously match orders as long as there are any *)
let continuous_matching_thread () =
  while true do
    List.iter (fun security ->
      sync_ob_operation (fun () ->
        let ob = get_or_create_order_book security in
        let market_conditions = create_dynamic_market_conditions security in
        let trades = match_orders ob market_conditions in
        List.iter (fun trade ->
          match (get_order trade.buy_order_id, get_order trade.sell_order_id) with
          | Ok buy_res, Ok sell_res ->
            let buy_order = db_result_to_order buy_res security in
            let sell_order = db_result_to_order sell_res security in
            (match execute_trade buy_order sell_order with
            | Ok (_qty, _price) ->
              print_trade trade security;
            | Error e ->
              Printf.printf "Trade execution error: %s\n" e)
          | _ -> Printf.printf "Error fetching orders for trade.\n"
        ) trades
      )
    ) available_securities;
    Unix.sleepf 0.0005
  done