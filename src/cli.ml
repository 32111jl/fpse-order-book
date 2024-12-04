open Order_book_lib.Order
open Order_book_lib.Order_book
open Order_book_lib.Market_conditions
open Order_book_lib.Matching_engine
open Order_book_lib.User
open Printer

let order_books : (string, order_book) Hashtbl.t = Hashtbl.create 16

let curr_user_id = ref None

let current_time () = Unix.gettimeofday ()

let set_user_id () = 
  match !curr_user_id with
  | Some id -> Printf.printf "User ID already set to %d. You can't change your user ID once set.\n" id
  | None ->
    Printf.printf "Enter your user ID: ";
    let user_id = int_of_string (read_line ()) in
    curr_user_id := Some user_id;
    if get_balance user_id = 0.0 then begin
      ignore (create_user user_id 1000.0);
      Printf.printf "New user created with initial balance of $1000.00\n"
    end

let get_base_price (security : string) : float = match security with
  | "AAPL" -> 150.0 | "MSFT" -> 330.0 | "GOOGL" -> 140.0
  | "AMZN" -> 180.0 | "TSLA" -> 300.0 | "META" -> 300.0
  | "NVDA" -> 400.0 | "RKLB" -> 20.0 | "RIVN" -> 15.0
  | "PLTR" -> 53.0 | _ -> 100.0

let create_dynamic_market_conditions (security : string) : market_conditions =
  let base_price = get_base_price security in
  let spread = base_price *. 0.10 in (* 10% of base price, can be changed *)
  create_market_conditions spread 0.5

(* let print_trade (trade : trade) (security : string) =
  Printf.printf "Trade executed: %f units of %s between orders %d and %d.\n" 
                trade.trade_qty security trade.buy_order_id trade.sell_order_id *)

let get_or_create_order_book (security : string) : order_book =
  match Hashtbl.find_opt order_books security with
  | Some ob -> ob
  | None ->
    let new_ob = create_order_book security in
    Hashtbl.add order_books security new_ob;
    new_ob

let place_order (security : string) (order_type : order_type) (buy_sell : buy_sell) (qty : float) (user_id : int) =
  let order_book = get_or_create_order_book security in
  let order = create_order (generate_order_id order_book) security order_type buy_sell qty user_id in
  
  (* For market orders, we need to check if there's enough balance based on the best available price *)
  if buy_sell = Buy && order_type = Market then
    match get_best_ask order_book with
    | None -> 
      Printf.printf "No asks available for Market Buy order. Try a Limit order instead.\n";
      ()
    | Some price ->
      let cost = price *. qty in
      if cost > get_balance user_id then
        Printf.printf "Insufficient funds for market buy order. Required: $%.2f, Available: $%.2f\n" 
          cost (get_balance user_id)
      else begin
        add_order order_book order;
        let market_conditions = create_dynamic_market_conditions security in
        let trades = match_orders order_book market_conditions in
        List.iter (fun trade -> print_trade trade security) trades
      end
  else begin
    add_order order_book order;
    let market_conditions = create_dynamic_market_conditions security in
    let trades = match_orders order_book market_conditions in
    List.iter (fun trade -> print_trade trade security) trades
  end

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

(* let print_market_prices () =
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

let print_orders ob =
  Printf.printf "\nOrder book for %s:\n" (get_security ob);
  Printf.printf "Bids:\n";
  let bids = get_bids ob in
  if bids = [] then Printf.printf "No bids\n"
  else
    List.iter (fun (order : order) -> 
      Printf.printf "Price: $%.2f, Qty: %.2f\n" (get_price_helper order) order.qty
    ) bids;
  Printf.printf "\nAsks:\n";
  let asks = get_asks ob in
  if asks = [] then Printf.printf "No asks\n"
  else
    List.iter (fun order -> 
      Printf.printf "Price: $%.2f, Qty: %.2f\n" (get_price_helper order) order.qty
    ) asks;
  Printf.printf "------------------------\n" *)

let place_order_interactive () =
  match !curr_user_id with
  | None -> Printf.printf "Please set your user ID first.\n"
  | Some user_id ->
    print_market_prices order_books;
    Printf.printf "Enter the security (eg. AAPL): ";
    let security = read_line () in
    (* show specific security's order book *)
    (match Hashtbl.find_opt order_books security with
    | Some ob -> print_orders ob
    | None ->
      Printf.printf "No orders yet for %s.\n" security);
      Hashtbl.remove order_books security; (* temporary fix bc otherwise nonexistent security will be created *)
    let buy_sell = get_order_direction () in
    let order_type = get_order_type () in
    let qty = get_quantity () in
    
    (* get estimated cost based on order type *)
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
    | Margin price -> price *. 0.5
    in
    let curr_balance = get_balance user_id in
    if buy_sell = Buy && estimated_cost > curr_balance then
      Printf.printf "Insufficient funds. Required: $%.2f, Available: $%.2f\n" estimated_cost curr_balance
    else place_order security order_type buy_sell qty user_id

let print_user_orders (user_id : int) =
  let user_orders = ref [] in
  Hashtbl.iter (fun security ob ->
    let orders = List.filter (fun order -> order.user_id = user_id) (get_bids ob @ get_asks ob) in
    List.iter (fun order -> 
      user_orders := (security, order) :: !user_orders
    ) orders
  ) order_books;
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
    print_user_orders user_id;
    Printf.printf "Enter the order ID to cancel (or 0 to go back): ";
    let order_id = int_of_string (read_line ()) in
    if order_id = 0 then
      Printf.printf "Cancellation aborted.\n"
    else
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
      match order_book_and_order_opt with
      | None -> Printf.printf "Order with ID %d not found or doesn't belong to you!\n" order_id
      | Some (ob, _) ->
        remove_order ob order_id;
        Printf.printf "Order cancelled.\n"

let get_price_helper order = 
  match get_price order with
  | None -> 0.0  (* or another suitable default for market orders *)
  | Some price -> price

  (* let print_orders ob =
  Printf.printf "Order book for %s:\n" (get_security ob);
  let print_order order = 
    Printf.printf "Price: %f, Qty: %f\n" (get_price_helper order) order.qty
  in
  Printf.printf "Bids:\n";
  List.iter print_order (get_bids ob);
  Printf.printf "Asks:\n";
  List.iter print_order (get_asks ob) *)

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
    Hashtbl.iter (fun _ ob ->
      let orders = List.filter (fun order -> order.user_id = user_id) 
                    (get_bids ob @ get_asks ob) in
      if orders <> [] then begin
        user_orders := true;
        Printf.printf "Orders in %s:\n" (get_security ob);
        List.iter (fun (order : order) -> 
          Printf.printf "ID: %d, Type: %s, Price: %f, Qty: %f\n" 
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
    if not !user_orders then Printf.printf "You have no orders!\n"

let view_bal () = 
  match !curr_user_id with
  | None -> Printf.printf "Please set your user ID first.\n"
  | Some user_id ->
    Printf.printf "Cash balance: $%.2f\n" (get_balance user_id);
    Printf.printf "\nPositions:\n";
    Hashtbl.iter (fun security _ ->
      let pos = get_position user_id security in
      if pos <> 0.0 then
        Printf.printf "%s: %.2f shares\n" security pos
    ) order_books

let continuous_matching_thread () =
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

let available_securities = [
  "AAPL"; "MSFT"; "GOOGL"; "AMZN"; "TSLA"; 
  "META"; "NVDA"; "RKLB"; "RIVN"; "PLTR"
]

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
    (-id)
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
    Printf.printf "5. View my orders\n";
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
let () = run_cli ()