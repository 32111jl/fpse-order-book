open Order_book_lib.Order
open Order_book_lib.Order_book

let order_books : (string, order_book) Hashtbl.t = Hashtbl.create 16
let user_balances : (int, float) Hashtbl.t = Hashtbl.create 16

let curr_user_id = ref None

let get_user_balance user_id = 
  try Hashtbl.find user_balances user_id with _ -> 0.0

let update_user_balance user_id amount = 
  let current_balance = get_user_balance user_id in
  Hashtbl.replace user_balances user_id (current_balance +. amount)

let current_time () = Unix.gettimeofday ()

let set_user_id () = 
  Printf.printf "Enter your user ID: ";
  let user_id = int_of_string (read_line ()) in
  curr_user_id := Some user_id

let place_order () = 
  match !curr_user_id with
  | None -> Printf.printf "Please set your user ID first.\n"
  | Some user_id ->
    Printf.printf "Enter the security (eg. AAPL): ";
    let security = read_line () in
    Printf.printf "Enter the order direction (Buy/Sell): ";
    let buy_sell =
      match read_line () with
      | "Buy" -> Buy
      | "Sell" -> Sell
      | _ -> failwith "Invalid order direction. Currently, you may only choose Buy or Sell.\n"
    in
    Printf.printf "Enter the order type (Market/Limit/Margin): ";
    let order_type =
      match String.lowercase_ascii (read_line ()) with
      | "market" -> Market
      | "limit" ->
        Printf.printf "Enter the price: ";
        let curr_price = float_of_string (read_line ()) in
        Limit { price = curr_price; expiration = Some (current_time () +. 3600.0) }
      | "margin" ->
        Printf.printf "Enter the price: ";
        Margin (float_of_string (read_line ()))
      | _ -> failwith "Invalid order type. Currently, you may only choose one of Market, Limit, or Margin.\n"
    in
    Printf.printf "Enter the quantity: ";
    let qty = float_of_string (read_line ()) in
    let total_cost = 
      match order_type with
      | Market | Limit _ -> qty
      | Margin price -> price *. 0.5
    in
    let curr_balance = get_user_balance user_id in
    if buy_sell = Buy && total_cost > curr_balance then
      Printf.printf "Insufficient funds. Please deposit more money.\n"
    else
      let order_book = 
        match Hashtbl.find_opt order_books security with
        | Some ob -> ob
        | None ->
            let new_ob = create_order_book security in
            Hashtbl.add order_books security new_ob;
            new_ob
      in
      let order = create_order (generate_order_id order_book) security order_type buy_sell qty user_id in
      add_order order_book order;
      if buy_sell = Buy then update_user_balance user_id (-. total_cost);
      Printf.printf "Order with ID %d placed!\n" order.id

let cancel_order () = 
  match !curr_user_id with
  | None -> Printf.printf "Please set your user ID first.\n"
  | Some _ ->
    Printf.printf "Enter the order ID to cancel: ";
    let order_id = int_of_string (read_line ()) in
    let order_book_opt = 
      Hashtbl.fold (fun _ ob acc ->
        match acc with
        | Some _ -> acc
        | None when check_order_exists ob order_id -> Some ob
        | None -> None
      ) order_books None
    in
    match order_book_opt with
    | None -> Printf.printf "Order with ID %d not found.\n" order_id
    | Some ob ->
      remove_order ob order_id;
      Printf.printf "Order cancelled.\n"

let get_price_helper order = 
  match get_price order with
  | None -> 0.0  (* or another suitable default for market orders *)
  | Some price -> price

let print_orders ob =
  Printf.printf "Order book for %s:\n" (get_security ob);
  let print_order order = 
    Printf.printf "Price: %f, Qty: %f\n" (get_price_helper order) order.qty
  in
  Printf.printf "Bids:\n";
  List.iter print_order (get_bids ob);
  Printf.printf "Asks:\n";
  List.iter print_order (get_asks ob)

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
    Printf.printf "Your orders:\n";
    Hashtbl.iter (fun _ ob ->
      let orders = List.filter (fun order -> order.user_id = user_id) 
                    (get_bids ob @ get_asks ob) in
      if orders <> [] then begin
        Printf.printf "Orders in %s:\n" (get_security ob);
        List.iter (fun order -> 
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
    ) order_books

let view_bal () = 
  match !curr_user_id with
  | None -> Printf.printf "Please set your user ID first.\n"
  | Some user_id ->
    Printf.printf "Your balance is: %f\n" (get_user_balance user_id)

let run_cli () = 
  let rec loop () =
    Printf.printf "\nSelect an option:\n";
    Printf.printf "1. Set user ID\n";
    Printf.printf "2. Place order\n";
    Printf.printf "3. Cancel order\n";
    Printf.printf "4. View order book\n";
    Printf.printf "5. View my orders\n";
    Printf.printf "6. View my balance\n";
    Printf.printf "7. Exit\n";
    match read_line () with
    | "1" -> set_user_id (); loop ()
    | "2" -> place_order (); loop ()
    | "3" -> cancel_order (); loop ()
    | "4" -> view_book (); loop ()
    | "5" -> view_my_orders (); loop ()
    | "6" -> view_bal (); loop ()
    | "7" -> Printf.printf "Goodbye! Thanks for trading!\n"
    | _ -> Printf.printf "Invalid option. Please try again.\n"; loop ()
  in loop ()