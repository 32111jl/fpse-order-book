open Order_book_lib.Order
open Order_book_lib.Order_book
open Unix

let order_books = Hashtbl.create 16
let user_balances = Hashtbl.create 16

let curr_user_id = ref None

let get_user_balance user_id = 
  try Hashtbl.find user_balances user_id with _ -> 0.0

let update_user_balance user_id amount = 
  let current_balance = get_user_balance user_id in
  Hashtbl.replace user_balances user_id (current_balance +. amount)

let current_time () = 
  let tm = Unix.localtime (Unix.time ()) in
  let hour = tm.tm_hour in
  let min = tm.tm_min in
  let sec = tm.tm_sec in
  let ms = Unix.gettimeofday () |> mod_float 1.0 |> ( *. ) 1000.0 |> int_of_float in
  let time = float_of_int (hour * 3600 + min * 60 + sec) +. float_of_int ms in
  time

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
    let order_dir = read_line () in
    let buy_sell =
      match order_dir with
      | "Buy" -> Buy
      | "Sell" -> Sell
      | _ -> failwith "Invalid order direction. Currently, you may only choose Buy or Sell.\n"
    in
    Printf.printf "Enter the order type (Market/Limit/Margin): ";
    let order_type_str = read_line () in
    let order_type =
      match order_type_str with
      | "Market" -> Market
      | "Limit" ->
        Printf.printf "Enter the price: ";
        let curr_price = float_of_string (read_line ()) in
        Limit { price = curr_price; expiration = Some (current_time () +. 3600.0) }
      | "Margin" ->
        Printf.printf "Enter the price: ";
        let curr_price = float_of_string (read_line ()) in
        Margin curr_price
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
      let order = create_order security order_type buy_sell qty user_id in
      let order_book = 
        try Hashtbl.find order_books security
        with Not_found ->
          let new_order_book = create_order_book security in
          Hashtbl.add order_books security new_order_book;
          new_order_book
      in
      add_order order_book order;
      if buy_sell = Buy then update_user_balance user_id (-. total_cost);
      Printf.printf "Order with ID %d placed!\n" order.id


let cancel_order () = 
  match !curr_user_id with
  | None -> Printf.printf "Please set your user ID first.\n"
  | Some _ ->
    Printf.printf "Enter the order ID to cancel: ";
    let order_id = int_of_string (read_line ()) in
    let order_book = Hashtbl.find order_books "AAPL" in
    remove_order order_book order_id;
    Printf.printf "Order cancelled.\n"


let view_book () = 
  Printf.printf "Enter the security you want to view, or 'ALL' to view all securities: ";
  let input = read_line () in
  if input = "ALL" then
    Hashtbl.iter (fun _ order_book ->
      Printf.printf "Order book for %s:\n" order_book.security;
      let bids = get_bids order_book in
      let asks = get_asks order_book in
      Printf.printf "Bids:\n";
      List.iter (fun order -> Printf.printf "Price: %f, Qty: %f\n" (get_price order) order.qty) bids;
      Printf.printf "Asks:\n";
      List.iter (fun order -> Printf.printf "Price: %f, Qty: %f\n" (get_price order) order.qty) asks
    ) order_books
    else
      match Hashtbl.find_opt order_books input with
      | None -> Printf.printf "No order book found for %s.\n" input
      | Some order_book ->
        Printf.printf "Order book for %s:\n" order_book.security;
        let bids = get_bids order_book in
        let asks = get_asks order_book in
        Printf.printf "Bids:\n";
        List.iter (fun order -> Printf.printf "Price: %f, Qty: %f\n" (get_price order) order.qty) bids;
        Printf.printf "Asks:\n";
        List.iter (fun order -> Printf.printf "Price: %f, Qty: %f\n" (get_price order) order.qty) asks

let view_my_orders () = 
  match !curr_user_id with
  | None -> Printf.printf "Please set your user ID first.\n"
  | Some user_id ->
    Printf.printf "Your orders: \n";
    Hashtbl.iter (fun _ order_book ->
      let orders = 
        List.filter (fun order -> order.user_id = user_id) 
          (get_bids order_book @ get_asks order_book) 
      in
      if orders <> [] then 
        Printf.printf "Orders in %s:\n" order_book.security;
      List.iter (fun order -> 
        Printf.printf "ID: %d, Type: %s, Price: %f, Qty: %f\n" 
          order.id 
          (match order.order_type with
            | Market -> "Market"
            | Limit _ -> "Limit"
            | Margin _ -> "Margin")
          (get_price order) 
          order.qty
      ) orders
    ) order_books

let view_bal () = 
  match !curr_user_id with
  | None -> Printf.printf "Please set your user ID first.\n"
  | Some user_id ->
    let balance = get_user_balance user_id in
    Printf.printf "Your balance is: %f\n" balance

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
    let option = read_line () in
    match option with
    | "1" -> set_user_id (); loop ()
    | "2" -> place_order (); loop ()
    | "3" -> cancel_order (); loop ()
    | "4" -> view_book (); loop ()
    | "5" -> view_my_orders (); loop ()
    | "6" -> view_bal (); loop ()
    | "7" -> Printf.printf "Goodbye! Thanks for trading!\n"
    | _ -> Printf.printf "Invalid option. Please try again.\n"; loop ()
  in loop ()