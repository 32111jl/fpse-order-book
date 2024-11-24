
(** CLI interface for user interaction *)

open Order
open Order_book
open Market_conditions
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
    Printf.printf "Enter the order type (Market/Limit/Margin): ";
    let order_type_str = read_line () in
    let order_type =
      match order_type_str with
      | "Market" -> Market
      | "Limit" ->
        Printf.printf "Enter the price: ";
        let price = float_of_string (read_line ()) in
        Limit { price = price; expiration = Some (current_time () +. 3600.0) }
      | "Margin" ->
        Printf.printf "Enter the price: ";
        let price = float_of_string (read_line ()) in
        Margin price
      | _ -> Printf.printf "Invalid order type. Currently, you may only choose one of Market, Limit, or Margin.\n"
    in
    Printf.printf "Enter the quantity: ";
    let qty = float_of_string (read_line ()) in
    let order = create_order security order_type qty user_id in
    let order_book = Hashtbl.find_or_add order_books security (fun () -> create_order_book security) in
    add_order order_book order;
    Printf.printf "Order placed!\n"


let cancel_order () = 
  match !curr_user_id with
  | None -> Printf.printf "Please set your user ID first.\n"
  | Some user_id ->
    Printf.printf "Enter the order ID to cancel: ";
    let order_id = int_of_string (read_line ()) in
    let order_book = Hashtbl.find order_books "AAPL" in
    remove_order order_book order_id;
    Printf.printf "Order cancelled.\n"


let view_book () = 
  Hashtbl.iter (fun security order_book ->
    Printf.printf "Order book for %s:\n" order_book.security;
    let bids = get_bids order_book in
    let asks = get_asks order_book in
    Printf.printf "Bids:\n";
    List.iter (fun order -> Printf.printf "ID: %d, Price: &f, Qty: %f\n" order.id order.price order.qty) bids;
    Printf.printf "Asks:\n";
    List.iter (fun order -> Printf.printf "ID: %d, Price: &f, Qty: %f\n" order.id order.price order.qty) asks;
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
    Printf.printf "5. View balance\n";
    Printf.printf "6. Exit\n";
    let option = read_line () in
    match option with
    | "1" -> set_user_id (); loop ()
    | "2" -> place_order (); loop ()
    | "3" -> cancel_order (); loop ()
    | "4" -> view_book (); loop ()
    | "5" -> view_bal (); loop ()
    | "6" -> Printf.printf "Goodbye! Thanks for trading!\n"
    | _ -> Printf.printf "Invalid option. Please try again.\n"; loop ()
  in loop ()