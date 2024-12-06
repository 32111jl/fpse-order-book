open Order_types
open Postgresql
open Result

let conn_info = "host=localhost dbname=order_book user=ob1 password=123"

(* let pool_size = 10
let connection_pool = Queue.create ()

let init_connection_pool () =
  for _ = 1 to pool_size do
    try
      let conn = new connection ~conninfo:conn_info () in
      Queue.add conn connection_pool
    with _ ->
      Printf.eprintf "Failed to create database connection. Exiting...\n";
      exit 1
  done

let get_connection () =
  try
    if Queue.is_empty connection_pool then init_connection_pool ();
    Some (Queue.take connection_pool)
  with _ -> None

let return_connection conn =
  Queue.add conn connection_pool *)

let with_connection f =
  let conn = new connection ~conninfo:conn_info () in
  try
    let result = f conn in
    conn#finish;
    Ok result
  with e ->
    conn#finish;
    Error (Printexc.to_string e)

let execute_query query params =
  (* match get_connection () with
  | Some conn -> 
    begin try
      let statement_name = "statement_" ^ string_of_int (Hashtbl.hash query) in
      ignore (conn#prepare statement_name query);
      let result = conn#exec_prepared statement_name ~params in
      return_connection conn;
      Ok result
    with e ->
      return_connection conn;
      Printf.eprintf "Database error: %s\n" (Printexc.to_string e);
      exit 1
    end
  | None -> 
    Printf.eprintf "No available connections in the pool\n";
    exit 1 *)
  with_connection (fun conn ->
    let result = conn#exec query ~params in
    result
  )

(* user-related operations *)
let create_user_in_db ~id ~name ~balance =
  let query = "INSERT INTO users (id, name, balance) VALUES ($1, $2, $3)" in
  execute_query query [| string_of_int id; name; string_of_float balance |]

let get_user_balance user_id =
  let query = "SELECT balance FROM users WHERE id = $1" in
  match execute_query query [| string_of_int user_id |] with
  | Ok result -> 
    if result#ntuples > 0 then
      Some (float_of_string (result#getvalue 0 0))
    else None
  | Error _ -> None

let update_user_balance user_id balance =
  let query = "UPDATE users SET balance = $1 WHERE id = $2" in
  execute_query query [| string_of_float balance; string_of_int user_id |]


(* order-related operations *)
let create_order ~id ~user_id ~security ~order_type ~buy_sell ~qty ~price =
  let query = "
    INSERT INTO orders (
      id, user_id, security, order_type, 
      buy_sell, quantity, price, status
    ) VALUES (
      $1, $2, $3, $4, $5, $6, $7, 'ACTIVE'
    ) RETURNING id" in
  let order_type_str = match order_type with
    | Market -> "MARKET"
    | Limit _ -> "LIMIT"
    | Margin _ -> "MARGIN"
  in
  let buy_sell_str = match buy_sell with
    | Buy -> "BUY"
    | Sell -> "SELL"
  in
  let params = [|
    string_of_int id;
    string_of_int user_id;
    security;
    order_type_str;
    buy_sell_str;
    string_of_float qty;
    string_of_float price
  |] in
  execute_query query params
let get_order order_id =
  let query = "SELECT * FROM orders WHERE id = $1" in
  execute_query query [| string_of_int order_id |]

let get_orders_by_user user_id =
  let query = "SELECT * FROM orders WHERE user_id = $1" in
  execute_query query [| string_of_int user_id |]

let update_order_status order_id status =
  let query = "UPDATE orders SET status = $1 WHERE id = $2" in
  execute_query query [| status; string_of_int order_id |]

let get_active_orders_given_security security =
  let query = "SELECT * FROM orders WHERE security = $1 AND status = 'ACTIVE'" in
  execute_query query [| security |]

let remove_expired_orders current_time =
  let query = "UPDATE orders SET status = 'EXPIRED' WHERE expiration_time < $1 AND status = 'ACTIVE'" in
  execute_query query [| string_of_float current_time |]

let cancel_order order_id =
  let query = "UPDATE orders SET status = 'CANCELLED' WHERE id = $1" in
  execute_query query [| string_of_int order_id |]

let fill_order order_id =
  let query = "UPDATE orders SET status = 'FILLED' WHERE id = $1" in
  execute_query query [| string_of_int order_id |]


(* position-related operations *)
let update_position user_id security qty =
  let query = "INSERT INTO positions (user_id, security, quantity) VALUES ($1, $2, $3)
              ON CONFLICT (user_id, security) DO UPDATE SET quantity = positions.quantity + $3" in
  execute_query query [|
    string_of_int user_id;
    security;
    string_of_float qty
  |]

let get_positions_by_user user_id =
  let query = "SELECT * FROM positions WHERE user_id = $1" in
  execute_query query [| string_of_int user_id |]

let get_positions_by_security user_id security =
  let query = "SELECT * FROM positions WHERE user_id = $1 AND security = $2" in
  match execute_query query [| string_of_int user_id; security |] with
  | Ok result -> 
    if result#ntuples > 0 then
      Some (float_of_string (result#getvalue 0 0))
    else None
  | Error _ -> None

let get_positions_value user_id =
  let query = "SELECT SUM(quantity * price) FROM positions WHERE user_id = $1" in
  execute_query query [| string_of_int user_id |]


(* trade operations *)
let record_trade ~buy_order_id ~sell_order_id ~security ~qty ~price =
  let query = "INSERT INTO trades (id, buy_order_id, sell_order_id, security, quantity, price)
                VALUES ($1, $2, $3, $4, $5, $6)" in
  execute_query query [| 
    string_of_int buy_order_id; 
    string_of_int sell_order_id; 
    security; 
    string_of_float qty; 
    string_of_float price 
  |]

let get_trade_history user_id =
  let query = "SELECT * FROM trades WHERE buy_order_id = $1 OR sell_order_id = $1" in
  execute_query query [| string_of_int user_id |]

let get_trades_by_security security =
  let query = "SELECT * FROM trades WHERE security = $1" in
  execute_query query [| security |]


(* security operations *)
let create_security symbol price = 
  let query = "INSERT INTO securities (symbol, price) VALUES ($1, $2)" in
  execute_query query [| symbol; string_of_float price |]

let update_security_status symbol status = 
  let query = "UPDATE securities SET status = $1 WHERE symbol = $2" in
  execute_query query [| status; symbol |]

let get_all_securities () =
  let query = "SELECT * FROM securities" in
  execute_query query [||]

let get_security_info security =
  let query = "SELECT * FROM securities WHERE symbol = $1" in
  execute_query query [| security |]

let get_security_price security =
  let query = "SELECT price FROM securities WHERE symbol = $1" in
  execute_query query [| security |]


(* transaction helpers (for atomicity) *)
let begin_transaction _conn = execute_query "BEGIN" [||]

let commit_transaction _conn = execute_query "COMMIT" [||]

let rollback_transaction _conn = execute_query "ROLLBACK" [||]

let with_transaction f =
  match with_connection (fun conn ->
    let _ = begin_transaction conn in
    try
      let result = f conn in
      let _ = commit_transaction conn in
      Ok result
    with e ->
      let _ = rollback_transaction conn in
      Error (Printexc.to_string e)
  ) with
  | Ok result -> result
  | Error e -> Error e
