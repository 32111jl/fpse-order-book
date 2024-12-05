(* open Postgresql

type db_config = {
  dbname : string;
  host : string;
  user : string;
  password : string;
  port : int;
}

let default_db_config = {
  dbname = "order_book";
  host = "localhost";
  user = "postgres";
  password = "";
  port = 5432;
}

let connect db_config =
  try
    new connection ~dbname:db_config.dbname ~host:db_config.host 
      ~user:db_config.user ~password:db_config.password 
      ~port:db_config.port ()
  with Error e ->
    Printf.printf "Database connection error: %s\n" (string_of_error e);
    raise e

let add_order conn (order : order) =
  try
    let order_type = order_type_to_string order.order_type in
    let side = buy_sell_to_string order.buy_sell in
    let price = match get_price order with
      | Some p -> Some (string_of_float p)
      | None -> None
    in
    let _ = conn#exec ~expect:[Command_ok]
      ~params:[|
        string_of_int order.id;
        string_of_int order.user_id;
        order.security;
        order_type;
        (match price with Some p -> p | None -> "");
        string_of_float order.qty;
        side;
        "ACTIVE";
        string_of_float order.timestamp
      |]
      "INSERT INTO orders (id, user_id, security, order_type, price, quantity, side, status, timestamp)
       VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)" in
    true
  with Error e ->
    Printf.printf "Error adding order: %s\n" (string_of_error e);
    false

let update_order_status conn order_id status =
  try
    let _ = conn#exec ~expect:[Command_ok] ~params:[|status; string_of_int order_id|]
      "UPDATE orders SET status = $1 WHERE id = $2" in
    true
  with Error e ->
    Printf.printf "Error updating order status: %s\n" (string_of_error e);
    false

let record_trade conn buy_order sell_order trade_qty trade_price =
  try
    let _ = conn#exec ~expect:[Command_ok] 
      ~params:[|
        string_of_int buy_order.id;
        string_of_int sell_order.id;
        string_of_float trade_qty;
        string_of_float trade_price;
        buy_order.security;
      |]
      "INSERT INTO trades (buy_order_id, sell_order_id, quantity, price, security, timestamp)
       VALUES ($1, $2, $3, $4, $5, NOW())" in
    true
  with Error e ->
    Printf.printf "Error recording trade: %s\n" (string_of_error e);
    false

let update_balance conn user_id amount =
  try
    let _ = conn#exec ~expect:[Command_ok] ~params:[|string_of_float amount; string_of_int user_id|]
      "UPDATE user_balances SET cash_balance = cash_balance + $1 WHERE user_id = $2" in
    true
  with Error e ->
    Printf.printf "Error updating balance: %s\n" (string_of_error e);
    false

let update_position conn user_id security amount =
  try
    let _ = conn#exec ~expect:[Command_ok] ~params:[|string_of_float amount; string_of_int user_id; security|]
      "UPDATE positions SET quantity = quantity + $1 
       WHERE user_id = $2 AND security = $3" in
    true
  with Error e ->
    Printf.printf "Error updating position: %s\n" (string_of_error e);
    false

let add_security conn symbol base_price =
  try
    let _ = conn#exec ~expect:[Command_ok] ~params:[|symbol; string_of_float base_price|]
      "INSERT INTO securities (symbol, base_price, status)
       VALUES ($1, $2, true)
       ON CONFLICT (symbol) DO UPDATE SET base_price = $2" in
    true
  with Error e ->
    Printf.printf "Error adding security: %s\n" (string_of_error e);
    false

let init_db conn =
  try
    (* Create tables *)
    let _ = conn#exec ~expect:[Command_ok]
      "CREATE TABLE IF NOT EXISTS orders (
         id SERIAL PRIMARY KEY,
         user_id INTEGER NOT NULL,
         security VARCHAR(10) NOT NULL,
         order_type VARCHAR(10) NOT NULL,
         price NUMERIC,
         quantity NUMERIC NOT NULL,
         side VARCHAR(4) NOT NULL,
         status VARCHAR(10) NOT NULL,
         timestamp TIMESTAMP NOT NULL
       )" in

    let _ = conn#exec ~expect:[Command_ok]
      "CREATE TABLE IF NOT EXISTS trades (
         id SERIAL PRIMARY KEY,
         buy_order_id INTEGER NOT NULL,
         sell_order_id INTEGER NOT NULL,
         quantity NUMERIC NOT NULL,
         price NUMERIC NOT NULL,
         security VARCHAR(10) NOT NULL,
         timestamp TIMESTAMP NOT NULL
       )" in

    let _ = conn#exec ~expect:[Command_ok]
      "CREATE TABLE IF NOT EXISTS user_balances (
         user_id INTEGER PRIMARY KEY,
         cash_balance NUMERIC NOT NULL DEFAULT 0,
         timestamp TIMESTAMP NOT NULL DEFAULT NOW()
       )" in

    let _ = conn#exec ~expect:[Command_ok]
      "CREATE TABLE IF NOT EXISTS positions (
         id SERIAL PRIMARY KEY,
         user_id INTEGER NOT NULL,
         security VARCHAR(10) NOT NULL,
         quantity NUMERIC NOT NULL DEFAULT 0,
         timestamp TIMESTAMP NOT NULL DEFAULT NOW(),
         UNIQUE(user_id, security)
       )" in

    let _ = conn#exec ~expect:[Command_ok]
      "CREATE TABLE IF NOT EXISTS securities (
         symbol VARCHAR(10) PRIMARY KEY,
         base_price NUMERIC NOT NULL,
         status BOOLEAN NOT NULL DEFAULT true,
         timestamp TIMESTAMP NOT NULL DEFAULT NOW()
       )" in

    (* Initialize securities *)
    List.iter (fun (symbol, price) -> ignore (add_security conn symbol price)) Securities.available_securities;

    Printf.printf "Database initialized successfully.\n"
  with Error e ->
    Printf.printf "Error initializing database: %s\n" (string_of_error e);
    raise e *)