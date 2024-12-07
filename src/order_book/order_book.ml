open Database.Db
open Utils.Order_types

type order_book = {
  security : string;
  mutable best_bid : float option;
  mutable best_ask : float option;
}

let create_order_book (security : string) = {
  security = security;
  best_bid = None;
  best_ask = None;
}

let get_security (order_book : order_book) : string = order_book.security

let get_price (order : Utils.Order_types.db_order) : float option = 
  match order.order_type with
  | Market -> None
  | Limit { price; _ } -> Some price
  | Margin price -> Some price

let get_best_bid (order_book : order_book) : float option = 
  match order_book.best_bid with
  | Some _ as bid -> bid
  | None ->
    let query = "
      SELECT price FROM orders 
      WHERE security = $1 
      AND buy_sell = 'BUY' 
      AND status IN ('ACTIVE', 'PARTIAL')
      AND order_type != 'MARKET'
      ORDER BY price DESC 
      LIMIT 1" in
    match execute_query query [| order_book.security |] with
    | Ok result when result#ntuples > 0 ->
      let price = float_of_string (result#getvalue 0 0) in
      order_book.best_bid <- Some price;
      Some price
    | _ -> None

let get_best_ask (order_book : order_book) : float option = 
  match order_book.best_ask with
  | Some _ as ask -> ask
  | None ->
    let query = "
      SELECT price FROM orders 
      WHERE security = $1 
      AND buy_sell = 'SELL' 
      AND status IN ('ACTIVE', 'PARTIAL')
      AND order_type != 'MARKET'
      ORDER BY price ASC 
      LIMIT 1" in
    match execute_query query [| order_book.security |] with
    | Ok result when result#ntuples > 0 ->
      let price = float_of_string (result#getvalue 0 0) in
      order_book.best_ask <- Some price;
      Some price
    | _ -> None

let get_bids (order_book : order_book) : Utils.Order_types.db_order list = 
  let query = "
    SELECT * FROM orders 
    WHERE security = $1 
    AND buy_sell = 'BUY' 
    AND status IN ('ACTIVE', 'PARTIAL')
    ORDER BY 
      CASE WHEN order_type = 'MARKET' THEN 0 ELSE 1 END,
      price DESC,
      id ASC
      FOR UPDATE" in
  match execute_query query [| order_book.security |] with
  | Ok result ->
    let orders = ref [] in
    for i = 0 to result#ntuples - 1 do
      let id = int_of_string (result#getvalue i 0) in
      let user_id = int_of_string (result#getvalue i 1) in
      let qty = float_of_string (result#getvalue i 5) in
      let price = float_of_string (result#getvalue i 6) in
      let order_type = match result#getvalue i 3 with
        | "MARKET" -> Market
        | "LIMIT" -> Limit { price; expiration = None }
        | "MARGIN" -> Margin price
        | _ -> failwith "Invalid order type"
      in
      orders := { id; user_id; security = order_book.security; 
                  order_type; buy_sell = Buy; qty } :: !orders
    done;
    List.rev !orders
  | Error _ -> []

let get_asks (order_book : order_book) : Utils.Order_types.db_order list = 
  let query = "
    SELECT * FROM orders 
    WHERE security = $1 
    AND buy_sell = 'SELL' 
    AND status IN ('ACTIVE', 'PARTIAL')
    ORDER BY 
      CASE WHEN order_type = 'MARKET' THEN 0 ELSE 1 END,
      price ASC,
      id ASC
      FOR UPDATE" in (* id ASC sorts by order id, for update locks the rows, avoids race conditions *)
  match execute_query query [| order_book.security |] with
  | Ok result ->
    let orders = ref [] in
    for i = 0 to result#ntuples - 1 do
      let id = int_of_string (result#getvalue i 0) in
      let user_id = int_of_string (result#getvalue i 1) in
      let qty = float_of_string (result#getvalue i 5) in
      let price = float_of_string (result#getvalue i 6) in
      let order_type = match result#getvalue i 3 with
        | "MARKET" -> Market
        | "LIMIT" -> Limit { price; expiration = None }
        | "MARGIN" -> Margin price
        | _ -> failwith "Invalid order type"
      in
      orders := { id; user_id; security = order_book.security;
                  order_type; buy_sell = Sell; qty } :: !orders
    done;
    List.rev !orders
  | Error _ -> []

let add_order (order_book : order_book) (order : Utils.Order_types.db_order) = 
  create_order order.id order.user_id order_book.security order.order_type order.buy_sell order.qty (match get_price order with Some p -> p | None -> 0.0)

let remove_order _order_book (order_id : int) = cancel_order order_id

let remove_expired_orders _order_book (curr_time : float) = remove_expired_orders curr_time

let check_order_exists _order_book (order_id : int) =
  match get_order order_id with
  | Ok result -> result#ntuples > 0
  | Error _ -> false