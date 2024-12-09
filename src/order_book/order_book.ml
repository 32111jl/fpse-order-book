open Database.Db
open Utils
open Utils.Order_sync
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
      let price = round_price (float_of_string (result#getvalue 0 0)) in
      order_book.best_bid <- Some price; (* should be unreachable *)
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
      let price = round_price (float_of_string (result#getvalue 0 0)) in
      order_book.best_ask <- Some price; (* should be unreachable *)
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
      let id = Some (int_of_string (result#getvalue i 0)) in
      let user_id = int_of_string (result#getvalue i 1) in
      let qty = round_quantity (float_of_string (result#getvalue i 5)) in
      let price = round_price (float_of_string (result#getvalue i 6)) in
      let order_type = match result#getvalue i 3 with
        | "MARKET" -> Market
        | "LIMIT" -> Limit { price; expiration = None }
        | "MARGIN" -> Margin price
        | _ -> failwith "Invalid order type" [@ coverage off] (* should never be reached since create_order checks already *)
      in
      orders := { id; user_id; security = order_book.security; 
                  order_type; buy_sell = Buy; qty } :: !orders
    done;
    List.rev !orders
  | Error _ -> [] (* i can't test this because of type checking and i can't pass a malformed query into this function directly *)

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
      let id = Some (int_of_string (result#getvalue i 0)) in
      let user_id = int_of_string (result#getvalue i 1) in
      let qty = round_quantity (float_of_string (result#getvalue i 5)) in
      let price = round_price (float_of_string (result#getvalue i 6)) in
      let order_type = match result#getvalue i 3 with
        | "MARKET" -> Market
        | "LIMIT" -> Limit { price; expiration = None }
        | "MARGIN" -> Margin price
        | _ -> failwith "Invalid order type" [@ coverage off] (* should never be reached since create_order checks already *)
      in
      orders := { id; user_id; security = order_book.security;
                  order_type; buy_sell = Sell; qty } :: !orders
    done;
    List.rev !orders
  | Error _ -> [] (* see above, also execute_query only produces error if db is down or query is malformed... *)

let add_order (book : order_book) (order : Utils.Order_types.db_order) = 
  match create_order order.user_id book.security order.order_type order.buy_sell order.qty (match get_price order with Some p -> p | None -> 0.0) with
  | Ok result ->
    (* immediately update best bid/ask *)
    (match order.order_type, order.buy_sell with
    | Limit { price; _ }, Buy | Margin price, Buy ->
      (match book.best_bid with
      | Some best_bid when price > best_bid -> book.best_bid <- Some price
      | None -> book.best_bid <- Some price
      | _ -> ())
    (* if price > (Option.value ~default:0.0 book.best_bid) then book.best_bid <- Some price *)
    | Limit { price; _ }, Sell | Margin price, Sell ->
      (match book.best_ask with
      | Some best_ask when price < best_ask -> book.best_ask <- Some price
      | None -> book.best_ask <- Some price
      | _ -> ())
    (* if price < (Option.value ~default:0.0 book.best_ask) then book.best_ask <- Some price *)
    | _ -> ()); (* no need to update best bid/ask for market orders *)
    Ok result
  | Error e -> Error e

let add_order_to_memory (book : order_book) (order : db_order) =
  match order.order_type, order.buy_sell with
  | Limit { price; _ }, Buy | Margin price, Buy ->
    (match book.best_bid with
    | Some best_bid when price > best_bid -> book.best_bid <- Some price
    | None -> book.best_bid <- Some price
    | _ -> ())
  | Limit { price; _ }, Sell | Margin price, Sell ->
    (match book.best_ask with
    | Some best_ask when price < best_ask -> book.best_ask <- Some price
    | None -> book.best_ask <- Some price
    | _ -> ())
  | _ -> ()

let remove_order (order_book : order_book) (order_id : int) = 
  sync_ob_operation (fun () ->
    let result = cancel_order order_id in
    order_book.best_bid <- None; (* yes/no? *)
    order_book.best_ask <- None;
    result
  )

let remove_expired_orders _order_book (curr_time : float) = remove_expired_orders curr_time

let check_order_exists _order_book (order_id : int) =
  let query = "SELECT COUNT(*) FROM orders 
              WHERE id = $1 AND status IN ('ACTIVE', 'PARTIAL')"
  in
  match execute_query query [| string_of_int order_id |] with
  | Ok result -> result#ntuples > 0 && (int_of_string (result#getvalue 0 0)) > 0
  | Error _ -> false (* not sure how to test this *)

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