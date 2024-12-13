open Price

let round_price (price : float) = Float.round (price *. 100.) /. 100.

let round_quantity (qty : float) = Float.round (qty *. 100.) /. 100.

let random_float_between (min : float) (max : float) =
  min +. Random.float (max -. min)

let random_price (base : price) (spread : price) =
  let base_float = price_to_float base in
  let spread_float = price_to_float spread in
  float_to_price (round_price (base_float +. Random.float spread_float -. (spread_float /. 2.)))

let current_time () = Unix.gettimeofday ()

let is_expired (expiration_time : float option) = 
  match expiration_time with
  | None -> false
  | Some t -> current_time () > t

let string_to_order_type (str : string) (price : price) =
  match String.uppercase_ascii str with
  | "MARKET" -> Order_types.Market
  | "LIMIT" -> Order_types.Limit { price = price; expiration = None }
  | "MARGIN" -> Order_types.Margin price
  | _ -> failwith ("Invalid order type: " ^ str) (* should never be reached since constructor doesn't contain other types *)

let order_type_to_string (order_type : Order_types.order_type) =
  match order_type with
  | Order_types.Market -> "MARKET"
  | Order_types.Limit _ -> "LIMIT"
  | Order_types.Margin _ -> "MARGIN"

let string_to_buy_sell (str : string) =
  match String.uppercase_ascii str with
  | "BUY" -> Order_types.Buy
  | "SELL" -> Order_types.Sell
  | _ -> failwith ("Invalid buy/sell: " ^ str)

let buy_sell_to_string (buy_sell : Order_types.buy_sell) =
  match buy_sell with
  | Order_types.Buy -> "BUY"
  | Order_types.Sell -> "SELL"

let unwrap_id (id : int option) =
  match id with
  | Some id -> id
  | None -> failwith "Order has no ID."

let compare_price_options (price1 : price option) (price2 : price option) =
  match price1, price2 with
  | Some p1, Some p2 -> compare_price p1 p2
  | Some _, None -> -1 (* non-market order comes after *)
  | None, Some _ -> 1 (* market order has priority *)
  | None, None -> 0 (* both are market orders *)