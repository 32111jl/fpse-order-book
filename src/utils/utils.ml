module Order_types = Order_types
module Order_sync = Order_sync

let round_price (price : float) = Float.round (price *. 100.) /. 100.

let round_quantity (qty : float) = Float.round (qty *. 100.) /. 100.

let random_float_between (min : float) (max : float) =
  min +. Random.float (max -. min)

let random_price (base : float) (spread : float) =
  round_price (base +. Random.float spread -. (spread /. 2.))

let current_time () = Unix.gettimeofday ()

let is_expired (expiration_time : float option) = 
  match expiration_time with
  | None -> false
  | Some t -> current_time () > t

let string_to_order_type (str : string) (price : float) =
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