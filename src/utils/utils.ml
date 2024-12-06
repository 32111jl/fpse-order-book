module Order_types = Order_types

let round_price price = Float.round (price *. 100.) /. 100.

let round_quantity qty = Float.round (qty *. 100.) /. 100.

let random_float_between min max =
  min +. Random.float (max -. min)

let random_price base spread =
  round_price (base +. Random.float spread -. (spread /. 2.))

let current_time = Unix.gettimeofday

let is_expired expiration_time = 
  match expiration_time with
  | None -> false
  | Some t -> current_time () > t

let string_to_order_type str price =
  match String.uppercase_ascii str with
  | "MARKET" -> Order_types.Market
  | "LIMIT" -> Order_types.Limit { price = price; expiration = None }
  | "MARGIN" -> Order_types.Margin price
  | _ -> failwith ("Invalid order type: " ^ str)

let order_type_to_string order_type =
  match order_type with
  | Order_types.Market -> "MARKET"
  | Order_types.Limit _ -> "LIMIT"
  | Order_types.Margin _ -> "MARGIN"

let string_to_buy_sell str =
  match String.uppercase_ascii str with
  | "BUY" -> Order_types.Buy
  | "SELL" -> Order_types.Sell
  | _ -> failwith ("Invalid buy/sell: " ^ str)

let buy_sell_to_string buy_sell =
  match buy_sell with
  | Order_types.Buy -> "BUY"
  | Order_types.Sell -> "SELL"
