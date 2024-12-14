open Order_types
open Price

let create_market_conditions (bid_ask_spread : float) (margin_rate : float) = 
  { bid_ask_spread = bid_ask_spread; margin_rate = margin_rate }

let get_margin_rate (market_conditions : market_conditions) = 
  market_conditions.margin_rate

let check_spread (market_conds : market_conditions) (base_price : price) (order_price : price) : spread_check_result =
  let base_float = price_to_float base_price in
  let max_deviation = base_float *. market_conds.bid_ask_spread in
  let max_price = float_to_price (base_float +. max_deviation) in
  let min_price = float_to_price (base_float -. max_deviation) in
  
  if compare_price order_price max_price > 0 then
    PriceTooHigh (order_price, max_price)
  else if compare_price order_price min_price < 0 then
    PriceTooLow (order_price, min_price)
  else
    ValidPrice