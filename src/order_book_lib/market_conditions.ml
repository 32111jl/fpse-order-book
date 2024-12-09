open Order_types

let create_market_conditions (bid_ask_spread : float) (margin_rate : float) = 
  { bid_ask_spread = bid_ask_spread; margin_rate = margin_rate }

let get_margin_rate (market_conditions : market_conditions) = 
  market_conditions.margin_rate