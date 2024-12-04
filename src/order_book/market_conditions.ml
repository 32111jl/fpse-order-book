
type market_conditions = {
  bid_ask_spread : float;
  margin_rate : float;
}


let create_market_conditions bid_ask_spread margin_rate = 
  {bid_ask_spread = bid_ask_spread; margin_rate = margin_rate }

let check_spread_conditions market_conditions best_bid best_ask = 
  best_ask -. best_bid <= market_conditions.bid_ask_spread

let get_margin_rate market_conditions = 
  market_conditions.margin_rate