(** Handles the market conditions. *)

type market_conditions = {
  bid_ask_spread : float;
  margin_rate : float;
}

val create_market_conditions : float -> float -> market_conditions
(** [create_market_conditions bid_ask_spread margin_rate] creates market conditions with the given bid-ask spread and margin rate. *)

val check_spread_conditions : market_conditions -> float -> float -> bool
(** [check_spread_conditions market_conditions best_bid best_ask] checks if the spread is within the market conditions. *)

val get_margin_rate : market_conditions -> float
(** [get_margin_rate market_conditions] returns the margin rate. *)