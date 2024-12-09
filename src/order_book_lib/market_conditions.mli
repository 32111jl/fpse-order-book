val create_market_conditions : float -> float -> Order_types.market_conditions
(** [create_market_conditions bid_ask_spread margin_rate] creates market conditions with the given bid-ask spread and margin rate. *)

val get_margin_rate : Order_types.market_conditions -> float
(** [get_margin_rate market_conditions] returns the margin rate. *)
