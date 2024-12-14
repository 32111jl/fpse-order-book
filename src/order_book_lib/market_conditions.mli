val create_market_conditions : float -> float -> Order_types.market_conditions
(** [create_market_conditions bid_ask_spread margin_rate] creates market conditions with the given bid-ask spread and margin rate. *)

val get_margin_rate : Order_types.market_conditions -> float
(** [get_margin_rate market_conditions] returns the margin rate. *)

val check_spread : Order_types.market_conditions -> Price.price -> Price.price -> Order_types.spread_check_result
(** [check_spread market_conditions base_price order_price] checks if the order price is within the allowed spread (+/- 25% of base price). *)
