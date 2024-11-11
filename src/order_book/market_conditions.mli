(** Handles the market conditions. *)

type t = {
  bid_ask_spread : float;
  margin_rate : float;
}

val create : float -> float -> t
(** [create bid_ask_spread margin_rate] creates market conditions with the given bid-ask spread and margin rate. *)

val check_spread_conditions : t -> float -> float -> bool
(** [check_spread_conditions market_conditions best_bid best_ask] checks if the spread is within the market conditions. *)

val get_margin_rate : t -> float
(** [get_margin_rate market_conditions] returns the margin rate. *)