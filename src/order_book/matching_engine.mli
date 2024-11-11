(** Matches buy/sell orders based on price-time priority. *)

val match_orders : order_book -> market_conditions -> (order * order) list
(** [match_orders order_book market_conditions] matches buy/sell orders based on price-time priority. *)

val check_spread : order_book -> market_conditions -> bool
(** [check_spread order_book market_conditions] checks if the spread is within the market conditions. *)

val execute_order : order -> order -> (order * order) list
(** [execute_order buy_order sell_order] executes the buy and sell orders. *)