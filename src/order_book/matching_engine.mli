(** Matches buy/sell orders based on price-time priority. *)

val match_orders : order_book -> market_conditions -> (order * order) list
(** [match_orders order_book market_conditions] matches buy/sell orders based on price-time priority. *)

val match_all_books : order_book list -> market_conditions -> float -> (order * order) list
(** [match_all_books books market_conditions curr_time] matches buy/sell orders for the given order books. *)

val check_spread : order_book -> market_conditions -> bool
(** [check_spread order_book market_conditions] checks if the spread is within the market conditions. *)

val execute_trade : order -> order -> (order * order) option
(** [execute_trade buy_order sell_order] executes the buy and sell orders. *)