(** Matches buy/sell orders based on price-time priority. *)

val match_orders : Order_book.order_book -> Market_conditions.t -> (Order.order * Order.order) list
(** [match_orders order_book market_conditions] matches buy/sell orders based on price-time priority. *)

val match_all_books : Order_book.order_book list -> Market_conditions.t -> float -> (Order.order * Order.order) list
(** [match_all_books books market_conditions curr_time] matches buy/sell orders for the given order books. *)

val check_spread : Order_book.order_book -> Market_conditions.t -> bool
(** [check_spread order_book market_conditions] checks if the spread is within the market conditions. *)

val execute_trade : Order.order -> Order.order -> (Order.order * Order.order) option
(** [execute_trade buy_order sell_order] executes the buy and sell orders. *)