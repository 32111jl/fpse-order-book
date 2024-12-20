(* Matches buy/sell orders based on price-time priority. *)

val match_orders : Order_book.order_book -> Market_conditions.market_conditions -> Utils.Order_types.trade list
(** [match_orders order_book market_conditions] matches buy/sell orders based on price-time priority. *)

val match_all_books : Order_book.order_book list -> Market_conditions.market_conditions -> Utils.Order_types.trade list
(** [match_all_books books market_conditions] matches buy/sell orders for the given order books. *)

val check_spread : Order_book.order_book -> Market_conditions.market_conditions -> bool
(** [check_spread order_book market_conditions] checks if the spread is within the market conditions. *)

val execute_trade : Utils.Order_types.db_order -> Utils.Order_types.db_order -> (float * float, string) result
(** [execute_trade buy_order sell_order] executes the buy and sell orders, returning the trade quantity. *)

val get_trade_price : Utils.Order_types.db_order -> Utils.Order_types.db_order -> float
(** [get_trade_price buy_order sell_order] returns the trade price. *)