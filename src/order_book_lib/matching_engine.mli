(* Matches buy/sell orders based on price-time priority. *)

val create_trade : Order_types.db_order -> Order_types.db_order -> float -> Price.price -> Order_types.trade
(** [create_trade buy_order sell_order trade_qty trade_price] creates a trade given the buy and sell orders at the indicated price and quantity. *)

val can_match : Order_types.db_order -> Order_types.db_order -> bool
(** [can_match buy_order sell_order] determines if the buy and sell orders can be matched. *)

val get_trade_price : Order_types.db_order -> Order_types.db_order -> Price.price
(** [get_trade_price buy_order sell_order] returns the trade price. *)

val update_order_book : Order_types.order_book -> Order_types.db_order -> Order_types.db_order -> float -> Order_types.db_order list -> unit
(** [update_order_book order_book order best_match trade_qty potential_matches] updates the order book with the trade. *)

val process_match : Order_types.order_book -> Order_types.db_order -> Order_types.db_order -> float -> Order_types.db_order list -> Order_types.trade option * float
(** [process_match order_book order best_match trade_qty potential_matches] processes a single match between the buy and sell orders. *)

val match_orders : Order_types.order_book -> Order_types.market_conditions -> Order_types.trade list
(** [match_orders order_book market_conditions] attempts to match buy/sell orders in the order book based on price-time priority. It will cascade down the order book if there are multiple matches. *)

val match_all_books : Order_types.order_book list -> Order_types.market_conditions -> Order_types.trade list
(** [match_all_books books market_conditions] matches buy/sell orders for the given order books. *)