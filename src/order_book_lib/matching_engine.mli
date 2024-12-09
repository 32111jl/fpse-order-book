(* Matches buy/sell orders based on price-time priority. *)

val get_trade_price : Order_types.db_order -> Order_types.db_order -> float
(** [get_trade_price buy_order sell_order] returns the trade price. *)

val match_orders : Order_types.order_book -> Order_types.market_conditions -> Order_types.trade list
(** [match_orders order_book market_conditions] matches buy/sell orders based on price-time priority. *)

val match_all_books : Order_types.order_book list -> Order_types.market_conditions -> Order_types.trade list
(** [match_all_books books market_conditions] matches buy/sell orders for the given order books. *)
