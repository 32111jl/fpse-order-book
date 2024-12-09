

val create_order_book : string -> Order_types.order_book

val add_order_to_memory : Order_types.order_book -> Order_types.db_order -> unit

val remove_order_from_memory : Order_types.order_book -> int -> unit

val get_best_bid : Order_types.order_book -> float option

val get_best_ask : Order_types.order_book -> float option

val get_bids : Order_types.order_book -> Order_types.db_order list

val get_asks : Order_types.order_book -> Order_types.db_order list

val print_orders : Order_types.order_book -> unit

val print_trade : Order_types.trade -> string -> unit