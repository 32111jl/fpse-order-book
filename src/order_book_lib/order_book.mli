(* Order book functions *)

val create_order_book : string -> Order_types.order_book
(** [create_order_book security] creates an empty order book for the given security. *)

val add_order_to_memory : Order_types.order_book -> Order_types.db_order -> unit
(** [add_order_to_memory order_book order] adds an order to the in-memory order book. *)

val remove_order_from_memory : Order_types.order_book -> int -> unit
(** [remove_order_from_memory order_book order_id] removes an order from the in-memory order book. *)

val get_price : Order_types.db_order -> Price.price option
(** [get_price order] returns the price of the order. *)

val get_best_bid : Order_types.order_book -> Price.price option
(** [get_best_bid order_book] returns the best bid price (highest price) in the order book. *)

val get_best_ask : Order_types.order_book -> Price.price option
(** [get_best_ask order_book] returns the best ask price (lowest price) in the order book. *)

val get_bids : Order_types.order_book -> Order_types.db_order list
(** [get_bids order_book] returns a sorted list of buy orders in the order book. *)

val get_asks : Order_types.order_book -> Order_types.db_order list
(** [get_asks order_book] returns a sorted list of sell orders in the order book. *)

val print_orders : Order_types.order_book -> unit
(** [print_orders order_book] prints all orders (on both sides) in the order book. *)