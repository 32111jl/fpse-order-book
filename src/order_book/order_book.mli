(** Interface for the order book module. *)

type order_book

val create_order_book : string -> order_book
(** [create_order_book security] creates an order book for the specified security. *)

val generate_order_id : order_book -> int
(** [generate_order_id order_book] generates a unique order ID for the given order book. *)

val get_security : order_book -> string
(** [get_security order_book] returns the security of the order book. *)

val get_price : Order.order -> float option
(** [get_price order] returns the price of the order, or None if the order is a market order. *)

val get_best_bid : order_book -> float option
(** [get_best_bid order_book] returns the best bid in the order book. *)

val get_best_ask : order_book -> float option
(** [get_best_ask order_book] returns the best ask in the order book. *)

val get_bids : order_book -> Order.order list
(** [get_bids order_book] returns the bids in the order book. *)

val get_asks : order_book -> Order.order list
(** [get_asks order_book] returns the asks in the order book. *)

val add_order : order_book -> Order.order -> unit
(** [add_order order_book order] adds an order to the order book. *)

val remove_order : order_book -> int -> unit
(** [remove_order order_book order_id] removes an order with the given ID from the order book. *)

val remove_expired_orders : order_book -> float -> unit
(** [remove_expired_orders order_book curr_time] removes all expired orders from the order book given the current time. *)

val check_order_exists : order_book -> int -> bool
(** [check_order_exists order_book order_id] checks if an order with the given ID exists in the order book. *)