(** Interface for the order book module. *)

type order_book = {
  security : string;
  bids : (float, Order.order Queue.t) Hashtbl.t;
  asks : (float, Order.order Queue.t) Hashtbl.t;
  mutable order_counter : int;
}

val create_order_book : string -> order_book
(** [create_order_book security] creates an order book for the specified security. *)

val generate_order_id : order_book -> int
(** [generate_order_id order_book] generates a unique order ID for the given order book. *)

val get_price : Order.order -> float
(** [get_price order] returns the price of the order. *)

val get_best_bid : order_book -> Order.order option
(** [get_best_bid order_book] returns the best bid in the order book. *)

val get_best_ask : order_book -> Order.order option
(** [get_best_ask order_book] returns the best ask in the order book. *)

val get_bids : order_book -> Order.order list
(** [get_bids order_book] returns the bids in the order book. *)

val get_asks : order_book -> Order.order list
(** [get_asks order_book] returns the asks in the order book. *)

val add_order : order_book -> Order.order -> unit
(** [add_order order_book order] adds an order to the order book. *)

val remove_order : order_book -> int -> unit
(** [remove_order order_book order_id] removes an order with the given ID from the order book. *)

val match_orders : order_book -> Market_conditions.t -> float -> (Order.order * Order.order * float) list
(** [match_orders order_book market_conditions curr_time] matches buy/sell orders based on price-time priority and current time. *)

val remove_expired_orders : order_book -> float -> unit
(** [remove_expired_orders order_book curr_time] removes all expired orders from the order book given the current time. *)