(** Interface for the order book module. *)

type order_book = {
  security : string;
  bids : (float, order Queue.t) Hashtbl.t;
  asks : (float, order Queue.t) Hashtbl.t;
}

val create_order_book : string -> order_book
(** [create_order_book security] creates an order book for the specified security. *)

val add_order : order_book -> order -> unit
(** [add_order order_book order] adds an order to the order book. *)

val remove_order : order_book -> order -> unit
(** [remove_order order_book order] removes an order from the order book. *)

val get_best_bid : order_book -> order option
(** [get_best_bid order_book] returns the best bid in the order book. *)

val get_best_ask : order_book -> order option
(** [get_best_ask order_book] returns the best ask in the order book. *)

val get_bids : order_book -> order list
(** [get_bids order_book] returns the bids in the order book. *)

val get_asks : order_book -> order list
(** [get_asks order_book] returns the asks in the order book. *)

val match_orders : order_book -> market_conditions -> (order * order) list
(** [match_orders order_book market_conditions] matches buy/sell orders based on price-time priority. *)

val removed_expired_orders : order_book -> float -> unit
(** [remove_expired_orders order_book curr_time] removes all expired orders from the order book given the current time. *)