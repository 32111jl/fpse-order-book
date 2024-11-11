(** Order module for the order book. *)

type order_type = 
| Market


type order = {
  id : int;
  order_type : order_type;
  price : float;
  qty : float;
  timestamp : float;
  (* user_id : int; *)
}


val create_order : order_type -> float -> float -> order
(** [create_order o_type price qty] creates an order with the given type, price, and quantity. *)

val is_expired : order -> float -> bool
(** [is_expired ()] returns true if the order is expired, false otherwise. *)