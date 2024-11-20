(** Order module for the order book. *)

type order_type = 
| Market
| Limit of { price : float; expiration : float option }
| Margin of float


type order = {
  id : int;                 (* order ID *)
  security : string;        (* security being traded (ex. "AAPL") *)
  order_type : order_type;  (* type of order (Market, Limit, Margin) *)
  price : float;            (* price for limit/margin orders *)
  qty : float;              (* quantity to buy or sell *)
  timestamp : float;        (* timestamp at which order was placed*)
  user_id : int;            (* ID of the user who placed the order *)
}


val create_order : string -> order_type -> float -> float -> order
(** [create_order security o_type price qty] creates an order with the given type, price, and quantity. *)

val is_expired : order -> float -> bool
(** [is_expired order curr_time] returns true if the order is expired, false otherwise. *)