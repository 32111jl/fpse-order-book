(** Order module for the order book. *)
type buy_sell = Buy | Sell

type order_type = 
| Market
| Limit of { price : float; expiration : float option }
| Margin of float


type order = {
  id : int;                 (* order ID *)
  security : string;        (* security being traded (ex. "AAPL") *)
  order_type : order_type;  (* type of order (Market, Limit, Margin) *)
  buy_sell : buy_sell;      (* indicates if order is to buy or sell *)
  mutable qty : float;              (* quantity to buy or sell *)
  timestamp : float;        (* timestamp at which order was placed*)
  user_id : int;            (* ID of the user who placed the order *)
}


val create_order : int -> string -> order_type -> buy_sell -> float -> int -> order
(** [create_order security o_type buy_sell qty user_id] creates an order with the given type, quantity, and user_id. *)

val is_expired : order -> float -> bool
(** [is_expired order curr_time] returns true if the order is expired, false otherwise. *)