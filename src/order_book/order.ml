
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


let order_counter = ref 0


let generate_order_id () = 
  let id = !order_counter in 
  order_counter := id + 1;
  id


let create_order (security : string) (order_type : order_type) (price : float) (qty : float) : order = 
  let id = generate_order_id () in
  let timestamp = Unix.time () in
  { id; security; order_type; price; qty; timestamp; user_id = 0 }


let is_expired (order : order) (curr_time : float) : bool = 
  match order.order_type with
  | Limit { price = _; expiration = Some exp } -> curr_time >= exp
  | _ -> false