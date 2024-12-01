
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
  timestamp : float;        (* timestamp at which order was placed *)
  user_id : int;            (* ID of the user who placed the order *)
}


let create_order (id : int) (security : string) (order_type : order_type) (buy_sell : buy_sell) (qty : float) (user_id : int) : order = 
  let timestamp = Unix.time () in
  { id; security; order_type; buy_sell; qty; timestamp; user_id }


let is_expired (order : order) (curr_time : float) : bool = 
  match order.order_type with
  | Limit { price = _; expiration = Some exp } -> curr_time >= exp
  | _ -> false