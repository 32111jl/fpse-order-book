open Order
open Market_conditions

type order_book = {
  security : string;
  bids : (float, order Queue.t) Hashtbl.t;
  asks : (float, order Queue.t) Hashtbl.t;
}

let create_order_book (security : string) = 
  {
    security = security;
    bids = Hashtbl.create 128; (* arbitrary? maybe change *)
    asks = Hashtbl.create 128;
  }

let add_order (order_book : Order_book) (order : Order) = 
  let table = if order.price > 0.0 then order_book.bids else order_book.asks in
  let price = order.price in
  match Hashtbl.find_opt table price with
  | None -> 
    let q = Queue.create () in
    Queue.push q order;
    Hashtbl.add table price q
  | Some q -> Queue.push q order

let remove_order (order_book : Order_book) (order_id : int) = 
  let remove_order_from_table table = 
    Hashtbl.iter (fun key queue ->
      let remaining_orders = Queue.filter queue ~f:(fun order -> order.id <> order_id) in
      if Queue.length remaining_orders = 0 then
        Hashtbl.remove table key
      else
        Hashtbl.set table key remaining_orders
    ) table
  in
  remove_order_from_table order_book.bids;
  remove_order_from_table order_book.asks

let get_best_bid (order_book : Order_book) = 
  let best_bid = 
    Hashtbl.fold (fun price queue best_bid ->
      match best_bid with
      | None -> Queue.peek queue
      | Some order -> 
        if order.price < price then Queue.peek queue
        else best_bid
    ) order_book.bids None
  in
  best_bid

let get_best_ask (order_book : Order_book) = 
  let best_ask = 
    Hashtbl.fold (fun price queue best_ask ->
      match best_ask with
      | None -> Queue.peek queue
      | Some order -> 
        if order.price > price then Queue.peek queue
        else best_ask
    ) order_book.asks None
  in
  best_ask

val get_bids : order_book -> order list
let get_bids (order_book : Order_book) = 
  Hashtbl.fold (fun _ queue acc -> acc @ Queue.to_list queue) order_book.bids []

val get_asks : order_book -> order list
let get_asks (order_book : Order_book) = 
  Hashtbl.fold (fun _ queue acc -> acc @ Queue.to_list queue) order_book.asks []

val match_orders : order_book -> market_conditions -> float -> (order * order) list
let match_orders (order_book : Order_book) (market_conditions : Market_conditions) (curr_time : float) = 
  let bids = get_bids order_book in
  let asks = get_asks order_book in
  let sorted_bids = List.sort bids ~compare:(fun a b -> Float.compare b.price a.price) in
  let sorted_asks = List.sort asks ~compare:(fun a b -> Float.compare a.price b.price) in
  let rec match_aux sorted_bids sorted_asks acc = 
    match sorted_bids, sorted_asks with
    | [], _ | _, [] -> acc
    | bid :: rest_bids, ask :: rest_asks -> 
      if bid.price >= ask.price then
        let matched_order = (bid, ask) in
        match_aux rest_bids rest_asks (matched_order :: acc)
      else
        acc
  in
  match_aux sorted_bids sorted_asks []

let remove_expired_orders (order_book : Order_book) (curr_time : float) = 
  let remove_expired_orders_from_table table = 
    Hashtbl.iter (fun price queue ->
      let remaining_orders = Queue.filter queue ~f:(fun order -> not (is_expired order curr_time)) in
      if Queue.length remaining_orders = 0 then
        Hashtbl.remove table price
      else
        Hashtbl.set table price remaining_orders
    ) table
  in
  remove_expired_orders_from_table order_book.bids;
  remove_expired_orders_from_table order_book.asks