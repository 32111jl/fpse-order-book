open Order

type order_book = {
  security : string;
  bids : (float, Order.order Queue.t) Hashtbl.t;
  asks : (float, Order.order Queue.t) Hashtbl.t;
}

let create_order_book (security : string) = 
  {
    security = security;
    bids = Hashtbl.create 128; (* arbitrary? maybe change *)
    asks = Hashtbl.create 128;
  }

let get_price (order : Order.order) : float = 
  match order.order_type with
  | Market -> failwith "Market orders do not have a price."
  | Limit { price; expiration = _ } -> price
  | Margin price -> price

let add_order (order_book : order_book) (order : Order.order) = 
  let price = get_price order in
  let table = if price > 0.0 then order_book.bids else order_book.asks in
  match Hashtbl.find_opt table price with
  | None -> 
    let q = Queue.create () in
    Queue.push order q;
    Hashtbl.add table price q
  | Some q -> Queue.push order q

let remove_order (order_book : order_book) (order_id : int) = 
  let remove_order_from_table table = 
    Hashtbl.iter (fun key queue ->
      let remaining_orders = Queue.create () in
      Queue.iter (fun order -> 
        if order.id <> order_id then Queue.push order remaining_orders) queue;
      if Queue.is_empty remaining_orders then
        Hashtbl.remove table key
      else
        Hashtbl.replace table key remaining_orders
    ) table
  in
  remove_order_from_table order_book.bids;
  remove_order_from_table order_book.asks

let get_best_bid (order_book : order_book) = 
  Hashtbl.fold (fun price queue best_bid ->
    match best_bid with
    | None -> Queue.peek_opt queue
    | Some order -> 
      let curr_best = get_price order in
      if price > curr_best then Queue.peek_opt queue
      else best_bid
  ) order_book.bids None

let get_best_ask (order_book : order_book) = 
  Hashtbl.fold (fun price queue best_ask ->
    match best_ask with
    | None -> Queue.peek_opt queue
    | Some order -> 
      let curr_best = get_price order in
      if price < curr_best then Queue.peek_opt queue
      else best_ask
  ) order_book.asks None

let get_bids (order_book : order_book) = 
  Hashtbl.fold (fun _ queue acc -> acc @ List.of_seq (Queue.to_seq queue)) order_book.bids []

let get_asks (order_book : order_book) = 
  Hashtbl.fold (fun _ queue acc -> acc @ List.of_seq (Queue.to_seq queue)) order_book.asks []

let match_orders (order_book : order_book) (market_conditions : Market_conditions.t) (curr_time : float) = 
  let bids = List.filter (fun order -> not (is_expired order curr_time)) (get_bids order_book) in
  let asks = List.filter (fun order -> not (is_expired order curr_time)) (get_asks order_book) in
  
  let sorted_bids = List.sort (fun a b -> Float.compare (get_price b) (get_price a)) bids in
  let sorted_asks = List.sort (fun a b -> Float.compare (get_price a) (get_price b)) asks in
  
  let rec match_aux sorted_bids sorted_asks acc = 
    match sorted_bids, sorted_asks with
    | [], _ | _, [] -> acc
    | bid :: rest_bids, ask :: rest_asks -> 
      let bid_price = get_price bid in
      let ask_price = get_price ask in
      if bid_price >= ask_price && Market_conditions.check_spread_conditions market_conditions bid_price ask_price then
        let matched_order = (bid, ask) in
        match_aux rest_bids rest_asks (matched_order :: acc)
      else
        acc
  in
  match_aux sorted_bids sorted_asks []

let remove_expired_orders (order_book : order_book) (curr_time : float) = 
  let remove_expired_orders_from_table table = 
    Hashtbl.iter (fun price queue ->
      let remaining_orders = Queue.create () in
      Queue.iter (fun order -> if not (is_expired order curr_time) then Queue.push order remaining_orders) queue;
      if Queue.is_empty remaining_orders then
        Hashtbl.remove table price
      else
        Hashtbl.replace table price remaining_orders
    ) table
  in
  remove_expired_orders_from_table order_book.bids;
  remove_expired_orders_from_table order_book.asks