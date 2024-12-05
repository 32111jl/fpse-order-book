open Core
open OUnit2
(* open QCheck *)
open Order_book_lib.Order
open Order_book_lib.Order_book
open Order_book_lib.Market_conditions
open Order_book_lib.Matching_engine

let get_price_helper order = 
  match get_price order with
  | None -> assert_failure "Expected order to have a price."
  | Some price -> price

module OrderTests = struct

  let test_create_order _ =
    let order = create_order 0 "AAPL" (Limit { price = 150.0; expiration = None }) Buy 10.0 1 in
    assert_equal order.security "AAPL";
    assert_equal order.order_type (Limit { price = 150.0; expiration = None });
    assert_equal order.buy_sell Buy;
    assert_equal order.qty 10.0;
    assert_equal order.user_id 1;
    assert_equal order.id 0;
    let order2 = create_order 1 "AAPL" (Limit { price = 150.0; expiration = None }) Sell 20.0 2 in
    let order3 = create_order 2 "AAPL" (Limit { price = 150.0; expiration = None }) Buy 5.0 3 in
    assert_equal order2.id 1;
    assert_equal order3.id 2

  let test_is_expired _ =
    let curr_time = Core_unix.time () in
    let expired = create_order 0 "AAPL" (Limit { price = 150.0; expiration = Some (curr_time -. 10.0) }) Buy 5.0 2 in
    let active = create_order 1 "AAPL" (Limit { price = 150.0; expiration = Some (curr_time +. 10.0) }) Buy 5.0 2 in
    let market_order = create_order 2 "AAPL" Market Sell 20.0 3 in
    assert_equal (is_expired expired curr_time) true;
    assert_equal (is_expired active curr_time) false;
    assert_equal (is_expired market_order curr_time) false

  let test_order_price _ =
    let limit_order = create_order 0 "AAPL" (Limit { price = 150.0; expiration = None }) Buy 10.0 1 in
    let market_order = create_order 1 "AAPL" Market Buy 10.0 1 in
    let margin_order = create_order 2 "AAPL" (Margin 160.0) Buy 10.0 1 in
    assert_equal (get_price limit_order) (Some 150.0);
    assert_equal (get_price market_order) None;
    assert_equal (get_price margin_order) (Some 160.0)

  let series = 
    "order_tests" >::: [
      "test_create_order" >:: test_create_order;
      "test_is_expired" >:: test_is_expired;
      "test_order_price" >:: test_order_price
    ]
end

module OBTests = struct

  let test_create_ob _ = 
    let book = create_order_book "AAPL" in
    assert_equal "AAPL" (get_security book);
    let bids = get_bids book in
    let asks = get_asks book in
    assert_equal (List.length bids) 0;
    assert_equal (List.length asks) 0

  let test_create_dupe_id _ =
    let book = create_order_book "AAPL" in
    let order1 = create_order (generate_order_id book) "AAPL" (Limit { price = 150.0; expiration = None }) Buy 10.0 1 in
    let order2 = create_order (generate_order_id book) "AAPL" (Limit { price = 155.0; expiration = None }) Sell 5.0 2 in
    add_order book order1;
    add_order book order2;
    let dupe_order = { order1 with qty = 20.0 } in
    assert_raises (Failure "duplicate order ID") (fun () -> add_order book dupe_order);
    let bids = get_bids book in
    let asks = get_asks book in
    (* should be one bid and one ask, dupe didn't count *)
    assert_equal (List.length bids) 1;
    assert_equal (List.length asks) 1;
    assert_equal (get_price_helper (List.hd_exn bids)) 150.0;
    assert_equal (get_price_helper (List.hd_exn asks)) 155.0

  let test_add_partial_fill_market_order _ =
    let book = create_order_book "AAPL" in
    let sell_order = create_order (generate_order_id book) "AAPL" (Limit { price = 150.0; expiration = None }) Sell 10.0 1 in
    add_order book sell_order;
    let order = create_order (generate_order_id book) "AAPL" Market Buy 5.0 2 in
    add_order book order;
    let asks = get_asks book in
    assert_equal (List.length asks) 1;
    assert_equal (List.hd_exn asks).qty 5.0;
    (* should be zero bids *)
    let bids = get_bids book in
    assert_equal (List.length bids) 0

  let test_add_insufficient_market_order _ =
    let book = create_order_book "AAPL" in
    let order = create_order (generate_order_id book) "AAPL" Market Buy 10.0 2 in
    assert_raises (Failure "Not enough liquidity for market order.") (fun () -> add_order book order)

  let test_add_limit_order _ =
    let book = create_order_book "AAPL" in
    
    (* test buy side *)
    let buy_order = create_order (generate_order_id book) "AAPL" (Limit { price = 150.0; expiration = None }) Buy 10.0 1 in
    add_order book buy_order;
    let bids = get_bids book in
    assert_equal (List.length bids) 1;
    assert_equal (get_price_helper (List.hd_exn bids)) 150.0;
    assert_equal (List.hd_exn bids).qty 10.0;
    
    (* test sell side *)
    let sell_order1 = create_order (generate_order_id book) "AAPL" (Limit { price = 155.0; expiration = None }) Sell 5.0 2 in
    add_order book sell_order1;
    assert_equal (get_best_ask book) (Some 155.0);
    
    (* sell side, better price updates best ask *)
    let sell_order2 = create_order (generate_order_id book) "AAPL" (Limit { price = 153.0; expiration = None }) Sell 7.0 3 in
    add_order book sell_order2;
    assert_equal (get_best_ask book) (Some 153.0);
    
    (* sell side, worse price doesn't update best ask *)
    let sell_order3 = create_order (generate_order_id book) "AAPL" (Limit { price = 156.0; expiration = None }) Sell 3.0 4 in
    add_order book sell_order3;
    assert_equal (get_best_ask book) (Some 153.0);
    
    (* check that the asks are in the correct order *)
    let asks = get_asks book in
    assert_equal (List.length asks) 3;
    assert_equal (get_price_helper (List.hd_exn asks)) 153.0

  let test_add_margin_order _ =
    let book = create_order_book "AAPL" in
    let order = create_order (generate_order_id book) "AAPL" (Margin 150.0) Buy 10.0 1 in
    add_order book order;
    let bids = get_bids book in
    assert_equal (List.length bids) 1;
    assert_equal (get_price_helper (List.hd_exn bids)) 150.0;
    assert_equal (List.hd_exn bids).qty 10.0

  let test_remove_market_order _ = 
    let book = create_order_book "AAPL" in
    let order = create_order (generate_order_id book) "AAPL" Market Sell 10.0 1 in
    assert_raises (Failure "Not enough liquidity for market order.") (fun () -> add_order book order)

  let test_margin_insufficient_bal _ =
    let book = create_order_book "AAPL" in
    let order = create_order (generate_order_id book) "AAPL" (Margin 150.0) Buy 10.0 1 in
    add_order book order;
    let bids = get_bids book in
    assert_equal (List.length bids) 1;
    assert_equal (get_price_helper (List.hd_exn bids)) 150.0;
    assert_equal (List.hd_exn bids).qty 10.0

  let test_remove_limit_order _ =
    let book = create_order_book "AAPL" in
    let order1 = create_order (generate_order_id book) "AAPL" (Limit { price = 150.0; expiration = None }) Buy 10.0 1 in
    let order2 = create_order (generate_order_id book) "AAPL" (Limit { price = 155.0; expiration = None }) Buy 10.0 2 in
    let order3 = create_order (generate_order_id book) "AAPL" (Limit { price = 150.0; expiration = None }) Buy 5.0 3 in
    add_order book order1;
    add_order book order2;
    add_order book order3;
    remove_order book 999;  (* should do nothing *)
    
    let curr_bids = get_bids book in
    assert_equal (List.length curr_bids) 3;
    
    remove_order book 0;
    let new_bids = get_bids book in
    assert_equal (List.length new_bids) 2;
    
    remove_order book 2;
    let final_bids = get_bids book in
    assert_equal (List.length final_bids) 1;
    assert_equal (get_price_helper (List.hd_exn final_bids)) 155.0;
    
    let non_existent_order = create_order 999 "AAPL" (Limit { price = 160.0; expiration = None }) Buy 10.0 1 in
    remove_order book non_existent_order.id

  let test_best_bid_ask _ =
    let book = create_order_book "AAPL" in
    assert_equal (get_best_bid book) None;
    assert_equal (get_best_ask book) None;
    let order1 = create_order (generate_order_id book) "AAPL" (Limit { price = 150.0; expiration = None }) Buy 10.0 1 in
    let order2 = create_order (generate_order_id book) "AAPL" (Limit { price = 155.0; expiration = None }) Sell 5.0 2 in
    add_order book order1;
    add_order book order2;
    let best_bid = get_best_bid book in
    let best_ask = get_best_ask book in
    assert_equal best_bid (Some 150.0);
    assert_equal best_ask (Some 155.0)
  
  let test_get_best_ask_some _ =
    let book = create_order_book "AAPL" in
    let sell_order = create_order (generate_order_id book) "AAPL" 
      (Limit { price = 150.0; expiration = None }) Sell 10.0 1 in
    add_order book sell_order;
    let first_ask = get_best_ask book in
    assert_equal first_ask (Some 150.0);
    let second_ask = get_best_ask book in
    assert_equal second_ask (Some 150.0)

  let test_remove_expired _ = 
    let book = create_order_book "AAPL" in
    (* order 1 should be expired *)
    let order1 = create_order (generate_order_id book) "AAPL" (Limit { price = 150.0; expiration = Some 0.0 }) Buy 10.0 1 in
    (* order 2 should not be expired *)
    let order2 = create_order (generate_order_id book) "AAPL" (Limit { price = 155.0; expiration = None }) Buy 5.0 2 in
    add_order book order1;
    add_order book order2;
    remove_expired_orders book 1.0;
    let bids = get_bids book in
    assert_equal (List.length bids) 1;
    assert_equal (get_price_helper (List.hd_exn bids)) 155.0;
    assert_equal (List.length (get_asks book)) 0

  let test_order_book_state _ =
    let book = create_order_book "AAPL" in
    let order1 = create_order (generate_order_id book) "AAPL" (Limit { price = 100.0; expiration = None }) Buy 10.0 1 in
    let order2 = create_order (generate_order_id book) "AAPL" (Limit { price = 102.0; expiration = None }) Sell 5.0 2 in
    add_order book order1;
    add_order book order2;
    let bids = get_bids book in
    let asks = get_asks book in
    assert_equal (List.length bids) 1;
    assert_equal (List.length asks) 1;
    assert_equal (List.hd_exn bids).qty 10.0;
    assert_equal (List.hd_exn asks).qty 5.0

  let test_match_market_order_recursive _ =
    let book = create_order_book "AAPL" in
    let sell_order = create_order (generate_order_id book) "AAPL" (Limit { price = 150.0; expiration = None }) Sell 5.0 1 in
    add_order book sell_order;
    let buy_order = create_order (generate_order_id book) "AAPL" Market Buy 10.0 2 in
    assert_raises (Failure "Not enough liquidity for market order.") 
      (fun () -> add_order book buy_order);
    let asks = get_asks book in
    assert_equal (List.length asks) 0

  let test_check_order_exists _ =
    let book = create_order_book "AAPL" in
    let order1 = create_order (generate_order_id book) "AAPL" (Limit { price = 150.0; expiration = None }) Buy 10.0 1 in
    let order2 = create_order (generate_order_id book) "AAPL" (Limit { price = 155.0; expiration = None }) Buy 10.0 2 in
    
    assert_equal (check_order_exists book 999) false; (* non-existent order *)
    
    add_order book order1;
    add_order book order2;
    assert_equal (check_order_exists book order1.id) true;
    assert_equal (check_order_exists book order2.id) true;
    
    remove_order book order1.id;
    assert_equal (check_order_exists book order1.id) false;
    assert_equal (check_order_exists book order2.id) true

  let series = "order_book_tests" >::: [
    "test_create_ob" >:: test_create_ob;
    "test_create_dupe_id" >:: test_create_dupe_id;
    "test_add_partial_fill_market_order" >:: test_add_partial_fill_market_order;
    "test_add_insufficient_market_order" >:: test_add_insufficient_market_order;
    "test_add_order" >:: test_add_limit_order;
    "test_add_margin_order" >:: test_add_margin_order;
    "test_remove_market_order" >:: test_remove_market_order;
    "test_margin_insufficient_bal" >:: test_margin_insufficient_bal;
    "test_remove_order" >:: test_remove_limit_order;
    "test_best_bid_ask" >:: test_best_bid_ask;
    "test_get_best_ask_some" >:: test_get_best_ask_some;
    "test_remove_expired" >:: test_remove_expired;
    "test_order_book_state" >:: test_order_book_state;
    "test_match_market_order_recursive" >:: test_match_market_order_recursive;
    "test_check_order_exists" >:: test_check_order_exists
  ]
end

module MatchingEngineTests = struct
  let create_market_conds ba_spread margin_rate = 
    create_market_conditions ba_spread margin_rate

  let test_get_margin_rate _ =
    let market_conds = create_market_conds 3.0 0.5 in
    assert_equal (get_margin_rate market_conds) 0.5;
    let market_conds2 = create_market_conds 2.0 0.75 in
    assert_equal (get_margin_rate market_conds2) 0.75

  let test_check_spread _ = 
    let book = create_order_book "AAPL" in
    let order1 = create_order (generate_order_id book) "AAPL" (Limit { price = 100.0; expiration = None }) Buy 10.0 1 in
    let order2 = create_order (generate_order_id book) "AAPL" (Limit { price = 102.0; expiration = None }) Sell 5.0 2 in
    add_order book order1;
    add_order book order2;
    let market_conds = create_market_conds 3.0 0.5 in
    assert_equal (check_spread book market_conds) true;
    let tighter_market_conds = create_market_conds 0.5 0.5 in
    assert_equal (check_spread book tighter_market_conds) false

  let test_execute_trade _ =
    let buy = create_order 0 "AAPL" (Limit { price = 100.0; expiration = None }) Buy 10.0 1 in
    let sell = create_order 1 "AAPL" (Limit { price = 100.0; expiration = None }) Sell 5.0 2 in
    let trade_qty, trade_price = execute_trade buy sell in
    assert_equal trade_qty 5.0;
    assert_equal trade_price 100.0;
    assert_equal buy.qty 5.0;
    assert_equal sell.qty 0.0

  let test_match_orders _ = 
    let book = create_order_book "AAPL" in
    let order1 = create_order (generate_order_id book) "AAPL" (Limit { price = 100.0; expiration = None }) Buy 10.0 1 in
    let order2 = create_order (generate_order_id book) "AAPL" (Limit { price = 102.0; expiration = None }) Sell 5.0 2 in
    add_order book order1;
    add_order book order2;
    let market_conds = create_market_conds 5.0 0.5 in
    let trades = match_orders book market_conds in
    let trades = List.rev trades in
    assert_equal (List.length trades) 1;
    match trades with
    | trade :: [] ->
      assert_equal trade.trade_qty 5.0;
      assert_equal trade.buy_qty_after 5.0;
      assert_equal trade.sell_qty_after 0.0
    | _ -> assert_failure "Expected 1 trade"

  let test_match_multiple_books _ =
    let book1 = create_order_book "AAPL" in
    let book2 = create_order_book "TSLA" in
    
    let order1 = create_order (generate_order_id book1) "AAPL" (Limit { price = 100.0; expiration = None }) Buy 10.0 1 in
    let order2 = create_order (generate_order_id book1) "AAPL" (Limit { price = 102.0; expiration = None }) Sell 5.0 2 in
    let order3 = create_order (generate_order_id book2) "TSLA" (Limit { price = 200.0; expiration = None }) Buy 15.0 3 in
    let order4 = create_order (generate_order_id book2) "TSLA" (Limit { price = 198.0; expiration = None }) Sell 10.0 4 in
    
    add_order book1 order1;
    add_order book1 order2;
    add_order book2 order3;
    add_order book2 order4;
    
    let market_conds = create_market_conds 10.0 0.5 in
    let trades = match_all_books [book1; book2] market_conds in
    assert_equal (List.length trades) 2;
    match trades with
    | trade1 :: trade2 :: [] ->
      assert_equal trade1.trade_qty 5.0;
      assert_equal trade1.buy_qty_after 5.0;
      assert_equal trade1.sell_qty_after 0.0;
      assert_equal trade2.trade_qty 10.0;
      assert_equal trade2.buy_qty_after 5.0;
      assert_equal trade2.sell_qty_after 0.0
    | _ -> assert_failure "Expected 2 trades"

  let test_no_matching_orders _ =
    let book = create_order_book "AAPL" in
    let order1 = create_order (generate_order_id book) "AAPL" (Limit { price = 100.0; expiration = None }) Buy 10.0 1 in
    let order2 = create_order (generate_order_id book) "AAPL" (Limit { price = 105.0; expiration = None }) Sell 5.0 2 in
    add_order book order1;
    add_order book order2;
    let market_conds = create_market_conds 2.0 0.5 in
    let trades = match_orders book market_conds in
    assert_equal (List.length trades) 0

  let series = "matching_engine_tests" >::: [
    "test_check_spread" >:: test_check_spread;
    "test_execute_trade" >:: test_execute_trade;
    "test_match_orders" >:: test_match_orders;
    "test_match_multiple_books" >:: test_match_multiple_books;
    "test_no_matching_orders" >:: test_no_matching_orders;
    "test_get_margin_rate" >:: test_get_margin_rate
  ]
end

(* module QuickCheckTests = struct

  let price_gen = Gen.(float_range 1.0 1000.0)
  let qty_gen = Gen.(float_range 1.0 100.0)
  let user_id_gen = Gen.(int_range 1 1000)
  let order_type_gen = Gen.(oneof [
    return Market;
    map (fun price -> Limit { price; expiration = None }) price_gen;
    map (fun price -> Margin price) price_gen
  ])

  let side_gen = Gen.(map (fun b -> if b then Buy else Sell) bool)

  let test_order_creation = Test.make
    ~name:"random order creation preserves properties"
    ~count:1000
    (quad 
      (make price_gen)
      (make (Gen.map int_of_float qty_gen))
      (make user_id_gen)
      (make side_gen))
    (fun (price, qty, user_id, side) ->
      let order = create_order 0 "AAPL" (Limit { price; expiration = None }) side (float_of_int qty) user_id in
      Float.equal order.qty (float_of_int qty) && 
      order.user_id = user_id && 
      order.buy_sell = side &&
      match order.order_type with
      | Limit { price = p; _ } -> Float.abs (p -. price) < 0.001
      | _ -> false)

  let test_order_book_sorting = Test.make
    ~name:"order book maintains price-time priority"
    ~count:100
    (list_of_size Gen.(int_range 2 10) (pair price_gen qty_gen))
    (fun price_qty_pairs ->
      let book = create_order_book "AAPL" in
      List.iter (fun (price, qty) ->
        let order = create_order (generate_order_id book) "AAPL" (Limit { price; expiration = None }) Buy qty 1 in
        add_order book order
      ) price_qty_pairs;
      true)

  let series = "quickcheck_tests" >::: [
    QCheck_runner.to_ounit2_test test_order_creation;
    QCheck_runner.to_ounit2_test test_order_book_sorting;
  ]
end *)

let series = 
  "Order Book tests" >::: [
    OrderTests.series;
    OBTests.series;
    MatchingEngineTests.series;
    (* QuickCheckTests.series *)
  ]

let () =
  run_test_tt_main series;