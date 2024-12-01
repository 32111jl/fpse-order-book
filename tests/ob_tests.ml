open Core
open OUnit2
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

  let series = 
    "order_tests" >::: [
      "test_create_order" >:: test_create_order;
      "test_is_expired" >:: test_is_expired
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
    let bids = get_bids book in
    assert_equal (List.length bids) 0

  let test_add_insufficient_market_order _ =
    let book = create_order_book "AAPL" in
    let order = create_order (generate_order_id book) "AAPL" Market Buy 10.0 2 in
    assert_raises (Failure "Not enough liquidity for market order.") (fun () -> add_order book order)

  let test_add_limit_order _ =
    let book = create_order_book "AAPL" in
    let order = create_order (generate_order_id book) "AAPL" (Limit { price = 150.0; expiration = None }) Buy 10.0 1 in
    add_order book order;
    let bids = get_bids book in
    assert_equal (List.length bids) 1;
    assert_equal (get_price_helper (List.hd_exn bids)) 150.0;
    assert_equal (List.hd_exn bids).qty 10.0

  (* let test_add_margin_order _ =
    let book = create_order_book "AAPL" in
    let user_id = 1 in
    update_user_balance user_id 100.0;
    let order = create_order (generate_order_id book) "AAPL" (Margin 50.0) Buy 2.0 user_id in
    add_order book order;
    let bids = get_bids book in
    assert_equal (List.length bids) 1;
    assert_equal (get_price (List.hd_exn bids)) 50.0;
    assert_equal (List.hd_exn bids).qty 2.0
    assert_equal (get_user_balance user_id) 75.0 *)

  let test_remove_market_order _ = 
    let book = create_order_book "AAPL" in
    let order = create_order (generate_order_id book) "AAPL" Market Sell 10.0 1 in
    assert_raises (Failure "Not enough liquidity for market order.") (fun () -> add_order book order)

  (* let test_margin_insufficient_bal _ =
    let book = create_order_book "AAPL" in
    let user_id = 1 in
    update_user_balance user_id 10.0;
    let order = create_order (generate_order_id book) "AAPL" (Margin 50.0) Buy 2.0 user_id in
    assert_raises (Failure "Insufficient funds. Please deposit more money.") (fun () -> add_order book order) *)

  let test_remove_limit_order _ =
    let book = create_order_book "AAPL" in
    let order1 = create_order (generate_order_id book) "AAPL" (Limit { price = 150.0; expiration = None }) Buy 10.0 1 in
    let order2 = create_order (generate_order_id book) "AAPL" (Limit { price = 155.0; expiration = None }) Buy 10.0 2 in
    add_order book order1; (* id = 0 *)
    add_order book order2; (* id = 1 *)
    let curr_bids = get_bids book in
    assert_equal (List.length curr_bids) 2;
    remove_order book 0; (* first order has ID of 0 *)
    let new_bids = get_bids book in
    assert_equal (List.length new_bids) 1;
    assert_equal (get_price_helper (List.hd_exn new_bids)) 155.0

  let test_best_bid_ask _ =
    let book = create_order_book "AAPL" in
    let order1 = create_order (generate_order_id book) "AAPL" (Limit { price = 150.0; expiration = None }) Buy 10.0 1 in
    let order2 = create_order (generate_order_id book) "AAPL" (Limit { price = 155.0; expiration = None }) Sell 5.0 2 in
    add_order book order1;
    add_order book order2;
    let best_bid = get_best_bid book in
    let best_ask = get_best_ask book in
    assert_equal (get_price_helper (Option.value_exn best_bid)) 150.0;
    assert_equal (get_price_helper (Option.value_exn best_ask)) 155.0

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


  let series = "order_book_tests" >::: [
    "test_create_ob" >:: test_create_ob;
    "test_create_dupe_id" >:: test_create_dupe_id;
    "test_add_partial_fill_market_order" >:: test_add_partial_fill_market_order;
    "test_add_insufficient_market_order" >:: test_add_insufficient_market_order;
    "test_add_order" >:: test_add_limit_order;
    (* "test_add_margin_order" >:: test_add_margin_order; *)
    "test_remove_market_order" >:: test_remove_market_order;
    (* "test_margin_insufficient_bal" >:: test_margin_insufficient_bal; *)
    "test_remove_order" >:: test_remove_limit_order;
    "test_best_bid_ask" >:: test_best_bid_ask;
    "test_remove_expired" >:: test_remove_expired
  ]
end

module MatchingEngineTests = struct
  let create_market_conds ba_spread margin_rate = 
    create_market_conditions ba_spread margin_rate

  let populate_ob () = 
    let book = create_order_book "AAPL" in
    let order1 = create_order (generate_order_id book) "AAPL" (Limit { price = 100.0; expiration = None }) Buy 10.0 1 in
    let order2 = create_order (generate_order_id book) "AAPL" (Limit { price = 102.0; expiration = None }) Sell 5.0 2 in
    let order3 = create_order (generate_order_id book) "AAPL" (Limit { price = 101.0; expiration = None }) Buy 8.0 3 in
    let order4 = create_order (generate_order_id book) "AAPL" (Limit { price = 103.0; expiration = None }) Sell 12.0 4 in
    add_order book order1;
    add_order book order2;
    add_order book order3;
    add_order book order4;
    book

  let test_check_spread _ = 
    let book = populate_ob () in
    let market_conds = create_market_conds 3.0 0.5 in
    assert_equal (check_spread book market_conds) true;
    let tighter_market_conds = create_market_conds 0.5 0.5 in
    assert_equal (check_spread book tighter_market_conds) false

  let test_execute_trade _ =
    let buy = create_order 0 "AAPL" (Limit { price = 100.0; expiration = None }) Buy 10.0 1 in
    let sell = create_order 1 "AAPL" (Limit { price = 100.0; expiration = None }) Sell 5.0 2 in
    let trade_qty = execute_trade buy sell in
    assert_equal trade_qty 5.0;
    assert_equal buy.qty 5.0;
    assert_equal sell.qty 0.0

  let test_match_orders _ = 
    let book = populate_ob () in
    let market_conds = create_market_conds 5.0 0.5 in
    let trades = match_orders book market_conds in
    let trades = List.rev trades in
    assert_equal (List.length trades) 3;
    match trades with
    | trade1 :: trade2 :: trade3 :: _ ->
      assert_equal trade1.trade_qty 5.0; (* buy @ 101 / 8.0, ask @ 102 / 5.0 *)
      assert_equal trade1.buy_qty_after 3.0; (* match above, 8.0 - 5.0 --> buy @ 101 / 3.0 remaining *)
      assert_equal trade1.sell_qty_after 0.0; (* should be nothing, all 102 / 5.0 were sold *)

      assert_equal trade2.trade_qty 3.0; (* buy @ 101 / 3.0, ask @ 103 / 12.0 *)
      assert_equal trade2.buy_qty_after 0.0; (* second buy is 0.0 *)
      assert_equal trade2.sell_qty_after 9.0; (* should be 103 / 9.0, 103 / (12.0 - 3.0) sell remaining *)

      assert_equal trade3.trade_qty 9.0; (* buy @ 100 / 10.0, ask @ 103 / 9.0 *)
      assert_equal trade3.buy_qty_after 1.0; (* match above, 10.0 - 9.0 --> buy @ 100 / 1.0 remaining *)
      assert_equal trade3.sell_qty_after 0.0 (* should be nothing, all 9.0 were sold *)
    | _ -> assert_failure "Expected 3 trades"

  let test_match_multiple_books _ =
    let book1 = populate_ob () in
    let book2 = create_order_book "TSLA" in
    let order5 = create_order (generate_order_id book2) "TSLA" (Limit { price = 200.0; expiration = None }) Buy 15.0 5 in
    let order6 = create_order (generate_order_id book2) "TSLA" (Limit { price = 198.0; expiration = None }) Sell 10.0 6 in
    add_order book2 order5;
    add_order book2 order6;
    let market_conds = create_market_conds 10.0 0.5 in
    let trades = match_all_books [book1; book2] market_conds in
    assert_equal (List.length trades) 4

  let series = "matching_engine_tests" >::: [
      "test_check_spread" >:: test_check_spread;
      "test_execute_trade" >:: test_execute_trade;
      "test_match_orders" >:: test_match_orders;
      "test_match_multiple_books" >:: test_match_multiple_books
    ]
end

let series = 
  "Order Book tests" >::: [
    OrderTests.series;
    OBTests.series;
    MatchingEngineTests.series
  ]

let () =
  run_test_tt_main series;