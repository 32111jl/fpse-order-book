open Core
open OUnit2
open Order_book_lib.Order
open Order_book_lib.Order_book

module OBTests = struct
  let test_create_ob _ = 
    let book = create_order_book "AAPL" in
    assert_equal "AAPL" book.security;
    let bids = get_bids book in
    let asks = get_asks book in
    assert_equal (List.length bids) 0;
    assert_equal (List.length asks) 0

  let test_add_order _ =
    let book = create_order_book "AAPL" in
    let order = create_order "AAPL" (Limit { price = 150.0; expiration = None }) Buy 10.0 1 in
    add_order book order;
    let bids = get_bids book in
    assert_equal (List.length bids) 1;
    assert_equal (get_price (List.hd_exn bids)) 150.0;
    assert_equal (List.hd_exn bids).qty 10.0

  let test_remove_order _ =
    let book = create_order_book "AAPL" in
    let order1 = create_order "AAPL" (Limit { price = 150.0; expiration = None }) Buy 10.0 1 in
    let order2 = create_order "AAPL" (Limit { price = 155.0; expiration = None }) Buy 10.0 2 in
    add_order book order1;
    add_order book order2;
    remove_order book 0; (* first order has ID of 0 *)
    let bids = get_bids book in
    assert_equal (List.length bids) 1;
    assert_equal (get_price (List.hd_exn bids)) 155.0

  let test_best_bid_ask _ =
    let book = create_order_book "AAPL" in
    let order1 = create_order "AAPL" (Limit { price = 150.0; expiration = None }) Buy 10.0 1 in
    let order2 = create_order "AAPL" (Limit { price = 155.0; expiration = None }) Sell 5.0 2 in
    add_order book order1;
    add_order book order2;
    let best_bid = get_best_bid book in
    let best_ask = get_best_ask book in
    assert_equal (get_price (Option.value_exn best_bid)) 150.0;
    assert_equal (get_price (Option.value_exn best_ask)) 155.0

  let test_remove_expired _ = 
    let book = create_order_book "AAPL" in
    (* order 1 should be expired *)
    let order1 = create_order "AAPL" (Limit { price = 150.0; expiration = Some 0.0 }) Buy 10.0 1 in
    (* order 2 should not be expired *)
    let order2 = create_order "AAPL" (Limit { price = 155.0; expiration = None }) Buy 5.0 2 in
    add_order book order1;
    add_order book order2;
    remove_expired_orders book 1.0;
    let bids = get_bids book in
    assert_equal (List.length bids) 1;
    assert_equal (get_price (List.hd_exn bids)) 155.0;
    assert_equal (List.length (get_asks book)) 0


  let series = "order_book_tests" >::: [
    "test_create_ob" >:: test_create_ob;
    "test_add_order" >:: test_add_order;
    "test_remove_order" >:: test_remove_order;
    "test_best_bid_ask" >:: test_best_bid_ask;
    "test_remove_expired" >:: test_remove_expired;
  ]
end

let () =
  run_test_tt_main OBTests.series