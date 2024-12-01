open Core
open OUnit2
open Order_book_lib.Order

module OrderTests = struct
  let test_gen_unique_id _ =
    let id1 = gen_unique_id () in
    let id2 = gen_unique_id () in
    assert_bool "IDs should be unique" (id1 <> id2)

  let test_create_order _ =
    let order = create_order "AAPL" (Limit { price = 150.0; expiration = None }) Buy 10.0 1 in
    assert_equal order.security "AAPL";
    assert_equal order.order_type (Limit { price = 150.0; expiration = None });
    assert_equal order.buy_sell Buy;
    assert_equal order.qty 10.0;
    assert_equal order.user_id 1
    assert_equal order.id 0

  let test_is_expired _ =
    let curr_time = Unix.time () in
    let expired = create_order "AAPL" (Limit { price = 150.0; expiration = Some (curr_time -. 10.0) }) Buy 5.0 2 in
    let active = create_order "AAPL" (Limit { price = 150.0; expiration = Some (curr_time +. 10.0) }) Buy 5.0 2 in
    let market_order = create_order "AAPL" Market Sell 20.0 3 in
    assert_equal (is_expired expired curr_time) true;
    assert_equal (is_expired active curr_time) false;
    assert_equal (is_expired market_order curr_time) false

  let series = 
    "order_tests" >::: [
      "test_gen_unique_id" >:: test_gen_unique_id;
      "test_create_order" >:: test_create_order;
      "test_is_expired" >:: test_is_expired
    ]
end

let () =
  run_test_tt_main OrderTests.series