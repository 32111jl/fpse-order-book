open QCheck
open Order_book_lib.Order
open Order_book_lib.Order_book

let order_gen =
  let open Gen in
  let* id = int in
  let* qty = float in
  let* price = float in
  let* user_id = int in
  return (create_order "AAPL" (Limit { price; expiration = None }) qty user_id)

let test_no_duplicate_ids =
  Test.make ~name:"No duplicate IDs" (list_of_size Gen.(int_range 1 100) order_gen)
    (fun orders ->
      let ids = List.map (fun o -> o.id) orders in
      List.length ids = List.length (List.sort_uniq compare ids))

let () = QCheck_runner.run_tests_main [test_no_duplicate_ids]
