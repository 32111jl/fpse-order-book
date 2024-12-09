let order_book_mutex = Mutex.create ()

let with_lock f =
  Mutex.lock order_book_mutex;
  try
    let result = f () in
    Mutex.unlock order_book_mutex;
    result
  with e ->
    Mutex.unlock order_book_mutex;
    raise e

let sync_ob_operation f =
  with_lock f