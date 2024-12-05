type trade_record = {
  timestamp : float;    (* seconds since epoch *)
  security : string;   (* security symbol *)  
  qty : float;         (* quantity *)
  price : float;        (* price *)
  is_buy : bool;        (* true if buy, false if sell *)
}

type user = {
  id : int;
  mutable balance : float;
  mutable positions : (string, float) Hashtbl.t;  (* security, quantity *)
  mutable trade_history : (string, trade_record list) Hashtbl.t;  (* security, list of trades *)
}

let user_balances : (int, user) Hashtbl.t = Hashtbl.create 16

let create_user id initial_balance =
  let user = { 
    id; 
    balance = initial_balance;
    positions = Hashtbl.create 16;
    trade_history = Hashtbl.create 16;
  } in
  Hashtbl.add user_balances id user;
  user

let get_balance user_id =
  match Hashtbl.find_opt user_balances user_id with
  | Some user -> user.balance
  | None -> 0.0

let update_balance user_id amount =
  match Hashtbl.find_opt user_balances user_id with
  | Some user -> user.balance <- user.balance +. amount
  | None -> 
    let user = create_user user_id amount in
    user.balance <- amount

let get_position user_id security =
  match Hashtbl.find_opt user_balances user_id with
  | None -> 0.0
  | Some user ->
    match Hashtbl.find_opt user.positions security with
    | None -> 0.0
    | Some qty -> qty

let update_position user_id security qty_change =
  match Hashtbl.find_opt user_balances user_id with
  | None -> 
    let user = create_user user_id 0.0 in
    Hashtbl.add user.positions security qty_change
  | Some user ->
    let current = get_position user_id security in
    Hashtbl.replace user.positions security (current +. qty_change)