open Database.Db

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


let create_user id initial_balance =
  if initial_balance > 0.0 then create_user_in_db id "" initial_balance
  else Error "Initial balance cannot be negative!"

let get_balance user_id = get_user_balance user_id

let update_balance user_id amount =
  match get_user_balance user_id with
  | Some current_balance when current_balance +. amount >= 0.0 ->
    update_user_balance user_id (current_balance +. amount)
  | Some _ -> Error "Insufficient balance!"
  | None -> Error "User not found!"

let get_position user_id security = get_positions_by_security user_id security

let update_position user_id security qty_change =
  match get_positions_by_security user_id security with
  | Some current_qty when current_qty +. qty_change >= 0.0 ->
    update_position user_id security qty_change
  | Some _ -> Error "Insufficient position!"
  | None when qty_change >= 0.0 -> update_position user_id security qty_change
  | None -> Error "Can't sell what you don't have..."