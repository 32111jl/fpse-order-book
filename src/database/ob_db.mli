(** Postgresql database, handles persistence of order books and trades *)
(* 
type db_config = {
  dbname : string;
  host : string;
  user : string;
  password : string;
  port : string;
}

val default_db_config : db_config

val connect : db_config -> Postgresql.connection

val init_db : Postgresql.connection -> unit

val create_user : Postgresql.connection -> int -> float -> bool

val get_balance : Postgresql.connection -> int -> float

val update_balance : Postgresql.connection -> int -> float -> bool

val get_position : Postgresql.connection -> int -> string -> float

val update_position : Postgresql.connection -> int -> string -> float -> bool

val add_order : Postgresql.connection -> Order_types.order -> bool

val record_trade : Postgresql.connection -> Order_types.order -> Order_types.order -> float -> float -> bool

val add_security : Postgresql.connection -> string -> bool

val update_security_status : Postgresql.connection -> string -> bool -> bool *)