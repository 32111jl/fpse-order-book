(* Simple test to check/verify if db connection actually works. *)
open Database.Db

let () =
  match execute_query "SELECT 1" [||] with
  | Ok _ -> Printf.printf "Successfully connected to database!\n"
  | Error msg -> Printf.printf "Connection failed: %s\n" msg