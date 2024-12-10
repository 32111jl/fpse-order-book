type security_info = {
  symbol: string;
  name: string;
}

let available_securities = [
  "AAPL"; "MSFT"; "GOOGL"; "AMZN"; "TSLA"; 
  "META"; "NVDA"; "RKLB"; "RIVN"; "PLTR"
]

(* prices here BUT DON'T DO THIS *)
let get_base_price (security : string) : float = match security with
  | "AAPL" -> 150.0 | "MSFT" -> 330.0 | "GOOGL" -> 140.0
  | "AMZN" -> 180.0 | "TSLA" -> 300.0 | "META" -> 300.0
  | "NVDA" -> 400.0 | "RKLB" -> 20.0 | "RIVN" -> 15.0
  | "PLTR" -> 53.0 | _ -> 100.0