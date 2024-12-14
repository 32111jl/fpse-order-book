type price = int (* price in thousandths of a unit *)

let float_to_price f = int_of_float (f *. 1000.)

let price_to_float p = float_of_int p /. 1000.

let price_to_string p = string_of_int p

let string_to_price s = int_of_string s

let compare_price p1 p2 = compare p1 p2

let price_max = max_int
let price_min = min_int