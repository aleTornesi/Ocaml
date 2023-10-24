let alkaline_earth_metals = [4; 12; 20; 38; 56; 88]
let noble_gases = [2; 10; 18; 36; 54; 86]

let highestUnsignedIntegerInList list =
  let rec helper max = function
      [] -> max
    | head::tail -> if (head > max) then helper head tail else helper max tail
  in helper 0 list



  
let rec min = function
    x::[] -> x
  | x::tl -> if (x < min tl) then x else min tl
  
let rec remove value = function
  [] -> []
  | x::tl -> if(x == value) then tl else x::remove value tl
  
let rec selectionSort = function
  [] -> []
  | l -> let m = min l in m::(selectionSort (remove m l))

let rec merge l = function
    [] -> l
  | h::t -> merge (h::l) t;;

let get_name = function
    2 -> "helium"
  | 4 -> "Beryllium"
  | 10 -> "Neon"
  | 12 -> "Magnesium"
  | 18 -> "Argon"
  | 20 -> "Calcium"
  | 36 -> "Krypton"
  | 38 -> "Strontium"
  | 54 -> "Xenon"
  | 56 -> "Barium"
  | 86 -> "Radon"
  | 88 -> "Radium"
  | _ -> "N/A"
let print_name_and_value v = print_string(get_name v);print_int(v);print_newline()

let third_f =
  let rec third_f l = if(l != []) then (print_name_and_value(List.hd l); third_f(List.tl l))
in third_f (selectionSort(merge alkaline_earth_metals noble_gases))




let main() = third_f;;

main();;