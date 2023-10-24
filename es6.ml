let rec pow x = function
| 0 -> 1
| 1 -> x
| n -> let b = pow x (n / 2) 
  in b * b * if n mod  2 == 0 then 1 else x

let rec fact = function
| 0 -> 1
| n -> n * fact (n - 1) 

let rec sin x = function
| 0 -> x
| n -> (Float.pow (-1.0) (Int.to_float n))/.(Int.to_float(fact (2 * n + 1)))*.(Float.pow x (Int.to_float(2 * n + 1))) +. sin x (n - 1)

let rec cos x = function
| 0 -> 1.0
| n -> (Float.pow (-1.0) (Int.to_float n))/.(Int.to_float(fact (2 * n)))*.(Float.pow x (Int.to_float(2 * n))) +. cos x (n - 1)

