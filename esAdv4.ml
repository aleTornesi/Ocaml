let isPrime n =
    let rec isPrime n = function
        | m when(m > int_of_float (Float.sqrt (float_of_int n))) -> true
        | m when(n mod m == 0) -> false
        | m -> isPrime n (m + 1)
in isPrime n 2

let goldbach n =
    let rec goldbach n = function
        | m when(isPrime m && isPrime (n - m)) -> (m, n - m)
        | m -> goldbach n (m+1)
in goldbach n 2

let rec goldbach_list n = function
    | m when (m < n) -> []
    | m when(m mod 2 == 1) -> goldbach_list n (m - 1)
    | m -> goldbach_list n (m - 2) @ [goldbach m]
