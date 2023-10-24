module Matrix = struct
    type matrix = int list list

    let getSize m = 
        (List.length m1, List.length (List.nth m1 0))

    let matricesAreSameSize m1 m2 =
        let (n, m) = getSize m1 
        and (p, q) = getSize m2
        in n == p && m == q
    


    let (=) m1 m2 =
        if matricesAreSameSize m1 m2 then
            List.for_all2 (fun arr1 arr2 -> List.for_all2 (fun m1El m2El -> m1El == m2El) arr1 arr2) m1 m2
        else
            false


    let copy (m: matrix) = List.init
        (List.length m)
        (let size = List.length (List.nth m 0) in
            fun _ -> List.init size (fun e -> e))

    let (+) m1 m2 = if (matricesAreSameSize m1 m2) then
        List.map2 (fun a1 a2 -> List.map2 (fun e1 e2 -> e1 + e2) a1 a2) m1 m2
    else raise (Invalid_argument "Matrices must be of the same size");;

    let ( * ) v m = List.init
        (List.length m)
        (let size = List.length (List.nth m 0) in
            fun _ -> List.init size (fun e -> v * e))


    
    
    let traspose matrix =
        let rec traspose result = function
        | -1 -> Array.of_list result
        | n -> traspose (Array.init (Array.length matrix) (fun (i) -> matrix.(i).(n))::result) (n - 1)
    in traspose [] (Array.length matrix.(0) - 1)

    let ( * ) m1 m2 = let (n, m) = getSize m1 and (p, q) = getSize m2 
        if(p == m) then 
            Array.map(
                fun array1 ->
                    Array.map (
                        fun (array2) -> Array.fold_left (fun acc v -> (v + acc))
                        0
                        (Array.map2 (fun e1 e2 -> (e1 * e2)) array1 array2)
                    ) (traspose matrix2)
            ) matrix1
        else
            raise (Invalid_argument "Matrices are not compatible for multiplication")


end


