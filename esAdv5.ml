module Matrix = struct
    type matrix = int list list

    let matricesAreSameSize m1 m2 =
        let n = List.length m1 and  m = List.length (List.nth m1 0)
        and p = List.length m2 and q = List.length (List.nth m2 0)
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


end


