module Matrix =
struct
  let zeroes n m = (Array.make_matrix n m 0)
  let identity size =
  let rec make_identity result cur_row =
    match cur_row with
      | 0 -> Array.of_list result
      | _ -> make_identity ((Array.init size (fun n -> if n = (cur_row-1) then 1 else 0)) :: result) (cur_row-1)
  in make_identity [] size;;

  let init n =
    let rec init result = function
    | -1 -> Array.of_list result
    | row -> init ((Array.init n (fun i -> n * row + i))::result) (row - 1)
  in init [] (n - 1)

  let traspose matrix =
    let rec traspose result = function
    | -1 -> Array.of_list result
    | n -> traspose (Array.init (Array.length matrix) (fun (i) -> matrix.(i).(n))::result) (n - 1)
  in traspose [] (Array.length matrix.(0) - 1)

  let sum matrix1 matrix2 = Array.map2 (fun array1 array2 -> Array.map2 (fun e1 e2 -> (e1 + e2)) array1 array2) matrix1 matrix2

  let product matrix1 matrix2 = Array.map(
    fun array1 ->
      Array.map (
        fun (array2) -> Array.fold_left (fun acc v -> (v + acc))
        0
        (Array.map2 (fun e1 e2 -> (e1 * e2)) array1 array2)
      ) (traspose matrix2)
    ) matrix1;;

  let print_matrix matrix =
  let print_array array =
    print_string "[ ";
    Array.iter (fun x -> Printf.printf "%d " x) array;
    print_string "]\n";
    ()
  in
    Array.iter (fun row -> print_array row) matrix;
    print_string "\n";
    ();;
end

let main = Matrix.print_matrix(Matrix.product [|[|1;2;3|];[|4;5;6|]|] [|[|10; 11|];[|20; 21|]; [|30; 31|]|])
