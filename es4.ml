let rec remove_punctuation = function
| "" -> ""
| s when(
  s.[0] == ',' || 
  s.[0] == '.' || 
  s.[0] == ';' || 
  s.[0] == '?' || 
  s.[0] == '!' || 
  s.[0] == '(' || 
  s.[0] == ')' || 
  s.[0] == '[' || 
  s.[0] == ']' || 
  s.[0] == '{' || 
  s.[0] == '}' || 
  s.[0] == ':' 
  ) -> remove_punctuation(String.sub s 1 (String.length s - 1))
  | s -> String.make 1 s.[0] ^ remove_punctuation(String.sub s 1 (String.length s - 1))

let countOccorences list = List.fold_left (fun a e -> a + 1) 0 list
let rec removeElement x = function
| [] -> []
| h::l when(String.compare h x == 0) -> removeElement x l
| h::l -> h::removeElement x l


let f string = 
  let rec f result = function
  | [] -> result
  | h::l -> f ((h, countOccorences(h::l))::result) (removeElement h l)
in f [] (List.map (fun s -> remove_punctuation s) (String.split_on_char (Str.regexp "[ \n\r\x0c\t]+") (String.lowercase_ascii string)))

let print_string_int_list l = print_string "[";List.iter(fun t -> print_string(fst t); print_string ": "; print_int(snd t) ; print_string " ") l ;print_string"]"

let print_string_list l = print_string "[";List.iter(fun (s) -> (print_string (s); print_string(" "))) l;print_string "]"
let print_int_list l = print_string "[";List.iter(fun (v) -> (print_int (v); print_string(" "))) l;print_string "]"

let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
let s = read_whole_file "es4.in"
let res = f s
let main() = print_string_int_list res;;

main()