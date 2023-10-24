module CharMap = Map.Make (Char);;

let isLetter = function
| 'a' .. 'z' | 'A' .. 'Z' -> true
| _ -> false

let rec isPalindrome = function
| s when(String.length s <= 1) -> true
| s when(not(isLetter s.[0])) -> isPalindrome (String.sub s 1 ((String.length s) - 1))
| s when(not(isLetter s.[(String.length s - 1)])) -> isPalindrome (String.sub s 0 ((String.length s) - 1))
| s when(not(Char.equal (Char.lowercase_ascii s.[0]) (Char.lowercase_ascii s.[(String.length s - 1)]))) -> false
| s -> isPalindrome (String.sub s 1 ((String.length s) - 2))

let rec removeChar c = function
| "" -> ""
| s when(s.[0] == c) -> removeChar c (String.sub s 1 (String.length s - 1))
| s -> String.make 1 s.[0] ^ removeChar c (String.sub s 1 (String.length s - 1))

let rec subtractString s1 = function
| "" -> s1
| s2 -> subtractString (removeChar s2.[0] s1) (String.sub s2 1 (String.length s2 - 1));;

let valueOrZero = function
| None -> 0
| v -> Option.get v

let isAnagram s1 s2 = 
  let rec isAnagram s1 s2 h1 h2 = match s1, s2 with
  | "", "" -> CharMap.equal (fun v1 v2 -> v1 == v2) h1 h2
  | s1, s2 when(String.length s1 != String.length s2) -> false
  | s1, s2 -> isAnagram (String.sub s1 1 (String.length s1 - 1)) (String.sub s2 1 (String.length s2 - 1)) (CharMap.add s1.[0] (valueOrZero (CharMap.find_opt s1.[0] h1)) h1) (CharMap.add s2.[0] (valueOrZero (CharMap.find_opt s2.[0] h2)) h2)  
in isAnagram s1 s2 CharMap.empty CharMap.empty


let rec anagram s = function
| [] -> false
| h::l -> isAnagram s h || anagram s l;;

print_string (Bool.to_string (isPalindrome " "));
print_newline();
print_string (subtractString "Walter Cazzola" "abcwxyz");
print_newline();
print_string (Bool.to_string (anagram "ciao" ["c"; "ooociao"; "acio"; "i"]));
print_newline();
print_string (Bool.to_string (anagram "puffo" ["opff"; "opfffu"; "acio"; "i"]));