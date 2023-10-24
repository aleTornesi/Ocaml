module type IntervalI = sig
  type interval
  type endpoint
  val create : endpoint -> endpoint -> interval 
  val is_empty : interval -> bool
  val contains : interval -> endpoint -> bool
  val intersect : interval -> interval -> interval
  val tostring : interval -> string
  exception WrongInterval
end

module type Comparable = sig
  type t
  val compare : t -> t -> int
  val tostring : t -> string 
end

module ComparableString = struct
  type t = string
  let compare s1 s2 = String.compare s1 s2
  let tostring string = "\"" ^ string ^ "\""
end

module IntComparable = struct
  type t = int
  let compare v1 v2 = Int.compare v1 v2
  let tostring v = string_of_int v
end

module Interval(M: Comparable) = struct
    type interval = M.t * M.t
    type endpoint = M.t
    exception WrongInterval
    
    let is_empty e = (fst e) == (snd e)
    let create e1 e2 = if e1 <= e2 then (e1, e2) else raise WrongInterval
    let contains i e = (fst i) <= e && e <= (snd i) 
    let intersect i1 i2 = let lower = max (fst i1) (fst i2) and upper = min (snd i1) (snd i2) in 
    if(lower <= upper) then (lower, upper) else raise WrongInterval
    let tostring i = "[" ^ M.tostring(fst i) ^ ", " ^ M.tostring(snd i) ^ "]"
end

module StringInterval = Interval(ComparableString)
module IntInterval = Interval(IntComparable)