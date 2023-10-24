module type HasTypeAndFunction = sig
    type t
    val add: t -> t -> t
end

(* f(x, y) = f(x)(y)  *)

module Monoid(M: HasTypeAndFunction) = struct
    type set = M.t

    let add = M.add

end

let x =  0
