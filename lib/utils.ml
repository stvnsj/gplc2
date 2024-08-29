
(** Zarith module *)
open Q

(** fraction type. This is the type of probabilities throughout
    the system. *)
type fraction = Q.t


module Fraction = struct
  let isZero : fraction -> bool =
    fun frac -> 
    let kind = Q.classify frac in
    match kind with
    | Q.NZERO -> true
    | Q.ZERO
      | Q.INF
      | Q.MINF
      | Q.UNDEF -> false
end 

  

(** [optionToBool] takes an optional type argument [x].
    It returns true if [x] is Some value. If None, it
    returns false. *)
let optionToBool : 'a option -> bool =
  fun x ->
  match x with
  | Some _ -> true
  | None   -> false
