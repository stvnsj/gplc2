open Printf
open Matrix
open Utils
open Solver
(* open Q  *)

type tagVar       = TagVar of fraction option * int * int * int
type simpleType   = Real | Bool | Dynamic | FunType of simpleType * distType
and  distType     = DistType of (simpleType * tagVar) list


let makeTagVar : string -> tagVar =
  fun p ->
  let frac = Q.of_string p in
  TagVar (Some frac , 1 , 1 , 1)



let isZero : tagVar -> bool =
  fun tv ->
  match tv with
  | TagVar (None , _ ,_ ,_) -> false
  | TagVar (Some frac,_,_,_) ->
     if Fraction.isZero frac then true
     else false

(***********************)
(* TO STRING FUNCTIONS *)
(***********************)
let support : distType -> distType =
  fun dt ->
  let DistType lst = dt in
  let filteredList =
    List.filter
      (fun pair -> match pair with | (_,p) -> isZero p) lst in
  DistType filteredList
     
     
     
     

let tagVarToString : tagVar -> string =
  fun tv ->
  match tv with
  | TagVar (Some f,l,r,_) -> sprintf "%s" (Float.to_string (Q.to_float f))
  | TagVar (None,l,r,_) -> sprintf "?"


let tagVarToFraction : tagVar -> fraction option =
  fun tg ->
  match tg with
  | TagVar (fr,_,_,_) -> fr


let rec simpleTypeToString : simpleType -> string =
  fun st ->
  match st with
  | Real -> "Real"
  | Bool -> "Bool"
  | Dynamic -> "?"
  | FunType (st,dt) ->
     sprintf "(%s -> %s)"
       (simpleTypeToString st)
       (distTypeToString dt)


and distTypeToString : distType -> string =
  fun dt ->
  let DistType lst = dt in
  let (ts , ps) = List.split lst in
  let ts_str = List.map simpleTypeToString ts in
  let ps_str = List.map tagVarToString ps in
  let tps_str_lst = CCList.combine ts_str ps_str in
  (List.fold_left
    (fun x pair ->
      let (t,p) = pair in
      (sprintf "%s%s:%s  " x t p)) "{  " tps_str_lst) ^ "}"


(***************************)
(* SIMPLE TYPE CONSISTENCY *)
(***************************)

let simpleConsistency : simpleType -> simpleType -> bool =
  fun t1 t2 ->
  match (t1,t2) with
  | (_, Dynamic) | (Dynamic, _) | (Real, Real) | (Bool,Bool) -> true
  | _ -> failwith "Not implemented yet"



let getSymprobList : distType -> tagVar list =
  fun dt ->
  let DistType lst = dt in
  let (_,ps) = List.split lst in
  ps



(******************)
(* MEET OPERATION *)
(******************)

(** [simpleTypeMeet] takes two simple types [st1] and [st2],
    and returns their meet. *)
let rec simpleTypeMeet : simpleType -> simpleType -> simpleType option =
  fun st1 st2 ->
  match (st1,st2) with
  | (Dynamic, t) | (t, Dynamic) -> Some t
  | (Real, Real) -> Some Real
  | (Bool, Bool) -> Some Bool
  | (FunType (st1,dt1), FunType (st2,dt2)) ->
     let meet1 = simpleTypeMeet st1 st2 in
     let meet2 = distTypeMeet dt1 dt2 in
     (match (meet1,meet2) with
     | (None,_) | (_,None) -> None
     | (Some t1, Some t2) -> Some (FunType (t1,t2)))
     
  | _ -> None


and distTypeMeet : distType -> distType -> distType option =
  fun dt1 dt2 ->

  (* This is a matrix with the meet between simple types
   * from the argument distribution types *)
  let meetM : simpleType option matrix = meetMatrix dt1 dt2 in

  (* An entry Mij in this matrix is true if meet(ti,tj) is defined.
   * It is false other wise *)
  let boolMatrix : bool matrix =  List.map (List.map optionToBool) meetM in

  (* Lists of probabilities from the distribution types.
     A fraction might be None, in the case of an unknown
     probability *)
  let fracList1 : fraction option list = List.map tagVarToFraction (getSymprobList dt1) in
  let fracList2 : fraction option list = List.map tagVarToFraction (getSymprobList dt2) in

  let len1 = List.length fracList1 in
  let len2 = List.length fracList2 in

  (* This is the probability matrix of the meet distribution type.
     It might be None, since the meet between two distribution types
     can be undefined. *)
  let couplingMatrixOption : fraction matrix option =
    Solver.coupling fracList1 fracList2 boolMatrix in

  
  match couplingMatrixOption with

  (* Meet is undefined *)
  | None -> None

  (* Meet is defined *)
  | Some couplingMatrix ->

     
     let tagVarList : tagVar list =
       List.mapi (fun i p -> TagVar (Some p, (i / len2), (i mod len2),1)) (List.flatten couplingMatrix) in
     
     let typeTagVarList =
       List.combine (List.flatten meetM) tagVarList |>
         List.filter (fun pair -> match pair with | (None,_) -> false | (Some _, p) -> (isZero p)) |>
         List.map (fun pair -> match pair with | (Some t, p)-> (t,p) |(None, p) -> (Dynamic,p)) in

     Some (DistType  typeTagVarList)
     

  
(** [meetMatrix] takes two distribution types [dt1] and [dt2]
    and returns a matrix with the meet between types of the two
    distributions. The type of the entries is optional, since meet
    might not be defined for two simple types. *)
and meetMatrix : distType -> distType -> simpleType option matrix =
  fun dt1 dt2 ->
  match dt1 with
  | DistType [] -> []
  | DistType ((t1,_) :: tail) ->
     let DistType lst = dt2 in
     let (typList,_) = List.split lst in
     let head = List.map (fun t2 -> simpleTypeMeet t1 t2) typList in
     let tail = meetMatrix (DistType tail) dt2 in 
     head :: tail
