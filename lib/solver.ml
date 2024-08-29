 (*************************************)
 (*   _____ _____  _      _____ ___   *)
 (*  / ____|  __ \| |    / ____|__ \  *)
 (* | |  __| |__) | |   | |       ) | *)
 (* | | |_ |  ___/| |   | |      / /  *)
 (* | |__| | |    | |___| |____ / /_  *)
 (*  \_____|_|    |______\_____|____| *)
(*************************************)



open Z3
open Z3.Symbol
open Z3.Sort
open Z3.Expr
open Z3.Boolean
open Z3.FuncDecl
open Z3.Goal
open Z3.Tactic
open Z3.Tactic.ApplyResult
open Z3.Probe
open Z3.Solver
open Z3.Arithmetic
open Z3.Arithmetic.Integer
open Z3.Arithmetic.Real
open Z3.BitVector
open Z3.FloatingPoint
open Printf
open Matrix
open Utils

exception TestFailedException of string

(* Takes a list of expressions and produces an addition *)
let addition : context -> expr list -> expr =
  fun ctx es -> Arithmetic.mk_add ctx es

let rec constant_list : context -> int -> sort -> expr list =
  fun ctx length sort -> 
  match length with
  | 0 -> []
  | _ -> (mk_fresh_const ctx "x" sort) ::
           (constant_list ctx (length - 1) sort)

let real_num_frac : context -> fraction -> expr =
  fun ctx frac ->
  let numerator = Q.num frac in
  let denominator = Q.den frac in
  Arithmetic.Real.mk_numeral_nd ctx (Z.to_int numerator) (Z.to_int denominator)


(** [symProb] takes an optional fraction representation of a probability [prob].
    If [prob] is None, it returns a symbolic representation. If it is
    Some fraction, it returns the Real numeral *)
let symProb : context -> sort -> fraction option -> expr =
  fun ctx srt fraction -> 
  match fraction with
  | None -> mk_fresh_const ctx "p" srt
  | Some f  ->  real_num_frac ctx f

let rec constMatrix : context -> int -> int -> sort -> expr matrix =
  fun ctx rows cols sort ->
  match rows with
  | 0 -> []
  | _ ->
     let head = constant_list ctx cols sort in
     let tail = constMatrix ctx (rows - 1) cols sort in
     head :: tail


let equation : context -> expr -> expr -> expr =
  fun context lh rh -> Boolean.mk_eq context lh rh


let real_num_int : context -> int -> expr =
  fun ctx i -> Arithmetic.Real.mk_numeral_i ctx i


let real_num_str : context -> string -> expr =
  fun ctx s -> Arithmetic.Real.mk_numeral_s ctx s



(*********************)
(* Constants 1 and 0 *)
(*********************)

let one : context -> expr =
  fun ctx -> Arithmetic.Real.mk_numeral_i ctx 1

let zero : context -> expr =
  fun ctx -> Arithmetic.Real.mk_numeral_i ctx 0


(*********************************************************)
(* These are the three main constraints recurrently used *)
(*********************************************************)

(* x = 0 *)
let zero_constraint : context -> expr -> expr =
  fun ctx expr -> Boolean.mk_eq ctx (zero ctx) expr

(* x1 + ... + xn = 1 *)
let sum_one_constraint : context -> expr list -> expr =
  fun ctx es ->
  let rh = addition ctx es in
  let lh = one ctx in
  equation ctx lh rh

(* x>=0 AND x<=1 *)
let unit_interval_constraint : context -> expr -> expr =
  fun ctx e ->
  let c1 = Arithmetic.mk_le ctx (zero ctx) e in
  let c2 = Arithmetic.mk_ge ctx (one ctx)  e in
  Boolean.mk_and ctx [c1 ; c2]


let boolToConstraint : context -> bool -> expr -> expr =
  fun ctx bool expr ->
  if bool then unit_interval_constraint ctx expr
  else zero_constraint ctx expr

let constraintMatrix : context -> bool matrix -> expr matrix -> expr matrix =
  fun ctx boolMatrix constrMatrix ->
  List.map2 (List.map2 (boolToConstraint ctx)) boolMatrix constrMatrix




let coupling : fraction option list -> fraction option list -> bool matrix -> fraction matrix option =
  fun plst1 plst2 boolMatrix ->

  let cfg = [("model", "true"); ("proof", "false")] in
  let ctx = (mk_context cfg) in
  let real_sort = (Arithmetic.Real.mk_sort ctx) in
  let solver = (mk_solver ctx None) in
  
  (* length of probability lists *)
  let len1 = List.length plst1 in
  let len2 = List.length plst2 in

  let symprob_lst1 = List.map (symProb ctx real_sort) plst1 in
  let symprob_lst2 = List.map (symProb ctx real_sort) plst2 in

  (* Unit Interval Constraint:
   * Each symbolic p probability must satisfy the
   * constraint  p>= 0 and p<=1 *)
  let c01 = List.map (unit_interval_constraint ctx) symprob_lst1 in
  let c02 = List.map (unit_interval_constraint ctx) symprob_lst2 in

  (* Sum 1 constraint: 
   * each set of symbolic probabilities {p1,...,pn}
   * must satisfy the constraint p1+...+pn = 1 *)
  let c001 = sum_one_constraint ctx symprob_lst1 in
  let c002 = sum_one_constraint ctx symprob_lst2 in

  (* matrix1 is the coupling matrix. matrix2 is just
   * the tranposition of matrix1 *)
  let matrix1 = constMatrix ctx len1 len2 real_sort in
  let matrix2 = transpose matrix1 in

  (* This constraint matrix determines which entries
   * in the coupling matrix must be 0, and which ones
   * are allowed in the [0,1] interval *)
  let constr_matrix = constraintMatrix ctx boolMatrix matrix1 in


  (* Horizontal and vertical sums of the coupling matrix *)
  let h_sum_lst = List.map (addition ctx) matrix1 in
  let v_sum_lst = List.map (addition ctx) matrix2 in

  let h_eq_lst = List.map2 (equation ctx) symprob_lst1 h_sum_lst in
  let v_eq_lst = List.map2 (equation ctx) symprob_lst2 v_sum_lst in


  let constraint_list = h_eq_lst @ v_eq_lst 
                        @ (List.flatten constr_matrix)
                        @ [c001;c002]
                        @ c01 @ c02 in

  Solver.add solver constraint_list ;
  

  match Solver.check solver [] with
  | SATISFIABLE ->

     let model = Option.get (Solver.get_model solver) in

     let coupling_matrix = List.map (
                               List.map (
                                   fun x ->
                                   Model.eval model x true |>
                                     Option.get |>
                                     Arithmetic.Real.numeral_to_string |>
                                     Q.of_string)) matrix1 in
     
     let solution_matrix = matrix_to_string
                             matrix1
                             (fun x ->
                               Model.eval model x true |>
                                 Option.get |>
                                 Arithmetic.Real.numeral_to_string |>
                                 Q.of_string |> Q.to_float |> Float.to_string |> 
                                 (fun s -> Printf.sprintf "%s\t" s)) in

     (* Printf.printf *)
     (*   "\n\n model = %s\n\n\n\nSolution Matrix = \n\n%s\n\n" *)
     (*   (Model.to_string model) solution_matrix *)
     printf "\nCoupling:\n---------%s\n" solution_matrix;

     Some coupling_matrix

  | _ -> None



  
  
  
  
  
  
  


