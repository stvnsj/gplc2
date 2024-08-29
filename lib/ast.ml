open Type
open Utils

type info = string

type expr =
  | TmBool       of info * bool (* Boolean Term *)
  | TmNum        of info * float (* Numeric Term*)
  | TmVar        of info * string (* Variable Term *)
  | TmLet        of info * string * expr * expr (* Let Term *)
  | TmLambda     of info * string * expr * expr (* Lambda Term *)
  | TmChoice     of info * expr * expr * expr (* Binary Choice Term *)
  | TmAscr1      of info * expr * expr (* Simple Type Ascription *)
  | TmAscr2      of info * expr * expr (* Distribution Type Ascription *)
  | TmProb       of info * fraction (* Choice Probability *)
  | TmSimpleType of info * simpleType (* Simple Type Term *)
  | TmDistType   of info * distType (* Distribution Type Term *)


