
open Lib.Type;;

CCFormat.set_color_default true;;
Format.printf
  "\n@{<Red>G@}@{<Green>P@}@{<Blue>L@}@{<Yellow>C@}:\n@.";;


let f1 = FunType(Dynamic, DistType[(Real, TagVar(None,0,0,0));
                                (Dynamic, makeTagVar "0.4")])

let f2 = FunType(Bool, DistType[(Dynamic, makeTagVar "0.8");
                                (Bool, makeTagVar "0.2")])

let y  = simpleTypeMeet f1 f2 ;;

match y with
| None ->  Printf.printf "%s\n" "No meet"
| Some t -> Printf.printf "%s\n" (simpleTypeToString t)


               

