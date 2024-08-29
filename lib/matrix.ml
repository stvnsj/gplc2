

type 'a matrix = 'a list list 


let rec transpose (m : 'a matrix) : 'a matrix =
  match m with
  | [] -> []
  | [] :: _ -> []
  | rows ->
    let first_column = List.map List.hd rows in
    let rest_matrix = List.map List.tl rows in
    first_column :: transpose rest_matrix



let rec matrix_to_string (m : 'a matrix) (f : 'a -> string) : string =
  match m with
  | [] -> ""
  | r :: rs -> (List.fold_left (fun acc x -> acc ^ " " ^ (f x)) "\n" r) ^ (matrix_to_string rs f)
 


