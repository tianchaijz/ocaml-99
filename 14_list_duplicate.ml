let duplicate list =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: h :: acc) t
  in List.rev (aux [] list)
;;


let rec duplicate = function
  | [] -> []
  | h :: t -> h :: h :: duplicate t
;;


duplicate ["a"; "b"; "c"; "c"; "d"];;
