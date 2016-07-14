let rec rev = function
    | [] -> []
    | h :: t -> rev t @ [h]
;;


let rev list =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t in
  aux [] list
;;


rev [1; 2; 3];;
rev ["a"; "b"; "c"];;
