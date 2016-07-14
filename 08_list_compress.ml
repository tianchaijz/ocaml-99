let compress list =
  let rec aux acc = function
    | [] -> acc
    | [x] -> x :: acc
    | x :: (y :: _ as t) -> if x = y then aux acc t else aux (x :: acc) t
  in List.rev (aux [] list)
;;


let rec compress = function
  | x :: (y :: _ as t) -> if x = y then compress t else x :: compress t
  | smaller -> smaller
;;


compress ["a"];;
compress ["a"; "a"];;
compress ["a"; "b"];;
compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
