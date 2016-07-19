let replicate list n =
  let rec prepend n acc x =
    if n <= 0 then acc else prepend (n - 1) (x :: acc) x in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (prepend n acc h) t
  in aux [] (List.rev list)
;;


let replicate list n =
  let rec prepend n acc x =
    if n <= 0 then acc else prepend (n - 1) (x :: acc) x in
  List.fold_left (prepend n) [] (List.rev list)
;;


replicate ["a"; "b"; "c"; "c"; "d"] 3;;
