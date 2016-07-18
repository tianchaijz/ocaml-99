type 'a rle =
  | One of 'a
  | Many of int * 'a
;;


let decode list =
  let rec many acc x n =
    if n <= 0 then acc else many (x :: acc) x (n - 1) in
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (count, x) :: t -> aux (many acc x count) t
  in aux [] (List.rev list)
;;


decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
