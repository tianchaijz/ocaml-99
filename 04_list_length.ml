let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t
;;


let rec length list =
  let rec aux n = function
    | [] -> n
    | _ :: t -> aux (n + 1) t
  in aux 0 list
;;


print_int (length []);;
print_int (length [1]);;
print_int (length [1; 2]);;
