let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t
;;


let print_last = function
  | None -> ()
  | Some x -> print_int x
;;


print_last (last []);;
print_last (last [1]);;
print_last (last [1; 2]);;
