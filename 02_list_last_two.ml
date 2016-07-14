let rec last_two = function
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: t -> last_two t
;;


let print_last_two = function
  | None -> ()
  | Some (x, y) -> print_string (x ^ " " ^ y)
;;


print_last_two (last_two ["a"]);;
print_last_two (last_two ["a"; "b"; "c"; "d"]);;
