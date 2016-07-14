(* A node of a nested list is either an element, or a list of nodes. *)
type 'a node =
  | One of 'a
  | Many of 'a node list
;;


(* val flatten : 'a node list -> 'a list = <fun> *)
let flatten list =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in List.rev (aux [] list)
;;


flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
