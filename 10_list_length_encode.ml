let encode list =
  let rec aux count acc = function
    | [] -> acc
    | [x] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (count + 1) acc t
      else aux 0 ((count + 1, a) :: acc) t
  in List.rev (aux 0 [] list)
;;


encode [];;
encode ["a"];;
encode ["a"; "a"];;
encode ["a"; "b"];;
encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
