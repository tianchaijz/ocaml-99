type 'a rle =
  | One of 'a
  | Many of int * 'a
;;


let encode list =
  let create_tuple n elem =
    if n = 1 then One elem
    else Many (n, elem) in
  let rec aux count acc = function
    | [] -> []
    | [x] -> (create_tuple (count + 1) x) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (count + 1) acc t
      else aux 0 ((create_tuple (count + 1) a) :: acc) t
  in List.rev (aux 0 [] list)
;;


encode [];;
encode ["a"];;
encode ["a"; "a"];;
encode ["a"; "b"];;
encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
