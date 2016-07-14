let is_palindrome list =
  list = List.rev list
;;


is_palindrome ["x"; "a"; "m"; "a"; "x"];;
is_palindrome ["a"; "b"];;
