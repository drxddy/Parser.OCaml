(* What a strage way to write comments *)
(* remove trailing % print *)
let rec print_list = function
    [] -> print_string "\n"
  | e::l -> print_int e ; print_string " " ; print_list l

let rec sort lst = 
  match lst with
  | [] -> []
  | head::tail -> insert head (sort tail)

and insert elt lst =
  match lst with
  | [] -> [elt]
  | head::tail -> if elt <= head
      then  elt :: lst
      else  head :: insert elt tail 
    ;;

let unsorted = [2; 1; 4; 5; 3; 8; 7; 6; 9];;

(*  How it works
    deeply recurses into the unsorted 
    list until it reaches the end, 
    then it starts to build the sorted

    (2, (1, (4, (5, (3, (8, (7, (6, (9, [])))))))))
    (2, (1, (4, (5, (3, (8, (7, (6, [9]))))))))
    (2, (1, (4, (5, (3, (8, (7, [6, 9])))))))
    (2, (1, (4, (5, (3, (8, (6, (7, [9]))))))))
    (2, (1, (4, (5, (3, (8, (6, [7, 9])))))))
    (2, (1, (4, (5, (3, (8, [6, 7, 9]))))))
    (2, (1, (4, (5, (3, (8, [6, 7, 9]))))))
    ...
*)

let sorted = sort unsorted;;

print_list sorted;;


(* 
  The sort function above does not modify its input list: it builds and 
  returns a new list containing the same elements as the input list, 
  in ascending order. There is actually no way in OCaml to modify a list 
  in-place once it is built: we say that lists are immutable data structures. 
  Most OCaml data structures are immutable, but a few (most notably arrays) 
  are mutable, meaning that they can be modified in-place at any time.

  The OCaml notation for the type of a function with multiple arguments is
  arg1_type -> arg2_type -> ... -> return_type. 
  For example, the type inferred for insert, 'a -> 'a list -> 'a list, 
  means that insert takes two arguments, an element of any type 'a and a list 
  with elements of the same type 'a and returns a list of the same type.
*)

let print_boolean b = if b then print_string "true\n" else print_string "false\n";;

print_boolean (1 < 2);;

(* 
  The type of the function print_boolean is bool -> unit. 
  The type bool is the type of booleans, and the type unit is the type of 
  the only value (), which is used as a placeholder when no useful value 
  is available. The type unit is similar to the type void in C or Java, 
  but it is a real type, and not a special case.
*)

let x = {|
  asdf;
  asdfasd;fklj 
  asdf;lkj asdfljk adsf
  \12 a;lkdf 
|}

let rec listlength = function
  | [] -> 0
  | _::cdr -> 1 + listlength cdr;;

print_int (listlength unsorted)

(* 3 Functions as values *)
let deriv f dx = fun x -> (f (x +. dx) -. f x) /. dx;;
(* f is a float function and it's the first argument to deriv function which returns a float function *)

let square x = x *. x;;

let dsquare = deriv square 1e-10;;

print_float (dsquare 2.0);;

(* 4.1.1 Currying *)
(* 
  The type of deriv is float -> float -> float. 
  This means that deriv takes a float as first argument, 
  and returns a function of type float -> float as result. 
  This is called currying: the function deriv takes its arguments 
  one at a time, and returns a function at each step. 
  The type of deriv can also be written float -> (float -> float), 
  which is equivalent to float -> float -> float. 
  The type of deriv is thus the type of a function that takes a float 
  and returns a function that takes a float and returns a float.

  The type of deriv square 1e-10 is float -> float.
  The type of deriv square is float -> float -> float.
  The type of deriv square 1e-10 is float -> float.
  The type of deriv square is float -> float -> float.
*)

