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


