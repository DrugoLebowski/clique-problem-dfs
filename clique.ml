exception NegativeClique;;
exception CliqueNotFound;;
type 'a graph = Gr of ('a * 'a) list;;

(* succ: 'a graph -> 'a -> 'a list *)
let rec succ (Gr arcs) node = match arcs with
    [] -> []
  | (n1, n2)::arcs -> 
      if n1 = node then
        n2::succ (Gr arcs) node
      else
        succ (Gr arcs) node;;

(* succOfNode ('a * 'b) list -> 'a -> 'b -> bool *)
let rec succOfNode graph n k = match graph with 
    [] -> false
  | (n1, n2)::rest -> 
      if n1 == n then
        n2 == k || succOfNode rest n k
      else
        succOfNode rest n k;;

(* haveNodeAdj: ('a * 'b) list -> 'a list -> 'b -> bool *)
let rec haveNodeAdj graph clique k = match clique with
    [] -> true
  | n::rest -> succOfNode graph n k && haveNodeAdj graph rest k;;

(* isInsertable: ('a * 'b) list -> 'a list -> 'a -> n *)
let isInsertable graph clique n =
  not(List.mem n clique) && haveNodeAdj graph clique n;;

(* dfsClique: 'a graph -> 'a -> int -> 'a list -> 'a list *)
let dfsClique (Gr arcs) k clique =
  let rec visit visited clique = function 
      [] -> raise CliqueNotFound
    | n::nodes ->
        if k == 1 then [List.hd clique]
        else if (List.length clique) == k then clique
        else if List.mem n visited then
          visit visited clique nodes
        else if isInsertable arcs clique n then
          visit (n::visited) (n::clique) ((succ (Gr arcs) n) @ nodes)
        else 
          visit (n::visited) clique ((succ (Gr arcs) n) @ nodes)
  in visit [] clique [(List.hd clique)];;


(* cliqueOfTwo: 'a graph -> 'a list -> 'a list list *)
let rec cliqueOfTwo (Gr arcs) = function
    [] -> []
  | n::rest -> 
      (let rec aux z = function
           [] -> []
         | (n1, n2)::rest ->
             if z = n1 then
               [z::[n2]]@ aux z rest
             else
               aux z rest
       in aux n arcs ) @ cliqueOfTwo (Gr arcs) rest;;


(* nodes: 'a graph -> 'a list -> 'a list *)
let nodes (Gr arcs) clique =
  let rec aux clique = function
      [] -> clique
    | (nodeOne, nodeTwo)::rest -> 
        if List.mem nodeOne clique then 
          aux clique rest
        else 
          aux (clique@[nodeOne]) rest
  in aux clique arcs;;

(* searchClique: 'a graph -> int -> 'a list -> 'a list *)
let rec searchClique graph n = function
    [] | _::[] -> []
  | clique::cliques -> 
      try 
        dfsClique graph n clique
      with CliqueNotFound ->
        searchClique graph n cliques;;

(* clique: 'a graph -> int -> 'a list *)
let clique (Gr arcs) n =
  if n < 0 then raise NegativeClique
  else if n = 0 then []
  else searchClique (Gr arcs) n (cliqueOfTwo (Gr arcs) (nodes (Gr arcs) []));;


let graph = Gr [(1, 2); (2, 1); (2, 3); (3, 2); (3, 4); (4, 3); (3, 5); (5, 3); (4, 5); (5, 4); (4, 6); (6, 3); (3, 6); (6, 4); (5, 6); (6, 5); (6, 7); (7, 6)];;

let search = clique graph;;
search 0;;
search 1;;
search 2;;
search 3;;
search 4;;
search 5;;

let graph = Gr [(999, 999); (1, 2); (1, 3); (1, 9); (1, 11); (2, 1); (2, 3); (2, 4); (2, 5); (3, 1); (3, 2); (3, 4); (3, 5); (3, 8); (3, 9); (3, 11); (4, 2); (4, 3); (4, 5); (4, 10); (5, 2); (5, 3); (5, 4); (5, 7); (5, 8); (6, 6); (6, 10); (7, 5); (7, 9); (8, 3); (8, 5); (8, 9); (8, 10); (9, 1); (9, 3); (9, 7); (9, 8); (9, 10); (10, 4); (10, 6); (10, 8); (10, 9); (10, 11); (10, 12); (10, 13); (10, 14); (10, 15); (10, 16); (11, 1); (11, 3); (11, 10); (11, 12); (11, 13); (11, 14); (11, 15); (11, 16); (12, 10); (12, 11); (12, 13); (12, 14); (12, 15); (12, 16); (13, 10); (13, 11); (13, 12); (13, 14); (13, 15); (13, 16); (14, 10); (14, 11); (14, 12); (14, 13); (14, 15); (14, 16); (15, 10); (15, 11); (15, 12); (15, 13); (15, 14); (15, 16); (16, 10); (16, 11); (16, 12); (16, 13); (16, 14); (16, 15); (16, 18); (16, 19); (17, 18); (18, 16); (18, 17); (18, 19); (19, 16); (19, 18); (19, 20); (20, 19)];;


let search = clique graph;;
search 0;;
search 1;;
search 2;;
search 3;;
search 4;;
search 5;;
search 7;;



let graph = Gr [(1, 4); (2, 5); (3, 6); (4, 1); (4, 5); (4, 6); (5, 2); (5, 4); (5, 6); (6, 3); (6, 4); (6, 5)];;
let search = clique graph;;
search 0;;
search 1;;
search 2;;
search 3;;
