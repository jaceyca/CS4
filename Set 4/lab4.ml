(* Jessica Choi *)
(* CS 4 Set 4 *)

(* A.1 *)
type mobile = Mobile of branch * branch  (* left and right branches *)
and branch = 
  | Weight    of int * int     (* length and weight *)
  | Structure of int * mobile  (* length and sub-mobile *)

(* l means left *)
let make_mobile l r = Mobile (l, r)
(* l means length *)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

(* A.1a *)
let left_branch = function 
  Mobile (l, _) -> l
let right_branch = function 
  Mobile (_, r) -> r

let branch_length = function 
  | Weight (l,_) -> l
  | Structure (l, _) -> l

let branch_structure = function
  | Weight (_, w) -> `Weight w
  | Structure (_, m) -> `Structure m

(* A.1b *)
let rec branch_weight1 = function
  | Weight (_, w) -> w
  | Structure (_, m) -> total_weight1 m 
and total_weight1 = function
    Mobile (l, r) -> (branch_weight1 l) + (branch_weight1 r)

let rec branch_weight2 b =
  match branch_structure b with
  | `Weight w -> w
  | `Structure m -> total_weight2 m
and total_weight2 m =
    let l_weight = branch_weight2 (left_branch m) in
    let r_weight = branch_weight2 (right_branch m)
  in
    l_weight + r_weight

(* A.1c *)
let rec is_balanced m =
  let balanced = (((branch_length (left_branch m)) * (branch_weight2 (left_branch m))) =
  ((branch_length (right_branch m)) * (branch_weight2 (right_branch m)))) in
  match (branch_structure (left_branch m), branch_structure (right_branch m)) with
  | (`Weight _, `Weight _) -> balanced
  | (`Weight _, `Structure s) -> balanced && is_balanced s
  | (`Structure s, `Weight _) -> balanced && is_balanced s
  | (`Structure s1, `Structure s2) -> balanced && is_balanced s1 && is_balanced s2
   
(* A.1d *)
type mobile'  = { left: branch'; right: branch' }
and  branch'  = Branch' of int * contents
and  contents = Weight' of int | Structure' of mobile'

let make_mobile' l r = { left = l; right = r }
let make_weight' l w = Branch' (l, Weight' w) 
let make_structure' l m = Branch' (l, Structure' m)

let left_branch' { left; right } = left
let right_branch' { left; right } = right

let branch_length' (Branch' (l, _)) = l

let branch_structure' (Branch' (_, m)) =
  match m with
  | Weight' weight -> `Weight weight
  | Structure' structure -> `Structure structure

let rec branch_weight' b =
  match branch_structure' b with
  | `Weight w -> w
  | `Structure m -> total_weight' m
and total_weight' = function m ->
  let l_weight = branch_weight' (left_branch' m) in
  let r_weight = branch_weight' (right_branch' m) in
  l_weight + r_weight

let rec is_balanced' m =
  let balanced = (((branch_length' (left_branch' m)) * (branch_weight' (left_branch' m))) =
  ((branch_length' (right_branch' m)) * (branch_weight' (right_branch' m)))) in
  match (branch_structure' (left_branch' m), branch_structure' (right_branch' m)) with
  | (`Weight _, `Weight _) -> balanced
  | (`Weight _, `Structure s) -> balanced && is_balanced' s
  | (`Structure s, `Weight _) -> balanced && is_balanced' s
  | (`Structure s1, `Structure s2) -> balanced && is_balanced' s1 && is_balanced' s2

(* A.2 *)
type tree = Tree of elem list
and elem = 
  | Num of int
  | Sub of tree

let rec square_tree = function Tree tree_list -> 
  let rec square_list = function
  | [] -> []
  | (Num h) :: t -> (Num (h * h)) :: (square_list t)
  | (Sub h) :: t -> (Sub (square_tree h)) :: (square_list t)
in
  Tree (square_list tree_list)

let square_tree' = function Tree tree_list ->
  let rec helper = function
    | Num n -> Num (n * n)
    | Sub (Tree s) -> Sub (Tree (List.map helper s))
  in
    Tree (List.map helper tree_list)

(* A.3 *)
let tree_map func = function Tree tree_list ->
  let rec helper = function
  | Num n -> Num (func n)
  | Sub (Tree s) -> Sub (Tree (List.map helper s))
in
  Tree (List.map helper tree_list)

let square_tree'' tree = tree_map (fun n -> n * n) tree

(* A.4 *)
let rec subsets = function
  | [] -> [[]]
  | h :: t -> let rest = subsets t
in
  rest @ (List.map (fun x -> h :: x) rest)

(* A.5 *)
let rec accumulate op initial sequence = 
  match sequence with
  | [] -> initial
  | h :: t -> op h (accumulate op initial t)

let map p sequence = 
  accumulate (fun x r -> (p x) :: r) [] sequence

let append seq1 seq2 = 
  accumulate (fun x r -> x :: r) seq2 seq1

let length sequence = 
  accumulate (fun x r -> r + 1) 0 sequence

(* A.6 *)
let rec accumulate_n op init seqs =
  match seqs with
  | [] -> failwith "empty list"
  | [] :: _ -> []
  | h :: t -> (accumulate op init (List.map List.hd seqs)) :: (accumulate_n op init
    (List.map List.tl seqs))

(* A.7 *)
let rec map2 f x y =
  match (x, y) with
  | ([], []) -> []
  | ([], _) -> failwith "unequal lists"
  | (_, []) -> failwith "unequal lists"
  | (lh :: lt, rh :: rt) -> (f lh rh) :: (map2 f lt rt)

let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)

let matrix_times_vector m v =
  map (fun row -> accumulate (+) 0 (map2 ( * ) v row)) m

let transpose mat = accumulate_n (fun x y -> x :: y) [] mat

let matrix_times_matrix m n =
  let cols = transpose n in
    map (fun row -> matrix_times_vector cols row) m

(* B.1 *)
let rec filter predicate sequence =
  match sequence with
  | [] -> []
  | h :: t when predicate h -> h :: filter predicate t
  | _ :: t -> filter predicate t

let rec quicksort l cmp =
  match l with
  | [] -> []
  | h :: t -> quicksort (filter (fun x -> cmp x h) t) cmp @ [h] @
    quicksort (filter (fun x -> not (cmp x h)) t) cmp

(* B.2 *)
(* Quicksort is an instance of generative recursion because the recursion on the
   greater than/less than pivot sublists are defined inside the function, and the
   original list isn't modified.
*)

(* B.3 *)
(* When Ben runs his merge_sort on a list with one element, the even_half list
   will be empty and the odd_half list will have one element. When merge_sort is
   called recursively, the list with one element cannot be reduced to two empty
   lists, so there will be an infinite loop.
*)

(* B.4 *)
let rec insert_in_order new_result a_list cmp = 
  match a_list with 
  | [] -> [new_result]  
  | h :: t when cmp new_result h -> new_result :: a_list
  | h :: t -> h :: insert_in_order new_result t cmp
  
let rec insertion_sort a_list cmp =
  match a_list with
  | [] -> []
  | h :: t -> insert_in_order h (insertion_sort t cmp) cmp

(* This is a structural recursion because there is no new data being generated,
   and we are recursively calling on parts of the input. *)

(* C.1 *)
type expr =
  | Int of int               (* constant *)
  | Var of string            (* variable *)
  | Add of expr * expr       (* expr1 + expr2 *)
  | Mul of expr * expr       (* expr1 * expr2 *)
  | Pow of expr * int        (* expr^n *)

let rec pow x y =
  if y = 0
    then 1
    else x * pow x (y - 1)

let rec simplify expr =
  let e = simplify1 expr in
    if expr = e
      then expr
      else simplify e
and simplify1 expr = 
  match expr with
  | Add (Int a', Int b') -> Int (a' + b')
  | Mul (Int a', Int b') -> Int (a' * b')
  | Pow (Int a', b') -> Int (pow a' b')
  | Add (Int 0, b) -> b
  | Add (a, Int 0) -> a
  | Mul (Int 0, b) -> Int 0
  | Mul (a, Int 0) -> Int 0
  | Mul (Int 1, b) -> b
  | Mul (a, Int 1) -> a
  | Pow (a, 0) -> Int 1
  | Pow (a, 1) -> a
  | Add (a, b) -> Add (simplify1 a, simplify1 b)
  | Mul (a, b) -> Mul (simplify1 a, simplify1 b)
  | Pow (a, b) -> Pow (simplify1 a, b)
  | _ -> expr

(* C.2 *)
let rec deriv expr var =
  match expr with
  | Int e -> Int 0
  | Var v when v = var -> Int 1
  | Var v when v <> var -> Int 0
  | Add (e1, e2) -> Add ((deriv e1 var) , (deriv e2 var))
  | Mul (e1, e2) -> Add (Mul (e1, deriv e2 var), Mul (deriv e1 var, e2))
  | Pow (e, p) -> Mul (Mul ((Int p), Pow(e, p - 1)), (deriv e var))
(* the compiler said pattern-matching wasn't exhaustive but i didn't know
   what to put for the result of the wildcard match *)
  | _ -> expr
  
let derivative expr var =
  let e = simplify expr in
  let d = deriv e var in
    simplify d
