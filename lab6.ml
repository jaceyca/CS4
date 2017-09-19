(* Jessica Choi *)
(* CS 4 Set 6 *)

(* A.1 *)
(*
Evaluation model.

-- Desugar expression to:
let factorial = fun n -> let iter = fun m r -> if m = 0 ...

-- Start with initial environment:

FRAME 0 (initial environment)
  parent: none
  bindings:
    - : [primitive function -]
    * : [primitive function *]

-- Evaluate the let expression:
--   Evaluate the fun expression:

FUNCTION 0 (fun n -> let iter ...)
  env: FRAME 0
  param: n
  body: let iter...

-- Create a new frame with the name "factorial" bound to the function object

FRAME 1 (let factorial = FUNCTION 0 in ...)
  parent: FRAME 0
  bindings:
    factorial : FUNCTION 0

-- Evaluate the expression "factorial 3" in the context of FRAME 1:
--   3 evaluates to itself
--   Look up "factorial" --> FUNCTION 0
--   Apply FUNCTION 0 to 3:
--     Create a new frame, binding the formal parameter of the "factorial" fxn:

FRAME 2 (FUNCTION 0 applied to 3)
  parent: FRAME 0
  bindings:
    n : 3

-- Evaluate "let iter = fun m r ... in iter n 1" in the context of FRAME 2:
--   Look up n: 3 (from FRAME 2)
--   Evaluate the let expression:
--     Evaluate the fun expression:

FUNCTION 1 (fun m r -> ...)
  env: FRAME 2
  param: m r
  body: if m = 0 ...

-- Create a new frame with name "iter" bound to the function object

FRAME 3 (let iter m r = FUNCTION 1 in ...)
  parent: FRAME 2
  bindings:
    iter: FUNCTION 1

-- Evaluate the expression "iter 3 1" in the context of FRAME 3
--   3 evaluates to itself
--   1 evaluates to itself
--   Look up "iter" --> FUNCTION 1
--   Apply FUNCTION 1 to 3 1:
--     Create a new frame, binding the formal parameter of the "iter" function:

FRAME 4 (FUNCTION 1 applied to 3 1)
  parent: FRAME 3
  bindings:
    m : 3
    r : 1

-- Evaluate "if m = 0 then ..."
--   Look up m: 3 (from FRAME 4)
--   Look up r: 1 (from FRAME 4)
--   Look up = : [comparison operator =]
--   Apply = to 3 0 --> false
--   Return else clause of if statement
--     iter (3 - 1) (1 * 3)
-- Return to FRAME 3

-- Evaluate the expression "iter (3 -1) (1 * 3) in context of FRAME 3"
--   Evaluate 3 - 1
--     Evaluate 3 to itself
--     Evaluate 1 to itself
--     Look up - : [primitive function -] (from FRAME 0 --> FRAME 3)
--     Apply - to 3 1 --> 2
--   Evaluate 1 * 3
--     Evaluate 1 to itself
--     Evaluate 3 to itself
--     Look up * : [primitive function *] (from FRAME 0 --> FRAME 3)
--     Apply * to 1 3 --> 3
--   Look up "iter" --> FUNCTION 1
--   Apply FUNCTION 1 to 2 3:
--     Create a new frame, binding the formal parameter of "iter" function

FRAME 5 (FUNCTION 1 applied to 2 3)
  parent: FRAME 3
  bindings:
    m : 2
    r : 3

-- Evaluate "if m = 0 then ..."
--   Look up m: 2 (from FRAME 5)
--   Look up r: 3 (from FRAME 5)
--   Look up = : [comparison operator]
--   Apply = to 2 0 --> false
--   Return else clause of if statement
--     iter (2 - 1) (3 * 2)
-- Return to FRAME 3

-- Evaluate the expression "iter (2-1) (3*2) in context of FRAME 3"
--   Evaluate 2 - 1
--     Evaluate 2 to itself
--     Evaluate 1 to itself
--     Look up - : [primitive function -] (from FRAME 0 --> FRAME 3)
--     Apply - to 2 1 --> 1
--   Evaluate 3 * 2
--     Evaluate 3 to itself
--     Evaluate 2 to itself
--     Look up * : [primitive function *] (from FRAME 0 --> FRAME 3)
--     Apply * to 3 2 --> 6
--   Look up "iter" --> FUNCTION 1
--   Apply FUNCTION 1 to 1 6:
--     Create a new frame, binding the formal parameter of "iter" function

FRAME 6 (FUNCTION 1 applied to 1 6)
  parent: FRAME 3
  bindings:
    m : 1
    r : 6

-- Evaluate "if m = 0 then ..."
--   Look up m: 1 (from FRAME 6)
--   Look up r: 6 (from FRAME 6)
--   Look up =: [comparison operator]
--   Apply = to 1 6 --> false
--   Return else clause of if statement
--     iter (1 - 1) (6 * 1)
-- Return to FRAME 3

-- Evaluate the expression "iter (1 - 1) (6 * 1)" in the context of FRAME 3:
--   Evaluate 1 - 1
--     Evaluate 1 to itself
--     Evaluate 1 to itself
--     Look up - : [primitive function -] (from FRAME 0 --> FRAME 3)
--     Apply - to 1 1 --> 0
--   Evaluate 6 * 1
--     Evaluate 6 to itself
--     Evaluate 1 to itself
--     Look up * : [primitive function *] (from FRAME 0 --> FRAME 3)
--     Apply * to 6 1 --> 6
--   Look up "iter" --> FUNCTION 1
--   Apply FUNCTION 1 to 0 6:
--     Create a new frame, binding the formal parameter of "iter" function

FRAME 7 (FUNCTION 1 applied to 0 6)
  parent: FRAME 3
  bindings:
    m : 0
    r : 6

-- Evaluate "if m = 0 then ..."
--   Look up m: 0 (from FRAME 7)
--   Look up r: 6 (from FRAME 7)
--   Look up = : [comparison oeprator]
--   Apply = to 0 0 --> true
--   Return then clause of if statement
--     r
--   Evaluate the expression "r" in the context of FRAME 7
--     r = 6
*)

(*
Environment description.

FRAME 0 (initial environment)
  parent: none
  bindings:
    - : [primitive function -]
    * : [primitive function *]

FUNCTION 0 (fun n -> let iter ...)
  env: FRAME 0
  param: n
  body: let iter ...

FRAME 1 (let factorial = FUNCTION 0 in ...)
  parent: FRAME 0
  bindings:
    factorial : FUNCTION 0

FRAME 2 (FUNCTION 0 applied to 3)
  parent: FRAME 0
  bindings:
    n : 3

FRAME 3 (let iter m r = FUNCTION 1 in ...)
  parent: FRAME 2
  bindings:
    iter : FUNCTION 1

FUNCTION 1 (fun m r -> ...)
  env: FRAME 3
  param: m r
  body: if m = 0 then r else iter (m - 1) (r * m)

FRAME 4 (FUNCTION 1 applied to 3 1)
  parent: FRAME 3
  bindings:
    m : 3
    r : 1

FRAME 5 (FUNCTION 1 applied to 2 3)
  parent: FRAME 3
  bindings:
    m : 2
    r : 3

FRAME 6 (FUNCTION 1 applied to 1 6)
  parent: FRAME 3
  bindings:
    m : 1
    r : 6

FRAME 7 (FUNCTION 1 applied to 0 6)
  parent: FRAME 3
  bindings:
    m : 0
    r : 6
*)

(* A.2 *)
let factorial =
  let f = ref (fun n -> 0) in
  f :=
  begin
    function
    | 0 -> 1
    | n -> n * (!f (n-1))
  end;
  !f

(* B.1 *)
exception Stat_error of string

let make_stat_1 () =
  let sum = ref 0.0 in
  let sumsq = ref 0.0 in
  let n = ref 0 in
  object

    method append x =
      begin
        sum := !sum +. x;
        sumsq := !sumsq +. (x *. x);
        n := !n + 1
      end

    method mean =
      if !n = 0 then raise (Stat_error "need at least one value for mean")
        else !sum /. (float_of_int !n)

    method variance =
      if !n = 0 then raise (Stat_error "need at least one value for variance")
        else (!sumsq -. (!sum *. !sum /. (float_of_int !n))) /. (float_of_int !n)

    method stdev =
      if !n = 0 then raise (Stat_error "need at least one value for stdev")
        else sqrt ((!sumsq -. (!sum *. !sum /. (float_of_int !n)))
                 /. (float_of_int !n))

    method clear =
      begin
        sum := 0.0;
        sumsq := 0.0;
        n := 0
      end
  end

(* B.2 *)
let make_stat_2 () =
  let sum = ref 0.0 in
  let sumsq = ref 0.0 in
  let n = ref 0 in
  object (self)

    method append x =
      begin
        sum := !sum +. x;
        sumsq := !sumsq +. (x *. x);
        n := !n + 1
      end

    method private _variance =
      (!sumsq -. (!sum *. !sum /. (float_of_int !n))) /. (float_of_int !n)

    method mean =
      if !n = 0 then raise (Stat_error "need at least one value for mean")
        else !sum /. (float_of_int !n)

    method variance =
      if !n = 0 then raise (Stat_error "need at least one value for variance")
        else self#_variance

    method stdev =
      if !n = 0 then raise (Stat_error "need at least one value for stdev")
        else sqrt (self#_variance)

    method clear =
      begin
        sum := 0.0;
        sumsq := 0.0;
        n := 0
      end
  end

(* C.1 *)

module type PRIORITY_QUEUE =
  sig
    exception Empty

    type elem      (* Abstract type of elements of queue. *)
    type t         (* Abstract type of queue. *)

    val empty      : t                (* The empty queue.         *)
    val is_empty   : t -> bool        (* Check if queue is empty. *)
    val insert     : t -> elem -> t   (* Insert item into queue.  *)
    val find_min   : t -> elem        (* Return minimum element.  *)
    val delete_min : t -> t           (* Delete minimum element.  *)
    val from_list  : elem list -> t   (* Convert list to queue.   *)
  end

module PriorityQueue : (PRIORITY_QUEUE with type elem = int) =
  struct
    exception Empty

    type elem = int
    type t =
      | Leaf
      | Node of elem * int * t * t

    let empty = Leaf
    let is_empty q = q = empty

    let rec merge h1 h2 =
      match (h1, h2) with
      | (Leaf, _) -> h2
      | (_, Leaf) -> h1
      | (Node (e1, rank1, left1, right1), Node (e2, rank2, left2, right2)) ->
        if e1 < e2
          then
            if rank1 - 1 > rank2
              then Node (e1, rank1, left1, merge right1 h2)
              else Node (e1, rank2 + 1, merge right1 h2, left1)
          else
          if rank2 - 1 > rank1
            then Node (e2, rank2, left2, merge right2 h1)
            else Node (e2, rank1 + 1, merge right2 h1, left2)

    let insert q item = merge q (Node (item, 1, Leaf, Leaf))

    let find_min x =
      match x with
      | Leaf -> raise Empty
      | Node (e, _, _, _) -> e

    let delete_min x =
      match x with
      | Leaf -> raise Empty
      | Node (_, _, l, r) -> merge l r

    let from_list lst =
      let rec iter lst q =
        match lst with
        | [] -> q
        | h :: t -> iter t (insert q h)
      in iter lst Leaf
  end

let heap_sort lst =
  let q = PriorityQueue.from_list lst in
  let rec iter q lst =
    match PriorityQueue.is_empty q with
    | true -> lst
    | false -> iter (PriorityQueue.delete_min q) (PriorityQueue.find_min q :: lst)
  in
  List.rev (iter q [])

(* C.2 *)
(* Type for ordered comparisons. *)
type comparison = LT | EQ | GT

(* Signature for ordered objects. *)
module type ORDERED =
  sig
    type t
    val cmp: t -> t -> comparison
  end

module MakePriorityQueue (Elt : ORDERED)
  : (PRIORITY_QUEUE with type elem = Elt.t) =
  struct
    exception Empty

    type elem = Elt.t
    type t =
      | Leaf
      | Node of elem * int * t * t

    let empty = Leaf
    let is_empty q = q = empty

    let rec merge h1 h2 =
      match (h1, h2) with
      | (Leaf, _) -> h2
      | (_, Leaf) -> h1
      | (Node (e1, rank1, left1, right1), Node (e2, rank2, left2, right2)) ->
        if Elt.cmp e1 e2 = LT
          then
            if rank1 - 1 > rank2
              then Node (e1, rank1, left1, merge right1 h2)
              else Node (e1, rank2 + 1, merge right1 h2, left1)
          else
          if rank2 - 1 > rank1
            then Node (e2, rank2, left2, merge right2 h1)
            else Node (e2, rank1 + 1, merge right2 h1, left2)

    let insert q item = merge q (Node (item, 1, Leaf, Leaf))

    let find_min x =
      match x with
      | Leaf -> raise Empty
      | Node (e, _, _, _) -> e

    let delete_min x =
      match x with
      | Leaf -> raise Empty
      | Node (_, _, l, r) -> merge l r

    let from_list lst =
      let rec iter lst q =
        match lst with
        | [] -> q
        | h :: t -> iter t (insert q h)
      in iter lst Leaf

  end

module OrderedString =
  struct
    type t = string
    let cmp x y =
      if x = y
        then EQ
        else if x < y
        then LT
        else GT
  end

module StringPQ = MakePriorityQueue(OrderedString)

let heap_sort_2 lst =
  let q = StringPQ.from_list lst in
  let rec iter q lst =
    match StringPQ.is_empty q with
    | true -> lst
    | false -> iter (StringPQ.delete_min q) (StringPQ.find_min q :: lst)
  in
  List.rev (iter q [])
