(* Jessica Choi *)
(* CS 4 Set 3 *)
(* I had an essay due the following day so I had to work on that instead. 
   I'm sorry for the trouble I caused you :( *)

(* A.1 *)
type point = { x : float; y : float }
let make_point x y = { x ; y }
let get_coords { x ; y } = (x, y)
let print_point { x ; y } = Printf.printf "(%g, %g)\n" x y

type segment = { startp : point; endp : point }
let make_segment p1 p2 = { startp = p1 ; endp = p2 }
let get_points { startp ; endp } = (startp, endp)
let midpoint_segment { startp ; endp } =
    { x = (startp.x +. endp.x) /. 2.0 ; y = (startp.y +. endp.y) /. 2.0 }
let segment_length { startp ; endp } =
  sqrt((startp.x -. endp.x) ** 2.0 +. (startp.y -. endp.y) ** 2.0)

(* A.2 *)
type rectangle = { lower_left : point; upper_right : point }
type rectangle2 = { lower_x : float; lower_y : float; upper_x : float; upper_y : float }

let rectangle_lower_segment { lower_left ; upper_right } =
{ startp = { x = lower_left.x ; y = lower_left.y };
  endp = { x = upper_right.x ; y = lower_left.y } }

let rectangle_upper_segment { lower_left ; upper_right } =
{ startp = { x = lower_left.x ; y = upper_right.y };
  endp = { x = upper_right.x ; y = upper_right.y } }

let rectangle_left_segment { lower_left ; upper_right } =
{ startp = { x = lower_left.x ; y = lower_left.y };
  endp = { x = lower_left.x ; y = upper_right.y } }

let rectangle_right_segment { lower_left ; upper_right } =
{ startp = { x = upper_right.x ; y = upper_right.y };
  endp = { x = upper_right.x ; y = lower_left.y } }

let rectangle_perimeter rectangle =
  segment_length (rectangle_upper_segment rectangle) +.
  segment_length (rectangle_lower_segment rectangle) +.
  segment_length (rectangle_left_segment rectangle) +.
  segment_length (rectangle_right_segment rectangle)

let rectangle_area rectangle =
 segment_length (rectangle_upper_segment rectangle) *.
 segment_length (rectangle_right_segment rectangle)

let rectangle_lower_segment2 { lower_x; lower_y; upper_x; upper_y } =
{ startp = { x = lower_x; y = lower_y };
  endp = { x = upper_x; y = lower_y } }

let rectangle_upper_segment2 { lower_x; lower_y; upper_x; upper_y } =
{ startp = { x = upper_x; y = upper_y };
  endp = { x = lower_x; y = upper_y } }

let rectangle_left_segment2 { lower_x; lower_y; upper_x; upper_y } =
{ startp = { x = lower_x; y = lower_y };
  endp = { x = lower_x; y = upper_y } }

let rectangle_right_segment2 { lower_x; lower_y; upper_x; upper_y } =
{ startp = { x = upper_x; y = upper_y };
  endp = { x = upper_x; y = lower_y } }

let rectangle_perimeter2 rectangle2 =
  segment_length (rectangle_upper_segment2 rectangle2) +.
  segment_length (rectangle_lower_segment2 rectangle2) +.
  segment_length (rectangle_left_segment2 rectangle2) +.
  segment_length (rectangle_right_segment2 rectangle2)

let rectangle_area2 rectangle2 =
  segment_length (rectangle_upper_segment2 rectangle2) *.
  segment_length (rectangle_right_segment2 rectangle2)

let make_rectangle p1 p2 =
  { lower_left = p1 ; upper_right = p2 }

let make_rectangle2 p1_x p1_y p2_x p2_y =
  { lower_x = p1_x; lower_y = p1_y; upper_x = p2_x; upper_y = p2_y }

(* A.3 *)
let make_pair x y = fun m -> m x y
let first z = z (fun x y -> x)
let second z = z (fun x y -> y)

(*
Evaluate first (make_pair x y)
Desugar into (make_pair x y) (fun x y -> x)
Evaluate (make_pair x y) (fun x y -> x)
Evaluate x -> x
Evaluate y -> y
Evaluate make_pair x y -> (fun m -> m x y)
Apply fun x y -> (fun m -> m x y) to x y
Substitute make_pair.x for fun.x and make_pair.y for fun.y
Evaluate (fun make_pair.x make_pair.y -> x)
 -> make_pair.x

Evaluate second (make_pair 1 2)
Desugar into (make_pair 1 2) (fun x y -> y)
Evaluate make_pair 1 2
  Evaluate 1 -> 1
  Evaluate 2 -> 2
  Evaluate make_pair -> fun x y -> fun m -> m x y
  Apply fun x y -> fun m -> m x y to 1 2
  Substitute 1 for x, 2 for y
  Apply make_pair to 1 2 -> fun m -> m 1 2
Substitute (fun x y -> y) for m -> (fun x y -> y) 1 2
Apply fun x y -> y to 1 2
  Evaluate 1 -> 1
  Evaluate 2 -> 2
  Evaluate fun 1 2 -> y
    -> 2
*)

(* A.4 *)
let rec pow x y =
  if y = 0
    then 1
    else x * pow x (y - 1)

let rec int_log x y =
  if y mod x <> 0
    then 0
    else 1 + int_log x (y / x)

let make_pairi a b =
  (pow 2 a) * (pow 3 b)

let firsti c = int_log 2 c
let secondi c = int_log 3 c

(* A.5 *)
let zero = []

let is_zero = function
  | [] -> true
  | () :: _ -> false

let succ u = () :: u

let prev = function
  | [] -> invalid_arg "Enter a unary integer that is greater than zero"
  | () :: n -> n

let rec integer_to_unary x =
  if x = 0
    then zero
    else succ (integer_to_unary (x - 1))

let rec unary_to_integer x =
  if is_zero x
    then 0
    else 1 + (unary_to_integer (prev x))

let rec unary_add x y =
  if is_zero y
    then x
    else succ (unary_add x (prev y))

type nat = Zero | Succ of nat

let zero' = Zero

let is_zero' = function
  | Zero -> true
  | Succ _ -> false

let succ' u = Succ u

let prev' = function
  | Zero -> invalid_arg "Input has to be greater than zero"
  | Succ n -> n

let rec integer_to_unary' x =
  if x = 0
    then zero'
    else succ' (integer_to_unary' (x - 1))

let rec unary_to_integer' x =
  if is_zero' x
    then 0
    else 1 + (unary_to_integer' (prev' x))

let rec unary_add' x y =
  if is_zero' y
    then x
    else succ' (unary_add' x (prev' y))

(* No changes to definitions have to be made because the abstraction layer takes
   care of the details of implementation. *)

(* A.6 *)

let zero = fun s -> fun z -> z
let add1 n = fun s -> fun z -> s (n s z)

let one = fun s -> fun z -> s (z)
let two = fun s -> fun z -> s (s (z))
let three = fun s -> fun z -> s (s (s (z)))
let four = fun s -> fun z -> s (s (s (s (z))))
let five = fun s -> fun z -> s (s (s (s (s (z)))))
let six = fun s -> fun z -> s (s (s (s (s (s (z))))))
let seven = fun s -> fun z -> s (s (s (s (s (s (s (z)))))))
let eight = fun s -> fun z -> s (s (s (s (s (s (s (s (z))))))))
let nine = fun s -> fun z -> s (s (s (s (s (s (s (s (s (z)))))))))
let ten = fun s -> fun z -> s (s (s (s (s (s (s (s (s (s (z))))))))))

let add m n s z = (m s) ((n s) z)

let church_to_integer x = x (fun y -> y + 1) 0

(* A.7 *)
(*
church_to_integer zero returns an integer because it applies (int -> int)
and int to 'a -> 'b -> 'b, which means that 'a is int -> int and 'b is int.
So, the 'b that is returned is also int.

church_to_integer one returns an integer because it applies (int -> int)
and int to ('a -> 'b) -> 'a -> 'b, which means that 'a -> 'b is int -> int.
'a is an int because it's the second input, which means that 'b is also an int.
So, the 'b that is returned is an int.
*)

(* B.1 *)
let rec last_sublist = function
  | [] -> invalid_arg "last_sublist: empty list"
  | [n'] -> [n']
  | _ :: t -> last_sublist t

(* B.2 *)
let reverse lst =
  let rec reverse_iter y = function
    | [] -> y
    | h :: t -> reverse_iter (h :: y) t
  in
    reverse_iter [] lst

(* B.3 *)
let rec square_list = function
  | [] -> []
  | h :: t -> (h * h) :: square_list t

let square_list2 items = List.map (fun x -> x * x) items

(* B.4 *)
(*
The answer list is in reverse order because he squares the head of the list,
removes it, and cons it to the answer. Then, the head would become the tail
in the answer list because it is like a queue.

The :: operator wants a single element on the left side, but answer is a list.
He can modify the solution by changing the cons to an append, and changing
the parentheses around the squaring to brackets, like so:
  | h :: t -> iter t (answer @ [h * h])
This isn't efficient because appending makes copies of the answer list.
*)

(* B.5.1 *)
let count_negative_numbers lst =
  let rec counter lst count =
    match lst with
    | [] -> count
    | h :: t when h < 0 -> counter t (count + 1)
    | _ :: t -> counter t count
  in
    counter lst 0

(* B.5.2 *)
let power_of_two_list n =
  let rec helper x y =
    match x with
    | x when x <= 0 -> y
    | _ -> helper (x - 1) ((pow 2 (x - 1)) :: y)
  in
    helper n []

(* B.5.3 *)
let prefix_sum lst =
  let rec helper lst sum answer =
    match lst with
    | [] -> (List.rev answer)
    | h :: t -> helper t (sum + h) ((sum + h) :: answer)
  in helper lst 0 []

(* B.6 *)
let deep_reverse lst =
  let rec deep_reverse_iter y = function
    | [] -> y
    | h :: t -> deep_reverse_iter ((reverse h) :: y) t
  in deep_reverse_iter [] lst

(* B.7 *)
type 'a nested_list =
  | Value of 'a
  | List of 'a nested_list list

let rec deep_reverse_nested lst =
  let reverse lst =
     let rec reverse_iter y = function
       | [] -> y
       | h :: t -> reverse_iter ((deep_reverse_nested h) :: y) t
     in
       reverse_iter [] lst
  in
  match lst with
  | Value a -> Value a
  | List lst -> List (reverse lst)


