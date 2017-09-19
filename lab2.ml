(* Jessica Choi *)
(* CS 4 Set 2 *)
(* Thanks for being understanding :) *)

(* Question A.1 *)
(* The space complexity is linear, or O(n) because the fib(n-1) term is 
   evaluated before the fib(n-2) term is, and it uses n memory slots.
   Since the fib(n-1) term is bigger than the fib(n-2) term, the function
   will never use more than n memory slots and the space complexity is linear. *)

(* A.2 *)
(* The function p is applied 5 times.
   sine 12.15 -> p(sine(12.15/3)) -> p(sine 4.05) -> p(p(sine 4.05/3)) -> 
   p(p(sine 1.35)) -> p(p(p(sine 1.35/3))) -> p(p(p(sine .45))) ->
   p(p(p(p(sine .15)))) -> p(p(p(p(p(sine .05))))) *)
(* The time complexity is logarithmic O(log a) because the recursion occurs
   through dividing the argument. The space complexity is logarithmic O(log a)
   because as arguments are evaluated, the memory is reused, and there are always 
   less than (log a) number of arguments. Also, the space complexity is equal to
   the time complexity in this function. *)

(* A.3.1 *)
let rec fast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
    match n with
    | 0 -> 1
    | n' when is_even n' = true -> square (fast_expt b (n' / 2))
    | _ -> b * fast_expt b (n-1)

(* A.3.2 *)
let ifast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
  let rec iter a b n =
    match n with
    | 0 -> a
    | n' when is_even n' = true -> (iter a (square b) (n' / 2))
    | _ -> iter (a * b) b (n - 1)
    in
  iter 1 b n

(* A.4 *)
let rec fast_mult b n =
  let double m = m * 2 in
  let is_even m = m mod 2 = 0 in
  let halve m = m / 2 in
    match n with
    | 0 -> 0
    | n' when is_even n' = true -> double (fast_mult b (halve n))
    | _ -> b + fast_mult b (n - 1)

(* A.5 *)
let ifast_mult b n = 
  let double m = m * 2 in
  let is_even m = m mod 2 = 0 in
  let halve m = m / 2 in
  let rec iter a b n =    
    match n with
    | 0 -> a
    | n' when is_even n' = true -> iter a (double b) (halve n)
    | _ -> iter (a + b) b (n - 1)
    in
  iter 0 b n
 

(* A.6 *)
(* The worst case time complexity of foo would be linear, or O(n) because 
   the recursion occurs through dividing the argument but evaluating the function
   twice, which would be 2^log n, which is similar to O(n). The worst case
   space complexity would be O(log n) because every foo f (n/2) would
   take a maximum of log n slots. *)

(* A.7 *)
(* This function is linear recursive because it remembers the last two
   fibonacci numbers for n to find the last two numbers for (n + 1), while keeping
   the pending operations. The space complexity is O(n) because for every n, a stack
   of size n must be remembered while computing n-1. The time complexity is O(n)
   because last_two is called once in every iteration, and takes a constant amount
   of time to run. *)

(* B.1a *)
(* (fun x y -> x * (2 + y)) 20 (2 * 4) *)

(* B.1b *)
(* (fun a b c -> sqrt (b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0 *)

(* B.1c *)
(* (fun x -> (fun y -> (fun z -> x * y * z) 3) 2) 1 *)

(* B.1d *)
(* (fun x -> x * (fun x -> x * (fun x -> x) 3) 2) 1 *)

(* B.2 *)
(*
Desugar let x = 2 * 10 and y = 3 + 4 ...
-> (fun x y -> let y = 14 in...) x y
Desugar let y = 14 in...
-> (fun x y -> (fun y -> let z = 22 in...) y) x y (y is shielded)
Desugar let z = 22 in x * y * z
-> (fun x y -> (fun y -> (fun z -> x * y * z) z) y) x y
Substitute 2 * 10 for x in (fun x y -> (fun y -> ...))
-> (fun x y -> (fun y -> (fun z -> x * y * z) z) y) (2 * 10) y
Substitute 3 + 4 for y in (fun x y -> (fun y -> ...))
-> (fun x y -> (fun y -> (fun z -> x * y * z) z) y) (2 * 10) (3 + 4)
Substitute 14 for y in (fun x -> (fun y -> ...))
-> (fun x y -> (fun y -> (fun z -> x * y * z) z) 14) (2 * 10) (3 + 4)
Substitute 22 for z in (fun x -> (fun y -> ...))
-> (fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)
Evaluate 2 * 10
  Evaluate 2 -> 2
  Evaluate 10 -> 10
  Evaluate * -> [primitive function *]
  Apply * to 2 10 -> 20
Evaluate 3 + 4
  Evaluate 3 -> 3
  Evaluate 4 -> 4
  Evaluate + -> [primitive function +]
  Apply + to 3 4 -> 7
Apply fun z -> x * y * z to 22
  Substitute 22 for z in x * y * z -> x * y * 22
Apply fun y -> x * y * 22 to 14
  Substitute 14 for y in x * y * 22 -> x * 14 * 22
Apply fun y -> x * 14 * 22 to 7
  y is shielded, and already has a value
Apply fun x -> x * 14 * 22 to 2 * 10
  Evaluate 2 * 10 -> 20
  Substitute 20 for x in x * 14 * 22 -> 20 * 14 *22
Evaluate 20 * 14 * 22
  Evaluate 20 -> 20
  Evaluate 14 -> 14
  Evaluate 22 -> 22
  Evaluate * -> [primitive function *]
  Apply * to 20 14 -> 280
  Apply * to 280 22 -> 6160
*)

(* B.3 *)
(* (fun x y z -> x + y + z) 10 (x * 2) (y + 3) *)
(* The x in y = x * 2 is not the same x as in x = 10. The y in z = y + 3 is not
   the same as the y in y = x * 2. This is due to how let statements get desugared
   when they are evaluated. *)
(* Ben can use nested lets to fix his code. *)
(*
let x = 10 in 
let y = x * 2 in
let z = y + 3 in
  x + y + z
*)


open Num
let ni = num_of_int 
let next x = x +/ (ni 1)

(* C.1 *)
let isum term a next b =
  let rec iter a result = 
    if a > b
      then result
      else iter (next a) (result +/ (term a))
  in
    iter a (ni 0)

(* C.2a *)
let rec product_rec term a next b = 
  if a > b
    then ni 1
    else (term a) */ (product_rec term (next a) next b)

let f x = x

let factorial_rec n = 
  product_rec f (ni 1) next n

(* C.2b *)
let product_iter term a next b =
  let rec iter a result =
    if a > b
      then result
      else iter (next a) (result */ (term a))
  in
    iter a (ni 1)

let factorial_iter n =
  product_iter f (ni 1) next n

let pi_product n =
  let dnext m = m +/ ni 2 in
  let square m = m */ m in
  let double m = m */ ni 2 in
(* multiplying by 4 because the formula is for pi/4, and multiplying by 2
   to take care of the first term in the numerator *)  
ni 4 */ ni 2 */ 
(* in the numerator, we want to square numbers starting from 4 and
   incrementing by 2 until we reach 2n+2 *)
(product_iter square (ni 4) dnext (double n +/ (ni 2))) //
(* we want to divide the last term out because the numerator and
   denominator start out mismatched *)
 (double n +/ (ni 2)) //
(* in the denominator, we want to square numbers starting from 3 and
   incrementing by 2 until we reach 2n+1 *)
 (product_iter square (ni 3) dnext (double n +/ (ni 1)))

let pi_approx = float_of_num(pi_product (ni 1000))

(* C.3 *)
let rec accumulate_rec combiner null_value term a next b =
  if a > b
    then null_value
    else combiner (term a)
         (accumulate_rec combiner null_value term (next a) next b)

let sum term a next b =
  accumulate_rec ( +/ ) (ni 0) term a next b

let product term a next b =
  accumulate_rec ( */ ) (ni 1) term a next b

let accumulate_iter combiner null_value term a next b =
  let rec iter a result =
    if a > b
      then result
      else iter (next a) (combiner (term a) result)
  in
    iter a (null_value)

(* C.4 *)
let compose f g =
  (fun x -> f (g x))

(* C.5 *)
let rec repeated f n =
  if n = 1
    then f
    else compose f (repeated f (n - 1))

(* C.6 *)
let smooth f dx =
  fun x -> (f x +. f (x -. dx) +. f (x +. dx)) /. 3.0

let nsmoothed f n dx =
  repeated (smooth f dx) n


(* D.1 *)
let is_prime x = 
  let rec helper x n =
    if x <= 1
      then false
      else if int_of_float(sqrt(float_of_int x) +. 1.0) = n 
        then true
      else if x mod n = 0 
        then false
      else helper x (n + 1)
  in 
    helper x 2

(* D.2 *)
let smallest_prime_factor x =
  let rec helper x n =
    if x <= 1
      then invalid_arg "input has to be greater than 1"
      else if int_of_float(sqrt(float_of_int x) +. 1.0) = n
        then failwith "x is prime"
      else if x mod n = 0
        then n
      else helper x (n + 1)
  in
    helper x 2
