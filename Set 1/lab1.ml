(* A.1 *)
(* - : int = 10 *)

(* A.2 *)
(* - : float = 10 *)

(* A.3 *)
(* - : int = 12 *)

(* A.4 *)
(* Error: This expression has type float but an expression was expected of type int *)
(* The operator is for integers, but the numbers are floats *)

(* A.5 *)
(* Error: This expression has type int but an expression was expected of type float *)
(* The operator is for floats, but the numbers are integers *)

(* A.6 *)
(* Error: This expression has type float but an expression was expected of type int *)
(* The operator is for integers, but 4.2 is a float *)

(* A.7 *)
(* Error: This expression has type int but an expression was expected of type float *)
(* The operator is for floats, but 3 is an int *)

(* A.8 *)
(* - : float = 7.2 *)

(* A.9 *)
(* - : int = 5 *)

(* A.10 *)
(* - : int = 7 *)

(* A.11 *)
(* val a : int = 3 *)

(* A.12 *)
(* val b : int = 4 *)

(* A.13 *)
(* - : bool = false *)

(* A.14 *)
(* - : bool = true *)

(* A.15 *)
(* - : bool = false *)
(* This is a different expression because it is asking if both lists are the same object in memory *)

(* A.16 *)
(* - : (int * int * int) list = [(1, 2, 3)] *)
(* Because the elements are separated by commas and not semicolons as lists should have, the interpreter thinks that this is a tuple inside a list *)

(* A.17 *)
(* - : int = 4 *)

(* A.18 *)
(* Error: Syntax error *)
(* 'and' cannot be used in place of the boolean operator '&&' because a boolean operand is expected *)

(* A.19 *)
(* - : int = 6 *)

(* A.20 *)
(* Error: This expression has type int but an expression was expected of type unit *)
(* Because the else was left off, OCaml assumed that it was of type unit. The then clause (b) is of type int, which does not match the type of the else clause, so there is a type error. *)

(* A.2 *)
let sum_of_squares_of_two_largest x y z = 
  if x >= y && y >= z
    then x * x + y * y
  else if x >= y && z >= y
    then x * x + z * z
  else if y >= x && z >= x
    then y * y + z * z
  else if y >= x && x >= z
    then y * y + x * x
  else if x >= z && y >= z
    then x * x + y * y
  else x * x + z * z

(* A.3 *)
(* If b is positive, a and b are added. If b is non-positive, b is subtracted from a. *)

(* B.1 *)
(* With an interpreter that uses applicative-order evaluation, there will be an infinite loop as the function p () is recursively called when y is evaluated. Consequently, nothing will be returned. With a normal-order evaluation, it will only evaluate when necessary, meaning it will return 0 and never evaluate y. *)

(* B.2 *)
(* When Alyssa uses this, the recursive sqrt_iter function will call itself repeatedly when it computes the else_clause of new_if because OCaml uses applicative-order evaluation. With the ordinary if function, it doesn't have to evaluate all the arguments first, but the new_if function does have to evaluate all arguments first. *)

(* B.3 *)
(*
Let us evaluate add_a 2 5 using the substitution model.
Desugar the function: let rec add_a = fun a b -> if a = 0...
Evaluate fun: fun a b -> if a = 0 ... -> itself
Make association between add_a and new function
Evaluate add_a 2 5
  Evaluate 2 -> 2
  Evaluate 5 -> 5
  Evaluate add_a -> fun a b = if a = 0...
  Apply fun a b -> a = 0... to 2 5
    Substitute 2 for a and 5 for b
      -> if 2 = 0
           then 5
           else inc (add_a (dec 2) 5)
    Evaluate if 2 = 0 then 5 else inc (add_a (dec 2) 5) [special form]
    Evaluate if clause first
    Evaluate 2 = 0
    Evaluate 2 -> 2
    Evaluate 0 -> 0
    Evaluate = -> [primitive function =]
    Evaluate 2 = 0 -> false
    Because if clause is false, evaluate else statement
      Evaluate inc (add_a (dec 2) 5)
      Evaluate according to precedence, which is innermost parentheses first
      Evaluate dec -> [primitive function dec]
      Apply dec to 2 -> 2 - 1
      Evaluate 2 - 1 -> 1
      Evaluate inc (add_a 1 5)
      Evaluate according to precedence, which is parentheses first
      Evaluate add_a 1 5 -> if a = 0...
      Evaluate 1 -> 1
      Evaluate 5 -> 5 
      Apply fun a b -> a = 0... to 1 5
        Substitute 1 for a and 5 for b
          -> if 1 = 0
               then 5
               else inc (add_a (dec 1) 5)
        Evaluate if 1 = 0 then 5 else inc (add_a (dec 1) 5) [special form]
        Evaluate if clause first
        Evaluate 1 = 0
        Evaluate 1 -> 1
        Evaluate 0 -> 0
        Evaluate = -> [primitive function =]
        Evaluate 1 = 0 -> false
        Because if clause is false, evaluate else statement
          Evaluate inc (add_a (dec a) 5)
          Evaluate according to precedence, which is innermost parentheses first
          Evaluate dec -> [primitive function dec]
          Apply dec to 1 -> 1 - 1
          Evaluate 1 - 1 -> 0
          Evaluate inc (add_a 0 5)
          Evaluate 0 -> 0
          Evaluate 5 -> 5
          Evaluate add_a 0 5 -> if a = 0...
          Apply fun a b -> a = 0... to 0 5
            Substitute 0 for a and 5 for b
              -> if 0 = 0
                   then 5
                   else inc (add_a (dec 0) 5)
            Evaluate if 0 = 0 then 5 else inc (add_a (dec 0) 5) [special form]
            Evaluate if clause first
            Evaluate 0 = 0
            Evaluate 0 -> 0
            Evaluate 0 -> 0
            Evaluate = -> [primitive function =]
            Evaluate 0 = 0 -> true
            Because if clause is true, evaluate then statement
            Go back and complete pending operations now that we have found the end of recursion.
        Substitute 5 for (add_a (dec 1) 5) in inc (add_a (dec 1) 5) -> inc 5
	Evaluate inc -> [primitive function inc]
        Apply inc to 5 -> 5 + 1
        5 + 1 -> 6
        Go back and complete pending operations.
      Substitute 6 for (add_a (dec 2) 5) in inc (add_a (dec 2) 5) -> inc 6
      Evaluate inc -> [primitive function inc]
      Apply inc to 6 -> 6 + 1
      6 + 1 -> 7
*)

(*
Let us evaluate add_b 2 5 using the substitution model.
Desugar the function: let rec add_b = fun a b -> if a = 0...
Evaluate fun: fun a b -> if a = 0... -> itself
Make association between add_b and new function
Evaluate add_b 2 5
  Evaluate 2 -> 2
  Evaluate 5 -> 5
  Evaluate add_b -> fun a b = if a = 0...
  Apply fun a b -> a = 0... to 2 5
    Substitute 2 for a and 5 for b
    -> if 2 = 0
         then 5
         else add_b (dec 2) (inc 5)
    Evaluate if 2 = 0 then 5 else add_b (dec 2) (inc 5) [special form]
    Evaluate if clause first
    Evaluate 2 = 0
    Evaluate 2 -> 2
    Evaluate 0 -> 0
    Evaluate = -> [primitive function =]
    Evaluate 2 = 0 -> false
    Because if clause is false, evaluate else statement
    Evaluate add_b (dec 2) (inc 5)
    Evaluate dec 2
      Evaluate dec -> [primitive function dec]
      Evaluate 2 -> 2
      Apply dec to 2 -> 2 - 1
      2 - 1 -> 1
    Evaluate inc 5
      Evaluate inc -> [primitive function inc]
      Evaluate 5 -> 5
      Apply inc to 5 -> 5 + 1
      5 + 1 -> 6
    Substitute 1 for dec 2 and 6 for inc 5 -> add_b 1 6
    Evaluate add_b 1 6
      Evaluate 1 -> 1
      Evaluate 6 -> 6
      Evaluate add_b -> fun a b = if a = 0...
      Apply fun a b -> a = 0... to 1 6
        Substitute 1 for a and 6 for b
        -> if 1 = 0
             then 6
             else add_b (dec 1) (inc 6)
        Evaluate if 1 = 0 then 6 else add_b (dec 1) (inc 6) [special form]
        Evaluate if clause first
        Evaluate 1 = 0
        Evaluate 1 -> 1
        Evaluate 0 -> 0
        Evaluate = -> [primitive function =]
        Evaluate 1 = 0 -> false
        Because if clause is false, evaluate else statement
        Evaluate add_b (dec 1) (inc 6)
        Evaluate dec 1
          Evaluate dec -> [primitive function dec]
          Evaluate 1 -> 1
          Apply dec to 1 -> 1 - 1
          1 - 1 -> 0
        Evaluate inc 6
          Evaluate inc -> [primitive function inc]
          Apply inc to 6 -> 6 + 1
          6 + 1 -> 7
        Substitute 0 for dec 1 and 7 for inc 6 -> add_b 0 7
        Evaluate add_b 0 7
          Evaluate 0 -> 0
          Evaluate 7 -> 7
          Evaluate add_b -> fun a b = if a = 0...
          Apply fun a b -> a = 0... to 0 7
            Substitute 0 for a and 7 for b
            -> if 0 = 0
                 then 7
                 else add_b (dec 0) (inc 7)
            Evaluate if 0 = 0 then 7 else add_b (dec 0) (inc 7) [special form]
            Evaluate if clause first
            Evaluate 0 = 0
            Evaluate 0 -> 0
            Evaluate = -> [primitive function =]
            Evaluate 0 = 0 -> true
            Because if clause is true, evaluate then statement
            7 -> 7
*)

(* add_a is a recursive process because there are pending operations, and add_b is an iterative process because the 'variables' are iterated. *)

(* C.1 *)
let rec factorial n =
  if n = 0 then 1 else n * factorial (n-1)

(* C.1.a *)
(* float_of_int and float are the same thing though ? *)
let e_term x =
  1.0 /. float_of_int (factorial x)

(* C.1.b *)
let rec e_approximation x =
  if x = -1 then 0.0 else e_term x +. e_approximation (x - 1)

(* C.1.c *)
(* e_approximation 20 = 2.71828182845904553 *)
(* exp 1.0 = 2.71828182845904509 *)

(* C.1.d *)
(* The interpeter outputs - : float = infinity because 64-bit numbers (which is what floats are) can only have up to 17 significant digits. When it computes e_term 100, it will find that factorial 100 = 0. So, when it divides 1.0 /. 0.0, it will return infinity. *)

(* C.2 *)
let rec is_even x =
  if x = 1 then false else is_odd (x - 1)
and is_odd x =
  if x = 1 then true else is_even (x - 1)

(* C.3 *)

let rec f_rec n =
  if n < 3 then n else f_rec (n - 1) + 2*f_rec (n - 2) + 3*f_rec (n - 3)

let rec f_helper x y z counter =
  if counter = 0 
     then x
  else f_helper y z (z + 2 * y + 3 * x) (counter - 1)

let f_iter n =
  f_helper 0 1 2 n 


(* C.4 *)
let rec pascal_coefficient row index = 
  match row, index with
  | row', index' when (index' > row') || (row' <= 0) || (index' <= 0) -> failwith "invalid arguments"
  | row', index' when (index' = 1) || (row' = index') -> 1
  | row', index' -> pascal_coefficient (row' - 1) (index' - 1) + pascal_coefficient (row' - 1) index'
