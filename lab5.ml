(* Jessica Choi *)
(* CS4 Set 5 *)

(* A.1 *)
let fibonacci n =
  let fib2 = ref 0 in
  let fib1 = ref 1 in
  let fib = ref 1 in
  match n with
  | 0 -> 0
  | 1 -> !fib1
  | 2 -> !fib
  | _ ->
    let i = ref 1 in
      while (!i+1) < n do
        fib := !fib1 + !fib;
        fib1 := !fib2 + !fib1;
        fib2 := !fib - !fib1;
        i := !i + 1
      done;
    !fib


let fibonacci2 n =
  let fib2 = ref 0 in
  let fib1 = ref 1 in
  let fib = ref 1 in
  match n with
  | 0 -> !fib2
  | 1 -> !fib1
  | 2 -> !fib
  | _ ->
    for i = 1 to (n-2) do
      fib := !fib1 + !fib;
      fib1 := !fib2 + !fib1;
      fib2 := !fib - !fib1;
    done;
  !fib

(* A.2 *)
let bubble_sort unsorted =
  let n = Array.length unsorted in
  if n < 2 then () else
    let swapped = ref true in
    let temp = ref unsorted.(0) in
      while !swapped do
        swapped := false;
        for i = 0 to (n-2) do
          if unsorted.(i) > unsorted.(i+1) then
            begin
              temp := unsorted.(i);
              unsorted.(i) <- unsorted.(i+1);
              unsorted.(i+1) <- !temp;
              swapped := true
            end
          else ()
        done;
      done;;

(* B.1a *)
let meters_per_foot = 0.3048

let get_meters len =
  match len with
  | `Meter m -> m
  | `Foot f -> f *. meters_per_foot
  | `Inch i -> i /. 12.0 *. meters_per_foot

let length_add a b = `Meter (get_meters a +. get_meters b)

(* B.1b *)
let grams_per_slug = 14593.903203

let get_grams mass =
  match mass with
  | `Gram g -> g
  | `Slug s -> s *. grams_per_slug
  | `Kilo k -> k *. 1000.0

let mass_add a b = `Gram (get_grams a +. get_grams b)


let get_seconds time =
  match time with
  | `Second s -> s
  | `Minute m -> 60. *. m
  | `Hour h -> 3600. *. h
  | `Day d -> 86400. *. d

let time_add a b = `Second (get_seconds a +. get_seconds b)

(* B.1c *)
let unit_add a b =
  match (a, b) with
  | (`Length a, `Length b) -> `Length (length_add a b)
  | (`Mass a, `Mass b) -> `Mass (mass_add a b)
  | (`Time a, `Time b) -> `Time (time_add a b)
  | (_, _) -> failwith "incompatible classes"

(* We do not get a combinatorial explosion, because there are n^2 possibilities
   with n unit classes, which is very manageable. Also, every unit class encompasses
   many unit tags, which means that only unit classes have to be added to unit_add.*)

(* C.1 *)
let rec make_gram g =
  let as_slug = 1. /. 14593.903203 in
    object (self)
      method get_grams = g
      method get_slugs = g *. as_slug
      method unit_type = `Gram
      method compatible other =
        match other#unit_type with
        | `Gram
        | `Slug -> true
        | _ -> false
      method add other =
        match other#unit_type with
        | `Gram
        | `Slug -> make_gram (g +. other#get_grams)
        | _ -> failwith "incompatible classes"
    end

(* C.2a *)
  (* Define a number as a message-passing object. *)
  (* "i" is an int. *)
  let rec make_number i =
    object
      method value = i
      method show = string_of_int i
      method is_zero = i = 0
      method is_number = true
      method evaluate _ _ = make_number i  (* must evaluate to an object *)
      method derive _ = make_number 0  (* derivative of a number is 0 *)
    end
  
  (* Define a variable as a message-passing object. *)
  (* "v" is a string. *)
  let rec make_variable v =
    object
      method value = failwith "variable has no numerical value"
      method show  = v
      method is_zero = false
      method is_number = false
      method evaluate v' n =
        if v = v'
          then make_number n
          else make_variable v
      method derive v' =
        if v = v'
          then make_number 1  (* d/dx(x) = 1 *)
          else make_number 0  (* d/dx(y) = 0 *)
    end
  
  (* Define a sum as a message-passing object. *)
  let rec make_sum expr1 expr2 =
    match () with
      | _ when expr1#is_zero -> expr2  (* 0 + expr = expr *)
      | _ when expr2#is_zero -> expr1  (* expr + 0 = expr *)
      | _ when expr1#is_number && expr2#is_number ->  (* add numbers *)
            make_number (expr1#value + expr2#value)
      | _ ->  (* create a new object representing the sum *)
            object
              method value = failwith "sum expression has no numerical value"
              method show = "(" ^ expr1#show ^ " + " ^ expr2#show ^ ")"
              method is_zero = false
              method is_number = false
              method evaluate v n = 
                make_sum (expr1#evaluate v n) (expr2#evaluate v n)
              method derive v = 
                make_sum (expr1#derive v) (expr2#derive v)
            end
  
  (* Evaluate a message-passing expression with a number 
     substituted for a variable. *)
  let evaluate expr v n = expr#evaluate v n
    
  (* Return the string representation of an expression. *)
  let show expr = expr#show
  
  (* Return the derivative of an expression. *)
  let differentiate expr v = expr#derive v

  (* Define a product as a message-passing object. *)
  let rec make_product expr1 expr2 =
    match () with
      | _ when expr1#is_zero -> make_number 0
      | _ when expr2#is_zero -> make_number 0
      | _ when expr1#is_number && expr1#value = 1 -> expr2
      | _ when expr2#is_number && expr2#value = 1 -> expr1
      | _ when expr1#is_number && expr2#is_number -> make_number (expr1#value * expr2#value)
      | _ ->
            object
              method value = failwith "sum expression has no numerical value"
              method show = "(" ^ expr1#show ^ " * " ^ expr2#show ^ ")"
              method is_zero = false
              method is_number = false
              method evaluate v n = 
                make_product (expr1#evaluate v n) (expr2#evaluate v n)
              method derive v = 
                make_sum (make_product (expr1#derive v) expr2)
                         (make_product expr1 (expr2#derive v))
            end

(* C.2b *)

(* 1 *)
(* f = x^3*y + 3*x^2*y^2 + y^2 + 2 *)
(*  let f =
    make_sum
     (make_product 
      (make_variable "x")
      (make_product 
       (make_variable "x")
       (make_product
        (make_variable "x")
        (make_variable "y"))))
     (make_sum
      (make_product 
       (make_number 3)
       (make_product
        (make_variable "x")
        (make_product
         (make_variable "x")
         (make_product
          (make_variable "y")
          (make_variable "y")))))
      (make_sum
       (make_product 
        (make_variable "y")
        (make_variable "y"))
       (make_number 2)))
*)
(*
val f :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj>
*)

(* 2 *)
(* let dfdx = differentiate f "x" *)
(*
val dfdx :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj>
*)

(* 3 *)
(* show dfdx *)
(*
- : string =
"(((x * (x * y)) + (x * ((x * y) + (x * y)))) + (3 * ((x * (y * y)) + (x * (y * y)))))"
*)

(* 4 *)
(* show (evaluate f "x" 3) *)
(*
- : string =
"((3 * (3 * (3 * y))) + ((3 * (3 * (3 * (y * y)))) + ((y * y) + 2)))"
*)

(* 5 *)
(* show (evaluate (evaluate f "x" 3) "y" 4) *)
(* - : string = "558" *)

(* 6 *)
(* show (evaluate (evaluate dfdx "x" 3) "y" 4) *)
(* - : string = "396" *)


