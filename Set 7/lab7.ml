(* Jessica Choi *)
(* CS 4 Set 7 *)

(* ----------------------------------------------------------------------
 * Utility types, functions and values.
 * ---------------------------------------------------------------------- *)

type light = On | Off

let size = 5  (* board size: 5x5 *)

let valid_loc (row, col) =
  let in_range n = n >= 0 && n < size in
    in_range row && in_range col

let newline () = Printf.printf "\n"

(* ----------------------------------------------------------------------
 * Board module type.
 * ---------------------------------------------------------------------- *)

type loc = int * int

module type BOARD =
  sig
    type t
    exception Off_board

    val make : loc list -> t
    val get : t -> loc -> bool
    val flip : t -> loc -> t
    val is_solved : t -> bool
  end

(* ----------------------------------------------------------------------
 * Board representation 1: an array of arrays of booleans.
 * ---------------------------------------------------------------------- *)
(* NO PROBLEMS *)
module ArrayBoard : BOARD =
  struct
    type t = bool array array
    exception Off_board

    let get b (row, col) =
      if not (valid_loc (row, col))
      then raise Off_board
      else b.(row).(col)

    let flip b (row, col) =
      if not (valid_loc (row, col))
      then raise Off_board
      else
        let new_array = Array.make_matrix 5 5 false in
          for i = 0 to (size - 1) do
            for j = 0 to (size - 1) do
              if b.(i).(j) = true then new_array.(i).(j) <- true;
              if (i, j) = (row, col) then new_array.(i).(j) <- (not b.(i).(j))
            done;
          done;
      new_array

    let make locs =
      let rec iter new_array locs =
        match locs with
        | [] -> new_array
        | ((row,col) :: t) ->
          if valid_loc (row, col) then
          begin
          new_array.(row).(col) <- true;
          iter new_array t;
          end
          else raise Off_board
      in
      iter (Array.make_matrix 5 5 false) locs

    let is_solved b =
      try
        for i = 0 to (size - 1) do
          for j = 0 to (size - 1) do
            if b.(i).(j) = true
            then raise Exit
          done;
        done;
      true
      with Exit -> false
  end

(* ----------------------------------------------------------------------
 * Board representation 2: a set of occupied locations.
 * ---------------------------------------------------------------------- *)

module LocM : Set.OrderedType with type t = loc =
  struct
    type t = loc
    let compare = Pervasives.compare
  end

module LocSet = Set.Make(LocM)
(* NO PROBLEMS *)
module SetBoard : BOARD =
  struct
    type t = LocSet.t
    exception Off_board

    let get b loc =
      if not (valid_loc loc) then raise Off_board
      else if LocSet.mem loc b then true
      else false

    let flip b loc =
      if get b loc then LocSet.remove loc b else LocSet.add loc b

    let make locs =
      let rec iter set locs =
        match locs with
        | [] -> set
        | ((row, col) :: t) -> iter (flip set (row, col)) t
      in
        iter LocSet.empty locs

    let is_solved b =
      LocSet.is_empty b
  end

(* ----------------------------------------------------------------------
 * Game object.
 * ---------------------------------------------------------------------- *)

module type GAME =
  sig
    type t

    val play : t -> loc -> t
    val play_many : t -> loc list -> t
    val from_array : light array array -> t
  end
(* NO PROBLEMS *)
module Game (Board : BOARD) : GAME with type t = Board.t =
  struct
    type t = Board.t

    let play b loc =
      let new_board = ref b in
      let new_lst = List.filter valid_loc [loc; (fst loc, snd loc + 1);
                                           (fst loc, snd loc - 1);
                                           (fst loc + 1, snd loc);
                                           (fst loc - 1, snd loc)] in
        for i = 1 to List.length new_lst do
          new_board := Board.flip !new_board (List.nth new_lst (i - 1));
        done;
      !new_board

    let play_many b locs =
      let new_board = ref b in
        for i = 1 to List.length locs do
          new_board := play !new_board (List.nth locs (i - 1));
        done;
      !new_board

    (* Initialize a board given an array of arrays of on/off values
       by setting the corresponding lights to those values. *)
    let from_array arr =
      let locs = ref [] in
        if Array.length arr != size
        then failwith "Does not have 5 rows"
        else
          for i = 0 to Array.length arr - 1 do
            if Array.length (Array.get arr i) != size
            then failwith "Does not have 5 columns";
          done;
        for i = 0 to size - 1 do
          for j = 0 to size - 1 do
            if arr.(i).(j) = On then locs := (i, j) :: !locs
            else ()
          done;
        done;
      Board.make !locs
  end

(* ----------------------------------------------------------------------
 * Playing the game interactively.
 * ---------------------------------------------------------------------- *)

module type INTERACT =
  sig
    type t
    type game_input = Quit | Coords of loc

    val is_digit : char -> bool
    val ok_coords_line : string -> bool
    val get_input : unit -> game_input
    val print : t -> unit
    val run : t -> unit
    val play : light array array -> unit
  end

(* Supplied to students. *)
module Interact (Board : BOARD) : INTERACT with type t = Board. t =
  struct
    (* local module *)
    module G = Game(Board)

    type t = Board.t
    type game_input = Quit | Coords of loc

    let is_digit c = c >= '0' && c <= '9'

    let ok_coords_line line =
      String.length line = 3
        && is_digit line.[0]
        && line.[1] = ' '
        && is_digit line.[2]

    let get_input () =
      let line = read_line () in
        match line with
          | "quit" -> Quit
          | _ when ok_coords_line line ->
            let row = int_of_string (Printf.sprintf "%c" line.[0]) in
            let col = int_of_string (Printf.sprintf "%c" line.[2]) in
            let loc = (row, col) in
              if valid_loc loc
                then Coords loc
                else failwith "invalid coordinates"
          | _ -> failwith "invalid input line"

    let print b =
      begin
        Printf.printf "    ";
        for col = 0 to size - 1 do
          Printf.printf "%d " col
        done;
        Printf.printf "\n   -----------\n";
        for row = 0 to size - 1 do
          Printf.printf "%d | " row;
          for col = 0 to size - 1 do
            Printf.printf "%c "
              (if (Board.get b (row, col)) then 'O' else '.')
          done;
          Printf.printf "|\n"
        done;
        Printf.printf "   -----------\n\n";
      end

    let rec run b =
      try
        begin
          Printf.printf "Enter move (row col): ";
          match get_input () with
            | Quit -> ()
            | Coords (row, col) ->
                let b' = G.play b (row, col) in
                begin
                  newline ();
                  print b';
                  if Board.is_solved b'
                    then Printf.printf "You win!\n\n"
                    else run b'
                end
        end
      with Failure msg -> (Printf.printf "ERROR: %s\n\n" msg; run b)

    let play init =
      let game = G.from_array init in
        begin
          newline ();
          print game;
          run game
        end
  end

(* ----------------------------------------------------------------------
 * Sample initial boards.
 * ---------------------------------------------------------------------- *)

module PlayA = Interact(ArrayBoard)
module PlayS = Interact(SetBoard)

let init1 =
  [|
     [| On;  On;  On;  Off; Off |];
     [| Off; Off; Off; On;  Off |];
     [| On;  On;  Off; On;  On  |];
     [| Off; Off; Off; On;  Off |];
     [| On;  On;  On;  Off; Off |]
  |]

let test_array_board () = PlayA.play init1
let test_set_board ()   = PlayS.play init1
