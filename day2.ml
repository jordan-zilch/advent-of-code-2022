open Char
open List

exception BadInput of string

let read_lines (name: string) : string list = 
  let ic = open_in name in
  let try_read () = 
    try Some (input_line ic) with End_of_file -> None in 
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in 
  loop []

(*A beats C, C beats B, B beats A*)
(*X beats Z, Z beats Y, Y beats X*)

let round_score (opp : string) (yours : string) : int =
  let yours_adj = Char.chr (Char.code yours.[0] - 23) in
  let a = match yours_adj with
  | 'A' -> 1
  | 'B' -> 2
  | 'C' -> 3 
  | _ -> raise (BadInput (String.make 1 yours_adj)) in
  let opp_chr = opp.[0] in
  if yours_adj = opp_chr then 
    a + 3
  else 
    if ((Char.code yours_adj) mod 3) + 65 = Char.code opp_chr then 
      a + 6 
    else a



let foo (line: string) : int =
  match String.split_on_char ' ' line with
    | opp_move :: your_move -> round_score opp_move (List.hd your_move)
    | _ -> 100000
;;

print_int (List.fold_right (fun x y -> x+y) (List.map foo (read_lines "day2.txt")) 0)