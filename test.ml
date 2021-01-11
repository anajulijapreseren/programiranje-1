
let remove_elt e l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x::xs when e = x -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l []

let remove_duplicates l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x :: xs -> go (remove_elt x xs) (x::acc)
  in go l []

let prestej_crke = function
  | [] -> 0
  | l-> List.length (remove_duplicates l)

let lis = [1;2;4;2;1;2]

let st = "a" ^ "b" ^ "b"

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let neki = (explode st)

let sez = ["acc -2"; "jp *7"]

let text = "abv cfdh"

let split besedilo = match besedilo with
    |""-> []
    |_-> String.split (String.regexp " ") besedilo