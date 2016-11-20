
let fst (a,b) = a
let snd (a,b) = b

let remove_elt e l compare =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x::xs when (compare e x) -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l []

let remove_duplicates l compare =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x :: xs -> go (remove_elt x xs compare) (x::acc)
  in go l []
