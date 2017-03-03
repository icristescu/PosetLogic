
let fst (a,b) = a
let snd (a,b) = b
let thd (a,b,c) = c

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

(* interleave 1 [2;3] = [ [1;2;3]; [2;1;3]; [2;3;1] ] *)
let rec interleave x lst =
match lst with
| [] -> [[x]]
| hd::tl -> (x::lst) :: (List.map (fun y -> hd::y) (interleave x tl))

(*permutations [1; 2; 3] =
[[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; [3; 1; 2]; [3; 2; 1]] *)
let rec permutations lst =
match lst with
| hd::tl -> List.concat (List.map (interleave hd) (permutations tl))
| _ -> [lst]
;;
