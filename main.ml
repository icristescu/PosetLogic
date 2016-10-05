

let files = ref []

let options = []

let printf = Printf.printf

let test_membership t =
  let posets = Poset.get_posets t in
  let events = Poset.get_events t in
  let e1 = List.nth events 5 in
  let p1 = List.hd posets in

  let valuation x =
    match x with
    | "x" -> Poset.Ev(e1)
    | "y" -> Poset.Pos(p1)
    | _ -> failwith "uninterpreted variable" in
  let fm =
    (Formulas.Atom(Formulas.R("in", [Formulas.Var "x"; Formulas.Var "y"]))) in
  (valuation,fm)

let () =
  let () =
    Arg.parse
      options
      (fun f -> files := f::(!files))
      (Sys.argv.(0) ^
       " stories\n outil") in

  let posets = Poset.gather_posets (!files) in
  (* ["story0.json"; "story1.json"; "story3.json"; "story4.json"] in*)
  let () = Poset.print_posets (posets) in
  let (func,pred) = Formulas.interpretation () in
  let (valuation,fm) = test_membership posets in (*false*)

  if (Formulas.holds (func,pred) valuation fm) then printf "true\n"
  else printf "false\n"
