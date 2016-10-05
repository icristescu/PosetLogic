
let print_prop p = print_string(pname p);;

let bracket f x y =
  (if p then print_string "(" else ());
  f x y;
  (if p then print_string ")" else ());;

let rec strip_quant fm =
  match fm with
    Forall(x,(Forall(y,p) as yp)) | Exists(x,(Exists(y,p) as yp)) ->
        let xs,q = strip_quant yp in x::xs,q
  |  Forall(x,p) | Exists(x,p) -> [x],p
  | _ -> [],fm;;

let print_formula =
  let rec print_formula pr fm =
    match fm with
      False -> print_string "false"
    | True -> print_string "true"
    | Atom(pargs) -> print_consts pargs
    | Not(p) -> bracket (print_prefix) "~" p
    | And(p,q) -> bracket (print_infix 8 "/\\") p q
    | Or(p,q) -> bracket (print_infix  6 "\\/") p q
    | Imp(p,q) -> bracket (print_infix 4 "==>") p q
    | Iff(p,q) -> bracket (print_infix 2 "<=>") p q
    | Forall(x,p) -> bracket print_qnt "forall" (strip_quant fm)
    | Exists(x,p) -> bracket print_qnt "exists" (strip_quant fm)
  and print_qnt qname (bvs,bod) =
    print_string qname;
    do_list (fun v -> print_string " "; print_string v) bvs;
    print_string "."; print_space();
    print_formula 0 bod;
  and print_prefix newpr sym p =
   print_string sym; print_formula (newpr+1) p
  and print_infix newpr sym p q =
    print_formula (newpr+1) p;
    print_string(" "^sym); print_space();
    print_formula newpr q in
  print_formula 0;;

let print_qformula fm =
  print_string "<<";
  print_formula fm;
  print_string ">>";;
