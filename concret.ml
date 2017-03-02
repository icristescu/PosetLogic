
let decorate s = [s]

let linears s =
  LinearPoset.linearisations(s)

let refine (t:Trace.t) (r: (Event.t*Transition.t) list) s id = [Trace.empty]

let concretise (s:LinearPoset.t) id concretisations1 =
  List.fold_left
    (fun concretisations2
         (t,r) -> refine t r s id) [] concretisations1

let concret (s:Poset.t) = concretise (linears s) 0 []
