val fst : 'a* 'b -> 'a
val snd : 'a* 'b -> 'b
val thd : 'a* 'b* 'c -> 'c

val remove_duplicates : 'a list -> ('a -> 'a -> bool) -> 'a list
val concat_without_duplicates : 'a list -> 'a list -> 'a list

val permutations : 'a list -> 'a list list
val mapping : 'a list -> 'b list -> ('a -> 'b -> bool) -> ('a*'b) list list
val print_cc : Signature.s -> Pattern.cc -> unit
val print_side :
  Signature.s -> ((int*int) list * Pattern.cc * Pattern.id) list -> unit
val print_int_list : (int*int) list -> unit
