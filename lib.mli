val fst : 'a* 'b -> 'a
val snd : 'a* 'b -> 'b
val thd : 'a* 'b* 'c -> 'c

val remove_duplicates : 'a list -> ('a -> 'a -> bool) -> 'a list
val permutations : 'a list -> 'a list list
