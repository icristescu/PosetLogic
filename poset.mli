type t = {
    kappa : bool;
    filename : string option;
    events : Event.t list;
    prec_1 : (int * int) list;
    inhibit : (int * int) list;
    mutable prec_star : (int*int list) list option;
  }

val empty_poset : t

val events : t -> Event.t list
val filename : t -> string option

val event_by_id : int -> t -> Event.t
val events_by_id_list : int list -> t -> Event.t list

val print_poset_name: t -> unit
val print_poset: t -> unit
val read_poset_from_file : string -> Model.t option -> t

(* predicates on poset*)
val intro : t -> t
val obs : t -> t
val past : Event.t -> t -> t
val check_prec_1 : Event.t -> Event.t -> t -> bool
val check_prec_star : Event.t -> Event.t -> t -> bool

val concrete :
  Model.t -> Pattern.Env.t -> Signature.s -> Event.t -> t -> Event.c

(* auxiliaries *)
val same_poset : t -> t -> bool
val prepare_for_morphism : t -> t
