type t = {
    kappa : bool;
    filename : string option;
    events : Event.t list;
    prec_1 : (int * int) list;
    inhibit : (int * int) list;
    mutable prec_star : int list array option;
  }

val empty_poset : t

val get_events_from_poset : t -> Event.t list

val get_event_by_id : int -> t -> Event.t

val get_events_by_id_list : int list -> t -> Event.t list

val print_poset: t -> unit

val intro : t -> t
val obs : t -> int
val remove_event : t -> int -> t

val read_poset_from_file : string -> Model.t option -> t

val check_prec_1 : Event.t -> Event.t -> t -> bool

val check_prec_star : Event.t -> Event.t -> t -> bool

val past : Event.t -> t -> t

val same_poset : t -> t -> bool
