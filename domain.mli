type domain =  Pos of Poset.t
             | Ev of Event.t

type t = {
    poset_list : Poset.t list;
    event_list : Event.t list;
  }

val get_events : t -> Event.t list

val get_posets : t -> Poset.t list

val print_posets: t -> unit

val print_domain_list : domain list -> unit

val set_posets : string list -> t
