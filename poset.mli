open Yojson

exception MalformedDatatype of string
exception ErrorParsing of string

type quark =
  | Tested of (int*int*int)
  | Modified of (int*int*int)
  | TestedMod of (int*int*int)

type event = {
    event_id : int;
    event_label : string;
    quarks : quark list;
  }

type poset = {
    events : event list;
    prec_1 : (int * int) list;
    inhibit : (int * int) list;
  }

type domain =  Pos of poset
             | Ev of event

type t = {
    poset_list : poset list;
    events_list : event list;
  }

val empty_poset : poset

val set_posets : string list -> t

val get_events : t -> event list

val get_posets : t -> poset list

val get_events_from_poset : poset -> event list

val get_event_by_id : int -> poset -> event

val get_event_id : event -> int

val print_posets: t -> unit

val print_poset: poset -> unit

val print_event: event -> unit

val intro : poset -> poset

val remove_obs : poset -> poset
