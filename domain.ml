type domain =  Pos of Poset.t
             | Ev of Event.t


type t = {
    poset_list : Poset.t list;
    event_list : Event.t list;
  }

let get_events t = t.event_list

let get_posets t = t.poset_list

let get_poset_from_filename s dlist =
  List.find
    (fun d ->
      match d with Ev e -> false
                 | Pos p ->
                    (match (Poset.filename p) with Some n -> (s=n)
                                                 | None -> false))
    dlist

let print_posets t =
  List.iteri
    (fun i p -> Format.printf "@.poset nb %d" i; Poset.print_poset p)
    t.poset_list

let print_domain = function
  | Pos p -> (match p.Poset.filename with
              | Some f -> Format.printf"%s " f
              | None -> Poset.print_poset p)
  | Ev e -> Event.print_event e

let print_domain_list l =
  List.iter (fun d -> print_domain d) l;
  Format.printf"\n"

let add_posets p current_posets =
  { poset_list = p::current_posets.poset_list;
    event_list = (Poset.events p)@current_posets.event_list; }

let set_posets file_list env =
  let posets : t = { poset_list = []; event_list = []; } in
  let posets_file =
    List.fold_left
      (fun t file ->
        let s = Poset.read_poset_from_file file env in
        add_posets s t) posets file_list in
  let () = if (!Param.debug_mode)
           then (Format.printf "\n********* the posets are:";
                 print_posets posets_file) in
  posets_file
