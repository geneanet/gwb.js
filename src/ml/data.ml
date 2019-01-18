open Geneweb
open Jingoo
open Jg_types

let mk_opt fn = function None -> Tnull | Some x -> fn x

let date_compare_aux =
  let compare field d1 d2 =
    Jg_runtime.jg_compare
      (Jg_runtime.jg_obj_lookup d1 field)
      (Jg_runtime.jg_obj_lookup d2 field)
  in
  fun d1 d2 ->
    match unbox_int @@ compare "year" d1 d2 with
    | 0 -> begin match unbox_int @@ compare "month" d1 d2 with
        | 0 -> begin match unbox_int @@ compare "day" d1 d2 with
            | 0 -> begin match Jg_runtime.jg_obj_lookup d1 "prec", Jg_runtime.jg_obj_lookup d2 "prec" with
                | p1, p2 when p1 = p2 -> Tint 0
                | Tstr "before", _ -> Tint (-1)
                | Tstr "after", _ -> Tint 1
                | _ -> Tint 0
              end
            | c -> Tint c
          end
        | c -> Tint c
      end
    | c -> Tint c

let rec mk_family (conf : Config.config) base fcd =
  let module E = Ezgw.Family in
  let get wrap fn = try wrap (fn fcd) with Not_found -> Tnull in
  let get_str = get box_string in
  let get_bool = get box_bool in
  let f = E.father fcd in
  let m = E.mother fcd in
  let divorce_date = mk_opt (mk_date conf) (E.divorce_date fcd) in
  let father = Tlazy (lazy (get_n_mk_person conf base f) ) in
  let mother = Tlazy (lazy (get_n_mk_person conf base m) ) in
  let spouse =
    let (_, _, (ifath, imoth, ispouse), _) = fcd in
    if ifath = ispouse then father
    else if imoth = ispouse then mother
    else Tnull
  in
  let children =
    Tlazy (lazy (Tlist (get_n_mk_persons conf base @@ Array.to_list (E.children fcd)) ) )
  in
  let marriage_date = mk_opt (mk_date conf) (E.marriage_date fcd) in
  let marriage_place = get_str (E.marriage_place base) in
  let marriage_note = get_str (E.marriage_note conf base) in
  let marriage_source = get_str (E.marriage_source conf base) in
  let are_divorced = get_bool E.are_divorced in
  let are_separated = get_bool E.are_separated in
  let are_married = get_bool E.are_married in
  let are_engaged = get_bool E.are_engaged in
  let are_not_married = get_bool E.are_not_married in
  let ifam = Tstr (Gwdb.string_of_ifam @@ E.ifam fcd) in
  let witnesses =
    Tlazy (lazy (get box_list @@
                 fun fcd -> get_n_mk_persons conf base @@ Array.to_list @@ E.witnesses fcd))
  in
  let origin_file = Tlazy (lazy (get_str (E.origin_file conf base))) in
  Tpat (function
      | "are_divorced" -> are_divorced
      | "are_married" -> are_married
      | "are_engaged" -> are_engaged
      | "are_not_married" -> are_not_married
      | "are_separated" -> are_separated
      | "divorce_date" -> divorce_date
      | "children" -> children
      | "father" -> father
      | "ifam" -> ifam
      | "marriage_date" -> marriage_date
      | "marriage_place" -> marriage_place
      | "marriage_note" -> marriage_note
      | "marriage_source" -> marriage_source
      | "mother" -> mother
      | "origin_file" -> origin_file
      | "spouse" -> spouse
      | "witnesses" -> witnesses
      | _ -> raise Not_found
    )

and get_n_mk_family conf base ?(origin = Gwdb.dummy_iper) (ifam : Gwdb.ifam) cpl =
  let ifath = Gwdb.get_father cpl in
  let imoth = Gwdb.get_mother cpl in
  let cpl =
    ifath, imoth, (if ifath = origin then imoth
                   else if imoth = origin then ifath
                   else origin)
  in
  let m_auth = true in
  mk_family conf base (ifam, Gwdb.foi base ifam, cpl, m_auth)

and date_compare = func_arg2_no_kw date_compare_aux

and date_eq = func_arg2_no_kw (fun d1 d2 -> Tbool (date_compare_aux d1 d2 = Tint 0))

and mk_date conf d =
  let opt =
    match d with
    | Def.Dtext _ -> fun _ -> Tnull
    | Dgreg (d, c) -> fun fn -> fn d c
  in
  let year = opt (fun d _ -> Tint d.Def.year) in
  let month = opt (fun d _ -> Tint d.Def.month) in
  let day = opt (fun d _ -> Tint d.Def.day) in
  let prec = opt (fun d _ -> Tstr (match d.Def.prec with
      | Def.Sure -> "sure"
      | About -> "about"
      | Maybe -> "maybe"
      | Before -> "before"
      | After -> "after"
      | OrYear _ -> "oryear"
      | YearInt _ -> "yearint")
    )
  in
  let d2 = opt (fun d c -> match d.Def.prec with
      | OrYear d2 | YearInt d2 ->
        mk_date conf (Def.Dgreg ( { Def.day = d2.Def.day2
                                  ; month = d2.Def.month2
                                  ; year = d2.Def.year2
                                  ; prec = Def.Sure ; delta = 0 }
                                , c) )
      | _ -> Tnull )
  in
  let calendar =
    opt (fun _ -> function
        | Dgregorian -> Tstr "Dgregorian"
        | Djulian -> Tstr "Djulian"
        | Dfrench -> Tstr "Dfrench"
        | Dhebrew -> Tstr "Dhebrew")
  in
  Tpat (function
      | "calendar" -> calendar
      | "d2" -> d2
      | "day" -> day
      | "month" -> month
      | "prec" -> prec
      | "year" -> year
      | "__compare__" -> date_compare
      | "__eq__" -> date_eq
      | _ -> raise Not_found
    )

and to_dmy d =
  let int s = unbox_int (Jg_runtime.jg_obj_lookup d s) in
  { Def.day = int "day" ; month = int "month" ; year = int "year"
  ; prec = of_prec d
  ; delta = 0 }

and to_dmy2 d =
  let int s = unbox_int (Jg_runtime.jg_obj_lookup d s) in
  { Def.day2 = int "day" ; month2 = int "month" ; year2 = int "year"
  ; delta2 = 0 }

and to_prec = function
  | Def.Sure -> "sure"
  | About -> "about"
  | Maybe -> "maybe"
  | Before -> "before"
  | After -> "after"
  | OrYear _ -> "oryear"
  | YearInt _ -> "yearint"

and of_prec d = match Jg_runtime.jg_obj_lookup d "prec" with
  | Tstr "sure" -> Def.Sure
  | Tstr "about" -> About
  | Tstr "maybe" -> Maybe
  | Tstr "before" -> Before
  | Tstr "after" -> After
  | Tstr "oryear" -> OrYear (to_dmy2 d)
  | Tstr "yearint" -> YearInt (to_dmy2 d)
  | _ -> assert false

and to_gregorian_aux calendar d =
  let d = to_dmy d in
  match calendar with
  | "Dgregorian" -> d
  | "Djulian" -> Calendar.gregorian_of_julian d
  | "Dfrench" -> Calendar.gregorian_of_french d
  | "Dhebrew" -> Calendar.gregorian_of_hebrew d
  | x -> print_endline @@ Printf.sprintf "%s: %s" __LOC__ x ; assert false

and module_date conf =
  let now =
    Tvolatile (fun () ->
        let open Js_of_ocaml in
        let now = new%js Js.date_now in
        let day = Tint now##getDate in
        let month = Tint (now##getMonth + 1) in
        let year = Tint now##getFullYear in
        Tpat (function
            | "day" -> day
            | "month" -> month
            | "year" -> year
            | "prec" -> Tstr "sure"
            | _ -> raise Not_found)
      )
  in
  let string_of_ondate =
    func_arg1_no_kw @@ fun d ->
    Tlazy (lazy (Tstr (Date.string_of_ondate conf @@ Def.Dgreg (to_dmy d, Def.Dgregorian) ) ) )
  in
  let code_french_year =
    func_arg1_no_kw (fun i -> box_string @@ Date.code_french_year conf (unbox_int i))
  in
  let string_of_age =
    func_arg1_no_kw (fun d -> box_string @@ Date.string_of_age conf (to_dmy d) )
  in
  Tpat (function
      | "calendar" -> func_arg2_no_kw (fun dst d ->
          (* let src = unbox_string @@ Jg_runtime.jg_obj_lookup d "calendar" in *)
          let convert fn = mk_dmy @@ fn @@ to_dmy (* @@ to_gregorian_aux src *) d in
          match unbox_string @@ dst with
          | "Dgregorian" -> convert (fun x -> x)
          | "Djulian" -> convert Calendar.julian_of_gregorian
          | "Dfrench" -> convert Calendar.french_of_gregorian
          | "Dhebrew" -> convert Calendar.hebrew_of_gregorian
          | s -> failwith @@ "Unknown calendar: " ^ s
        )
      | "compare" -> date_compare
      | "code_french_year" -> code_french_year
      | "eq" -> date_eq
      | "now" -> now
      | "string_of_age" -> string_of_age
      | "string_of_ondate" -> string_of_ondate
      | "sub" -> func_arg2_no_kw (fun d1 d2 -> mk_dmy @@ CheckItem.time_elapsed (to_dmy d2) (to_dmy d1))
      | _ -> raise Not_found
    )

and get_n_mk_person conf base (i : Gwdb.iper) =
  unsafe_mk_person conf base (Gwdb.poi base i)

and get_n_mk_persons conf base (i : Gwdb.iper list) =
  Gwdb.poi_batch base i
  |> List.map (unsafe_mk_person conf base)

and mk_relation conf base r =
  let module E = Ezgw.Relation in
  let get wrap fn = try wrap (fn r) with Not_found -> Tnull in
  let has_relation_her = get box_bool E.has_relation_her in
  let has_relation_him = get box_bool E.has_relation_him in
  let related = get (get_n_mk_person conf base) @@ fun r -> Ezgw.Related.iper @@ E.related r in
  let related_type = get box_string (E.related_type conf) in
  let relation_type = get box_string (E.relation_type conf) in
  let relation_her = get (get_n_mk_person conf base) @@ fun r -> Ezgw.Related.iper @@ E.relation_her r in
  let relation_him = get (get_n_mk_person conf base) @@ fun r -> Ezgw.Related.iper @@ E.relation_him r in
  Tpat (function
      | "has_relation_her" -> has_relation_her
      | "has_relation_him" -> has_relation_him
      | "related" -> related
      | "related_type" -> related_type
      | "relation_type" -> relation_type
      | "relation_her" -> relation_her
      | "relation_him" -> relation_him
      | _ -> raise Not_found
    )

and mk_related conf base r =
  let module E = Ezgw.Related in
  let iper_ = E.iper r in
  let iper = Tstr (Gwdb.string_of_iper iper_) in
  let person = Tlazy (lazy (get_n_mk_person conf base iper_)) in
  let type_ = Tstr (E.type_ conf r) in
  Tpat (function
      | "type" -> type_
      | "iper" -> iper
      | "person" -> person
      | _ -> raise Not_found
    )

and mk_event conf base d =
  let module E = Ezgw.Event in
  let date = match E.date d with Some d -> mk_date conf d | None -> Tnull in
  let name = Tstr (E.name conf base d)  in
  let spouse =
    Tlazy (lazy (try unsafe_mk_person conf base @@ E.spouse base d
                 with Not_found -> Tnull))
  in
  let kind = Tstr (E.kind base d) in
  let witnesses =
    Tlazy (lazy (Tarray (Array.map (fun (i, k) ->
        let p = get_n_mk_person conf base i in
        let unbox p = unbox_pat (Lazy.force (unbox_lazy p)) in
        let sex = Ezgw.sex_of_index @@ unbox_int @@ unbox p @@ "sex" in
        let k = Util.string_of_witness_kind conf sex k in
        Tpat (function "kind" -> Tstr k
                     | s -> (unbox p) s) )
        (E.witnesses d) ) ) )
  in
  let place = Tstr (E.place conf base d) in
  let src = Tstr (E.src base d) in
  let note = Tstr (E.note base d) in
  Tpat (function "date" -> date
               | "kind" -> kind
               | "name" -> name
               | "note" -> note
               | "place" -> place
               | "spouse" -> spouse
               | "src" -> src
               | "witnesses" -> witnesses
               | _ -> raise Not_found)

and mk_title conf base t =
  print_endline __LOC__ ;
  let open Adef in
  let ident = Tstr (Gwdb.sou base t.Def.t_ident) in
  let name = match t.t_name with
    | Tmain -> Tstr ""
    | Tname s -> Tstr (Gwdb.sou base s)
    | Tnone -> Tnull
  in
  let place = Tstr (Gwdb.sou base t.t_place) in
  let date_start = mk_opt (mk_date conf) (Adef.od_of_cdate t.t_date_start) in
  let date_end = mk_opt (mk_date conf) (Adef.od_of_cdate t.t_date_start) in
  let nth = Tint t.t_nth in
  print_endline __LOC__ ;
  Tpat (function
      | "ident" -> ident
      | "name" -> name
      | "place" -> place
      | "date_start" -> date_start
      | "date_end" -> date_end
      | "nth" -> nth
      | _ -> raise Not_found)

and unsafe_mk_person conf base (p : Gwdb.person) =
  let get wrap fn = try wrap (fn p) with Not_found -> Tnull in
  let get_str = get box_string in
  let get_int = get box_int in
  let get_float = get box_float in
  let module E = Ezgw.Person in
  let parents =
    lazy (match E.parents p with
        | Some ifam -> Some (Gwdb.foi base ifam)
        | None -> None)
  in
  let mk_parent fn =
    Tlazy (lazy (match Lazy.force parents with
        | Some f -> get_n_mk_person conf base (fn f)
        | None -> Tnull (* fixme *) ) )
  in
  let iper' = Gwdb.get_key_index p in
  let access = get_str (E.access conf base) in
  let age = get mk_dmy (E.age conf) in
  let baptism_date = mk_opt (mk_date conf) (E.baptism_date p) in
  let baptism_place = get_str (E.baptism_place conf base) in
  let birth_date = mk_opt (mk_date conf) (E.birth_date p) in
  let birth_place = get_str (E.birth_place conf base) in
  let burial = get (mk_burial conf) E.burial in
  let burial_place = get_str (E.burial_place conf base) in
  let children =
    box_lazy @@ lazy (box_list @@ get_n_mk_persons conf base (E.children base p))
  in
  let consanguinity = get_float (E.consanguinity) in
  let cremation_place = get_str (E.cremation_place conf base) in
  let date = get_str (Date.short_dates_text conf base) in
  let dates = get_str (E.dates conf base) in
  let death = get (mk_death conf) E.death in
  let death_age = get_str (E.death_age conf) in
  let death_place = get_str (E.death_place conf base) in
  let died = get_str (E.died conf) in
  let digest = Tlazy (lazy (get_str (E.digest base) ) ) in
  let events = Tlazy (lazy (Tlist (E.events conf base p |> List.map (mk_event conf base)))) in
  let lazy_families = lazy (Array.map (fun ifam -> ifam, Gwdb.foi base ifam) @@ Gwdb.get_family p) in
  let families =
    Tlazy (lazy (Tarray (Array.map (fun (ifam, cpl) -> get_n_mk_family conf base ~origin:iper' ifam cpl) @@
                         Lazy.force lazy_families) ) )
  in
  let spouses =
    Tlazy (lazy (Tarray (Array.map (fun (_, c) ->
        let f = Gwdb.get_father c in
        get_n_mk_person conf base (if f = iper' then Gwdb.get_mother c else f) )
        (Lazy.force lazy_families) ) ) )
  in
  let father = mk_parent Gwdb.get_father in
  let first_name = get_str (E.first_name base) in
  let first_name_aliases =
    box_list @@
    List.map box_string (E.first_name_aliases base p)
  in
  let first_name_key = get_str (E.first_name_key base) in
  let first_name_key_val = get_str (E.first_name_key_val base) in
  let image_url = get_str (fun p -> E.image_url conf base p) in
  let iper = Tstr (Gwdb.string_of_iper iper') in
  let linked_page =
    box_lazy @@
    lazy (let fn = E.linked_page conf base p in Tpat (fun s -> Tstr (fn s) ) )
  in
  let max_ancestor_level = Tlazy (lazy (get_int (E.max_ancestor_level conf base) ) ) in
  let mother = mk_parent Gwdb.get_mother in
  let nobility_titles =
    box_list @@ List.map (mk_title conf base) @@ E.nobility_titles conf base p
  in
  let occ = get_int E.occ in
  let occupation = get_str (E.occupation conf base) in
  let public_name = get_str (E.public_name base) in
  let qualifier = get_str (E.qualifier base) in
  let qualifiers =
    Tlist (List.map box_string @@ E.qualifiers base p)
  in
  let related =
    box_lazy @@
    lazy (box_list @@
          List.map (fun (a, b) -> mk_relation conf base (b, Some a)) @@ (* FIXME? *)
          E.related conf base p)
  in
  let relations =
    box_lazy @@
    lazy (box_list @@
          List.map (fun x -> mk_relation conf base (x, None)) @@ (* FIXME *)
          E.relations p)
  in
  let sex = get_int E.sex in
  let siblings = Tlazy (lazy (Tlist (get_n_mk_persons conf base @@ E.siblings base p))) in
  let half_siblings = Tlazy (lazy (Tlist (let parents, siblings = List.split @@ E.half_siblings base p in
                                          List.map2 (fun parent siblings -> Tset [ parent ; Tlist siblings ] )
                                            (get_n_mk_persons conf base parents)
                                            (List.map (get_n_mk_persons conf base) siblings)))) in
  let source_baptism = get_str @@ E.source_baptism base in
  let source_birth = get_str @@ E.source_birth base in
  let source_burial = get_str @@ E.source_burial base in
  let source_death = get_str @@ E.source_death base in
  let source_fsource =
                            box_lazy @@ lazy (box_array @@
                                              Array.map box_string @@
                                              E.source_fsource conf base p)
  in
  let source_marriage =
    box_lazy @@ lazy (box_array @@
                      Array.map box_string @@
                      E.source_marriage conf base p)
  in
  let source_psources = get_str @@ E.source_psources base in
  let static_max_ancestor_level =
    box_lazy @@
    lazy (get_int ( (E.static_max_ancestor_level conf base) ) )
  in
  let str__ =
    box_lazy @@
    lazy (get_str (Util.person_text conf base)) (* FIXME *)
  in
  let surname_key_val = get_str (E.surname_key_val base) in
  let surname = get_str (E.surname base) in
  let surname_aliases = Tlist (List.map box_string (E.surname_aliases base p) ) in
  let surname_key = get_str (E.surname_key base) in
  let title = get_str (E.title conf base) in
  Tlazy (lazy (Tpat
                 (function
                   | "access" -> access
                   | "age" -> age
                   | "baptism_date" -> baptism_date
                   | "baptism_place" -> baptism_place
                   | "birth_date" -> birth_date
                   | "birth_place" -> birth_place
                   | "burial" -> burial
                   | "burial_place" -> burial_place
                   | "children" -> children
                   | "cremation_place" -> cremation_place
                   | "consanguinity" -> consanguinity
                   | "date" -> date
                   | "dates" -> dates
                   | "death" -> death
                   | "death_age" -> death_age
                   | "death_place" -> death_place
                   | "died" -> died
                   | "digest" -> digest
                   | "events" -> events
                   | "families" -> families
                   | "father" -> father
                   | "first_name" -> first_name
                   | "first_name_aliases" -> first_name_aliases
                   | "first_name_key" -> first_name_key
                   | "first_name_key_val" -> first_name_key_val
                   | "half_siblings" -> half_siblings
                   | "image_url" -> image_url
                   | "iper" -> iper
                   | "linked_page" -> linked_page
                   | "max_ancestor_level" -> max_ancestor_level
                   | "mother" -> mother
                   | "occ" -> occ
                   | "occupation" -> occupation
                   | "public_name" -> public_name
                   | "qualifier" -> qualifier
                   | "qualifiers" -> qualifiers
                   | "relations" -> relations
                   | "related" -> related
                   | "sex" -> sex
                   | "siblings" -> siblings
                   | "source_baptism" -> source_baptism
                   | "source_birth" -> source_birth
                   | "source_burial" -> source_burial
                   | "source_death" -> source_death
                   | "source_fsource" -> source_fsource
                   | "source_marriage" -> source_marriage
                   | "source_psources" -> source_psources
                   | "spouses" -> spouses
                   | "static_max_ancestor_level" -> static_max_ancestor_level
                   | "surname" -> surname
                   | "surname_aliases" -> surname_aliases
                   | "surname_key" -> surname_key
                   | "surname_key_val" -> surname_key_val
                   | "title" -> title
                   | "titles" -> nobility_titles
                   | "__str__" -> str__
                   | _ -> raise Not_found
                 )
              )
        )

(* FIXME *)
and mk_source _s =
  let source_type = Tstr "" in
  let str__ = Tfun (fun ?kwargs:_ _ -> Tstr "") in
  Tpat (function
      | "source_type" -> source_type
      | "__str__" -> str__
      | _ -> raise Not_found
    )

and mk_time (hh, mm, ss) =
  Tpat (function
      | "__str__" -> Tstr (Printf.sprintf "%02d:%02d:%02d" hh mm ss)
      | "hours" -> Tstr (Printf.sprintf "%02d" hh)
      | "minutes" -> Tstr (Printf.sprintf "%02d" mm)
      | "seconds" -> Tstr (Printf.sprintf "%02d" ss)
      | _ -> raise Not_found)

and mk_dmy { Def.day ; month ; year ; delta ; prec } =
  let day = Tint day in
  let month = Tint month in
  let year = Tint year in
  let delta = Tint delta in
  let prec = Tstr (to_prec prec) in
  Tpat (function
      | "day" -> day
      | "month" -> month
      | "year" -> year
      | "delta" -> delta
      | "prec" -> prec
      | _ -> raise Not_found
    )

and mk_gen_title conf base t =
  Tpat (function "t_ident" -> Tstr (Gwdb.sou base t.Def.t_ident)
               | "t_place" -> Tstr (Gwdb.sou base t.t_place)
               | "t_date_start" ->
                 begin match Adef.od_of_cdate t.t_date_start with
                   | Some d -> mk_date conf d
                   | None -> Tnull
                 end
               | "t_date_end" ->
                 begin match Adef.od_of_cdate t.t_date_end with
                   | Some d -> mk_date conf d
                   | None -> Tnull
                 end
               | _ -> raise Not_found)

and mk_death_reason = function
  | Def.Killed -> Tstr "Killed"
  | Murdered -> Tstr "Murdered"
  | Executed -> Tstr "Executed"
  | Disappeared -> Tstr "Disappeared"
  | Unspecified -> Tstr "Unspecified"

and mk_death conf =
  let wrap s = Tpat (function "death_reason" -> Tstr s | _ -> raise Not_found) in
  function
  | Def.NotDead -> Tnull
  | Death (r, cd) ->
    let death_reason = mk_death_reason r in
    let date = mk_date conf (Adef.date_of_cdate cd) in
    Tpat (function "death_reason" -> death_reason
                 | "date" -> date
                 | _ -> raise Not_found)
  | DeadYoung -> wrap "DeadYoung"
  | DeadDontKnowWhen -> wrap "DeadDontKnowWhen"
  | DontKnowIfDead -> wrap "DontKnowIfDead"
  | OfCourseDead -> wrap "OfCourseDead"

and mk_burial conf = function
  | Def.UnknownBurial -> Tnull
  | Buried d ->
    let type_ = Tstr "Buried" in
    let date = match Adef.od_of_cdate d with
      | Some d -> mk_date conf d
      | None -> Tnull
    in
    Tpat (function "type" -> type_
                 | "date" -> date
                 | _ -> raise Not_found)
  | Cremated d ->
    let type_ = Tstr "Cremated" in
    let date = match Adef.od_of_cdate d with
      | Some d -> mk_date conf d
      | None -> Tnull
    in
    Tpat (function "type" -> type_
                 | "date" -> date
                 | _ -> raise Not_found)

let decode_varenv =
  func_arg1_no_kw @@ fun str ->
  try Tstr (Wserver.decode @@ unbox_string str)
  with _ -> failwith_type_error_1 "decode_varenv" str

let code_varenv =
  func_arg1_no_kw @@ fun str ->
  try Tstr (Wserver.encode @@ unbox_string str)
  with _ -> failwith_type_error_1 "decode_varenv" str

let mk_evar conf =
  Tpat (fun v -> match Util.p_getenv (conf.Config.env @ conf.henv) v with
      | Some vv -> Tstr (Util.escape_html vv)
      | None -> Tnull)

let mk_base base =
  Tpat (function
      | "nb_of_persons" -> Tint (Gwdb.nb_of_persons base)
      | "nb_of_families" -> Tint (Gwdb.nb_of_families base)
      | _ -> raise Not_found
    )

(* Copy of Geneweb.Util.nth_field *)
let nth_field_abs w n =
  let rec start i n =
    if n = 0 then i
    else if i < String.length w then
      match w.[i] with
        '<' -> start (i + 2) n
      | '/' -> start (i + 1) (n - 1)
      | _ -> start (i + 1) n
    else i
  in
  let rec stop i =
    if i < String.length w then
      match w.[i] with
        '<' -> stop (i + 2)
      | '/' -> i
      | _ -> stop (i + 1)
    else i
  in
  let i1 = start 0 n in let i2 = stop i1 in i1, i2
let nth_field w n =
  let (i1, i2) = nth_field_abs w n in
  let (i1, i2) = if i2 = i1 then nth_field_abs w 0 else i1, i2 in
  String.sub w i1 (i2 - i1)

let i18n_ht : (string, string) Hashtbl.t = Marshal.from_string Templates.i18n 0

let trans =
  func_arg1_no_kw @@ function
  | Tstr s -> Tstr (try Hashtbl.find i18n_ht s with _ -> failwith s)
  | x -> failwith_type_error_1 "trans" x

let trans_nth =
  func_no_kw
    (function
      | [ Tint n ; Tstr s ] ->
        Tstr (nth_field (try Hashtbl.find i18n_ht s with _ -> failwith s) n)
      | x -> failwith_type_error "trans" (List.map (fun x -> "", x) x)
    )
    2

let default_env conf base =
  ("trans", trans)
  :: ("trans_nth", trans_nth)
  :: ("i18n", I18n.f)
  :: ("DATE", module_date conf)
  :: ("decode_varenv", decode_varenv)
  :: ("code_varenv", code_varenv)
  :: ("base", mk_base base)
  :: []

let sandbox (conf : Config.config) base =
  let die =
    let rec printer = function
      | Tint x -> Printf.sprintf "Tint %d" x
      | Tfloat x -> Printf.sprintf "Tfloat %f" x
      | Tstr x -> Printf.sprintf "Tstr %s" (String.escaped x)
      | Tbool x -> Printf.sprintf "Tbool %b" x
      | Tobj _ -> Printf.sprintf "<Tobj>"
      | Thash _ -> Printf.sprintf "<Thash>"
      | Tlist x -> Printf.sprintf "Tlist [ %s ]" (String.concat ";" @@ List.map printer x)
      | Tpat _ -> Printf.sprintf "<Tpat>"
      | Tset _ -> Printf.sprintf "<Tset>"
      | Tfun _ -> Printf.sprintf "<Tfun>"
      | Tnull -> Printf.sprintf "Tnull"
      | Tarray x -> Printf.sprintf "Tarray [| %s |]" (String.concat ";" @@ List.map printer @@ Array.to_list x)
      | Tlazy _ -> Printf.sprintf "Tlazy"
      | Tvolatile _ -> Printf.sprintf "Tvolatile"
    in
    func_arg1_no_kw @@ fun x -> Tstr (printer x)
  in
  let get_person = func_arg1_no_kw @@ function
    | Tstr i -> get_n_mk_person conf base (Gwdb.iper_of_string i)
    | x -> failwith_type_error_1 "GET_PERSON" x
  in
  let get_family = func_arg1_no_kw @@ function
    | Tstr i ->
      let ifam = Gwdb.ifam_of_string i in
      let cpl = Gwdb.foi base ifam in
      get_n_mk_family conf base ifam cpl
    | x -> failwith_type_error_1 "GET_FAMILY" x
  in
  let () = Random.self_init () in
  ("DIE", die)
  :: ("GET_PERSON", get_person)
  :: ("GET_FAMILY", get_family)
  :: ("RANDOM_IPER", Tvolatile (fun () -> Tint (Random.int (Gwdb.nb_of_persons base))))
  :: ("RANDOM_IFAM", Tvolatile (fun () -> Tint (Random.int (Gwdb.nb_of_families base))))
  :: ("DATE", module_date conf)
  :: ("trans", trans)
  :: default_env conf base
