open Js_of_ocaml
open Adef
open Def

type iper = string
type ifam = string
type istr = string

module Json = struct

  let parse s = Js._JSON##parse (Js.string s)
  let stringify x = Js.to_string (Js._JSON##stringify x)

  let member name js = Js.Unsafe.get js name

  let int i = Js.Unsafe.inject i
  let string s = Js.Unsafe.inject (Js.string s)
  let assoc o = Js.Unsafe.inject (Array.of_list o)
  let null = Js.Unsafe.inject Js.undefined
  let list l = Js.Unsafe.inject (Array.of_list l)
  let bool b = Js.Unsafe.inject (Js.bool b)

  let to_int (js : Js.Unsafe.any) =
    if Obj.magic js = Js.null || Obj.magic js = Js.undefined then 0
    else Obj.magic js

  let to_list_aux fn (js : Js.Unsafe.any) =
    if Obj.magic js = Js.null || Obj.magic js = Js.undefined then []
    else Array.to_list (Array.map fn @@ Js.to_array (Obj.magic js))

  let to_list = to_list_aux (fun x -> x)

  let to_string (js : Js.Unsafe.any) =
    if Obj.magic js = Js.null || Obj.magic js = Js.undefined then ""
    else Js.to_string (Obj.magic js)

  let to_opt_string (js : Js.Unsafe.any) =
    if Obj.magic js = Js.null || Obj.magic js = Js.undefined then None
    else Some (Js.to_string (Obj.magic js))

  let get_string ~__LOC__:x js name =
    print_endline @@ Printf.sprintf "%s:%s" x (stringify js) ;
    let x = to_string @@ member name js in
    print_endline @@ Printf.sprintf "%s:%s:%s" __LOC__ name x ;
    x

  let get_int ~__LOC__:x js name =
    print_endline x ;
    to_int (member name js)

  let get_list name fn js =
    to_list_aux fn (member name js)

  (** gwdb to json  *)

  let json_of_dmy dmy = assoc [
      ("day", int dmy.day);
      ("month", int dmy.month);
      ("year", int dmy.year);
    ]

  let dmy_of_json prec json =
    { day = get_int ~__LOC__ json "day"
    ; month = get_int ~__LOC__ json "month"
    ; year = get_int ~__LOC__ json "year"
    ; prec
    ; delta = 0
    }

  let json_of_dmy2 dmy = assoc [
      ("day", int dmy.day2);
      ("month", int dmy.month2);
      ("year", int dmy.year2);
    ]

  let dmy2_of_json json =
    { day2 = get_int ~__LOC__ json "day"
    ; month2 = get_int ~__LOC__ json "month"
    ; year2 = get_int ~__LOC__ json "year"
    ; delta2 = 0
    }

  let json_of_date_cal dt cal =
    let date1 = json_of_dmy dt in
    let prec = match dt.prec with
      | Sure -> "sure"
      | About -> "about"
      | Maybe -> "maybe"
      | Before -> "before"
      | After -> "after"
      | OrYear _ -> "or"
      | YearInt _ -> "between"
    in
    let date2 = match dt.prec with
      | OrYear dmy2 -> json_of_dmy2 dmy2
      | YearInt dmy2 -> json_of_dmy2 dmy2
      | _ -> null
    in
    assoc [
      ("prec", string prec);
      ("dmy1", date1);
      ("dmy2", date2);
      ("calendar", string cal);
    ]

  let json_of_date oc =
    match oc with
    | Dgreg (d, Dgregorian) -> json_of_date_cal d "gregorian"
    | Dgreg (d, Djulian) -> json_of_date_cal d "julian"
    | Dgreg (d, Dfrench) -> json_of_date_cal d "french"
    | Dgreg (d, Dhebrew) -> json_of_date_cal d "hebrew"
    | Dtext t -> string t

  let date_of_json (js : Js.Unsafe.any) =
    let prec = member "prec" js in
    if prec = null then Dtext (to_string js)
    else
      let prec = match to_string prec with
        | "sure" -> Sure
        | "about" -> About
        | "maybe" -> Maybe
        | "before" -> Before
        | "after" -> After
        | "or" -> OrYear (dmy2_of_json @@ member "dmy2" js)
        | "between" -> YearInt (dmy2_of_json @@ member "dmy2" js)
        | _ -> failwith @@ Printf.sprintf "%s: %s" __LOC__ (stringify js)
      in
      let d = dmy_of_json prec (member "dmy1" js) in
      match to_string @@ member "calendar" js with
      | "gregorian" -> Dgreg (d, Dgregorian)
      | "julian" -> Dgreg (d, Djulian)
      | "french" -> Dgreg (d, Dfrench)
      | "hebrew" -> Dgreg (d, Dhebrew)
      | x -> failwith @@ Printf.sprintf "%s: %s" __LOC__ (stringify x)

  let json_of_cdate cd = match Adef.od_of_cdate cd with
    | None -> null
    | Some date -> json_of_date date

  let cdate_of_json js =
    print_endline __LOC__ ;
    let x =
      if js = null then cdate_of_od None
      else cdate_of_od @@ Some (date_of_json js)
    in
    print_endline __LOC__ ;
    x

  let json_of_pevent_name = function
    | Epers_Birth -> string "birth"
    | Epers_Baptism -> string "baptism"
    | Epers_Death -> string "death"
    | Epers_Burial -> string "burial"
    | Epers_Cremation -> string "cremation"
    | Epers_Accomplishment -> string "accomplishment"
    | Epers_Acquisition -> string "aquisition"
    | Epers_Adhesion -> string "adhesion"
    | Epers_BaptismLDS -> string "baptismlds"
    | Epers_BarMitzvah -> string "barmitzvah"
    | Epers_BatMitzvah -> string "batmitzvah"
    | Epers_Benediction -> string "benediction"
    | Epers_ChangeName -> string "changename"
    | Epers_Circumcision -> string "circumcision"
    | Epers_Confirmation -> string "confirmation"
    | Epers_ConfirmationLDS -> string "confirmationlds"
    | Epers_Decoration -> string "decoration"
    | Epers_DemobilisationMilitaire -> string "demobilisationmilitaire"
    | Epers_Diploma -> string "diploma"
    | Epers_Distinction -> string "distinction"
    | Epers_Dotation -> string "dotation"
    | Epers_DotationLDS -> string "dotationlds"
    | Epers_Education -> string "education"
    | Epers_Election -> string "election"
    | Epers_Emigration -> string "emigration"
    | Epers_Excommunication -> string "excommunication"
    | Epers_FamilyLinkLDS -> string "familylinklds"
    | Epers_FirstCommunion -> string "firstcommunion"
    | Epers_Funeral -> string "funeral"
    | Epers_Graduate -> string "graduate"
    | Epers_Hospitalisation -> string "hospitalisation"
    | Epers_Illness -> string "illness"
    | Epers_Immigration -> string "immigration"
    | Epers_ListePassenger -> string "listepassenger"
    | Epers_MilitaryDistinction -> string "militarydistinction"
    | Epers_MilitaryPromotion -> string "militarypromotion"
    | Epers_MilitaryService -> string "militaryservice"
    | Epers_MobilisationMilitaire -> string "mobilisationmilitaire"
    | Epers_Naturalisation -> string "naturalisation"
    | Epers_Occupation -> string "occupation"
    | Epers_Ordination -> string "ordination"
    | Epers_Property -> string "property"
    | Epers_Recensement -> string "recensement"
    | Epers_Residence -> string "residence"
    | Epers_Retired -> string "retired"
    | Epers_ScellentChildLDS -> string "scellentchildlds"
    | Epers_ScellentParentLDS -> string "scellentparentlds"
    | Epers_ScellentSpouseLDS -> string "scellentspouselds"
    | Epers_VenteBien -> string "ventebien"
    | Epers_Will -> string "will"
    | Epers_Name name -> string name

  let pevent_name_of_string = function
    | "birth" -> Epers_Birth
    | "baptism" -> Epers_Baptism
    | "death" -> Epers_Death
    | "burial" -> Epers_Burial
    | "cremation" -> Epers_Cremation
    | "accomplishment" -> Epers_Accomplishment
    | "aquisition" -> Epers_Acquisition
    | "adhesion" -> Epers_Adhesion
    | "baptismlds" -> Epers_BaptismLDS
    | "barmitzvah" -> Epers_BarMitzvah
    | "batmitzvah" -> Epers_BatMitzvah
    | "benediction" -> Epers_Benediction
    | "changename" -> Epers_ChangeName
    | "circumcision" -> Epers_Circumcision
    | "confirmation" -> Epers_Confirmation
    | "confirmationlds" -> Epers_ConfirmationLDS
    | "decoration" -> Epers_Decoration
    | "demobilisationmilitaire" -> Epers_DemobilisationMilitaire
    | "diploma" -> Epers_Diploma
    | "distinction" -> Epers_Distinction
    | "dotation" -> Epers_Dotation
    | "dotationlds" -> Epers_DotationLDS
    | "education" -> Epers_Education
    | "election" -> Epers_Election
    | "emigration" -> Epers_Emigration
    | "excommunication" -> Epers_Excommunication
    | "familylinklds" -> Epers_FamilyLinkLDS
    | "firstcommunion" -> Epers_FirstCommunion
    | "funeral" -> Epers_Funeral
    | "graduate" -> Epers_Graduate
    | "hospitalisation" -> Epers_Hospitalisation
    | "illness" -> Epers_Illness
    | "immigration" -> Epers_Immigration
    | "listepassenger" -> Epers_ListePassenger
    | "militarydistinction" -> Epers_MilitaryDistinction
    | "militarypromotion" -> Epers_MilitaryPromotion
    | "militaryservice" -> Epers_MilitaryService
    | "mobilisationmilitaire" -> Epers_MobilisationMilitaire
    | "naturalisation" -> Epers_Naturalisation
    | "occupation" -> Epers_Occupation
    | "ordination" -> Epers_Ordination
    | "property" -> Epers_Property
    | "recensement" -> Epers_Recensement
    | "residence" -> Epers_Residence
    | "retired" -> Epers_Retired
    | "scellentchildlds" -> Epers_ScellentChildLDS
    | "scellentparentlds" -> Epers_ScellentParentLDS
    | "scellentspouselds" -> Epers_ScellentSpouseLDS
    | "ventebien" -> Epers_VenteBien
    | "will" -> Epers_Will
    | s -> Epers_Name s

  let json_of_pevent_witness _w = assert false
  let pevent_witness_of_json _json = assert false

  let json_of_pevent pevent =
    assoc [ ("place", string pevent.epers_place)
          ; ("reason", string pevent.epers_reason)
          ; ("note", string pevent.epers_note)
          ; ("src", string pevent.epers_src)
          ; ("name", json_of_pevent_name pevent.epers_name)
          ; ("date", json_of_cdate pevent.epers_date)
          ; ("witnesses", null (* (Array.to_list @@ Array.map json_of_pevent_witness pevent.epers_witnesses) *) )
          ]
  (* FIXME: witnesses *)

  let pevent_of_json json =
    { epers_place = get_string ~__LOC__ json "place"
    ; epers_reason = get_string ~__LOC__ json "reason"
    ; epers_note = get_string ~__LOC__ json "note"
    ; epers_src = get_string ~__LOC__ json "src"
    ; epers_name = pevent_name_of_string (get_string ~__LOC__ json "name")
    ; epers_date = cdate_of_json (member "date" json)
    ; epers_witnesses = [||] (* Array.of_list (get_list json "witnesses" pevent_witness_of_json) *)
    }

  let json_of_title_name = function
    | Tmain -> string ""
    | Tname s -> string s
    | Tnone -> null

  let title_name_of_json js = match to_opt_string js with
    | Some "" ->Tmain
    | Some s -> Tname s
    | None -> Tnone

  let json_of_title gen_title =
    assoc [ ("name", json_of_title_name gen_title.t_name)
           ; ("date_start", json_of_cdate gen_title.t_date_start)
           ; ("date_end", json_of_cdate gen_title.t_date_end)
           ; ("nth", int gen_title.t_nth)
           ; ("ident", string gen_title.t_ident)
           ; ("place", string gen_title.t_place)
           ]

  let title_of_json json =
    { t_name = title_name_of_json (member "name" json)
    ; t_ident = get_string ~__LOC__ json "ident"
    ; t_place = get_string ~__LOC__ json "place"
    ; t_date_start = cdate_of_json (member "date_start" json)
    ; t_date_end = cdate_of_json (member "date_end" json)
    ; t_nth = get_int ~__LOC__ json "nth" }

  let json_of_relation_kind = function
    | Married -> string "married"
    | NotMarried -> string "not_married"
    | Engaged -> string  "engaged"
    | NoSexesCheckNotMarried -> string "no_sexes_check_not_married"
    | NoMention -> string "no_mention"
    | NoSexesCheckMarried -> string "no_sexes_check_married"

  let relation_kind_of_json js = match to_string js with
    | "married" -> Married
    | "not_married" -> NotMarried
    | "engaged" -> Engaged
    | "no_sexes_check_not_married" -> NoSexesCheckNotMarried
    | "no_mention" -> NoMention
    | "no_sexes_check_married" -> NoSexesCheckMarried
    | _ -> failwith __LOC__

  let json_of_fevent_name = function
    | Efam_Marriage -> string "marriage"
    | Efam_NoMarriage -> string "no_marriage"
    | Efam_NoMention -> string "no_mention"
    | Efam_Engage -> string "engaged"
    | Efam_Divorce -> string "divorce"
    | Efam_Separated -> string "separated"
    | Efam_Annulation -> string "annulation"
    | Efam_MarriageBann -> string "marriage_bann"
    | Efam_MarriageContract -> string "marriage_contract"
    | Efam_MarriageLicense -> string "marriage_license"
    | Efam_PACS -> string "pacs"
    | Efam_Residence -> string "residence"
    | Efam_Name s -> string s

  (* FIXME *)
  let fevent_name_of_string js = match to_string js with
    | "marriage" -> Efam_Marriage
    | "no_marriage" -> Efam_NoMarriage
    | "no_mention" -> Efam_NoMention
    | "engaged" -> Efam_Engage
    | "divorce" -> Efam_Divorce
    | "separated" -> Efam_Separated
    | "annulation" -> Efam_Annulation
    | "marriage_bann" -> Efam_MarriageBann
    | "marriage_contract" -> Efam_MarriageContract
    | "marriage_license" -> Efam_MarriageLicense
    | "pacs" -> Efam_PACS
    | "residence" -> Efam_Residence
    | "" -> failwith __LOC__
    | s -> Efam_Name s

  let json_of_fevent_witness_kind = function
    | Witness -> string "witness"
    | Witness_GodParent -> string "godparent"
  (* | Witness_Officer -> string "officer" *)


  let fevent_witness_kind_of_json js = match to_string js with
    | "witness" -> Witness
    | "godparent" -> Witness_GodParent
    | _ -> failwith __LOC__
  (* | `String "officer" -> Witness_Officer *)

  let json_of_fevent_witness (person , witness_kind) =
    assoc [ ("person", string person)
           ; ("type", json_of_fevent_witness_kind witness_kind)
           ]

  let fevent_witness_of_json json =
    ( (get_int ~__LOC__ json "person" |> fun i -> "pierfit:" ^ string_of_int i) (* FIXME !!! *)
    , fevent_witness_kind_of_json (member "type" json) )

  let json_of_fevent fevent =
    assoc [ ("place", string fevent.efam_place)
           ; ("reason", string fevent.efam_reason)
           ; ("note", string fevent.efam_note)
           ; ("src", string fevent.efam_src)
           ; ("name", json_of_fevent_name fevent.efam_name)
           ; ("date", json_of_cdate fevent.efam_date)
           ; ("witnesses", list (Array.to_list @@ Array.map json_of_fevent_witness fevent.efam_witnesses) )
           ]

  let fevent_of_json json =
    { efam_place = get_string ~__LOC__ json "place"
    ; efam_reason = get_string ~__LOC__ json "reason"
    ; efam_note = get_string ~__LOC__ json "note"
    ; efam_src = get_string ~__LOC__ json "src"
    ; efam_name = fevent_name_of_string (member "name" json)
    ; efam_date = cdate_of_json (member "date" json)
    ; efam_witnesses = Array.of_list (get_list "witnesses" fevent_witness_of_json json)
    }

  let json_of_divorce = function
    | NotDivorced -> null
    | Divorced date -> json_of_cdate date
    | Separated -> bool true

  let divorce_of_json js =
    if js = null then NotDivorced
    else if js = bool true then Separated
    else Divorced (cdate_of_json js)

  let json_of_relation_type = function
    | Adoption -> string "adoption"
    | Recognition -> string "recognition"
    | CandidateParent -> string "candidate_parent"
    | GodParent -> string "god_parent"
    | FosterParent -> string "foster_parent"

  let relation_type_of_json rt = match to_string rt with
    | "adoption" -> Adoption
    | "recognition" -> Recognition
    | "candidate_parent" -> CandidateParent
    | "god_parent" -> GodParent
    | "foster_parent" -> FosterParent
    | _ -> failwith __LOC__

  let json_of_rparent gen_relation =
    assoc [ ("type", json_of_relation_type gen_relation.r_type )
           ; ("source", string gen_relation.r_sources)
           ; ("father", match gen_relation.r_fath with Some i -> string i | _ -> null)
           ; ("mother", match gen_relation.r_moth with Some i -> string i | _ -> null)
           ]

  let rparent_of_json json =
    { r_type = relation_type_of_json (member "type" json)
    ; r_fath = (match to_string @@ member "father" json with "" -> None | i -> Some i)
    ; r_moth = (match to_string @@ member "mother" json with "" -> None | i -> Some i)
    ; r_sources = get_string ~__LOC__ json "source"
    }

end

open Json

let string_of_iper : iper -> string = fun x -> x
let string_of_ifam : ifam -> string = fun x -> x
let string_of_istr : istr -> string = fun x -> x

let iper_of_string x = x
let ifam_of_string x = x
let istr_of_string x = x

type revision = string

type person = { revision : revision ; iper : iper ; person : Js.Unsafe.any }
type family = { ifam : ifam ; family : Js.Unsafe.any }

type relation = (iper, istr) gen_relation
type title = istr gen_title
type pers_event = (iper, istr) gen_pers_event
type fam_event = (iper, istr) gen_fam_event

type base = { basename : string
            ; get : __LOC__:string -> url:string -> string
            ; put : __LOC__:string -> url:string -> data:string -> string
            }

let iper_of_int i =
  Printf.sprintf "pierfit:%d" i

let open_base name =
  let open Js_of_ocaml in
  let xhr ~__LOC__:loc m u data =
    let url = Printf.sprintf "http://localhost:8529/_db/Trees/geneweb/%s/%s" name u in
    print_endline @@ Printf.sprintf "%s: %s" loc url ;
    let xhr = XmlHttpRequest.create () in
    xhr##_open (Js.string m) (Js.string url) (Js._false) ;
    xhr##send (data) ;
    if xhr##.status <> 200 then failwith @@ Printf.sprintf "%s:%s" __LOC__ u ;
    Js.to_string xhr##.responseText
  in
  { basename = name
  ; get = begin fun ~__LOC__:l ~url -> xhr ~__LOC__:l "GET" url Js.null end
  ; put = begin fun ~__LOC__:l ~url ~data -> xhr ~__LOC__:l "PUT" url (Js.some @@ Js.string data) end
  }

let close_base _base = ()

let dummy_iper : iper = ""
let dummy_ifam : ifam = ""
let dummy_istr : istr = ""

let eq_istr = (=)
let is_empty_string = (=) ""
let is_quest_string = (=) "?"
let empty_person _ _ = { revision = "" ; iper = dummy_iper ; person = null }
let empty_family _ _ = { ifam = dummy_ifam ; family = null }

let get_access { person = p ; _ } =
  match to_int @@ member "access" p with
  | 2 -> Private
  | 1 -> Public
  | 0 -> IfTitles
  | _ -> failwith __LOC__

let get_aliases { person = p ; _ } =
  member "aliases" p
  |> to_list
  |> List.map to_string

let get_pevents { person = p ; _ } =
  get_list "pevents" pevent_of_json p

let get_event_aux names fn =
  let rec loop = function
    | [] -> None
    | e :: _ when List.mem e.epers_name names -> Some e
    | _ :: tl -> loop tl
  in
  ( (fun p -> fn @@ loop (get_pevents p) )
  , (fun p -> match loop (get_pevents p) with Some e -> e.epers_place | None -> "")
  , (fun p -> match loop (get_pevents p) with Some e -> e.epers_note | None -> "")
  , (fun p -> match loop (get_pevents p) with Some e -> e.epers_src | None -> "")
  )

let get_baptism, get_baptism_place, get_baptism_note, get_baptism_src =
  get_event_aux [ Epers_Baptism ] @@
  function Some e -> e.epers_date | None -> Adef.cdate_None

let get_birth, get_birth_place, get_birth_note, get_birth_src =
  get_event_aux [ Epers_Birth ] @@
  function Some e -> e.epers_date | None -> Adef.cdate_None

let get_burial, get_burial_place, get_burial_note, get_burial_src =
  get_event_aux [ Epers_Cremation ; Epers_Burial ] @@
  function
  | Some { epers_name = Epers_Cremation ; epers_date ; _ } -> Cremated epers_date
  | Some { epers_name = Epers_Burial ; epers_date ; _ } -> Buried epers_date
  | _ -> UnknownBurial

(* FIXME *)
let get_death, get_death_place, get_death_note, get_death_src =
  get_event_aux [ Epers_Death ] @@
  function None -> NotDead
         | Some e ->
           if Adef.od_of_cdate e.epers_date <> None
           then Death (Unspecified, e.epers_date)
           else DeadDontKnowWhen

let get_first_name { person = p ; _ } =
  to_string @@ member "firstname" p

let get_first_names_aliases { person = p ; _ } =
  get_list "first_names_aliases" to_string p

let get_image { person = p ; _ } =
  get_string ~__LOC__ p "image"

let get_key_index : person -> iper = fun { iper ; _ } -> iper
(* get_string ~__LOC__ p "index" *)

let get_notes { person = p ; _ } =
  get_string ~__LOC__ p "note"

let get_occ { person = p ; _ } =
  get_int ~__LOC__ p "occ"

let get_occupation { person = p ; _ } =
  get_string ~__LOC__ p "occupation"

let get_psources { person = p ; _ } =
  get_string ~__LOC__ p "psources"

let get_public_name { person = p ; _ } =
  get_string ~__LOC__ p "public_name"

let get_qualifiers { person = p ; _ } =
  get_list "qualifiers" to_string p

let get_related { person = p ; _ } =
  get_list "related" (* to_string *) to_int p
  |> List.map iper_of_int

let get_rparents { person = p ; _ } =
  get_list "rparents" rparent_of_json p

let get_parents { person = p ; _ } =
  match to_string @@ member "parents" p with
  | "" -> None (* FIXME *)
  | i -> Some i

let get_sex { person = p ; _ } = match get_int ~__LOC__ p "sex" with
  | 1 -> Def.Male
  | 2 -> Def.Female
  | _ -> Def.Neuter

let get_surname { person = p ; _ } =
  get_string ~__LOC__ p "lastname"

let get_surnames_aliases { person = p ; _ } =
  get_list "surnames_aliases" to_string p

let get_titles : person -> title list = fun _p -> []

let clear_families_array _ = ()
let clear_persons_array _ = ()
let clear_strings_array _ = ()
let clear_descends_array _ = ()
let clear_couples_array _ = ()
let clear_unions_array _ = ()
let clear_ascends_array _ = ()
let load_families_array _ = ()
let load_persons_array _ = ()
let load_strings_array _ = ()
let load_descends_array _ = ()
let load_couples_array _ = ()
let load_unions_array _ = ()
let load_ascends_array _ = ()

let sou _base istr = istr

let foi_cache : (ifam, family) Hashtbl.t = Hashtbl.create 1024

let read_family x =
  print_endline __LOC__ ;
  let x=
    { ifam = to_string (member "_key" x)
    ; family = member "family" x
    }
  in
  print_endline __LOC__ ;
  x

let foi { get ; _ } ifam =
  if ifam = dummy_ifam then raise Not_found ;
  try Hashtbl.find foi_cache ifam
  with Not_found ->
    let x =
      let url =  "families/" ^ (String.split_on_char ':' ifam |> String.concat "%3A") in
      get ~__LOC__ ~url
      |> parse
      |> read_family
    in
    Hashtbl.replace foi_cache ifam x ;
    x

let foi_batch = fun ({ get ; _ } as base) ifams ->
  match ifams with
  | [] -> []
  | [ ifam ] -> [ foi base ifam ]
  | _ ->
    try List.map (Hashtbl.find foi_cache) ifams
    with Not_found ->
      let rec split current acc list = match (acc, list) with
        | (acc, []) -> acc
        | ((hdacc :: tlacc), (hd :: tl)) when current < 500 ->
          split (current + 1) ((hd :: hdacc) :: tlacc) tl
        | (acc, hd :: tl) ->
          split 1 ([hd] :: acc) tl
      in
      split 0 [] ifams
      |> List.map
        (function
          | [] -> []
          | [ ifam ] -> [ foi base ifam ]
          | ifams ->
            let url =
              "families/?ids[]=" ^
              String.concat "&ids[]=" (List.map (fun i -> String.split_on_char ':' i |> String.concat "%3A") ifams)
            in
            let x =
              get ~__LOC__ ~url
              |> parse
              |> to_list
              |> List.map read_family
            in
            List.iter2 (Hashtbl.replace foi_cache) ifams x ;
            x)
      |> List.flatten

let p_rev_cache : (iper, string) Hashtbl.t = Hashtbl.create 2048
let poi_cache : (iper, person) Hashtbl.t = Hashtbl.create 2048

let read_person x =
  { revision = to_string (member "_rev" x)
  ; iper = to_string (member "_key" x)
  ; person = member "person" x
  }

(* FIXME *)
let poi { get ; _ } iper : person =
  print_endline @@ Printf.sprintf "%s: %s" __LOC__ iper ;
  if iper = dummy_iper then raise Not_found ;
  try
    let x = Hashtbl.find poi_cache iper in
    print_endline @@ Printf.sprintf "Found in cache: %s" iper ;
    x
  with Not_found ->
    let x =
      let url = "persons/" ^ (String.split_on_char ':' iper |> String.concat "%3A") in
      get ~__LOC__ ~url
      |> parse
      |> read_person
    in
    (* print_endline (stringify x) ; *)
    Hashtbl.replace poi_cache iper x ;
    Hashtbl.replace p_rev_cache iper x.revision ;
    x

let poi_batch ({ get ; _ } as base) ipers =
  print_endline @@ Printf.sprintf "%s" __LOC__ ;
  match ipers with
  | [] -> []
  | [ iper ] -> [poi base iper]
  | _ ->
    try List.map (Hashtbl.find poi_cache) ipers
    with Not_found ->
      let rec split current acc list = match (acc, list) with
        | (acc, []) -> acc
        | ((hdacc :: tlacc), (hd :: tl)) when current < 500 ->
          split (current + 1) ((hd :: hdacc) :: tlacc) tl
        | (acc, hd :: tl) ->
          split 1 ([hd] :: acc) tl
      in
      split 0 [] ipers
      |> List.map
        (function
          | [] -> []
          | [ iper ] -> [ poi base iper ]
          | ipers ->
            let url =
              "persons/?ids[]=" ^
              String.concat "&ids[]=" (List.map (fun i -> String.split_on_char ':' i |> String.concat "%3A") ipers)
            in
            let x =
              get ~__LOC__ ~url
              |> parse
              |> to_list
              |> List.map read_person
            in
            List.iter
              (fun x ->
                 Hashtbl.replace poi_cache x.iper x ;
                 Hashtbl.replace p_rev_cache x.iper x.revision ;
              ) x ;
            x)
      |> List.flatten

let family_of_gen_family _base (f, _c, _d)
  =
  let open Def in
  { ifam = f.fam_index
  ; family = assoc [ ("marriage", json_of_cdate f.marriage)
                    ; ("marriage_place", string f.marriage_place)
                    ; ("marriage_note", string f.marriage_note)
                    ; ("marriage_src", string f.marriage_src)
                    ; ("witnesses", list (Array.to_list @@ Array.map (fun x -> string x) f.witnesses) )
                    ; ("relation", json_of_relation_kind f.relation)
                    ; ("divorce", json_of_divorce f.divorce)
                    ; ("fevents", list (List.map json_of_fevent f.fevents))
                    ; ("comment", string f.comment)
                    ; ("origin_file", string f.origin_file)
                    ; ("fsources", string f.fsources)
                    ]
  }

let person_of_gen_person _base (p, _a, _u) =
  let open Def in
  { revision = ""
  ; iper = p.key_index
  ; person = assoc [ ("firstname", string p.first_name)
                    ; ("lastname", string p.surname)
                    ; ("occ", int p.occ)
                    ; ("image", string p.image)
                    ; ("public_name", string p.public_name)
                    ; ("qualifiers", list (List.map (fun x -> string x) p.qualifiers) )
                    ; ("aliases", list (List.map (fun x -> string x) p.aliases) )
                    ; ("first_names_aliases", list (List.map (fun x -> string x) p.first_names_aliases) )
                    ; ("surnames_aliases", list (List.map (fun x -> string x) p.surnames_aliases) )
                    ; ("titles", list (List.map json_of_title p.titles))
                    ; ("rparents", list (List.map json_of_rparent p.rparents))
                    ; ("related", list (List.map (fun x -> string x) p.related))
                    ; ("occupation", string p.occupation)
                    ; ("sex", match p.sex with Male -> int 0 | Female -> int 1 | Neuter -> int 2)
                    ; ("access", match p.access with Private -> int 2 | Public  -> int 1 | IfTitles -> int 0)
                    ; ("pevents", list (List.map json_of_pevent p.pevents))
                    ; ("notes", string p.notes)
                    ; ("psources", string p.psources)
                    ]
  }

let gen_person_of_person : person -> (iper, iper, istr) Def.gen_person =
  fun p ->
  let open Def in
  { first_name = get_first_name p
  ; surname = get_surname p
  ; occ = get_occ p
  ; image = get_image p
  ; public_name = get_public_name p
  ; qualifiers = get_qualifiers p
  ; aliases = get_aliases p
  ; first_names_aliases = get_first_names_aliases p
  ; surnames_aliases = get_surnames_aliases p
  ; titles = get_titles p
  ; rparents = get_rparents p
  ; related = get_related p
  ; occupation = get_occupation p
  ; sex = get_sex p
  ; access = get_access p
  ; birth = get_birth p
  ; birth_place = get_birth_place p
  ; birth_note = get_birth_note p
  ; birth_src = get_birth_src p
  ; baptism = get_baptism p
  ; baptism_place = get_baptism_place p
  ; baptism_note = get_baptism_note p
  ; baptism_src = get_baptism_src p
  ; death = get_death p
  ; death_place = get_death_place p
  ; death_note = get_death_note p
  ; death_src = get_death_src p
  ; burial = get_burial p
  ; burial_place = get_burial_place p
  ; burial_note = get_burial_note p
  ; burial_src = get_burial_src p
  ; pevents = get_pevents p
  ; notes = get_notes p
  ; psources = get_psources p
  ; key_index = get_key_index p
  }

let date_of_last_change _base = 0. (* FIXME? *)
let p_surname _base = get_surname
let p_first_name _base = get_first_name

type string_person_index =
  { find : istr -> iper list
  ; cursor : string -> istr
  ; next : istr -> istr
  }

let mk_spi fn ({ get ; _ } : base) =
  { find = begin fun istr ->
        get ~__LOC__ ~url:(fn istr)
        |> parse
        |> to_list
        |> List.map (fun x -> get_string ~__LOC__ x "_key")
      end
  ; cursor = begin fun _ -> assert false end
  ; next = begin fun _ -> assert false end
  }


let spi_find spi = spi.find
let spi_first spi = spi.cursor
let spi_next (spi : string_person_index) istr (_need_whole_list : bool) = spi.next istr, 1

let persons_of_surname =
  mk_spi (fun istr -> Printf.sprintf "persons?lastname=%s&start_with=true" (Wserver.encode istr))

let persons_of_first_name  =
  mk_spi (fun istr -> Printf.sprintf "persons?firstname=%s&start_with=true" (Wserver.encode istr))

(* FIXME! *)
let persons_of_name _base _istr = []

let base_strings_of_surname _base _istr = [] (* FIXME *)

let base_strings_of_first_name _base _istr = [] (* FIXME *)

let nobtit _base _ _ { person = p ; _ } =
  get_list "titles" title_of_json p

let person_misc_names _f = failwith __LOC__
let gen_person_misc_names _f = failwith __LOC__

(* Copied from gwdb1/database.ml *)
let read_notes bname fnotes rn_mode =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let fname =
    if fnotes = "" then "notes"
    else Filename.concat "notes_d" (fnotes ^ ".txt")
  in
  try
    let ic = Secure.open_in (Filename.concat bname fname) in
    let str =
      match rn_mode with
        RnDeg -> if in_channel_length ic = 0 then "" else " "
      | Rn1Ln -> (try input_line ic with End_of_file -> "")
      | RnAll ->
        let rec loop len =
          match input_char ic with
          | exception End_of_file -> Buff.get len
          | c -> loop (Buff.store len c)
        in
        loop 0
    in
    close_in ic ;
    str
  with Sys_error _ -> ""


let base_wiznotes_dir _base =
  "notes_d"

let base_notes_dir _base =
  "wiznotes"

(* FIXME *)
let base_notes_origin_file _base =
  let () = print_endline __LOC__ in ""

let base_notes_are_empty { basename ; _ } fnotes =
  read_notes basename fnotes RnDeg = ""

let base_notes_read_first_line { basename ; _ } fnotes =
  read_notes basename fnotes Rn1Ln

let base_notes_read { basename ; _ } fnotes =
  read_notes basename fnotes RnAll

let ascends_array _f = let () = print_endline __LOC__ in failwith __LOC__
let persons_array _base = let () = print_endline __LOC__ in failwith __LOC__
let base_particles { basename ; _ } =
  (* FIXME: memoize *)
  let () = print_endline __LOC__ in
  Mutil.input_particles (Filename.concat basename "particles.txt")

let base_visible_write _ = (* failwith __LOC__ *) let () = print_endline __LOC__ in ()
let base_visible_get _ _ _ = (* failwith __LOC__ *) let () = print_endline __LOC__ in true

let delete_family _f = let () = print_endline __LOC__ in failwith __LOC__
let insert_family _f = let () = print_endline __LOC__ in failwith __LOC__
let insert_person _f = let () = print_endline __LOC__ in failwith __LOC__
let patched_ascends _f = let () = print_endline __LOC__ in failwith __LOC__
let is_patched_person _f = let () = print_endline __LOC__ in failwith __LOC__
let commit_notes _f = let () = print_endline __LOC__ in failwith __LOC__
let commit_patches _f = let () = print_endline __LOC__ in failwith __LOC__
let insert_string _base s = s
let delete_key _f = let () = print_endline __LOC__ in failwith __LOC__
let patch_key _f = let () = print_endline __LOC__ in failwith __LOC__
let patch_name _f = let () = print_endline __LOC__ in failwith __LOC__
let patch_couple _f = let () = print_endline __LOC__ in failwith __LOC__
let patch_descend _f = let () = print_endline __LOC__ in failwith __LOC__
let patch_family _f = let () = print_endline __LOC__ in failwith __LOC__
let patch_union _f = let () = print_endline __LOC__ in failwith __LOC__
let patch_ascend _f = let () = print_endline __LOC__ in failwith __LOC__

let patch_person ({ put ; _ } as base) iper gen_person =
  let res =
    let p = person_of_gen_person base (gen_person, (), ()) in
    let json =
      assoc [ ("_rev", string (Hashtbl.find p_rev_cache iper) )
             ; ("_key", string iper)
             ; ("person", p.person)
             ]
    in
    put ~__LOC__ ~url:("persons/" ^ iper) ~data:(stringify json)
    |> (fun x -> print_endline x ; x)
    |> parse
    |> read_person
  in
  Hashtbl.replace poi_cache iper res ;
  Hashtbl.replace p_rev_cache iper res.revision

let nb_of_families : base -> int = fun { get ; _ } ->
  get ~__LOC__ ~url:"nb_families"
  |> parse
  |> to_list
  |> List.hd
  |> to_int

let nb_of_persons : base -> int = fun { get ; _ } ->
  get ~__LOC__ ~url:"nb_persons"
  |> parse
  |> to_list
  |> List.hd
  |> to_int

let person_of_key : base -> string -> string -> int -> iper option =
  fun { get ; _ } p n oc ->
  (* FIXME *)
  get ~__LOC__ ~url:(Printf.sprintf "persons?n=%s&p=%s&oc=%d" (Wserver.encode n) (Wserver.encode p) oc)
  |> parse
  |> to_list
  |> function
  | [] -> None
  | (x :: _) -> Some (get_string ~__LOC__ x "_key")

let get_children { family = f ; _ } =
  get_list "children" to_int (* to_string *) f
  |> List.map iper_of_int
  |> Array.of_list

let get_parent_array { family = f ; _ } =
  match to_list @@ member "parents" f with
  | [ father ; mother ] ->
    begin  (* FIXME: To be removed *)
      try [| iper_of_int (to_int father) ; iper_of_int (to_int mother) |]
      with _ -> [| to_string father ; to_string mother |]
    end
  | _ -> failwith @@ Printf.sprintf "%s: %s" __LOC__ (stringify @@ member "parents" f)

let get_mother { family = f ; _ } =
  match to_list @@ member "parents" f with
  | [ _father ; mother ] ->
    begin  (* FIXME: To be removed *)
      try iper_of_int (to_int mother)
      with _ -> to_string mother
    end
  | _ -> failwith @@ Printf.sprintf "%s: %s" __LOC__ (stringify @@ member "parents" f)

let get_father { family = f ; _ } =
  match to_list @@ member "parents" f with
  | [ father ; _mother ] ->
    begin  (* FIXME: To be removed *)
      try iper_of_int (to_int father)
      with _ -> to_string father
    end
  | _ -> failwith @@ Printf.sprintf "%s: %s" __LOC__ (stringify @@ member "parents" f)

let get_witnesses { family = f ; _ } : iper array =
  Array.of_list (get_list "witnesses" to_string f)

let get_relation { family = f ; _ } =
  member "relation_kind" f
  |> relation_kind_of_json

let get_origin_file { family = f ; _ } =
  get_string ~__LOC__ f "origin_file"

let get_marriage_src { family = f ; _ } =
  get_string ~__LOC__ f "marriage_src"

let get_marriage_note { family = f ; _ } =
  get_string ~__LOC__ f "marriage_note"

let get_marriage_place { family = f ; _ } =
  get_string ~__LOC__ f "marriage_place"

let get_marriage { family = f ; _ } =
  print_endline __LOC__ ;
  cdate_of_json @@ member "marriage" f

let get_fsources { family = f ; _ } =
  get_string ~__LOC__ f "fsources"

let get_fevents { family = f ; _ } =
  get_list "fevents" fevent_of_json f

let get_divorce { family = f ; _ } =
  divorce_of_json (member "divorce" f)

let get_comment { family = f ; _ } =
  get_string ~__LOC__ f "comment"

let get_family { person = p ; _ } =
  to_list @@ member "families" p
  |> Array.of_list
  |> Array.map to_string

let get_consang _f = Adef.no_consang (* FIXME *)

(* FIXME: removed fam_index from json *)
let get_fam_index { ifam ; _ } =
  ifam

let gen_family_of_family : family -> (iper, ifam, istr) Def.gen_family =
  fun f ->
  let open Def in
  { marriage = get_marriage f
  ; marriage_place = get_marriage_place f
  ; marriage_note = get_marriage_note f
  ; marriage_src = get_marriage_src f
  ; witnesses = get_witnesses f
  ; relation = get_relation f
  ; divorce = get_divorce f
  ; fevents = get_fevents f
  ; comment = get_comment f
  ; origin_file = get_origin_file f
  ; fsources = get_fsources f
  ; fam_index = get_fam_index f
  }

let gen_couple_of_couple : family -> iper Def.gen_couple =
  fun f ->
  Adef.couple (get_father f) (get_mother f)

let gen_descend_of_descend : family -> iper Def.gen_descend =
  fun f ->
  { Def.children = get_children f }


module Collection = struct

  type 'a t =
    { length : int
    ; get : int -> 'a
    }

  let length x =
    x.length

  let map fn x =
    { x with get = fun i -> fn (x.get i) }

  let iter fn x =
    for i = 0 to length x - 1 do fn (x.get i) done

  let iteri fn x =
    for i = 0 to length x - 1 do fn i (x.get i) done

  let fold fn acc { get ; length } =
    let rec loop acc i =
      if i = length then acc
      else loop (fn acc (get i)) (i + 1)
    in
    loop acc 0

  let fold_until continue fn acc { get ; length } =
    let rec loop acc i =
      if not (continue acc) || i = length then acc
      else loop (fn acc (get i)) (i + 1)
    in
    loop acc 0

  let iterator { get ; length } =
    let cursor = ref 0 in
    fun () ->
      if !cursor < length then
        let v = get !cursor in
        incr cursor ;
        Some v
      else None

end

(* TODO: do not keep full array, only current bulk *)
let mk_collection len init get bulk_size =
  let current = ref (-1) in
  let cache = Array.make len init in
  let fetch i =
    let offset = (i / bulk_size) * bulk_size in
    let list = get offset bulk_size in
    List.iteri (fun i v -> Array.set cache (i + offset) v) list ;
    current := offset + bulk_size
  in
  let get i =
    if i < !current then Array.get cache i
    else begin fetch i ; Array.get cache i end
  in
  Collection.{ length = len ; get }

let mk_collection' len get bulk_size =
  let current = ref (-1) in
  let cache = ref [] in
  let fetch i =
    let offset = (i / bulk_size) * bulk_size in
    get offset bulk_size
  in
  let get i =
    incr current ;
    assert (i = !current) ;
    match !cache with
    | hd :: tl -> cache := tl ; hd
    | [] ->
      let res = fetch i in
      cache := List.tl res ;
      List.hd res
  in
  Collection.{ length = len ; get }

module Marker = struct
  type ('k, 'v) t = ('v * ('k, 'v) Hashtbl.t)
  let create nb d = (d, Hashtbl.create nb)
  let get (d, m) k = try Hashtbl.find m k with Not_found -> d
  let set (_, m) k v = Hashtbl.replace m k v
end

(* let of_list fn = function
 *   | [] -> [||]
 *   | hd :: tl as l ->
 *     let init = fn hd in
 *     let a = Array.make (List.length l) init in
 *     List.iteri (fun i x -> Array.unsafe_set a (i + 1) (fn x)) tl ;
 *     a *)

(* FIXME *)
let ipers ({ get ; _ } as base) : iper Collection.t =
  mk_collection (nb_of_persons base) dummy_iper
    (fun offset limit ->
       get ~__LOC__ ~url:(Printf.sprintf "persons?scope=ids&offset=%d&limit=%d" offset limit)
       |> parse
       |> to_list
       |> List.map to_string
    )
    100000

let persons ({ get ; _ } as base : base) : person Collection.t =
  mk_collection' (nb_of_persons base)
    (fun offset limit ->
       get ~__LOC__ ~url:(Printf.sprintf "persons?offset=%d&limit=%d" offset limit)
       |> parse
       |> to_list
       |> List.map read_person
    )
    10000

let ifams ({ get ; _ } as base) : ifam Collection.t =
  mk_collection (nb_of_families base) dummy_ifam
    (fun offset limit ->
       get ~__LOC__ ~url:(Printf.sprintf "families?scope=ids&offset=%d&limit=%d" offset limit)
       |> parse
       |> to_list
       |> List.map to_string
    )
    100000

let families ({ get ; _ } as base) : family Collection.t =
  mk_collection' (nb_of_families base)
    (fun offset limit ->
       get ~__LOC__ ~url:(Printf.sprintf "families?offset=%d&limit=%d" offset limit)
       |> parse
       |> to_list
       |> List.map read_family
    )
    10000

let ifam_marker ifams init =
  Marker.create (Collection.length ifams) init

let iper_marker ipers init =
  Marker.create (Collection.length ipers) init
