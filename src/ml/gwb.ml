open Js_of_ocaml

let conf = Conf.conf

open Dom_html

let file_reader file callback =
  let reader = new%js File.fileReader in
  let () = reader##.onload := Dom.handler (fun e ->
      Js.Opt.case
        (e##.target)
        (fun () -> Js.bool false)
        (fun target ->
           Js.Opt.case
             (File.CoerceTo.string target##.result)
             (fun () -> Js.bool false)
             (fun result ->
                callback (Js.to_bytestring result) ) ) )
  in
  reader##readAsBinaryString file

let input_file ?(multiple=false) () =
  let x = createInput ~_type:(Js.string "file") document in
  if multiple then x##setAttribute (Js.string "multiple") (Js.string "") ;
  x

let bname = "foo"
let bdir = bname ^ ".gwb/"

module Page = struct

  open Jg_types

  let interp template models =
    Printexc.record_backtrace true ;
    try
      let ast : Jg_types.ast = Marshal.from_string template 0 in
      let buf = Buffer.create 1024 in
      let env = { Jg_types.autoescape = false
                ; template_dirs = []
                ; filters = []
                ; extensions = []
                ; strict_mode = true
                }
      in
      let output = Buffer.add_string buf in
      let ctx = Jg_interp.init_context ~env ~models ~output () in
      let ast = Jg_interp.import_macros env ctx ast in
      ignore @@ List.fold_left (Jg_interp.eval_statement env) ctx ast ;
      let body = Dom_html.document##.body in
      body##.innerHTML := Js.string @@ Buffer.contents buf
    with e ->
      Printexc.print_backtrace stdout ;
      raise e

  let summary base =
    interp Templates.summary @@
    let nbp = Gwdb.nb_of_persons base in
    let nbf = Gwdb.nb_of_families base in
    let r = Random.int nbp in
    ( "data"
    , Tpat (function
          | "nb_of_persons" -> Tint nbp
          | "nb_of_families" -> Tint nbf
          | "random_iper" -> Tint r
          | _ -> assert false)
    ) :: []

  let person base i =
    interp Templates.person @@
    ("ind", Data.get_n_mk_person conf base (Adef.iper_of_int i) ) :: []

  let searchPerson base fn sn occ =
    match
      Gwdb.person_of_key base
        (Js.to_string fn)
        (Js.to_string sn)
        (int_of_string @@ Js.to_string occ)
    with
    | Some i -> person base (Adef.int_of_iper i)
    | None -> interp Templates.error []

  let birth_death_aux base fn bool =
    List.map
      (fun (p, d, c) ->
         let person = Data.get_n_mk_person conf base (Gwdb.get_key_index p) in
         let date = Data.mk_date conf (Dgreg (d, c) ) in
         Tpat (function
             | "person" -> person
             | "date" -> date
             | _ -> raise Not_found) )
      (fst @@ Geneweb.BirthDeath.select conf base fn bool)

  let oldestAlive base =
    interp Templates.oldestAlive @@
    let get_oldest_alive p =
      match Gwdb.get_death p with
      | NotDead -> Adef.od_of_cdate (Gwdb.get_birth p)
      | _ -> None
    in
    ("data", Tlist (birth_death_aux base get_oldest_alive true)) :: []

end

let setup base =
  let o =
    object%js
      method person = Page.person base
      method summary = Page.summary base
      method searchPerson = Page.searchPerson base
      method oldestAlive = Page.oldestAlive base
    end
  in
  Js.Unsafe.global##.Page := o ;
  o

let init () =
  let create create ?onclick ?(att = []) content =
    let x = create document in
    begin match onclick with
      | Some fn -> x##.onclick := Dom.handler (fun e -> fn e ; Js._true)
      | None -> ()
    end ;
    List.iter (fun (k, v) -> x##setAttribute (Js.string k) (Js.string v)) att ;
    List.iter (Dom.appendChild x) content ;
    x
  in
  let h1 = create createH1 in
  let button = create createButton in
  let pcdata str = document##createTextNode (Js.string str) in
  let input = input_file ~multiple:true () in
  let span = create createSpan in
  let p = create createP in
  let ul = create createUl in
  let li = create createLi in
  let check =
    List.map
      (fun s -> (s, span ~att:[ ("style", "background-color:red;color:white;") ] [ pcdata s ] ))
      [ "tstab"
      ; "cache_visited"
      ; "cache_info"
      ; "synchro_patches"
      ; "patches"
      ; "notes_links"
      ; "notes"
      ; "command.txt"
      ; "fnames.inx"
      ; "fnames.dat"
      ; "snames.inx"
      ; "snames.dat"
      ; "names.inx"
      ; "names.acc"
      ; "base.acc"
      ; "strings.inx"
      ; "base"
      ]
  in
  input##.onchange := Dom.handler begin fun _e ->
      Js.Optdef.case (input##.files) (fun () -> failwith __LOC__) @@ fun files ->
      let len = files##.length in
      for i = 0 to len - 1 do
        Js.Opt.case (files##item i) (fun () -> failwith __LOC__) @@ fun file ->
        let name = Js.to_string file##.name in
        file_reader (Js.Unsafe.coerce file)
          (fun blob ->
             Sys_js.create_file ~name:(bdir ^ name) ~content:blob ;
             (List.assoc name check)##setAttribute
               (Js.string "style")
               (Js.string "background-color:green;color:white;") ;
             Js.bool true)
      done ;
      Js.bool true
    end ;
  let body = Dom_html.document##.body in
  body##.innerHTML := Js.string "" ;
  List.iter (Dom.appendChild body)
    [ (h1 [ pcdata "Load files and press ok" ] :> Dom.node Js.t)
    ; (input :> Dom.node Js.t)
    ; (button
         ~onclick:(fun _e ->
             let base = Gwdb.open_base bname in
             let o = setup base in
             o##summary)
         [ pcdata "GO!" ] :> Dom.node Js.t)
    ; (p [ ul (List.map (fun (_, x) -> li [ x ]) check) ] :> Dom.node Js.t)
    ]

let () =
  Js.export "GWB" begin object%js method init = init () end end
