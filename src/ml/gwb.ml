let conf = Conf.conf

open Js_of_ocaml
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
                callback (Js.to_bytestring result) ) ) ) (* caml_new_string *)
  in
  reader##readAsBinaryString file

let input_file ?(multiple=false) () =
  let x = createInput ~_type:(Js.string "file") document in
  if multiple then x##setAttribute (Js.string "multiple") (Js.string "") ;
  x

let bname = "grimaldi"
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
      Buffer.contents buf
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
    match Gwdb.person_of_key base fn sn occ with
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

let () =
  let global = Obj.magic @@ ref 0 in
  Worker.set_onmessage (fun e ->
      Firebug.console##log(Js.string "Message received from main script") ;
      Firebug.console##log(e) ;
      match Js.to_string e##.type_ with
      | "loadFiles" ->
        e##.data##forEach
          begin Js.wrap_callback @@ fun x _i ->
           let name = Js.to_string @@ Js.Unsafe.get x (Js.string "name") in
           let data = Js.Unsafe.get x (Js.string "data") in
           let data = Js.to_bytestring data in
           print_endline __LOC__ ;
           print_endline @@ Digest.to_hex @@ Digest.string data ;
           let data = Bytes.unsafe_of_string data in
           let len = Bytes.length data in
           print_endline @@ __LOC__ ^ " -- " ^ bdir ^ name ^ " -- " ^ (string_of_int len) ;
           let oc = open_out @@ bdir ^ name in
           let () = output oc data 0 len in
           let () = close_out oc in
           print_endline __LOC__ ;
           print_endline @@ Digest.to_hex @@ Digest.file @@ bdir ^ name ;
           print_endline @@ "Saved " ^ bdir ^ name
          end ;
        print_endline __LOC__ ;
      | "openBase" ->
        let base = Gwdb.open_base bname in
        let () = Gwdb.load_persons_array base in
        global :=
          object%js
            method person (p) = Page.person base p
            method summary = Page.summary base
            method searchPerson (fn, sn, occ) =
              Page.searchPerson base (Js.to_string fn) (Js.to_string sn) (int_of_string @@ Js.to_string occ)
            method oldestAlive = Page.oldestAlive base
          end ;
        print_endline "READY TO ROCK!!!"
      | "display" ->
        let page =
          match Js.to_string e##.data##.page with
          | "person" -> print_endline __LOC__ ; ignore e##.data##.payload ; (!global)##person 139
          | "summary" -> print_endline __LOC__ ; (!global)##summary
          | "searchPerson" -> print_endline __LOC__ ; (!global)##searchPerson e##.data##.payload
          | "oldestAlive" -> print_endline __LOC__ ; (!global)##oldestAlive e##.data##.payload
          | _ -> failwith __LOC__
        in
        Firebug.console##log page ;
        Worker.post_message (Js.string page)
      | _ -> failwith __LOC__
    ) ;
