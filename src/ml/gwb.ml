let conf = Conf.conf

open Js_of_ocaml
open Dom_html
open Jingoo

module Page = struct

  open Jg_types

  let interp template models =
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
      let output x = Buffer.add_string buf (Jg_runtime.string_of_tvalue x) in
      let ctx = Jg_interp.init_context ~env ~models ~output () in
      let ast = Jg_interp.import_macros env ctx ast in
      ignore @@ List.fold_left (Jg_interp.eval_statement env) ctx ast ;
      Dom_html.document##.body##.innerHTML := Js.string (Buffer.contents buf)
    with e ->
      Dom_html.document##.body##.innerHTML := Js.string (Printexc.to_string e)

  let summary conf base =
    print_endline __LOC__ ;
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
    ) :: Data.default_env conf base

  let person_aux conf base i =
    print_endline __LOC__ ;
    interp Templates.person @@
    ("ind", Data.get_n_mk_person conf base i)
    :: Data.default_env conf base

  let person conf base i =
    print_endline __LOC__ ;
    Firebug.console##log i ;
    let i = Js.to_string i in
    print_endline i ;
    person_aux conf base (Gwdb.iper_of_string i)

  let tree conf base i =
    print_endline __LOC__ ;
    Firebug.console##log i ;
    let i = Gwdb.iper_of_string @@ Js.to_string i in
    interp Templates.tree @@
    ("ind", Data.get_n_mk_person conf base i)
    :: Data.default_env conf base

  let searchPerson conf_ base fn sn occ =
    print_endline @@ Printf.sprintf "%s:%s:%s:%d" __LOC__ (Js.to_string fn) (Js.to_string sn) occ ;
    match Gwdb.person_of_key base (Js.to_string fn) (Js.to_string sn) occ with
    | Some i -> person_aux conf base i
    | None -> interp Templates.error []

  let birth_death_aux conf base fn bool =
    List.map
      (fun (p, d, c) ->
         let person = Data.get_n_mk_person conf base (Gwdb.get_key_index p) in
         let date = Data.mk_date conf (Dgreg (d, c) ) in
         Tpat (function
             | "person" -> person
             | "date" -> date
             | _ -> raise Not_found) )
      (fst @@ Geneweb.BirthDeath.select conf base fn bool)

  let oldestAlive conf base =
    print_endline __LOC__ ;
    interp Templates.oldestAlive @@
    let get_oldest_alive p =
      match Gwdb.get_death p with
      | NotDead -> Adef.od_of_cdate (Gwdb.get_birth p)
      | _ -> None
    in
    ("data", Tlist (birth_death_aux conf base get_oldest_alive true))
    :: Data.default_env conf base

  let timeline conf base i =
    print_endline __LOC__ ;
    let i = Js.to_string i in
    interp Templates.timeline @@
    ("root", Data.get_n_mk_person conf base @@ Gwdb.iper_of_string i)
    :: Data.default_env conf base

end

let init bname =
  let open Jg_types in
  let base = Gwdb.open_base bname in
  let conf = Conf.conf in
  let o =
    object%js
      method person (i) = Page.person conf base i
      method tree (i) = Page.tree conf base i
      method summary = Page.summary conf base
      method searchPerson (fn, sn, oc) = Page.searchPerson conf base fn sn oc
      method oldestAlive = Page.oldestAlive conf base
      method timeline (i) = Page.timeline conf base i
    end
  in
  Js.Unsafe.global##.Page := o ;
  o##searchPerson (Js.string "louis", Js.string "bourbon", 0)

  (* let ctx = ref [] in
   * 
   * let page =
   *   Tpat (function
   *       | "person" ->
   *         func_arg1_no_kw
   *           (function Tstr i -> show @@ Page.person base !ctx (Gwdb.iper_of_string i)
   *                   | x -> Jg_types.failwith_type_error_1 "person(iper)" x)
   *       | "summary" ->
   *         func_arg1_no_kw
   *           (function Tnull -> show @@ Page.summary base !ctx
   *                   | x -> Jg_types.failwith_type_error_1 "summary()" x)
   *       | "searchPerson" ->
   *         func_no_kw
   *           (function
   *             | [ Tstr fn ; Tstr sn ; Tint occ ] ->
   *               show @@ Page.searchPerson base !ctx fn sn occ
   *             | args ->
   *               failwith_type_error "searchPerson(string,string,int)"
   *                 (List.map (fun x -> ("", x)) args)
   *           ) 3
   *       | "oldestAlive" ->
   *         func_arg1_no_kw
   *           (function Tnull -> show @@ Page.oldestAlive base !ctx
   *                   | x -> Jg_types.failwith_type_error_1 "oldestAlive()" x)
   *       | x -> failwith x
   *     )
   * in
   * ctx := ("Page", page) :: Data.default_env conf base ; *)

let init () = init "pierfit"

let _ = Js.export "GWB" (object%js method init () = init () end)

  (* let global = Obj.magic @@ ref 0 in
   * Worker.set_onmessage (fun e ->
   *     Firebug.console##log(Js.string "Message received from main script") ;
   *     Firebug.console##log(e) ;
   *     match Js.to_string e##.type_ with
   *     | "loadFiles" ->
   *       e##.data##forEach
   *         begin Js.wrap_callback @@ fun x _i ->
   *          let name = Js.to_string @@ Js.Unsafe.get x (Js.string "name") in
   *          let data = Js.Unsafe.get x (Js.string "data") in
   *          let data = Js.to_bytestring data in
   *          print_endline __LOC__ ;
   *          print_endline @@ Digest.to_hex @@ Digest.string data ;
   *          let data = Bytes.unsafe_of_string data in
   *          let len = Bytes.length data in
   *          print_endline @@ __LOC__ ^ " -- " ^ bdir ^ name ^ " -- " ^ (string_of_int len) ;
   *          let oc = open_out @@ bdir ^ name in
   *          let () = output oc data 0 len in
   *          let () = close_out oc in
   *          print_endline __LOC__ ;
   *          print_endline @@ Digest.to_hex @@ Digest.file @@ bdir ^ name ;
   *          print_endline @@ "Saved " ^ bdir ^ name
   *         end ;
   *       print_endline __LOC__ ;
   *     | "openBase" ->
   *     | "display" ->
   *       let page =
   *         match Js.to_string e##.data##.page with
   *         | "person" -> print_endline __LOC__ ; ignore e##.data##.payload ; (!global)##person 139
   *         | "summary" -> print_endline __LOC__ ; (!global)##summary
   *         | "searchPerson" -> print_endline __LOC__ ; (!global)##searchPerson e##.data##.payload
   *         | "oldestAlive" -> print_endline __LOC__ ; (!global)##oldestAlive e##.data##.payload
   *         | _ -> failwith __LOC__
   *       in
   *       Firebug.console##log page ;
   *       Worker.post_message (Js.string page)
   *     | _ -> failwith __LOC__
   *   ) ; *)
