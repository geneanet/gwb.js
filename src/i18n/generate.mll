{

#use "topfind" ;;
#require "str" ;;

type i18n_expr =
  | Var of string                    (* {{ user }} *)
  | Var_typed of string * string     (* {{ n %.2f }} *)
  | Str of string                    (* This is a string *)
  | Cond of string * string * string (* {{{ many ? s || }}} *)

let flush buffer acc =
  let acc = match String.escaped (Buffer.contents buffer) with
    | "" -> acc
    | x -> Str x :: acc in
  Buffer.clear buffer ;
  acc

}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let num = ['0'-'9']

let id = (lower | ['_']) (lower | upper | num | ['_'])*

rule parse_lines langs acc = parse
  | (id as key) '\t' {
      let tr =
        let rec loop = function
          | [] -> []
          | lang :: tl -> (lang, parse_expr (Buffer.create 0) [] lexbuf) :: loop tl
        in loop langs
      in
    eol langs ((key, tr) :: acc) lexbuf }
  | eof { List.rev acc }

and eol langs acc = parse
  | [^'\n']* "\n" { Lexing.new_line lexbuf
                  ; parse_lines langs acc lexbuf}
  | eof { List.rev acc }

and parse_expr buffer acc = parse

  | "{{" ' '* (id as c) ' '* "??" {
    let s1 = parse_string_1 (Buffer.create 0) lexbuf in
    let s2 = parse_string_2 (Buffer.create 0) lexbuf in
    let acc = flush buffer acc in
    parse_expr buffer (Cond (c, s1, s2) :: acc) lexbuf
  }

  | "{{" ' '* (id as x) ' '* "}}" {
      let acc = flush buffer acc in
      parse_expr buffer (Var x :: acc) lexbuf }

  | "{{" ' '* (id as x) ' '* ('%' [^ ' ' '}']+ as f)  ' '* "}}" {
      let acc = flush buffer acc in
      parse_expr buffer (Var_typed (x, f) :: acc) lexbuf }

  | '\t' | "" { List.rev (flush buffer acc ) }

  | [^ '\n' '\t'] as c { Buffer.add_char buffer c
                       ; parse_expr buffer acc lexbuf }

and parse_string_1 buffer = parse
  | "||" { String.escaped (Buffer.contents buffer) }
  | _ as c { Buffer.add_char buffer c
           ; parse_string_1 buffer lexbuf }

and parse_string_2 buffer = parse
  | "}}" { String.escaped (Buffer.contents buffer) }
  | _ as c { Buffer.add_char buffer c
           ; parse_string_2 buffer lexbuf }

{

type arg =
  | M of string (* mandatory *)
  | O of string (* optionnal *)

let pp_print_list fmt printer =
  Format.fprintf fmt "[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ";")
       printer)

let print_module_body output key_value =
  let args languages =
    let rec f a =
      function [] -> List.rev a
             | Var x :: t          -> f (M x :: a) t
             | Var_typed (x, _) :: t -> f (M x :: a) t
             | Cond (x, _, _) :: t -> f (O x :: a) t
             | _ :: t              -> f a t in
    List.map (f []) languages
    |> List.flatten
    |> List.sort_uniq compare in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "\n")
    (fun fmt (key, tr) ->
       let args = args (List.map snd tr) in
       Format.fprintf fmt "let _%s =\n" key ;
       Format.fprintf fmt "let fn ?kwargs () =\n" ;
       Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_char fmt '\n')
         (fun fmt -> function M x | O x ->
             Format.fprintf fmt "let %s = try List.assoc \"%s\" kwargs with Not_found -> Tnull in\n" x x)
         fmt
         args ;
       Format.fprintf fmt "Jg_types.box_string @@ String.concat \"\" " ;
       pp_print_list fmt
         (fun fmt -> function
            | Str s -> Format.fprintf fmt "\"%s\"" s
            | Var v -> Format.fprintf fmt "string_of_tvalue %s" v
            | Var_typed (v, f) -> Format.fprintf fmt "Jg_runtime.jg_printf \"%s\" [%s]" f v
            | Cond (c, s1, s2) ->
              Format.fprintf fmt
                "(if Jg_runtime.jg_is_true %s then \"%s\" else \"%s\")"
                c s1 s2)
         (List.hd @@ List.map snd tr) ;
       Format.fprintf fmt " in\n" ;
       Format.fprintf fmt "Jg_types.func fn 0\n" ;
    )
    output
    key_value ;
  Format.pp_print_string output "let f =\nJg_types.Tfun (fun ?kwargs -> function\n" ;
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "\n")
    (fun fmt (key, _) -> Format.fprintf fmt "| Jg_types.Tstr \"%s\" -> jg_apply ?kwargs _%s []" key key)
    output
    key_value ;
  Format.pp_print_string output "\n| x -> Jg_types.failwith_type_error_1 \"f\")\n"

let input_file = ref "-"
let output_file = ref "-"
let languages = ref ""
let default_language = ref ""
let external_type = ref false

let options = Arg.align
    [ ( "--languages", Arg.Set_string languages
      , " Comma-separated languages (e.g. en,fr-fr, or Foo.Fr,Foo.Us if \
         using external types). \
         Must be ordered as in source TSV file.")
    ; ( "--default-language", Arg.Set_string default_language
      , " Set the default language (default is the first one in --languages).")
    ; ( "--input-file", Arg.Set_string input_file
      , " TSV file containing keys and translations. \
         If option is omited or set to -, read on stdin.")
    ; ( "--ouput-file", Arg.Set_string output_file
      , " File TSV file containing keys and translations. \
         If option is omited or set to -, write on stdout.")
    ; ( "--external-type", Arg.Set external_type
      , " Values passed to --languages option come from a predefined type \
         (do not generate the type nor from/to string functions).")
    ]

let usage = "usage: ocsigen-i18n-generator [options] [< input] [> output]"

let _ = Arg.parse options (fun s -> ()) usage

let _ =
  let in_chan =
    match !input_file with
    | "-" -> stdin
    | file -> open_in file in
  let out_chan =
    match !output_file with
    | "-" -> stdout
    | file -> open_out file in
  let strings = Str.split (Str.regexp ",") !languages in
  let variants = strings in
  let lexbuf = Lexing.from_channel in_chan in
  (try
     let key_values = parse_lines variants [] lexbuf in
     let output = Format.formatter_of_out_channel out_chan in
     print_module_body output key_values ;
   with Failure msg ->
     failwith (Printf.sprintf "%s line: %d" msg lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum) ) ;
  close_in in_chan ;
  close_out out_chan
}
