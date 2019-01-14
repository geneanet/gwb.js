#use "topfind" ;;
#require "jingoo" ;;

open Jingoo
open Jg_types

let compile dir filename =
  if Filename.check_suffix filename ".jinja2" then begin
    let env = { Jg_types.autoescape = false
              ; template_dirs = [ dir ]
              ; filters = []
              ; extensions = []
              ; strict_mode = false }
    in
    let ch_in = open_in @@ Filename.concat dir (Filename.basename filename) in
    let lexbuf = Lexing.from_channel ch_in in
    Jg_lexer.reset_context () ;
    Jg_lexer.init_lexer_pos (Some filename) lexbuf ;
    let ast =
      try Jg_parser.input Jg_lexer.main lexbuf
      with e -> raise @@ SyntaxError (Jg_utils.get_parser_error e lexbuf)
    in
    let ast = Jg_interp.unfold_extends env ast in
    let ast = Jg_interp.inline_include env ast in
    let ast = Jg_interp.replace_blocks ast in
    let var_name = Filename.basename @@ Filename.chop_suffix filename ".jinja2" in
    print_string "let " ;
    print_string var_name ;
    print_string " = {|" ;
    print_string @@ Marshal.to_string ast [ Marshal.Compat_32 ; Marshal.Closures ] ;
    print_endline "|}"
  end

let () =
  let dir = Sys.argv.(1) in
  Array.iter (compile dir) @@ Sys.readdir dir
