#use "topfind" ;;
#require "jingoo" ;;

open Jingoo
open Jg_types

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

let compile dir filename =
  let ht = Hashtbl.create 2048 in
  let input_lexicon lang fname =
    let chan = open_in fname in
    let foo = ref 0 in
    let rec loop key () = match incr foo ; (input_line chan, key) with
      | line, _ when String.length line < 4 -> loop key ()
      | line, _ when String.sub line 0 4 = "    " -> loop (Some (String.trim line)) ()
      | line, Some k ->
        begin match String.index_opt line ':' with
          | Some i when String.sub line 0 i = lang ->
            Hashtbl.add ht k @@ String.sub line (i + 2) (String.length line - i - 2) ;
            loop key ()
          | _ -> loop key ()
        end
      | _ -> loop key ()
      | exception End_of_file -> close_in chan
    in
    loop None ()
  in
  input_lexicon "fr" "/home/jsagot/workspace/geneanet.git/geneweb/gw_plus/gw/lang/lex_utf8.txt" ;
  input_lexicon "fr" "/home/jsagot/workspace/geneanet.git/geneweb/gw_plus/gw/lang/geneanet_utf8.txt" ;
  if Filename.check_suffix filename ".html.jingoo" then begin
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
    let ast = Jg_interp.dead_code_elimination ast in
    let ast =
      let expression self = function
        | ApplyExpr ( DotExpr (IdentExpr "translate", "transl")
                    , [ LiteralExpr (Tstr s) ]) ->
          LiteralExpr (Tstr (try Hashtbl.find ht s with _ -> failwith s))
        | ApplyExpr ( DotExpr (IdentExpr "translate", "nth")
                    , [ LiteralExpr (Tstr s) ; LiteralExpr (Tint i) ]) ->
          LiteralExpr (Tstr (nth_field (try Hashtbl.find ht s with _ -> failwith s) i))
        (* | ApplyExpr ( DotExpr (IdentExpr "translate", "nth")
         *             , [ LiteralExpr (Tstr s) ; e ]) ->
         *   let s = try Hashtbl.find ht s with _ -> failwith s in
         *   let fn = func_arg1_no_kw @@ function
         *     | Tint i -> Tstr (nth_field s i)
         *     | x -> failwith_type_error_1 "translate.nth" x
         *   in
         *   ApplyExpr (LiteralExpr fn, [ Jg_ast_mapper.default_mapper.expression self e ]) *)
        | e -> Jg_ast_mapper.default_mapper.expression self e
      in
      let mapper = { Jg_ast_mapper.default_mapper with expression } in
      mapper.ast mapper ast
    in
    let ast =
      let expression self = function
        | ApplyExpr (IdentExpr n , args) as e
          when List.for_all (function KeywordExpr (IdentExpr _, LiteralExpr _)
                                    | LiteralExpr _ -> true
                                    | _ -> false) args
               && Array.exists (fun (n', _) -> n' = n) Jg_runtime.std_filters
          ->
          let kwargs, args =
            let rec loop kwargs args = function
              | [] -> (kwargs, args)
              | KeywordExpr (IdentExpr k, LiteralExpr v) :: tl ->
                loop ((k, v) :: kwargs) args tl
              | LiteralExpr v :: tl ->
                loop kwargs (v :: args) tl
              | _ -> assert false
            in
            loop [] [] args
          in
          let rec loop i =
            let (n', fn) = Array.get Jg_runtime.std_filters i in
            if n = n'
            then match Jg_runtime.jg_apply ~kwargs fn args with
              | Tfun _ -> Jg_ast_mapper.default_mapper.expression self e
              | x -> LiteralExpr x
            else loop (i + 1)
          in loop 0
        | e -> Jg_ast_mapper.default_mapper.expression self e
      in
      let mapper = { Jg_ast_mapper.default_mapper with expression } in
      mapper.ast mapper ast
    in
    let ast =
      let statement self = function
        | ExpandStatement (LiteralExpr (Tint _ | Tfloat _ | Tstr _ | Tnull as t)) ->
          TextStatement (Jg_runtime.string_of_tvalue t)
        | e -> Jg_ast_mapper.default_mapper.statement self e
      in
      let mapper = { Jg_ast_mapper.default_mapper with statement } in
      mapper.ast mapper ast
    in
    let ast =
      let flush str acc =
        if str = [] then acc
        else TextStatement (String.concat "" (List.rev str) ) :: acc
      in
      let rec loop str acc = function
        | [] -> List.rev (flush str acc)
        | TextStatement s :: tl -> loop (s :: str) acc tl
        | s :: tl -> loop [] (s :: flush str acc) tl
      in
      loop [] [] ast
    in
    let var_name = Filename.basename @@ Filename.chop_suffix filename ".html.jingoo" in
    print_string "let " ;
    print_string var_name ;
    print_string " = {|" ;
    print_string @@ Marshal.to_string ast [ Marshal.Compat_32 ; Marshal.Closures ; Marshal.No_sharing ] ;
    print_endline "|}"
  end

let () =
  let dir = Sys.argv.(1) in
  Array.iter (compile dir) @@ Sys.readdir dir
