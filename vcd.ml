(**************************************************************************)
(*                                                                        *)
(* Original Author: Eric Anderson                                         *)
(*  Copyright (C) 2011 Carnegie Mellon University                         *)
(* Adapted to OCaml by Jonathan Kimmitt                                   *)
(*  Copyright 2016 University of Cambridge                                *)
(* OCaml template Copyright (C) 2004-2010                                 *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Vcd_types

(*
 let _ = Gc.set { (Gc.get()) with Gc.major_heap_increment = 1048576;  Gc.max_overhead = 1048576; Gc.allocation_policy = 1; }
 *)

let parse_vcd_ast_from_chan c =
  let lb = Lexing.from_channel c in
  let _ = try
      Scope.lincnt := 1;
      Vcd_parser.vcd_file Vcd_lexer.token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Vcd.parse: parse error character %d" n)
(*
    | _ ->
      failwith (Printf.sprintf "Parser error at line %d" !Scope.lincnt)
*)
  in
  Pervasives.close_in c

let parse_vcd_ast f =
  let chan = open_in f in
  crntf := open_out "vec.out";
  hierf := open_out "hier.out";
  parse_vcd_ast_from_chan chan;
  close_out !crntf;
  let encf = open_out "enc.out" in
  let arr = Array.of_list (List.rev !varlst) in
  Hashtbl.iter (fun k ix ->
                    if ix >= 0 then let (_,p,_) = arr.(ix) in path encf p else output_string encf "*** ";
                    output_string encf (" - "^k^": "^string_of_int ix^"\n")) vars;
  close_out encf;
  let stillx = open_out "stillx.log" in
  output_string stillx "module stillx;\n";
  output_string stillx "\ninitial\n\tbegin\n";
  for i = 0 to 1 do
  output_string stillx ("\t#"^string_of_int(990*i+10)^"\n");
  String.iteri (fun ix ch -> if ch == 'x' then match arr.(ix) with
    | (REG,p,rng) ->
      output_string stillx (if i > 0 then "\trelease " else "\tforce ");
      path stillx p;
      output_string stillx (if i > 0 then ";\n" else " = 'b0;\n")
    | (_,p,rng) ->
(*
      output_string stillx "\t// ";
      path stillx p;
      output_string stillx "\n"
*)
      ()) !crnthd;
  done;
  output_string stillx "end\nendmodule\n";
  close_out stillx

let _ = if Array.length Sys.argv > 1 then parse_vcd_ast Sys.argv.(1)
