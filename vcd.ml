/**************************************************************************/
/*                                                                        */
/* Original Author: Eric Anderson                                         */
/*  Copyright (C) 2011 Carnegie Mellon University                         */
/* Adapted to OCaml by Jonathan Kimmitt                                   */
/*  Copyright 2016 University of Cambridge                                */
/* OCaml template Copyright (C) 2004-2010                                 */
/*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        */
/*                                                                        */
/*  This software is free software; you can redistribute it and/or        */
/*  modify it under the terms of the GNU Library General Public           */
/*  License version 2.1, with the special exception on linking            */
/*  described in file LICENSE.                                            */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/**************************************************************************/

(** Parser for VCD file format *)

open Vcd_parser

let parse_vcd_ast_from_chan c =
  let lb = Lexing.from_channel c in
  let vcd = try
      Vcd_lexer.lincnt := 1;
      Vcd_parser.vcd_file Vcd_lexer.token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Vcd.parse: parse error character %d" n)
    | _ ->
      failwith (Printf.sprintf "Parser error at line %d" !Vcd_lexer.lincnt)
  in
  close_in c;
  vcd

let parse_vcd_ast f =
  let c = open_in f in
  parse_vcd_ast_from_chan c

let _ = if Array.length Sys.argv > 1 then parse_vcd_ast Sys.argv.(1)
