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

{
  open Lexing
  open Vcd_parser

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
    DATE, "$date";
    VERSION, "$version";
    ML_COMMENT, "$comment";
    ENDDEFNS , "$enddefinitions";
    END, "$end";
    TIMESCALE, "$timescale";
    SCOPE, "$scope";
    VAR, "$var";
    UPSCOPE, "$upscope";
    DUMPALL, "$dumpall";
    DUMPON, "$dumpon";
    DUMPOFF, "$dumpoff";
    DUMPVARS, "$dumpvars";
    BEGIN, "begin";
    FORK, "fork";
    FUNCTION, "function";
    MODULE, "module"   ;
    TASK, "task";
    EVENT, "event"; 
    INTEGER, "integer";
    PARAMETER, "parameter";
    REAL, "real";
    REG, "reg";
    SUPPLY0, "supply0";
    SUPPLY1, "supply1";
    TIME, "time";
    TRI, "tri";
    TRIAND, "triand";
    TRIOR, "trio";
    TRIREG, "trireg";
    TRI0, "tri0";
    TRI1, "tri1";
    WAND, "wand";
    WIRE, "wire";
    WOR, "wor";
      ];
    fun s -> let s = String.lowercase s in Hashtbl.find h s

}

let ident = ['!'-'~']+
let bin_num = ['b' 'B'] ['x' 'X' 'z' 'Z' '0' '1']+
let dec_num = ['0'-'9']+
let time = dec_num ("s" | "ms" | "us" | "ns" | "ps" | "fs")
let space = [' ' '\t' '\r']+
let newline = ['\n']
let hash = '#' dec_num
let range = '[' ['-']* dec_num ':' ['-']* dec_num ']'
let range1 = '[' ['-']* dec_num ']'

rule token = parse
  | space
      { token lexbuf }
  | newline
      { incr Scope.lincnt; NEWLINE }
  | bin_num as s
      { BIN_NUM s }
  | dec_num as s
      { DEC_NUM s }
  | time as s
      { TIME_UNIT s }
  | hash as s
      { Scanf.sscanf s "#%d" (fun t -> SIM_TIME(s,t)) }
  | range as s
      { Scanf.sscanf s "[%d:%d]" (fun hi lo -> RANGE(hi,lo)) }
  | range1 as s
      { Scanf.sscanf s "[%d]" (fun idx -> RANGE(idx,idx)) }
  | ident as s
      { try keyword s with Not_found -> IDENTIFIER s }
  | eof
      { EOF }
  | _ as c
      { failwith ("Vcd_lexer: invalid character " ^ String.make 1 c) }
