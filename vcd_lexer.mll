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

  let verbose = ref false
  let h = Hashtbl.create 17
  let r = Hashtbl.create 17
  let _ = List.iter 
      (fun (k,s) -> Hashtbl.add h s k; Hashtbl.add r k s)
      [
    BEGIN, "begin";
    DATE, "$date";
    DUMPALL, "$dumpall";
    DUMPON, "$dumpon";
    DUMPOFF, "$dumpoff";
    DUMPVARS, "$dumpvars";
    ENDDEFNS , "$enddefinitions";
    END, "$end";
    EVENT, "event"; 
    FORK, "fork";
    FUNCTION, "function";
    INTEGER, "integer";
    ML_COMMENT, "$comment";
    MODULE, "module"   ;
    NEWLINE, "newline";
    PARAMETER, "parameter";
    REAL, "real";
    REG, "reg";
    SCOPE, "$scope";
    SUPPLY0, "supply0";
    SUPPLY1, "supply1";
    TASK, "task";
    TIME, "time";
    TIMESCALE, "$timescale";
    TRI, "tri";
    TRIAND, "triand";
    TRIOR, "trio";
    TRIREG, "trireg";
    TRI0, "tri0";
    TRI1, "tri1";
    UPSCOPE, "$upscope";
    VAR, "$var";
    VERSION, "$version";
    WAND, "wand";
    WIRE, "wire";
    WOR, "wor";
      ]
  let keyword = fun s -> let s = String.lowercase s in Hashtbl.find h s

let tok k = if !verbose then (match k with
| BIN_NUM _ -> print_endline "bin_num"
| DEC_NUM _ -> print_endline "dec_num"
| REAL_NUM r -> print_endline ("real_num "^r)
| SIM_TIME _ -> print_endline "sim_time"
| TIME_UNIT _ -> print_endline "time_unit"
| RANGE _ -> print_endline "range"
| IDENTIFIER s -> print_endline ("identifier "^s)
| oth -> print_endline (Hashtbl.find r oth));
  flush stdout;
    k

}

let ident = ['!'-'~']+
let bin_num = ['b' 'B'] ['x' 'X' 'z' 'Z' '0' '1']+
let dec_num = ['0'-'9']+
let minus = '-'
let real_num = ['r' 'R'] minus* ['0'-'9']+ '.' ['0'-'9']+
let time = dec_num ("s" | "ms" | "us" | "ns" | "ps" | "fs")
let space = [' ' '\t' '\r']+
let newline = '\n'
let hash = '#' dec_num
let range = '[' ['-']* dec_num ':' ['-']* dec_num ']'
let range1 = '[' ['-']* dec_num ']'

rule token = parse
  | space
      { token lexbuf }
  | newline
      { incr Scope.lincnt; tok NEWLINE }
  | bin_num as s
      { tok (BIN_NUM s) }
  | dec_num as s
      { tok (DEC_NUM s) }
  | real_num as s
      { tok (REAL_NUM s ) }
  | time as s
      { tok (TIME_UNIT s) }
  | hash as s
      { Scanf.sscanf s "#%d" (fun t -> tok (SIM_TIME(s,t))) }
  | range as s
      { Scanf.sscanf s "[%d:%d]" (fun hi lo -> tok (RANGE(hi,lo))) }
  | range1 as s
      { Scanf.sscanf s "[%d]" (fun r1 -> tok (RANGE(r1,r1))) }
  | ident as s
      { try keyword s with Not_found -> tok (IDENTIFIER s) }
  | eof
      { tok EOF }
  | _ as c
      { failwith ("Vcd_lexer: invalid character " ^ String.make 1 c) }
