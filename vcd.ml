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
open Scope

(*
 let _ = Gc.set { (Gc.get()) with Gc.major_heap_increment = 1048576;  Gc.max_overhead = 1048576; Gc.allocation_policy = 1; }
 *)

let parse_vcd_ast_from_chan c =
  let lb = Lexing.from_channel c in
  let vcd = try
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
  Pervasives.close_in c;
  vcd

let errlst = ref []

let readscopes vars arg =
      let (varlen,varlst) = (ref 0, ref []) in
      Scope.scopes vars varlen varlst false [] (VCD_SCOPE(FILE, "", arg));
      let crnt = String.make !varlen 'x' in
      assert(List.length !varlst == !varlen);
      (ref [(0,!varlen,crnt)], Array.of_list (List.rev !varlst))
	  
let scalar_change vars crnt enc lev = if Hashtbl.mem vars enc then
      (let idx = Hashtbl.find vars enc in
      if idx >= 0 then String.set crnt idx lev)
      else (errlst := Change(enc,lev) :: !errlst; failwith ("encoding "^enc^" not found"))

let vector_change vars crnt lev enc = if Hashtbl.mem vars enc then
      (let idx = Hashtbl.find vars enc in
      let cnt = String.length lev - 1 in
      if idx >= 0 then for i = 1 to cnt do
        let off = idx+cnt-i in
        String.set crnt off lev.[i];
      done)
      else (errlst := Vector(lev,enc) :: !errlst; failwith ("encoding "^enc^" not found"))

let simx crntlst =
        let (tim,xcnt,hd) = List.hd crntlst in
        let tl = List.tl crntlst in
        let cnt = ref 0 in
        for i = 0 to String.length hd - 1 do if hd.[i] = 'x' then incr cnt done;
	(tim,!cnt,hd) :: tl

let sim_time crntlst n =
        let (_,_,hd) = List.hd !crntlst in
	crntlst := (n,0,String.copy hd) :: simx !crntlst

let rec spath = function
  | [] -> ""
  | Pstr hd :: [] -> hd
  | Pstr hd :: tl -> spath tl ^ "." ^ hd
  | Pidx hd :: tl -> spath tl

let parse_vcd_ast f outf =
  let vars = Hashtbl.create 131071 in
  let chan = open_in f in
  let scopes,chnglst = parse_vcd_ast_from_chan chan in
  let crntlst, scopes = readscopes vars scopes in
  List.iter (function
    | Tim n -> sim_time crntlst n
    | Change (enc,lev) -> let (_,_,hd) = List.hd !crntlst in scalar_change vars hd enc lev
    | Vector (lev,enc) -> let (_,_,hd) = List.hd !crntlst in vector_change vars hd lev enc
    | Nochange -> ()
    | Dumpvars -> ()
    | Dumpall -> ()
    | Dumpon -> ()
    | Dumpoff -> ())
    chnglst;
  let chngs = Array.of_list (List.tl (List.rev (simx !crntlst))) in
  let prevpath = ref "" in
  let (_, _, crnt) = chngs.(Array.length chngs - 1) in
  let fd = open_out outf in
  Printf.fprintf fd "module alwaysx(input clk, input rst);\n\ninteger i, fd, notif;\n\ninitial fd = $fopen(\"alwaysx.log\");\n\nalways @(negedge clk)\n\tif (~rst) begin\n";
  Array.iteri (fun ix (kind,lst,rng) ->
    let pth = spath lst in
    if (pth <> !prevpath) && crnt.[ix]='x' then
      begin
	let log = "$fdisplay(fd, " in
(*
	let log = "$display(" in
*)
	match rng with
       | SCALAR ->
	  Printf.fprintf fd "\tif (1'bx === ^%s)\n\t\tbegin\n\t\t%s\"%%t: %s is X\", $time);\n\t\t%s = 'b0;\n\t\tend\n" pth log pth pth
       | RANGE(hi,lo) ->
	  Printf.fprintf fd "\tnotif = 1;\n\tfor (i = %d; i <= %d; i=i+1) if (1'bx === ^%s[i])\n\t\tbegin\n\t\tif (notif) %s\"%%t: %s is X\", $time);\n\t\t%s[i] = 'b0;\n\t\tnotif = 0;\n\t\tend\n" lo hi pth log pth pth
       end;
    prevpath := pth;
    ) scopes;
  Printf.fprintf fd "\t$fflush(fd);\n\tend\n\nendmodule // alwaysx;\n";
  close_out fd;
  chngs, scopes

let _ = if Array.length Sys.argv > 2 then
	  parse_vcd_ast Sys.argv.(1) Sys.argv.(2) else ([||],[||])
