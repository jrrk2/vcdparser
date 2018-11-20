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
      let crnt = Bytes.init !varlen (fun _ -> 'x') in
      assert(List.length !varlst == !varlen);
      (ref [(0,!varlen,crnt)], Array.of_list (List.rev !varlst))
	  
let scalar_change vars crnt enc lev = if Hashtbl.mem vars enc then
      (let idx = Hashtbl.find vars enc in
      if idx >= 0 then Bytes.set crnt idx lev)
      else (errlst := Change(enc,lev) :: !errlst; failwith ("encoding "^enc^" not found"))

let vector_change vars crnt lev enc = if Hashtbl.mem vars enc then
      (let idx = Hashtbl.find vars enc in
      let cnt = String.length lev - 1 in
      if idx >= 0 then for i = 1 to cnt do
        let off = idx+cnt-i in
        Bytes.set crnt off lev.[i];
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
	crntlst := (n,0,Bytes.copy hd) :: simx !crntlst

let xanal arr chngs f =
  let (tim,xcnt,_) = chngs.(0) in
  let minx = ref (0,xcnt) in
  let plotlst = ref [] in
  Array.iteri (fun ix (tim,xcnt,_) -> plotlst := (float_of_int tim,float_of_int xcnt) :: !plotlst; if xcnt < snd (!minx) then minx := (ix,xcnt)) chngs;
  let (tim',xcnt',pattern) = chngs.(fst !minx) in
  output_string stderr ("X minimum of "^string_of_int xcnt'^" (out of "^ string_of_int xcnt^") occured at time "^string_of_int tim'^"\n");
  let remnant = open_out (f^".remnant.log") in
  let remnantlst = ref [] in
  String.iteri (fun ix ch ->
    if ch = 'x' then let (kind, pth, range) = arr.(ix) in
    Scope.path remnant pth; output_char remnant '\n'; remnantlst := arr.(ix) :: !remnantlst) pattern;
  (!remnantlst, !plotlst)

let parse_vcd_ast max f =
  let vars = Hashtbl.create 131071 in
  let chan = open_in f in
  let scopes,chnglst = parse_vcd_ast_from_chan chan in
  let crntlst, arr = readscopes vars scopes in
(*
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
  let (remnantlst,plotlst) = xanal arr chngs f in
  (remnantlst,plotlst)
  *)
  let fd = open_out "forcing.v" in
  Printf.fprintf fd "`timescale 1ns/1ps;\n";
  Printf.fprintf fd "module forcing;\n";
  Printf.fprintf fd "\ninteger fd;\n\n";
  let old = ref "" and clst = ref [] in Array.iter (function
    | (Vcd_types.REG,lst,rng) ->
      let concat = String.concat "" (List.rev (List.map (function Pstr s -> "."^s | Pidx n -> "") lst)) in
      if (List.hd lst <> Pstr "clk") && (concat <> !old) && (List.length lst <= max) then
        clst := String.sub concat 1 (String.length concat - 1) :: !clst;
      old := concat;
    | _ -> ()) arr;
  let clst = List.sort compare !clst in
  List.iter (fun concat -> Printf.fprintf fd "always @(%s)\n\t#1 if (1'bx === ^%s)\n\t\tbegin\n\t\tforce %s = 'b0;\n\t\t#1 release %s;\n\t\t$display(\"assign %s = 'b0;\");\n\t\tend\n\n" concat concat concat concat concat) clst;
  Printf.fprintf fd "endmodule // forcing;\n";
  close_out fd

let _ = match Array.length Sys.argv with
    | 2 -> parse_vcd_ast 6 Sys.argv.(1)
    | 3 -> parse_vcd_ast (int_of_string Sys.argv.(1)) Sys.argv.(2)
    | _ -> print_endline ("Usage "^Sys.argv.(0)^" vcdfile or depth vcdfile")
