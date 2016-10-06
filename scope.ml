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

type chng = 
  | VECTOR_CHANGE of (string*string)
  | VCD_SCOPE of (block*string*chng list)
  | NEWVAR of (kind*int*string*string*range)
  | TIME_UNIT of (string)
  | TIME_SCALE of (string)
  | COMMENT of (comment list)
  | VERSION
  | DATE

let typnam = function
  | EVENT  -> "EVENT"
  | INTEGER  -> "INTEGER"
  | PARAMETER  -> "PARAMETER"
  | REAL  -> "REAL"
  | REG  -> "REG"
  | SUPPLY0  -> "SUPPLY0"
  | SUPPLY1 -> "SUPPLY1"
  | TIME -> "TIME"
  | TRI -> "TRI"
  | TRIAND -> "TRIAND"
  | TRIOR -> "TRIOR"
  | TRIREG -> "TRIREG"
  | TRI0 -> "TRI0"
  | TRI1 -> "TRI1"
  | WAND -> "WAND"
  | WIRE -> "WIRE"
  | WOR -> "WOR"

let rec path = function
  | [] -> ()
  | hd :: tl -> path tl; print_char '.'; print_string hd

let rec scopes vars varlst verbose stem = function
| VCD_SCOPE(kind,nam',token_list) ->
    let nam = (if String.length nam' > 0 then nam' :: stem else stem) in List.iter (scopes vars varlst verbose nam) token_list;
    (match kind with
       | FILE -> ()
       | MODULE -> print_string "Module: "; path nam; print_newline ()
       | BLOCK -> print_string "Block: "; path nam; print_newline ()
       | FORK -> print_string "Fork: "; path nam; print_newline ()
       | FUNCTION -> print_string "Function: "; path nam; print_newline ()
       | TASK -> print_string "Task: "; path nam; print_newline ())
| NEWVAR(typ,num,enc,id,RANGE(hi,lo)) -> let nam = id :: stem in
    if verbose then (print_string (typnam typ); print_string ": "; path nam; print_newline ());
    Hashtbl.add vars enc (List.length !varlst);
    for i = 0 to num-1 do varlst := (typ,string_of_int (i+lo) :: nam,RANGE(hi,lo)) :: !varlst; done
| NEWVAR(typ,num,enc,id,SCALAR) -> let nam = id :: stem in
    if verbose then (print_string (typnam typ); print_string ": "; path nam; print_newline ());
    Hashtbl.add vars enc (List.length !varlst);
    varlst := (typ,nam,SCALAR) :: !varlst
| COMMENT _ -> ()
| TIME_SCALE _ -> ()
| VERSION -> ()
| DATE -> ()
| VECTOR_CHANGE _ -> ()
| TIME_UNIT _ -> ()

let errlst = ref []
let lincnt = ref 1
let changes = Hashtbl.create 256
let crnt = ref (Bytes.create 0)

let scalar_change vars (enc,lev) = if Hashtbl.mem vars enc then
      let idx = Hashtbl.find vars enc in
      Bytes.set !crnt idx lev;
      Chng(!lincnt,idx,lev)
      else (errlst := Change(enc,lev) :: !errlst; failwith ("encoding "^enc^" not found"))

let vector_change vars (lev,enc) = if Hashtbl.mem vars enc then
      let idx = Hashtbl.find vars enc in
      let cnt = String.length lev - 1 in
      let lmt = String.length !crnt in
      for i = 1 to cnt do
        let off = idx+cnt-i in
	assert (off >= 0 && off < lmt);
        Bytes.set !crnt off lev.[i];
      done;
      Vect(!lincnt,idx,lev)
      else (errlst := Vector(lev,enc) :: !errlst; failwith ("encoding "^enc^" not found"))

let sim_time (s,n) =
        let nxt = Bytes.copy !crnt in
	Hashtbl.add changes (n:int) nxt;
	crnt := nxt;
	Tim n

let dumpall () = Nochange
let dumpon () = Nochange
let dumpoff () = Nochange
let dumpvars () = Nochange
