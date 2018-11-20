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

type pth =
  | Pstr of string
  | Pidx of int

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

let rec path chan = function
  | [] -> ()
  | Pstr hd :: [] -> output_string chan hd
  | Pstr hd :: tl -> path chan tl; output_char chan '.'; output_string chan hd
  | Pidx hd :: tl -> path chan tl; output_char chan '['; output_string chan (string_of_int hd); output_char chan ']'

let rec scopes vars varlen varlst verbose stem = function
| VCD_SCOPE(kind,nam',token_list) ->
    let nam = (if String.length nam' > 0 then Pstr nam' :: stem else stem) in List.iter (scopes vars varlen varlst verbose nam) token_list;
    if verbose then (match kind with
       | FILE -> ()
       | MODULE -> prerr_string "Module: "; path stderr nam; prerr_newline ()
       | BLOCK -> prerr_string "Block: "; path stderr nam; prerr_newline ()
       | FORK -> prerr_string "Fork: "; path stderr nam; prerr_newline ()
       | FUNCTION -> prerr_string "Function: "; path stderr nam; prerr_newline ()
       | TASK -> prerr_string "Task: "; path stderr nam; prerr_newline ())
| NEWVAR((REG|WIRE) as typ,num,enc,id,(RANGE(hi,lo) as rng)) -> let nam = Pstr id :: stem in
    if verbose then (prerr_string (typnam typ); prerr_string ": "; path stderr nam; prerr_newline ());
    Hashtbl.add vars enc !varlen;
    for i = 0 to num-1 do incr varlen; varlst := (typ, Pidx (i+lo) :: nam, rng) :: !varlst; done
| NEWVAR((REG|WIRE) as typ,num,enc,id,SCALAR) -> let nam = Pstr id :: stem in
    if verbose then (prerr_string (typnam typ); prerr_string ": "; path stderr nam; prerr_newline ());
    Hashtbl.add vars enc !varlen;
    incr varlen; varlst := (typ,nam,SCALAR) :: !varlst
| NEWVAR(_,_,enc,_,_) -> Hashtbl.add vars enc (-1)
| COMMENT _ -> ()
| TIME_SCALE _ -> ()
| VERSION -> ()
| DATE -> ()
| TIME_UNIT _ -> ()

let lincnt = ref 1
