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

type kind =
  | EVENT  
  | INTEGER  
  | PARAMETER  
  | REAL  
  | REG  
  | SUPPLY0  
  | SUPPLY1 
  | TIME 
  | TRI 
  | TRIAND 
  | TRIOR 
  | TRIREG 
  | TRI0 
  | TRI1 
  | WAND 
  | WIRE 
  | WOR 

type range =
  | SCALAR
  | RANGE of (int*int)

type block =
  | BLOCK
  | FILE
  | MODULE
  | FORK
  | FUNCTION
  | TASK

type comment =
  | STRING of string

type chng = 
  | VECTOR_CHANGE of (string*string)
  | VCD_SCOPE of (block*string*chng list)
  | NEWVAR of (kind*string*string*string*range)
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

let vars = Hashtbl.create 256

let rec scopes verbose stem = function
| VCD_SCOPE(kind,nam',token_list) ->
    let nam = (if stem <> "" then stem^"." else "")^nam' in List.iter (scopes verbose nam) token_list;
    (match kind with
       | FILE -> ()
       | MODULE -> print_endline ("Module: "^nam)
       | BLOCK -> print_endline ("Block: "^nam)
       | FORK -> print_endline ("Fork: "^nam)
       | FUNCTION -> print_endline ("Function: "^nam)
       | TASK -> print_endline ("Task: "^nam))
| NEWVAR(typ,num,enc,id,rng) -> let nam = stem^"."^id in
    if verbose then print_endline (typnam typ^": "^nam);
    Hashtbl.add vars enc (typ,num,nam,rng)
| COMMENT _ -> ()
| TIME_SCALE _ -> ()
| VERSION -> ()
| DATE -> ()
| VECTOR_CHANGE _ -> ()
| TIME_UNIT _ -> ()

type chng' =
| Tim of int
| Chng of string*char
| Vect of string*string
| Change of string*char
| Vector of string*string

let chnglst = ref []
let errlst = ref []

let scalar_change (enc,lev) = if Hashtbl.mem vars enc then
      let (typ,num,nam,rng) = Hashtbl.find vars enc in
      chnglst := Chng(nam,lev) :: !chnglst
      else (errlst := Change(enc,lev) :: !errlst; failwith ("encoding "^enc^" not found"))

let vector_change (lev,enc) = if Hashtbl.mem vars enc then
      let (typ,num,nam,rng) = Hashtbl.find vars enc in
      chnglst := Vect(nam,lev) :: !chnglst
      else (errlst := Vector(lev,enc) :: !errlst; failwith ("encoding "^enc^" not found"))

let sim_time (s,n) =
      chnglst := Tim n :: !chnglst

let dumpall () = ()
let dumpon () = ()
let dumpoff () = ()
let dumpvars () = ()
