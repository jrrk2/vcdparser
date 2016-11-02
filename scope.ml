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

let flags = Array.make 256 false

let rec path chan = function
  | [] -> ()
  | Pstr hd :: [] -> output_string chan hd
  | Pstr hd :: tl -> path chan tl; output_char chan '.'; output_string chan hd
  | Pidx hd :: tl -> path chan tl; output_char chan '['; output_string chan (string_of_int hd); output_char chan ']'

let flagc ch = flags.(int_of_char ch)

let enabled = function
  | EVENT -> flagc 'e' || flagc 'a'
  | INTEGER  -> flagc 'i' || flagc 'a'
  | PARAMETER  -> flagc 'p' || flagc 'a'
  | REG  -> flagc 'r' || flagc 'a'
  | WIRE -> flagc 'w' || flagc 'a'
  | oth -> flagc 'a'

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
| NEWVAR(typ,num,enc,id,(RANGE(hi,lo) as rng)) when enabled typ -> let nam = Pstr id :: stem in
    if verbose then (prerr_string (typnam typ); prerr_string ": "; path stderr nam; prerr_newline ());
    Hashtbl.add vars enc !varlen;
    for i = 0 to num-1 do incr varlen; varlst := (typ, Pidx (i+lo) :: nam, rng) :: !varlst; done
| NEWVAR(typ,num,enc,id,SCALAR) when enabled typ -> let nam = Pstr id :: stem in
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

let vars = Hashtbl.create 131071
let fd = ref stdout

let nam str = let s = typnam str in String.lowercase s^String.make (max 1 (9-String.length s)) ' '
let enam s = s^String.make (max 1 (5-String.length s)) ' '

let blknam = function
  | BLOCK -> "block"
  | FILE -> "file"
  | MODULE -> "module"
  | FORK -> "fork"
  | FUNCTION -> "function"
  | TASK -> "task"

let rec dumpscopes = function
  | VCD_SCOPE(blk,str,scoplst) -> Printf.fprintf !fd "\n$scope %s %s $end\n" (blknam blk) str; 
        List.iter dumpscopes scoplst;
        Printf.fprintf !fd "$upscope $end\n"
  | NEWVAR(typ,int1,enc,str2,SCALAR) when enabled typ -> Printf.fprintf !fd "$var %s%3d %s%s $end\n" (nam typ) int1 (enam enc) str2
  | NEWVAR(typ,int1,enc,str2,RANGE(hi,lo)) when enabled typ -> Printf.fprintf !fd "$var %s%3d %s%s [%d:%d] $end\n" (nam typ) int1 (enam enc) str2 hi lo
  | NEWVAR(_,_,enc,_,_) -> ()
  | TIME_UNIT str -> Printf.fprintf !fd "$timeunit\n %s\n$end\n\n" str
  | TIME_SCALE str -> Printf.fprintf !fd "$timescale\n %s\n$end\n" str
  | COMMENT comment_lst -> Printf.fprintf !fd "$comment";
        List.iter (function STRING itm -> Printf.fprintf !fd " %s" itm) (List.rev comment_lst);
        Printf.fprintf !fd " $end\n";
  | VERSION -> Printf.fprintf !fd "$version\n\n"
  | DATE -> Printf.fprintf !fd "$date\n\n"
 
let readscopes arg =
      let (varlen,varlst) = (ref 0, ref []) in
      scopes vars varlen varlst false [] (VCD_SCOPE(FILE, "", arg));
      assert(List.length !varlst == !varlen);
      List.iter dumpscopes arg;
      Printf.fprintf !fd "$enddefinitions $end\n";
      (!varlen, Array.of_list (List.rev !varlst))

let changes = function
    | Tim n -> Printf.fprintf !fd "#%d\n" n
    | Change (enc,lev) -> if flags.(int_of_char 'a') || Hashtbl.find vars enc >= 0 then Printf.fprintf !fd "%c%s\n" lev enc
    | Vector (lev,enc) -> if flags.(int_of_char 'a') || Hashtbl.find vars enc >= 0 then Printf.fprintf !fd "%s %s\n" lev enc
    | Nochange -> Printf.fprintf !fd "$nochange\n"
    | Dumpvars -> Printf.fprintf !fd "$dumpvars\n"
    | Dumpall -> Printf.fprintf !fd "$dumpall\n"
    | Dumpon -> Printf.fprintf !fd "$dumpon\n"
    | Dumpoff -> Printf.fprintf !fd "$dumpoff\n"
