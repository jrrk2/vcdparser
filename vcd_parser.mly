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

/* $Id:$ */

%{
  open Parsing
  open Vcd_types
  let verbose = ref false
  let (varlen,stem) = (ref 0, ref [])

  let scalar_change enc lev =
      if Hashtbl.mem vars enc then
      let idx = Hashtbl.find vars enc in
      Bytes.set !crnthd idx lev;
      else (errlst := Change(enc,lev) :: !errlst; failwith ("encoding "^enc^" not found"))

  let vector_change lev enc =
      if Hashtbl.mem vars enc then
        begin
        let idx = Hashtbl.find vars enc in
        let cnt = String.length lev - 1 in
        Bytes.set !crnthd idx 'b';
        for i = 1 to cnt do
          if lev.[i] = 'x' then Bytes.set !crnthd idx 'x';
        done
        end
      else (errlst := Vector(lev,enc) :: !errlst; failwith ("encoding "^enc^" not found"))

  let sim_time n =
        let cnt = ref 0 in
        String.iter (fun c -> if c = 'x' then incr cnt) !crnthd;
(*
        Printf.fprintf !crntf "%d %d %s\n" n !cnt !crnthd;
 *)
        Printf.fprintf !crntf "%d %d\n" n !cnt;
        if (n > 100000) then end_input := true
%}

%token    NEWLINE
%token    ENDDEFNS 
%token    END
%token    TIMESCALE
%token    SCOPE
%token    VAR
%token    UPSCOPE
%token    DUMPALL
%token    DUMPON
%token    DUMPOFF
%token    DUMPVARS
%token    BEGIN
%token    FORK
%token    FUNCTION
%token    MODULE
%token    TASK
%token    EVENT
%token    INTEGER
%token    PARAMETER
%token    REAL
%token    REG
%token    SUPPLY0
%token    SUPPLY1
%token    TIME
%token    TRI
%token    TRIAND
%token    TRIOR
%token    TRIREG
%token    TRI0
%token    TRI1
%token    WAND
%token    WIRE
%token    WOR

%token <int*int> RANGE

%token <string*int>   SIM_TIME;
%token <char>   SCALAR_VALUE;
%token    DATE;
%token    VERSION;
%token <string>   BIN_NUM;
%token <string>   DEC_NUM;
%token <string>   REAL_NUM;
%token <string>   TIME_UNIT;
%token <string>   IDENTIFIER;
%token    ML_COMMENT;
%token    EOF

%type <unit> vcd_file
%type <Vcd_types.kind> var_type
%start vcd_file
%%


/* Parser rules */

vcd_file:
    vcd_header simulation_command_list EOF { }

// HEADER
vcd_header:
    decl_command_list ENDDEFNS END NEWLINE NEWLINE
        { ignore $1; crnthd := Bytes.init !varlen (fun _ -> 'x') }
    ;

decl_command_list:
        { [] }
    | decl_command_list decl_command
        { $2 :: $1 }
    | decl_command_list NEWLINE
        { $1 }
    ;

decl_command:
      vcd_decl_date      { $1 }
    | vcd_decl_version   { $1 }
    | vcd_decl_timescale { $1 }
    | vcd_decl_comment   { $1 }
    | vcd_scope1 vcd_scope2          { $1 }
    | vcd_decl_var       { $1 }
;

vcd_decl_date 
    : DATE NEWLINE IDENTIFIER IDENTIFIER DEC_NUM IDENTIFIER DEC_NUM NEWLINE END NEWLINE { ignore DATE }
    ;

vcd_decl_version 
    : VERSION NEWLINE IDENTIFIER IDENTIFIER IDENTIFIER IDENTIFIER NEWLINE END NEWLINE { ignore VERSION }
    ;

vcd_decl_comment 
    : ML_COMMENT identifier_list END NEWLINE { ignore $2 }
    ;

identifier_list:
        { [] }
    | identifier_list IDENTIFIER
        { STRING $2 :: $1 }
    | identifier_list DEC_NUM
        { STRING $2 :: $1 }
    ;

vcd_decl_timescale
    : TIMESCALE NEWLINE TIME_UNIT NEWLINE END NEWLINE
        { ignore $3 }
    ;

vcd_scope1
    : SCOPE scope_type IDENTIFIER
        {
        let kind = $2 in
        let nam' = $3 in
        stem := Pstr nam' :: !stem;
        }
    ;

vcd_scope2
    : END decl_command_list UPSCOPE END NEWLINE
        {
        stem := List.tl !stem
        }
    ;

scope_type:
      BEGIN  { BLOCK }
    | FORK  { FORK }
    | FUNCTION  { FUNCTION }
    | MODULE  { MODULE }
    | TASK  { TASK }
    ;

vcd_decl_var
    : VAR var_type DEC_NUM encoding IDENTIFIER range END NEWLINE
        { let typ = $2 in
          let wid = int_of_string $3 in
          let enc = $4 in
          let id = $5 in
          let rng = $6 in
          let nam = Pstr id :: !stem in
          output_string !hierf (typnam typ^": "); path !hierf nam; output_string !hierf "\n";
          if not (Hashtbl.mem vars enc) then
               begin
                 Hashtbl.replace vars enc !varlen;
                 incr varlen;
                 varlst := (typ, nam, rng) :: !varlst
               end
        }
               
encoding:
      DEC_NUM      { $1  }
    | BIN_NUM      { $1  }
    | REAL_NUM     { $1  }
    | TIME_UNIT    { $1  }
    | SIM_TIME     { fst $1  }
    | SCALAR_VALUE { String.make 1 $1  }
    | IDENTIFIER   { $1  }
    
range:
                 { SCALAR }
    | RANGE      { RANGE $1 }
    ;
    
var_type
    : EVENT  { EVENT }
    | INTEGER  { INTEGER }
    | PARAMETER  { PARAMETER }
    | REAL  { REAL }
    | REG  { REG }
    | SUPPLY0  { SUPPLY0 }
    | SUPPLY1 { SUPPLY1 }
    | TIME { TIME }
    | TRI { TRI }
    | TRIAND { TRIAND }
    | TRIOR { TRIOR }
    | TRIREG { TRIREG }
    | TRI0 { TRI0 }
    | TRI1 { TRI1 }
    | WAND { WAND }
    | WIRE { WIRE }
    | WOR { WOR }
    ;

//COMMANDS

simulation_command_list:
							{ () }
	| simulation_command_list simulation_command	{ $2 }
	;

simulation_command:
		SIM_TIME NEWLINE            { sim_time (snd $1) }
	|	IDENTIFIER NEWLINE	    { scalar_change (String.sub $1 1 (String.length $1 - 1)) $1.[0] }
	|	DEC_NUM NEWLINE             { scalar_change (String.sub $1 1 (String.length $1 - 1)) $1.[0] }
	|	BIN_NUM NEWLINE             { scalar_change (String.sub $1 1 (String.length $1 - 1)) $1.[0] }
	|	TIME_UNIT NEWLINE           { scalar_change (String.sub $1 1 (String.length $1 - 1)) $1.[0] }
	|	BIN_NUM IDENTIFIER NEWLINE  { vector_change $1 $2 }
	|	REAL_NUM IDENTIFIER NEWLINE { vector_change $1 $2 }
	|	BIN_NUM DEC_NUM NEWLINE     { vector_change $1 $2 }
	|	BIN_NUM BIN_NUM NEWLINE     { vector_change $1 $2 }
	|	BIN_NUM TIME_UNIT NEWLINE   { vector_change $1 $2 }
	|	BIN_NUM SIM_TIME NEWLINE    { vector_change $1 (fst $2) }
	|	ML_COMMENT NEWLINE          { }
	|	DUMPALL NEWLINE             { }
	| 	DUMPON NEWLINE              { }
	| 	DUMPOFF NEWLINE             { }
	| 	DUMPVARS NEWLINE            { }
	| 	END NEWLINE                 { }
	;
