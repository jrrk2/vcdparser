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
  open Scope
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
%token <string>   TIME_UNIT;
%token <string>   IDENTIFIER;
%token    ML_COMMENT;
%token    EOF

%type <unit> vcd_file
%type <Scope.kind> var_type
%start vcd_file
%%


/* Parser rules */

vcd_file:
    vcd_header simulation_command_list EOF { () }

// HEADER
vcd_header:
    decl_command_list ENDDEFNS END NEWLINE NEWLINE
        { Hashtbl.clear vars; scopes false "" (VCD_SCOPE(FILE, "", $1)) }
    ;

decl_command_list:
        { [] }
    | decl_command_list decl_command
        { $2 :: $1 }
    | decl_command_list NEWLINE
        { $1 }
    ;

decl_command:
      vcd_decl_date { $1 }
    | vcd_decl_version { $1 }
    | vcd_decl_timescale { $1 }
    | vcd_decl_comment { $1 }
    | vcd_scope  { $1 }
    | vcd_decl_var { $1 }
;

vcd_decl_date 
    : DATE NEWLINE IDENTIFIER IDENTIFIER DEC_NUM IDENTIFIER DEC_NUM NEWLINE END NEWLINE { DATE }
    ;

vcd_decl_version 
    : VERSION NEWLINE IDENTIFIER IDENTIFIER IDENTIFIER IDENTIFIER NEWLINE END NEWLINE { VERSION }
    ;

vcd_decl_comment 
    : ML_COMMENT identifier_list END NEWLINE { COMMENT $2 }
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
        { TIME_SCALE $3 }
    ;

vcd_scope
    : SCOPE scope_type IDENTIFIER END decl_command_list UPSCOPE END NEWLINE
        { VCD_SCOPE($2, $3, $5) }
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
        { NEWVAR($2,$3,$4,$5,$6) }
    ;

encoding:
      DEC_NUM      { $1  }
    | BIN_NUM      { $1  }
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
	| simulation_command_list simulation_command	{ () }
	;

simulation_command:
		SIM_TIME NEWLINE	   { sim_time $1 }
	|	IDENTIFIER NEWLINE	   { scalar_change (String.sub $1 1 (String.length $1 - 1), $1.[0]) }
	|	DEC_NUM NEWLINE 	   { scalar_change (String.sub $1 1 (String.length $1 - 1), $1.[0]) }
	|	BIN_NUM NEWLINE 	   { scalar_change (String.sub $1 1 (String.length $1 - 1), $1.[0]) }
	|	TIME_UNIT NEWLINE 	   { scalar_change (String.sub $1 1 (String.length $1 - 1), $1.[0]) }
	|	BIN_NUM IDENTIFIER NEWLINE { vector_change ($1,$2) }
	|	BIN_NUM DEC_NUM NEWLINE    { vector_change ($1,$2) }
	|	BIN_NUM BIN_NUM NEWLINE    { vector_change ($1,$2) }
	|	BIN_NUM TIME_UNIT NEWLINE  { vector_change ($1,$2) }
	|	BIN_NUM SIM_TIME NEWLINE   { vector_change ($1,fst $2) }
	|	ML_COMMENT NEWLINE         { () }
	|	DUMPALL NEWLINE 	   { dumpall() }
	| 	DUMPON NEWLINE 		   { dumpon() }
	| 	DUMPOFF NEWLINE 	   { dumpoff() }
	| 	DUMPVARS NEWLINE	   { dumpvars() }
	| 	END NEWLINE		   { () }
	;
