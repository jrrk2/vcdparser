#**************************************************************************/
#*                                                                        */
#* Original Author: Eric Anderson                                         */
#*  Copyright (C) 2011 Carnegie Mellon University                         */
#* Adapted to OCaml by Jonathan Kimmitt                                   */
#*  Copyright 2016 University of Cambridge                                */
#* OCaml template Copyright (C) 2004-2010                                 */
#*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        */
#*                                                                        */
#*  This software is free software; you can redistribute it and/or        */
#*  modify it under the terms of the GNU Library General Public           */
#*  License version 2.1, with the special exception on linking            */
#*  described in file LICENSE.                                            */
#*                                                                        */
#*  This software is distributed in the hope that it will be useful,      */
#*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
#*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
#*                                                                        */
#**************************************************************************/

MENHIR=/usr/lib/ocaml/menhirLib
MFLAGS=#--trace
YACC=ocamlyacc

.PHONY: everything

everything: vcdtrim.top vcdtrim

vcdtrim.top: vcd_types.mli vcd_trim.mli scope.ml vcd_trim.ml vcd_trim_lexer.ml vcdtrim.ml
	ocamlfind ocamlmktop -package menhirLib -thread -linkpkg -g -o $@ -I $(MENHIR) vcd_types.mli scope.ml vcd_trim.mli vcd_trim.ml vcd_trim_lexer.ml vcdtrim.ml

vcdtrim: vcd_types.mli vcd_trim.mli scope.ml vcd_trim.ml vcd_trim_lexer.ml vcdtrim.ml
	ocamlfind ocamlopt -package menhirLib -thread -linkpkg -g -o $@ -I $(MENHIR) vcd_types.mli scope.ml vcd_trim.mli vcd_trim.ml vcd_trim_lexer.ml vcdtrim.ml

vcd_trim_lexer.ml: vcd_trim_lexer.mll
	ocamllex vcd_trim_lexer.mll

vcd_trim.mli vcd_trim.ml: vcd_trim.mly
	$(YACC) vcd_trim.mly 
