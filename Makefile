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

.PHONY: everything

everything: vcdparser.top vcdparser

vcdparser.top: vcd_types.mli vcd_parser.mli ord.ml scope.ml vcd_parser.ml vcd_lexer.ml plot.ml vcd.ml
	ocamlfind ocamlmktop -package gnuplot,menhirLib -thread -linkpkg -g -o $@ -I $(MENHIR) vcd_types.mli vcd_parser.mli ord.ml scope.ml vcd_parser.ml vcd_lexer.ml plot.ml vcd.ml

vcdparser: vcd_types.mli vcd_parser.mli ord.ml scope.ml vcd_parser.ml vcd_lexer.ml plot.ml vcd.ml
	ocamlfind ocamlopt -package gnuplot,menhirLib -thread -linkpkg -g -o $@ -I $(MENHIR) vcd_types.mli vcd_parser.mli ord.ml scope.ml vcd_parser.ml vcd_lexer.ml plot.ml vcd.ml

vcd_lexer.ml: vcd_lexer.mll
	ocamllex vcd_lexer.mll

vcd_parser.mli vcd_parser.ml: vcd_parser.mly
	ocamlyacc vcd_parser.mly 

ord.ml: ord.sh vcd_parser.mli
	sh ord.sh
