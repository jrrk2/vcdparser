(* Reconstructed implementation of the Vcd_types interface (vcd_types.mli).
   Holds the shared parser state: the encoding->index map, the current-value
   byte vector (crnthd, one char per signal), output channels, and the ordered
   variable list. *)

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

type scoping =
  | VCD_SCOPE of (block*string*scoping list)
  | NEWVAR of (kind*int*string*string*range)
  | TIME_UNIT of (string)
  | TIME_SCALE of (string)
  | COMMENT of (comment list)
  | VERSION
  | DATE

type chng =
  | Tim of int
  | Change of string*char
  | Vector of string*string
  | Nochange
  | Dumpvars
  | Dumpall
  | Dumpon
  | Dumpoff

type pth =
  | Pstr of string
  | Pidx of int

let vars : (string, int) Hashtbl.t = Hashtbl.create 65536
let crnthd : bytes ref = ref (Bytes.create 0)
let crntf : out_channel ref = ref stdout
let hierf : out_channel ref = ref stdout
let errlst : chng list ref = ref []
let varlst : (kind * pth list * range) list ref = ref []
let end_input : bool ref = ref false

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
