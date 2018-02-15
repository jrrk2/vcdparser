
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

val vars : (string, int) Hashtbl.t
val crnthd : bytes ref
val crntf : out_channel ref
val hierf : out_channel ref
val errlst : chng list ref
val typnam : kind -> string
val path : out_channel -> pth list -> unit
val varlst : (kind * pth list * range) list ref
val end_input : bool ref
