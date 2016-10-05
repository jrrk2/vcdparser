
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

type chng' =
| Tim of int
| Chng of int*int*char
| Vect of int*int*string
| Change of string*char
| Vector of string*string
| Nochange
