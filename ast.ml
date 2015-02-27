(* Base type for all expression nodes *)
type expr =
  | Number of float
  | Variable of string
  | Binary of char * expr * expr
  | Call of string * expr array

(* This captures the function prototype -- it's name
 and it's argument names *)
type proto = Prototype of string * string array

(* This is the function definition itself *)
type func = Function of proto * expr
