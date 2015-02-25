(* Lexer tokens *)

(* The lexer returns these 'Kwd' if it is an unknown character, otherwise one of *)
(* these others for known things *)

type token =
  | Def | Extern                       (* Commands *)
  | Ident of string | Number of float  (* primaries *)
  | Kwd of char                        (* Unknowns *)
