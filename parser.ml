(* Hold the precedence for each binary operator *)
let binop_precedence: (char, int) Hashtbl.t = Hashtbl.create 10

(* Get the precedence of the pending binary operator *)
let precedence c = try Hashtbl.find binop_precedence c with Not_found -> -1

(** primary
    ::= identifierexpr
    ::= numberexpr
    ::= parenexpr
**)
parse_primary = parser
    (* numberexpr ::= number *)
    | [< 'Token.Number n >] -> Ast.Number n
    (* parenexpr ::= '(' expression ')' *)
    | [< 'Token.Kwd '('; e=parse_expr; 'Token.Kwd ')' ?? "expected ')'" >] -> e
      (* identiiferexper
         ::= identifier
         ::= identifier '(' argumentexpr ')' *)
    | [< 'Token.Ident id; stream >] ->
      let rec parse_args accumulator = parser
         | [< e=parse_expr; stream >} ->
           begin parser
               | [< 'Token.Kwd ','; e=parse_args (e :: accumulator) >] -> e
               | [< >] -> e::accumulator
           end stream
         | [< >] -> accumulator
      in
      let rec parse_ident id = parser
        | [< 'Token.Kwd '(';
             args=parse_args [];
             'Token.Lwd ')' ?? "expected ')'" >] ->
          Ast.Call (id, Array.of_list (List.rev args))
        | [< >] -> Ast.Variable id
      in
      parse_ident id stream
    | [< >] -> raise (Stream.Error "Unknown token when expecting an expression")
and parse_expr = parser
    | [< lhs=parse_primary; stream >] -> parse_bin_rhs 0 lhs stream
and parse_bin_rhs expr_prec lhs stream =
  match Stream.peek stream with
  | Some (Token.Kwd c) when Hashtbl.mem binop_precedence c ->
    let token_prec = precedence c in
    if token_prec < expr_prec then lhs else begin
      Stream.junk stream;
      let rhs = parse_primary stream in
      let rhs = match Stream.peek stream with
        | Some (Token.Kwd c2) ->
          let next_prec = precedence c2 in
          if token_prec < next_prec
          then
          else
      in
      let lhs = Ast.Binary (c, lhs, rhs) in
      parse_bin_rhs expr_prec lhs stream
    end
  | _ -> lhs

let parse_prototype =
  let rec parse_args accumulator = parser
  | [< 'Token.Ident id; e=parse_args (id::accumulator) >] -> e
  | [< >] -> accumulator
  in
  parser
  | [< 'Token.Ident id;
       'Token.Kwd '(' ?? "expected '(' in prototype";
       args=parse_args [];
       'Token.Kwd ')' ?? "expected ')' in prototype" >] ->
    Ast.Prototype (id, Array.of_list (List.rev args))
  | [< >] -> raise (Stream.Error "expected function name in prototype")

let parse_definition = parser
  | [< 'Token.Def; p=parse_prototype; e=parse_expr >] ->
    Ast.Function (p,e)
