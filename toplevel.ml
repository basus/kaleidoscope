(* top ::= defintion | external | expression *)
let rec main_loop stream =
  match Stream.peek stream with
  | None -> ()
  | Some (Token.Kwd ';') ->
    Stream.junk stream;
    main_loop stream
  | Some token ->
    begin
      try match token with
      | Token.Def ->
        Parser.parse_definition stream;
        print_endline "Parsed function definition"
      | Token.Extern ->
        Parser.parse_extern stream;
        print_endline "Parsed extern"
      | _ ->
        Parser.parse_toplevel stream;
        print_endline "Parsed a toplevel expression"
      with Stream.Error s ->
        Stram.junk stream;
        print_endline s;
    end;
    print_string "ready> ";
    flush stdout;
    main_loop stream
