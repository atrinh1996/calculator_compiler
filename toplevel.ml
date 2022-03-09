
type action = Ast | Sast | LLVM_IR

let () =
    let action = ref Ast in 
    let set_action a () = action := a in 
    let specialist = [
        ("-a", Arg.Unit (set_action Ast), "Print the AST");
        ("-s", Arg.Unit (set_action Sast), "Print the SAST");
        ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ] in
    let usage_msg = "usage: ./toplevel.native [-a|-s] [file]" in
    let channel = ref stdin in 
        Arg.parse specialist (fun filename -> channel := open_in filename) usage_msg;

let lexbuf = Lexing.from_channel !channel in 
    let ast = Parser.prog Scanner.tokenize lexbuf in 
    match !action with 
      Ast -> print_string (Ast.string_of_prog ast)
    | _ -> let sast = Semant.check ast in 

    (* Part 1 Semant Print Check *)
    (* sast *)
    (* print_string (Ast.string_of_prog ast) *)

        match !action with 
          Ast -> ()
        | Sast -> print_string (Sast.string_of_s_prog sast)
        | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate sast))