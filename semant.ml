(* Semantic cheching for math arith compiler  *)

open Ast 
open Sast 

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if 
   successful, throws exception o.w.

   Check each expression  *)

(* Given a Ast.exp list, needs to return a (Ast.typ * Sast.s_x) list *)
let check expr_list =

    (* 1. Check each var's scope. Create a name -> type mapping. Type check binop *)
    let symbols = 

        let type_of_identifier s env =
            try StringMap.find s env
            with Not_found -> raise (Failure ("undeclared identifier " ^ s))
        in

        (* Returns type of the expression given the current environment *)
        let rec typeof env expr = 
            match expr with 
              Iliteral _ -> Int
            | Fliteral _ -> Float 
            | Unop(uo, e) as ex -> 
                let t = typeof env e in 
                let ty = 
                    match uo with  
                      Neg when t = Int || t = Float -> t 
                    | _ -> raise (
                        Failure ("illegal unary operator " ^ 
                                        string_of_uop uo ^ string_of_typ t ^ 
                                        " in " ^ string_of_exp ex))
                in ty
            | Var s -> (* StringMap.find s env  *)
                type_of_identifier s env
            | Asn(s, e) -> typeof env e

            (* Both expressions must be of same type *)
            | Binop(e1, o, e2) as ex -> 
                let t1 = typeof env e1 and t2 = typeof env e2 in 
                let same = t1 = t2 in 
                let ty = 
                    match o with 
                      Add | Sub | Mul | Div when same && t1 = Int -> Int 
                    | Add | Sub | Mul | Div when same && t1 = Float -> Float 
                    | _ -> raise (
                        Failure ("illegal binary operator " ^ 
                                    string_of_typ t1 ^ " " ^ 
                                    string_of_op o ^ " " ^ 
                                    string_of_typ t2 ^ " in " ^ 
                                    string_of_exp ex))
                in ty 
        in 

        (* Collect a mapping of names to types *)
        let rec envbuilder env expr = 
            match expr with 
              Iliteral n -> env 
            | Fliteral s -> env 
            | Unop(uo, e) -> envbuilder env e
            | Asn(s, e) -> StringMap.add s (typeof env e) env
            | Var s ->  if StringMap.mem s env 
                            then env
                        else StringMap.add s Void env
            | Binop(e1, o, e2) as ex -> 
                (* let env1 = envbuilder env e1 in 
                let env2 = envbuilder env1 e2 in 
                let _ = typeof env2 ex in 
                env2  *)
                let t1 = typeof env e1 and t2 = typeof env e2 in 
                let same = t1 = t2 in 
                let _ = 
                    match o with 
                      Add | Sub | Mul | Div when same && t1 = Int -> Int 
                    | Add | Sub | Mul | Div when same && t1 = Float -> Float 
                    | _ -> raise (
                        Failure ("illegal binary operator " ^ 
                                    string_of_typ t1 ^ " " ^ 
                                    string_of_op o ^ " " ^ 
                                    string_of_typ t2 ^ " in " ^ 
                                    string_of_exp ex))
                in env
        in List.fold_left envbuilder StringMap.empty expr_list

    in 

    (********************* Part 1 Print check ******************************)
    (* let names_list = 
        StringMap.fold (fun k v pairlist -> (k, v) :: pairlist) symbols []
    in
    let print_tup tup = 
        print_string (fst tup);
        print_endline (" : " ^ string_of_typ (snd tup))
    in List.iter print_tup names_list *)
    (***********************************************************************)

    (* 2. Type check *)

    (* Returns the Ast.typ of the given name s *)
    let type_of_id s =
        try StringMap.find s symbols
        with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Raises error if lval and rval are not the same types *)
    let check_assign lvaltyp rvaltyp err = 
        if lvaltyp = rvaltyp then lvaltyp else raise (Failure err)
    in

    (* Create an Sexpr from Expr *)
    let rec expr_sexpr = function
          Iliteral n -> (Int, SIliteral n)
        | Fliteral s -> (Float, SFliteral s)
        | Unop(uo, e) -> 
            let (t, e') = expr_sexpr e 
            in (t, SUnop(uo, (t, e')))
        | Asn(var, e) as ex -> 
            let lt = type_of_id var 
            and (rt, e') = expr_sexpr e in 
            let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
                string_of_typ rt ^ " in " ^ string_of_exp ex 
            in (check_assign lt rt err, SAsn(var, (rt, e')))
        | Var s -> (type_of_id s, SVar s)
        | Binop(e1, o, e2) as ex -> 
            let (t1, e1') = expr_sexpr e1
            and (t2, e2') = expr_sexpr e2 in 
            let err = "incompatible types in binary operator " ^ 
                        string_of_typ t1 ^ " " ^ string_of_op o ^ " " ^
                        string_of_typ t2 ^ " in " ^ string_of_exp ex 
            in (check_assign t1 t2 err, SBinop((t1, e1'), o,(t2, e2')))

    in List.map expr_sexpr expr_list 

    (* let expr environ =  *)

        (* let type_of_identifier s env =
            try StringMap.find s env
            with Not_found -> raise (Failure ("undeclared identifier " ^ s))
        in

        (* Raises error if lval and rval are not the same types *)
        let check_assign lvaltyp rvaltyp err = 
            if lvaltyp = rvaltyp then lvaltyp else raise (Failure err)
        in

        (* Returns type of the expression given the current environment *)
        let rec typeof env expr = 
            match expr with 
              Iliteral _ -> Int
            | Fliteral _ -> Float 
            | Unop(uo, e) as ex -> 
                let t = typeof env e in 
                let ty = 
                    match uo with  
                      Neg when t = Int || t = Float -> t 
                    | _ -> raise (
                        Failure ("illegal unary operator " ^ 
                                        string_of_uop uo ^ string_of_typ t ^ 
                                        " in " ^ string_of_exp ex))
                in ty
            | Var s -> (* StringMap.find s env  *)
                type_of_identifier s env
            | Asn(s, e) -> typeof env e

            (* Both expressions must be of same type *)
            | Binop(e1, o, e2) as ex -> 
                let t1 = typeof env e1 and t2 = typeof env e2 in 
                let same = t1 = t2 in 
                let ty = 
                    match o with 
                      Add | Sub | Mul | Div when same && t1 = Int -> Int 
                    | Add | Sub | Mul | Div when same && t1 = Float -> Float 
                    | _ -> raise (
                        Failure ("illegal binary operator " ^ 
                                    string_of_typ t1 ^ " " ^ 
                                    string_of_op o ^ " " ^ 
                                    string_of_typ t2 ^ " in " ^ 
                                    string_of_exp ex))
                in ty 
        in 

        let rec expr_sexpr env expr = match expr with 
              Iliteral n -> (Int, SIliteral n)
            | Fliteral s -> (Float, SFliteral s)
            | Unop(uo, e) -> 
                let (t, e') = expr_sexpr e 
                in (t, SUnop(uo, (t, e')))
            | Asn(var, e) as ex -> 
                let env = StringMap.add var (typeof env e) env in 
                let lt = type_of_identifier var env 
                and (rt, e') = expr_sexpr e in 
                let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
                    string_of_typ rt ^ " in " ^ string_of_exp ex 
                in (check_assign lt rt err, SAsn(var, (rt, e')))
            | Var s -> (type_of_identifier s env, SVar s)
            | Binop(e1, o, e2) as ex -> 
                let (t1, e1') = expr_sexpr e1
                and (t2, e2') = expr_sexpr e2 in 
                let same = t1 = t2 in 
                let ty = 
                    match o with 
                      Add | Sub | Mul | Div when same && t1 = Int -> Int 
                    | Add | Sub | Mul | Div when same && t1 = Float -> Float 
                    | _ -> raise (
                        Failure ("illegal binary operator " ^ 
                                    string_of_typ t1 ^ " " ^ 
                                    string_of_op o ^ " " ^ 
                                    string_of_typ t2 ^ " in " ^ 
                                    string_of_exp ex))
                in (ty, SBinop((t1, e1'), o,(t2, e2')))

        in List.map (expr_sexpr StringMap.empty) expr_list  *)

    (* in expr StringMap.empty *)


    (* List.map expr_sexpr expr_list *)

