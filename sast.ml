(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast 

type s_exp = typ * s_x
and s_x = 
      SIliteral of int
    | SFliteral of string
    | SBinop of s_exp * op * s_exp 
    | SUnop of uop * s_exp 
    (* | SSeq of s_exp * s_exp  *)
    | SAsn of string * s_exp
    | SVar of string 

type s_prog = s_exp list 



(*********************** Pretty Print ***********************)
(* Semantic type checking: prints as (type : var) *)
let rec string_of_s_exp (t, e) = 
    "(" ^ string_of_typ t ^ " : " ^
    (match e with
          SIliteral(n) -> string_of_int n 
        | SFliteral(s) -> s
        | SBinop(e1, o, e2) ->
            string_of_s_exp e1 ^ " " ^ string_of_op o ^ " " ^ string_of_s_exp e2
        | SUnop(uo, e) -> string_of_uop uo ^ string_of_s_exp e
        (* | SSeq(e1, e2) -> string_of_s_exp e1 ^ ", " ^ string_of_s_exp e2 *)
        | SAsn(s, e) -> s ^ " = " ^ string_of_s_exp e
        | SVar(s) -> s
    )
    ^ ")"

let string_of_s_prog es = 
    String.concat "\n" (List.map string_of_s_exp es) ^ "\n"
(************************************************************)
