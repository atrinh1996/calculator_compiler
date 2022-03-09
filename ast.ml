(* AST 3/9/2022 *)

type op = Add | Sub | Mul | Div
type uop = Neg

type typ = 
      Int 
    | Float 
    | Void

type exp = 
      Iliteral of int
    | Fliteral of string
    | Binop of exp * op * exp 
    | Unop of uop * exp 
    (* | Seq of exp * exp  *)
    | Asn of string * exp
    | Var of string 

type prog = exp list 

(*********************** Pretty Print ***********************)
let string_of_op = function
      Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"

let string_of_typ = function
      Int -> "int"
    | Float -> "float"
    | Void -> "void"

let string_of_uop = function
    Neg -> "-"

let rec string_of_exp = function
      Iliteral(n) -> string_of_int n
    | Fliteral(s) -> s 
    | Binop(e1, o, e2) -> 
        string_of_exp e1 ^ " " ^ string_of_op o ^ " " ^ string_of_exp e2
    | Unop(uo, e) -> string_of_uop uo ^ string_of_exp e
    (* | Seq(e1, e2) -> string_of_exp e1 ^ ", " ^ string_of_exp e2 *)
    | Asn(s, e) -> s ^ " = " ^ string_of_exp e
    | Var(s) -> s

let string_of_prog es = 
    String.concat "\n" (List.map string_of_exp es) ^ "\n"
(************************************************************)

(* type floater =
    Float of string

type floaters = floater list

let string_of_float = function
    Float(n) -> n

let string_of_floats fs = String.concat "\n" (List.map string_of_float fs) ^ "\n"
*)