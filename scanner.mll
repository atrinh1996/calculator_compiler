(* 
    scanner

    CS107 - Arith Parser

    Name: Amy Bui (abui02)
*)


{ open Parser }


let digit = ['0'-'9']

let dot = '.'
let e = 'e'
let sign = ['+' '-']
let fluff = ['f' 'F' 'l' 'L']
let decimal = digit+ dot digit* | digit* dot digit+
let exponent = (e sign? digit+)
let float_num = (decimal exponent? | digit+ exponent) fluff?


let llett = ['a'-'z']
let ulett = ['A'-'Z']
let alpha = llett | ulett
let names = alpha (alpha | '?' | '_')*

let integer = ['1'-'9']? digit+

rule tokenize = parse
      [' ' '\n' '\t' '\r']          { tokenize lexbuf }
    | '+'                           { PLUS }
    | '-'                           { MINUS }
    | '*'                           { TIMES }
    | '/'                           { DIVIDE }
    | '='                           { ASSIGN }
    | ';'                           { SEMI }
    (* | ','                           { COMMA } *)
    | float_num as lxm              { FLOAT(lxm) }
    | names as lxm                  { VARIABLE(lxm) }
    | integer as lxm                { INT(int_of_string lxm) }
    | eof                           { EOF }
    | _ as c                        { raise(Failure("Illegal character: " ^ Char.escaped c)) }