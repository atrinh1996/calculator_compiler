# README

## Purpose
Lexer/Parser and Semantic checker for a simple calculator language that uses
simple arithmetic, assignment, and operates on int and float types. 

Math statement given by:

> 3 + 5;
>
> a = 3.5F;
>
> b = 10;
>
> c = 5 * b;

*each expression must have semicolon at end.*
*see `scanner.ml` for float formats.*

## Compile
> `make toplevel.native`

## Run
Please use the following test input file with the following flags. Successful run with output the expressions. 

*run with no arguments*
> `./toplevel.native`

*prints the ast*
> `./toplevel.native -a test-maths.arith`

*outputs type checked sast*
> `./toplevel.native -s test-maths.arith`



## TODO
- complete `codegen.ml` to create the LLVM.
- clean `semant.ml`. Three main sections essentially repeat code.
