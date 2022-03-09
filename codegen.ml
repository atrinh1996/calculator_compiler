(* Code generation: translate takes a semantically checked AST and
produces LLVM IR 3/9/22 *)

module L = Llvm
module A = Ast 
open Sast

module StringMap = Map.Make(String)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate sexpr_list = 