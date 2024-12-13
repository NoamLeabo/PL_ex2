[@@@ocaml.warning "-8"];;

open Ast
open Semantics

let rec nos (statment, s) = match statment with
                            | Ass (v, a) -> update v a s
                            | Skip -> s
                            | Comp (stm1, stm2) -> nos (stm2, nos (stm1, s))
                            | If (b, stm1, stm2) -> if (solve_b b s)
                                                      then nos (stm1, s)
                                                      else nos (stm2, s)
                            | While (b, stm) -> if (solve_b b s)
                                                  then nos (While (b, stm), nos (stm, s))
                                                  else s;;

(* tests *) 

print_string "x = ";;
print_int (let new_state = nos (Ast.test1, Semantics.s0) in new_state "x");;
print_endline "";;

print_string "x = ";;
print_int (let new_state = nos (Ast.test2, Semantics.s0) in new_state "x");;
print_endline "";;

print_string "x = ";;
print_int (let new_state = nos (Ast.test3, Semantics.s0) in new_state "x");;
print_endline "";;

print_string "x = ";;
print_int (let new_state = nos (Ast.test4, Semantics.s1) in new_state "x");;
print_endline "";;

print_string "y = ";;
print_int (let new_state = nos (Ast.test4, Semantics.s1) in new_state "y");;
print_endline "";;


