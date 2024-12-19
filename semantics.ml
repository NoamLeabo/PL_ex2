open Ast

let rec pow b e = match e with
                  | 0 -> 1
                  | 1 -> b  
                  | num -> b * (pow b (num-1));;

(* solve_a: aexp -> state -> int *) 
let rec solve_a e s = match e with
                      | Num n -> n
                      | Var v -> s v
                      | Add (e1, e2) -> solve_a e1 s + solve_a e2 s
                      | Mult (e1, e2) -> solve_a e1 s * solve_a e2 s
                      | Sub (e1, e2) -> solve_a e1 s - solve_a e2 s
                      | Shl (e1, e2) -> solve_a e1 s * (pow 2 (solve_a e2 s))
                      | Shr (e1, e2) ->
                        let divisor = pow 2 (solve_a e2 s) in
                        if divisor = 0 then raise (Division_by_zero)
                        else solve_a e1 s / divisor
                        ;;

(* solve_b: bexp -> state -> bool *) 
let rec solve_b e s = match e with
                      | True -> "tt"
                      | False -> "ff"
                      | Aeq (e1, e2) -> if ((solve_a e1 s) = (solve_a e2 s))
                                          then "tt"
                                          else "ff" 
                      | Beq (e1, e2) -> if ((solve_b e1 s) = (solve_b e2 s))
                                          then "tt"
                                          else "ff" 
                      | Gte (e1, e2) -> if (solve_a e1 s >= solve_a e2 s)
                                          then "tt"
                                          else "ff" 
                      | Neg e1 -> if ((solve_b e1 s) = "tt")
                                          then "ff"
                                          else "tt" 
                      | And (e1, e2) -> if (((solve_b e1 s) = "tt") && ((solve_b e2 s) = "tt")) 
                                          then "tt"
                                          else "ff";;


(* state update : to get a new state : -> *) 
let update x e s = fun y -> if y=x then solve_a e s else s y;; 

exception NotFound of string 
let default_state x = (* 0, default value? *)
raise (NotFound ("undefined variable: " ^ x));; 

(* example of an initial state *) 
let s0 = update "x" (Num 1) default_state;; 
let s1 = update "x" (Num 5) default_state;; 