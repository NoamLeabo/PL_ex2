(* solve_a: aexp -> state -> int *) 
let rec solve_a e s = match e with

 (* solve_b: bexp -> state -> bool *) 
 let rec solve_b e s = match e with


(* state update : to get a new state *) 
let update x e s = fun y -> if y=x then solve_a e s else s y;; 

exception NotFound of string 
let default_state x = (* 0, default value? *) 
 raise (NotFound "undefined variable");; 

 (* example of an initial state *) 
let s0 = update "x" (Num 1) default_state;; 
let s1 = update "x" (Num 5) default_state;; 