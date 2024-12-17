type var = string;;

type aexp = Num of int 
 | Var of var 
 | Add of aexp * aexp 
 | Mult of aexp * aexp 
 | Sub of aexp * aexp
 | Shr of aexp * aexp
 | Shl of aexp * aexp;;

type bexp = True 
 | False 
 | Aeq of aexp * aexp
 | Beq of bexp * bexp
 | Gte of aexp * aexp
 | Neg of bexp
 | And of bexp * bexp;;

type stm = Ass of var * aexp 
 | Skip 
 | Comp of stm * stm 
 | If of bexp * stm * stm 
 | While of bexp * stm
 | Repeat of stm * bexp;;

type state = var -> int ;;

(* test case*) 
let test0 = Ass ("x", Num 5);;
let test1 = Skip;;
let test2 = Comp (Ass ("x", Num 3), Ass ("x", Add(Var "x", Num 1)));; 
let test3 = If(Neg(Aeq(Var "x", Num 1)),Ass ("x", Num 3),Ass ("x", Num 7));;
let test4 = Comp (Ass("y", Num 1), While(Neg(Aeq(Var "x", Num 0)),Comp(Ass("y", Mult(Var "y", Var "x")),Ass("x", Sub(Var "x", Num 1))))) 
let test5 = Comp(
              Ass ("a", Num 84), 
              Comp(
                Ass ("b", Num 22), 
                Comp(
                  Ass ("c", Num 0), 
                  While ((Neg (Aeq(Var "b", Num 0))), 
                    Comp(
                      Ass ("a", Shl (Var "a", Num 1)), 
                      Ass ("b", Shr (Var "b", Num 1))	
					          )
				          )
			          )
		          )
	          )
let test6 = Comp(
              Ass ("x", Num 1), 
              Repeat(
                    Ass ("x", Shl(Var "x", Num 2)), 
                    Gte (Var "x" , Num 10)
                    )
              );;
let test7 = Comp
(Comp (Ass ("x5", Num 5),
  Comp (Ass ("x4", Num 4),
   Comp (Ass ("x3", Num 3),
    Comp (Ass ("x2", Num 2), Comp (Ass ("x1", Num 1), Skip))))),
If (Gte (Num 1, Num 5), Skip,
 Comp (Ass ("i", Num 1),
  While (Neg (Gte (Var "i", Num 5)),
   Comp (Ass ("j", Add (Var "i", Num 1)),
    Comp
     (While (Neg (Gte (Var "j", Num 6)),
       Comp
        (If (Gte (Var "x1", Var "x1"),
          Comp (Ass ("temp", Var "x1"),
           Comp (Ass ("x1", Var "x1"), Ass ("x1", Var "temp"))),
          Skip),
        Ass ("j", Add (Var "j", Num 1)))),
     Ass ("i", Add (Var "i", Num 1))))))));;

