open Ast
open Nos
open Semantics

let run_bubble_sort n =
  (* Generate variable names "X1", "X2", ..., "Xn" *)
  let var_name i = "x" ^ string_of_int i in

  (* Initialize variables X1 = n, X2 = n-1, ..., Xn = 1 *)
  let rec init_vars i =
    if i = 0 then Skip
    else Comp (Ass (var_name i, Num i), init_vars (i - 1))
  in

  (* Generate the swap logic for two variables xi and xj *)
  let swap_logic xi xj =
    Comp (Ass ("temp", Var xi),                   (* temp = xi *)
    Comp (Ass (xi, Var xj),                      (* xi = xj *)
          Ass (xj, Var "temp")))                 (* xj = temp *)
  in

  (* Inner loop: while j <= n, compare and swap xi and xj if needed *)
  let inner_loop i =
    While (Neg (Gte (Var "j", Num (n + 1))),     (* while j < n+1 *)
      Comp (
        If (Gte (Var (var_name i), Var ("x" ^ string_of_int i)),  (* if xi >= xj *)
            swap_logic (var_name i) ("x" ^ string_of_int i),     (* swap xi, xj *)
            Skip                                      (* else: do nothing *)
        ),
        Ass ("j", Add (Var "j", Num 1))          (* j = j + 1 *)
      )
    )
  in

  (* Outer loop: while i < n, iterate through elements *)
  let rec outer_loop i =
    If (Gte (Num i, Num n),                      (* if i >= n *)
        Skip,                                    (* then: do nothing *)
        Comp (                                   (* else: run the bubble sort logic *)
          Ass ("i", Num i),                      (* i = current iteration *)
          While (Neg (Gte (Var "i", Num n)),     (* while i < n *)
            Comp (
              Ass ("j", Add (Var "i", Num 1)),   (* j = i + 1 *)
              Comp (
                inner_loop i,                    (* Perform inner loop *)
                Ass ("i", Add (Var "i", Num 1))  (* i = i + 1 *)
              )
            )
          )
        )
    )
  in

  (* Combine everything: initialization and the bubble sort logic *)
  Comp (
    init_vars n,         (* Initialize the variables *)
    outer_loop 1         (* Run the bubble sort algorithm *)
  )
;;
