open Ast
open Nos
open Semantics

let run_bubble_sort n =
  (* Generate variable names "x1", "x2", ..., "xn" *)
  let var_name i = "x" ^ string_of_int i in

  (* Initialize variables x1 = n, x2 = n-1, ..., xn = 1 *)
  let rec init_vars i =
    if i = 0 then Skip
    else Comp (Ass (var_name i, Num (n + 1 - i)), init_vars (i - 1))
  in

  (* Swap logic for two variables xi and xj *)
  let swap_logic xi xj =
    Comp (Ass ("temp", Var xi),       (* temp = xi *)
    Comp (Ass (xi, Var xj),           (* xi = xj *)
          Ass (xj, Var "temp")))      (* xj = temp *)
  in

  (* Generate all pairwise comparisons and swaps (n^2 if-statements) *)
  let rec generate_if_statements i j =
    if i > n then Skip
    else if j > n then generate_if_statements (i + 1) (i + 1)  (* Move to next i, reset j *)
    else if i >= j then generate_if_statements i (j + 1)       (* Skip if i >= j *)
    else
      (* Compare xi and xj and swap if xi > xj *)
      Comp (
        If (Gte (Var (var_name i), Var (var_name j)),
            swap_logic (var_name i) (var_name j),
            Skip
        ),
        generate_if_statements i (j + 1)
      )
  in

  (* Combine initialization and explicit pairwise comparisons *)
  Comp (
    init_vars n,                    (* Initialize variables *)
    generate_if_statements 1 2      (* Generate all if-statements for i, j *)
  )
;;
