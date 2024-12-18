open Ast
open Nos
open Semantics
open Printf
    
let run_bubble_sort n =
  (* Generate variable names "x1", "x2", ..., "xn" *)
  let var_name i = "x" ^ string_of_int i in

  (* Initialize variables x1 = n, x2 = n-1, ..., xn = 1 *)
  let rec init_vars i =
    if i = 0 then Skip
    else Comp (Ass (var_name i, Num ((19*(n-i+1)) mod (2*n))), init_vars (i - 1))
  in

  (* Swap logic for two variables xi and xj *)
  let swap_logic xi xj =
    Comp (Ass ("temp", Var xi),
          Comp (Ass (xi, Var xj),
                Ass (xj, Var "temp")))
  in

  (* Generate all pairwise comparisons and swaps (n^2 if-statements) *)
  let rec generate_if_statements i j =
    if i > n then Skip
    else if j > n then generate_if_statements (i + 1) (i + 1)
    else if i >= j then generate_if_statements i (j + 1)
    else
      Comp (
        If (Gte (Var (var_name i), Var (var_name j)),
            swap_logic (var_name i) (var_name j),
            Skip
           ),
        generate_if_statements i (j + 1)
      )
  in

  (* Combine initialization and explicit pairwise comparisons *)
  let bubble_sort_program =
    Comp (
      init_vars n,
      generate_if_statements 1 2
    )
  in

  (* Print variables' values in a formatted way *)
  let print_values s =
    for i = 1 to n do
      let value = s (var_name i) in
      Printf.printf "x%d = %d; " i value
    done;
    print_endline ""
  in

  (* Run the program and return the final state *)
  let final_state =
    let initial_state = nos (init_vars n, default_state) in
    print_endline "Before sorting:";
    print_values initial_state;
    let sorted_state = nos (bubble_sort_program, initial_state) in
    print_endline "After sorting:";
    print_values sorted_state;
    sorted_state
  in

  final_state
;;

(* Run bubble sort for n elements *)
let () = ignore (run_bubble_sort 8)
