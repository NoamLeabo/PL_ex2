open Ast
open Nos
open Semantics
open Printf
    
let run_bubble_sort n =
  (* Generate variable names "x1", "x2", ..., "xn" *)
  let var_name i = "x" ^ string_of_int i in

  (* Initialize variables x1, x2, ..., xn with distinct values based on a formula.
     The formula (13 * (n - i + 1)) mod (10 * n) ensures unique values for sorting. *)
  let rec init_vars i =
    if i = 0 then Skip  (* Base case: no variables to initialize. *)
    else
      Comp (Ass (var_name i, Num ((13 * (n - i + 1)) mod (10 * n))), init_vars (i - 1))
      (* Assign a value to variable xi and recursively initialize the rest. *)
  in

  (* Swap logic for two variables xi and xj:
     - Use a temporary variable "temp" to store the value of xi.
     - Assign xj's value to xi.
     - Assign "temp"'s value to xj, completing the swap. *)
  let swap_logic xi xj =
    Comp (Ass ("temp", Var xi),
          Comp (Ass (xi, Var xj),
                Ass (xj, Var "temp")))
  in

  (* Generate all pairwise comparisons and swaps:
     - Iterate through all pairs of variables (i, j) where i < j.
     - If xi >= xj, swap their values using swap_logic. *)
  let rec generate_if_statements i j =
    if i > n then Skip  (* Base case: no more variables to compare. *)
    else if j > n then generate_if_statements (i + 1) (i + 1)
    (* Move to the next variable if j exceeds n. Start the next row of comparisons. *)
    else if i >= j then generate_if_statements i (j + 1)
    (* Skip invalid pairs where i >= j to avoid redundant or invalid comparisons. *)
    else
      Comp (
        If (Gte (Var (var_name i), Var (var_name j)),  (* Compare xi and xj. *)
            swap_logic (var_name i) (var_name j),     (* Swap if xi >= xj. *)
            Skip                                      (* Do nothing otherwise. *)
           ),
        generate_if_statements i (j + 1)              (* Continue to the next comparison. *)
      )
  in

  (* Combine initialization of variables and pairwise comparisons into the full bubble sort program. *)
  let bubble_sort_program =
    Comp (
      init_vars n,                (* Initialize all variables with their initial values. *)
      generate_if_statements 1 2  (* Generate all the required comparisons and swaps. *)
    )
  in

  (* Print the values of all variables in a formatted way:
     - Loop through all variables x1, x2, ..., xn and print their values. *)
  let print_values s =
    for i = 1 to n do
      let value = s (var_name i) in  (* Retrieve the value of variable xi from the state. *)
      Printf.printf "x%d = %d; " i value  (* Print the variable name and value. *)
    done;
    print_endline ""  (* Add a newline after printing all variables. *)
  in

  (* Run the program and return the final state:
     - Execute the initialization and print the state before sorting.
     - Execute the bubble sort program and print the state after sorting. *)
  let final_state =
    let initial_state = nos (init_vars n, default_state) in
    print_endline "Before sorting:";
    print_values initial_state;  (* Print the state before sorting. *)
    let sorted_state = nos (bubble_sort_program, initial_state) in
    print_endline "After sorting:";
    print_values sorted_state;   (* Print the state after sorting. *)
    sorted_state                 (* Return the final state. *)
  in

  final_state
;;

(* Run bubble sort for n elements with n = 10 as an example. *)
let () = ignore (run_bubble_sort 10)
