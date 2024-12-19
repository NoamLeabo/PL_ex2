open Ast
open Nos
open Semantics
open Printf

let initial_values = [("x1", 124); ("x2", 94); ("x3", 53); ("x4", 901); ("x5", 49); ("x6", 3); ("x7", 71); ("x8", 13);] 
let lookup_value var_name = List.assoc var_name initial_values
    
let run_bubble_sort n =
  (* we generate variable names "x1", "x2", ..., "xn" *)
  let var_name i = "x" ^ string_of_int i in

  (* 
     Initialize variables x1, x2, ..., xn with distinct values and there are two options for this.
     OPTION 1: using the formula (13 * (n - i + 1)) mod (10 * n) which sort of ensures rand/unique values for sorting. 
     OPTION 2: using the the initial_values that were assigned, PAT-ATTENTION - You must have AT LEAST n assignments otherwise error will occure 
     You may toggle between the options by making one a comment and removing the other from being a comment     
  *)
  let rec init_vars i =
    if i = 0 then Skip  (* this is the base case: no variables to initialize. *)
    else
      Comp (Ass (var_name i, Num ((13 * (n - i + 1)) mod (10 * n))), init_vars (i - 1))
      (* OPTION 1: Assign a value to variable xi and recursively initialize the rest, using the "RAND" formula *)

      (* Comp (Ass (var_name i, Num ((lookup_value (var_name i)))), init_vars (i - 1)) *)
      (* OPTION 2: Assign a value to variable xi and recursively initialize the rest, using the USRE'S ASSIGNMENTS *)
in

  (* we creat swap logic for two variables xi and xj:
     - first use a temporary variable "temp" to store the value of xi.
     - assign xj's value to xi.
     - assign "temp"'s value to xj, completing the swap. *)
  let swap_logic xi xj =
    Comp (Ass ("temp", Var xi),
          Comp (Ass (xi, Var xj),
                Ass (xj, Var "temp")))
  in

  (* first we generate all pairwise comparisons and swaps:
     - iterate through all pairs of variables (i, j) where i < j.
     - if xi >= xj, swap their values using swap_logic. *)
  let rec generate_if_statements i j =
    if i > n then Skip  (* Base case: no more variables to compare. *)
    else if j > n then generate_if_statements (i + 1) (i + 1)
    (* we move to the next variable if j exceeds n. Start the next row of comparisons. *)
    else if i >= j then generate_if_statements i (j + 1)
    (* then skip invalid pairs where i >= j to avoid redundant or invalid comparisons. *)
    else
      Comp (
        If (Gte (Var (var_name i), Var (var_name j)),  (* compare xi and xj. *)
            swap_logic (var_name i) (var_name j),     (* swap if xi >= xj. *)
            Skip                                      (* do nothing otherwise. *)
           ),
        generate_if_statements i (j + 1)              (* then continue to the next comparison. *)
      )
  in

  (* we combine initialization of variables and pairwise comparisons into the full bubble sort program. *)
  let bubble_sort_program =
    Comp (
      init_vars n,                (* initialize all variables with their initial values. *)
      generate_if_statements 1 2  (* generate all the required comparisons and swaps. *)
    )
  in

  (* a func to print the values of all variables in a formatted way:
     - loop through all variables x1, x2, ..., xn and print their values. *)
  let print_values s =
    for i = 1 to n do
      let value = s (var_name i) in  (* first retrieve the value of variable xi from the state. *)
      Printf.printf "x%d = %d; " i value  (* now print the variable name and value. *)
    done;
    print_endline ""  (* add a newline after printing all variables. *)
  in

  (* run the program and return the final state:
     - execute the initialization and print the state before sorting.
     - execute the bubble sort program and print the state after sorting. *)
  let final_state =
    let initial_state = nos (init_vars n, default_state) in
    print_endline "Before sorting:";
    print_values initial_state;  (* print the state before sorting. *)
    let sorted_state = nos (bubble_sort_program, initial_state) in
    print_endline "After sorting:";
    print_values sorted_state;   (* print the state after sorting. *)
    sorted_state                 (* return the final state. *)
  in

  final_state
;;

(* run bubble sort for n elements with n = 8 as an example. - of course this is changable *)
let () = ignore (run_bubble_sort 8)
