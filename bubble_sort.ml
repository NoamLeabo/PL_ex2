open Ast_sol
open Nos_sol
open Semantics_sol

let print_values values =
  List.iter (fun (var, value) -> Printf.printf "%s = %d\n" var value) values;
  Printf.printf "----------------\n"

let rec bubble_pass values changed =
  match values with
  | (v1, x1)::(v2, x2)::rest ->
      if x1 > x2 then
        let new_list = (v1, x2) :: (v2, x1) :: (bubble_pass rest true) in
        new_list
      else
        (v1, x1) :: (bubble_pass ((v2, x2) :: rest) changed)
  | _ -> values (* End of the list *)


let rec bubble_sort values =
  let sorted_values = bubble_pass values false in
  if sorted_values = values then
    values (* No changes, sorting done *)
  else
    bubble_sort sorted_values


let vars = [
  ("x1", 3);
  ("x2", 5);
  ("x3", 1);
  ("x4", -2);
  ("x5", 100)
]


let () =
  Printf.printf "Initial Values:\n";
  print_values vars;

  let sorted_vars = bubble_sort vars in

  Printf.printf "Sorted Values:\n";
  print_values sorted_vars;
