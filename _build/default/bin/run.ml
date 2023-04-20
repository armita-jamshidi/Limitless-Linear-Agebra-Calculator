open Rml.Main

let () =
  print_endline "Enter the path to the file you want to run.\n";
  print_string "> ";
  let input = read_line () in
  interp_file input
