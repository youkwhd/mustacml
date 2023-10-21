let rec print_list list =
    match list with
    | [] -> ()
    | x :: xs -> Printf.printf "%s\n" x.Mustacml.text; print_list xs

let () = print_list (Mustacml.generate_from_string "Hello {{name}}")
