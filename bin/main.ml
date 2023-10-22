(* Entry function *)
let () =
  let ht = Hashtbl.create 10 in
  Hashtbl.add ht "name" "Jackalope";

  List.iter print_string (Mustacml.parse (Mustacml.generate_from_string "Hello {{name}}") ht)
