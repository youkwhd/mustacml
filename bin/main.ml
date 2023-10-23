(* Entry function *)
let () =
  let ht = Hashtbl.create 10 in
  Hashtbl.add ht "name" "Jackalope";
  (* I thought Hashtbl has any type *)
  Hashtbl.add ht "isok" "__true";

  List.iter print_string (Mustacml.parse (Mustacml.generate_from_string "Hello {{name}} {{#isok}}what the fuck{{/isok}}") ht)
