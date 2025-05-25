open Core
open Graph
let run () =
  let module Test : NodeTy with type t=int = 
  struct
    type t = int [@@deriving compare, hash, sexp]
    let to_string = Int.to_string
  end
  in let module X = Make(Test) in
  let g = X.make_from_adjlist [
    (1, Hash_set.of_list (module Int) [2;3;4])
  ; (2, Hash_set.of_list (module Int) [1;3;4;5])
  ; (3, Hash_set.of_list (module Int) [1;2;5])
  ; (1, Hash_set.of_list (module Int) [5])
  ; (4, Hash_set.of_list (module Int) [1;2])
  ; (5, Hash_set.of_list (module Int) [1;2;3])
  ] in
  print_endline (X.to_string g);
;;