open Core
let rec pp_list ?(btwn=", ") ?(around=("", "")) f list = 
  let l, r = around in
  l ^ match list with
  | [] -> r
  | [x] -> f x ^ r
  | x :: xs -> (f x) ^ btwn ^ (pp_list ~btwn ~around:("", r) f xs)
;;

let pp_opt ?(default="") f opt = match opt with
  | None -> default
  | Some a -> f a
;;

(* repeat a string n times *)
let rec n_str ?ending s n =
  if n <= 0 then (match ending with None -> "" | Some s -> s)
  else s ^ n_str s (n - 1)
;;

(* Kind of hard to generalize. I want rows to be a list of lists*)
let pp_grid col_widths col_names rows = 
  let total_width = List.fold ~init:0 col_widths ~f:(Int.(+)) in
  let pp_row = fun row ->
    let both = List.zip col_widths row in
    match both with 
    | Unequal_lengths -> failwith "Please ensure the number of columns matches the widths provided"
    | Ok both -> List.fold_right ~init:"\n" both ~f:(fun (w, v) acc->
       sprintf "%*s" (-w) v ^ acc
    ) 
  in
  pp_row col_names ^
  n_str "-" total_width ^ "\n" ^
  (List.fold ~init:"" rows ~f:(fun acc row -> acc ^ pp_row row))
;;