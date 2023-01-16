type term = Var of string | Abs of string * term | App of term * term

let rec eval t =
  match t with
  | Var x -> t
  | Abs (x, t1) -> Abs (x, eval t1)
  | App (t1, t2) -> (
      let t1' = eval t1 in
      let t2' = eval t2 in
      match t1' with Abs (x, t3) -> eval (subst x t2' t3) | _ -> App (t1', t2'))

and subst x t' t =
  match t with
  | Var y -> if x = y then t' else t
  | Abs (y, t1) -> if x = y then t else Abs (y, subst x t' t1)
  | App (t1, t2) -> App (subst x t' t1, subst x t' t2)

let church_numeral n =
  let rec f n =
    if n = 0 then Abs ("f", Abs ("x", Var "x"))
    else Abs ("f", Abs ("x", App (Var "f", f (n - 1))))
  in
  f n

let rec string_of_term = function
  | Var x -> x
  | Abs (x, t) -> "(λ" ^ x ^ "." ^ string_of_term t ^ ")"
  | App (t1, t2) -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"

let factorial =
  App (church_numeral 5, Abs ("f", Abs ("n", App (Var "f", Var "n"))))

let evaluated = eval factorial
let zero = church_numeral 0
let one = church_numeral 1
let two = church_numeral 2
let three = church_numeral 3
let four = church_numeral 4

let () =
  print_endline (string_of_term evaluated);
  print_endline (string_of_term zero);
  print_endline (string_of_term one);
  print_endline (string_of_term two);
  print_endline (string_of_term three);
  print_endline (string_of_term four)

(* let identity = Abs("x", Var "x")
   let example = App(identity, Abs("y", Var "y"))

   let evaluated = eval example

   let () =
   print_endline (string_of_term identity);
   print_endline (string_of_term example);
   print_endline (string_of_term evaluated) *)

(* (λf.(λx.f (x x)) (λx.f (x x))) *)
(* Y Combinator *)
