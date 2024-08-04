[@@@warnerror "-unused-value-declaration"]
[@@@warnerror "-unused-extension"]


open Effect
open Effect.Deep



(* 2.1 Input & output *)

type _ Effect.t += Print: string -> unit t
type _ Effect.t += Read: unit -> string t

let print_full_name () =
  perform (Print "What is your forename ?");
  let forename = perform (Read ()) in
  perform (Print "What is your surname ?");
  let surname = perform (Read ()) in
  perform (Print (forename ^ " " ^ surname))

let handle_print = {effc = fun (type a) (eff: a t) ->
  match eff with
  | Print s -> Some (fun (k: (a, _) continuation) ->
      print_endline s;
      continue k ())
  | _ -> None
 }

let try_with_basic comp arg handler =
  try_with (fun () -> try_with comp arg handler) () handle_print


(* Combine two effect handlers *)
let (++) ({effc = effc1} : 'c effect_handler) ({effc = effc2} : 'c effect_handler) : 'c effect_handler = {
      effc = fun (type a) (eff: a t) -> match effc1 eff with
        | Some f -> Some f
        | None -> effc2 eff
    }

(* 2.1.1 Constant input *)

let always_read (type c) (s: string) : c effect_handler  = { effc = fun (type a) (eff: a t) ->
    match eff with
    | Read () -> Some (fun (k: (a, c) continuation) ->
        continue k s)
    | _ -> None
}

let _ =
  print_endline "2.1.1 Constant input: ";
  try_with print_full_name () ((always_read "Bob") ++ handle_print)


(* 2.1.2 Reversed output *)
let reverse = {
  effc = fun (type a) (eff: a t) -> match eff with
  | Print s -> Some (fun (k : (a, _) continuation) ->
      continue k ();
      perform (Print s))
  | _ -> None
}

let abc () =
  perform (Print "A");
  perform (Print "B");
  perform (Print "C")

let _ =
  print_endline "\n2.1.2 Reversed output: ";
  try_with_basic abc () reverse

(* 2.1.3  Collecting output *)


(* let collect = {
  effc = fun (type a) (eff: a t) -> match eff with
  | Print s -> Some (fun (k : (a, _) continuation) ->
      let acc = continue k () in
      s ^ acc)
  | _ -> None
} *)

let collect = {
  retc = (fun v -> (v, ""));
  exnc = raise;
  effc = (fun (type a) (eff: a t) -> match eff with
    | Print s -> Some (fun (k : (a, _) continuation) ->
        let (x, acc) = continue k () in
        (x, s ^ acc))
    | _ -> None);
}

let _ = print_endline "\n2.1.3 Collecting output: "
let output = match_with (fun () -> try_with abc () reverse) () collect
let _ = print_endline (snd output)

let collect' = {
  retc = (fun x -> fun acc -> (x, acc));
  exnc = raise;
  effc = (fun (type a) (eff: a t) -> match eff with
  | Print s -> Some (fun (k : (a, _) continuation) ->
      (fun acc -> continue k () (acc ^ s)))
  | _ -> None)
}

let output = match_with (fun () -> try_with abc () reverse; Fun.id) () collect' ""
let _ = print_endline (snd output)

(* 2.2 Exceptions *)
type _ Effect.t += Raise: exn -> 'a t

let default x =  {
  exnc = Fun.const x;
  retc = Fun.id;
  effc = fun (type a) (_eff: a t) -> None
}

let output = match_with (fun () -> raise (Invalid_argument "invalid")) () (default "default")
let _ = print_endline "\n2.2 Exceptions:"
let _ = print_endline output

(* 2.3 Non-determinism *)

type _ Effect.t += Decide: unit -> bool t

let choose x y =
  let b = perform (Decide ()) in
  if b then x else y

let pick_true = {
  effc = fun (type a) (eff: a t) -> match eff with
  | Decide () -> Some (fun (k : (a, _) continuation) ->
      continue k true)
  | _ -> None
}

let choose_diff () =
  let x1 = choose 15 30 in
  let x2 = choose 5 10 in
  x1 - x2 ;;

let output = try_with_basic choose_diff () pick_true

let _ = print_endline "\n2.3 Non-determinism:"
let _ = print_endline (Int.to_string output)


(* 2.3.1 Maximal result *)

type _ Effect.t += Choose: 'a * 'a -> 'a t

(* Slightly modified, OCaml cannot continue a continuation more than once *)
let pick_max = {
  effc = fun (type a) (eff: a t) -> match eff with
  | Choose (a, b) -> Some (fun (k : (a, _) continuation) ->
      continue k (max a b)
    )
  | _ -> None
}

let choose' x y =
  perform (Choose (x, y))

let choose_diff' () =
  let x1 = choose' 15 30 in
  let x2 = choose' 5 10 in
  x1 - x2 ;;

let output = try_with_basic choose_diff' () pick_max

let _ = print_endline "\n2.3.1 Maximal result:"
let _ = print_endline (Int.to_string output)

(* 2.3.2 Backtracking *)

(* Not possible, it requires multiple continue of continuation *)

(* type _ Effect.t += Fail: unit -> 'a t

let rec choose_int m n =
  if m > n then
    perform (Fail ())
  else
    let f = perform (Choose((fun () -> m ), (fun() -> choose_int (m + 1) n))) in
    f ()
let pythagorean m n  =
  let a = choose_int m (n - 1) in
  let b = choose_int (a + 1) n in
  let sq = Float.to_int (sqrt (Float.of_int (a*a + b*b))) in
  let is_square = sq * sq == a*a + b*b in
  if is_square then
    (a, b, sq)
  else
    perform (Fail ())
let backtrack = {
  effc = fun (type a) (eff: a t) -> match eff with
  | Choose (a, b) -> Some (fun (k : (a, _) continuation) ->
      try_with (continue k) a {
        effc = fun (type a) (eff: a t) -> match eff with
        | Fail () -> Some (fun (_ : (a, _) continuation) ->
            continue k b
          )
        | _ -> None
      }
    )
  | _ -> None
}

let (a, b, c) = try_with (pythagorean 4) 15  backtrack
let _ = print_endline "\n2.3.2 Backtracking:"
let _ = print_endline (Int.to_string a)
let _ = print_endline (Int.to_string b)
let _ = print_endline (Int.to_string c) *)