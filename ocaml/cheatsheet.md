
## OCaml cheatsheet

### 0) Running stuff

```bash
utop                 # REPL
ocamlc main.ml -o a.out     # bytecode
ocamlopt main.ml -o a.out   # native

dune init proj foo
dune build
dune exec foo
dune utop             # REPL with project loaded
```

---

### 1) Basics: values, functions, let-binding

```ocaml
let x = 3
let y = x + 2

let add a b = a + b          (* curried *)
let add' (a, b) = a + b      (* tuple arg *)

let inc = add 1              (* partial application *)
inc 10                        (* 11 *)

(* anonymous functions *)
List.map (fun x -> x + 1) [1;2;3]

(* local bindings *)
let area r =
  let pi = 3.14159 in
  pi *. r *. r
```

---

### 2) Types (and the “weird” ones)

```ocaml
(* Annotating *)
let (x : int) = 3
let (f : int -> int) = fun x -> x + 1

(* Common types *)
int        (* 1, 2, 3 *)
float      (* 1.0, 2.5  -- float ops use +. *. etc *)
bool       (* true/false *)
char       (* 'a' *)
string     (* "hi" *)
unit       (* ()  like void *)

(* Function types are right-associative *)
int -> int -> int    (* int -> (int -> int) *)
```

---

### 3) Conditionals & boolean ops

```ocaml
if x > 0 then "pos" else "non-pos"

(* booleans *)
&&  ||  not
=   <>    (* structural equality / inequality *)
==  !=    (* physical equality / inequality; rarely what you want *)
```

---

### 4) Pattern matching (the bread and butter)

```ocaml
match xs with
| [] -> 0
| x :: rest -> x + List.length rest

(* guards *)
match x with
| n when n >= 0 -> n
| n -> -n

(* destructuring *)
let (a, b) = (1, "hi")
let x :: xs = [1;2;3]   (* raises if empty; prefer match *)
```

---

### 5) Lists (immutable)

```ocaml
let xs = [1;2;3]
1 :: xs              (* cons *)
xs @ [4;5]           (* concat *)

List.map (fun x -> x * x) xs
List.filter (fun x -> x mod 2 = 0) xs
List.fold_left ( + ) 0 xs
List.length xs
List.rev xs

List.mem 2 xs
List.nth xs 0         (* raises if out of range *)

(* common idiom: pipeline *)
xs |> List.map (fun x -> x + 1) |> List.filter (fun x -> x > 2)
```

---

### 6) Options and Results (avoid exceptions)

```ocaml
(* Option *)
let hd_opt = function
| [] -> None
| x::_ -> Some x

match hd_opt xs with
| None -> "empty"
| Some x -> string_of_int x

Option.map (fun x -> x + 1) (Some 3)     (* Some 4 *)
Option.value ~default:0 None             (* 0 *)

(* Result *)
let parse_int s =
  try Ok (int_of_string s) with
  | Failure _ -> Error "bad int"

match parse_int "123" with
| Ok n -> n
| Error msg -> failwith msg
```

---

### 7) Records (named fields)

```ocaml
type user =
  { id : int
  ; name : string
  ; age : int
  }

let u = { id = 1; name = "Ada"; age = 20 }

u.name                      (* "Ada" *)
let u2 = { u with age = u.age + 1 }   (* copy-update *)

(* pattern match record *)
let greet { name; _ } = "Hi " ^ name
```

---

### 8) Variants / ADTs (sum types)

```ocaml
type shape =
  | Circle of float
  | Rect of float * float

let area = function
| Circle r -> 3.14159 *. r *. r
| Rect (w, h) -> w *. h
```

---

### 9) Recursive types (trees etc.)

```ocaml
type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree

let rec size = function
| Leaf _ -> 1
| Node (l, r) -> size l + size r
```

---

### 10) Modules (namespacing) and opening

```ocaml
module Math = struct
  let square x = x * x
end

Math.square 5

open List
map (fun x -> x + 1) [1;2;3]   (* now List. prefix not needed *)
```

(Real projects rely heavily on modules; OCaml’s module system is a “superpower”.)

---

### 11) References (mutable variables) — use sparingly

```ocaml
let r = ref 0
r := !r + 1
!r                      (* 1 *)
```

---

### 12) Strings (and bytes gotcha)

```ocaml
String.length "abc"           (* 3 *)
"ab" ^ "cd"                   (* "abcd" *)

String.sub "hello" 1 3        (* "ell" *)
String.split_on_char ' ' "a b c"  (* ["a";"b";"c"] *)

(* Strings are immutable; mutable string-like is Bytes *)
```

---

### 13) Printing & formatting

```ocaml
print_endline "hi"
print_int 3; print_newline ()

Printf.printf "x=%d y=%s\n" 3 "wow"
(* %d int, %f float, %s string *)
```

---

### 14) Exceptions (exist, but prefer Option/Result)

```ocaml
(* raising *)
failwith "nope"

(* catching *)
try
  int_of_string "xx"
with
| Failure _ -> 0
```

---

### 15) Iteration (for/while) + unit

```ocaml
for i = 1 to 5 do
  Printf.printf "%d\n" i
done

let i = ref 0 in
while !i < 3 do
  incr i
done

(* many “do something” functions return unit () *)
```

---

### 16) Common “operator” reminders

```ocaml
(* ints *)    +  -  *  /  mod
(* floats *)  +. -. *. /. **

(* compare *) < <= > >=
(* strings *) ^   (* concat *)
```

---

