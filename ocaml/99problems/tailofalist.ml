let rec last lst = match lst with [] -> None | x :: [] -> Some x | _ :: xs -> last xs

let rec last_two lst = match lst with
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: xs -> last_two xs

(* Test cases *)

let test1 = []
let test2 = [1; 2; 3; 4; 5]
let test3 = [1]

let () =
  (match last test1 with
   | None -> print_endline "Test1: None"
   | Some x -> Printf.printf "Test1: Some %d\n" x);

  (match last test2 with
   | None -> print_endline "Test2: None"
   | Some x -> Printf.printf "Test2: Some %d\n" x)

let () =
    (match last_two test1 with
     | None -> print_endline "Test1: None"
     | Some (x, y) -> Printf.printf "Test1: Some (%d, %d)\n" x y);
    
    (match last_two test2 with
     | None -> print_endline "Test2: None"
     | Some (x, y) -> Printf.printf "Test2: Some (%d, %d)\n" x y);
    
    (match last_two test3 with
     | None -> print_endline "Test3: None"
     | Some (x, y) -> Printf.printf "Test3: Some (%d, %d)\n" x y)

