let rec last lst = match lst with [] -> None | x :: [] -> Some x | _ :: xs -> last xs


let test1 = []
let test2 = [1; 2; 3; 4; 5]

let () =
  (match last test1 with
   | None -> print_endline "Test1: None"
   | Some x -> Printf.printf "Test1: Some %d\n" x);

  (match last test2 with
   | None -> print_endline "Test2: None"
   | Some x -> Printf.printf "Test2: Some %d\n" x)

