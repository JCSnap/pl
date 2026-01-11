let rec nthelem n lst = match n with 0 -> (match lst with [] ->  raise (Failure "nthelem") | x::_ -> x)
                            | _ -> (match lst with [] -> raise (Failure "nthelem") | _::xs -> nthelem (n-1) xs);;


let test1 = [];;
let test2 = [1;2;3;4;5];;

print_endline ("[1;2;3;4;5],2 " ^ string_of_int @@ nthelem 2 test2);;
print_endline ("[1;2;3;4;5],0 " ^ string_of_int @@ nthelem 0 test2);;
