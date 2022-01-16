type 'a t = 'a list
exception Empty of string

let empty() = []

let enqueue(x, q) = q @ [x]

let dequeue q = 
     match q with
        _ :: tl -> tl
    |   [] -> []

let first q = 
    match q with
        hd :: _ -> hd
    |   [] -> raise (Empty "module Queue_list: first")

let isEmpty q = q = []