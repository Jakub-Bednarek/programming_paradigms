type 'a t = 'a list * 'a list
exception Empty of string

let empty() = ([], [])

let enqueue(x, q) =
    match q with
        | ([], []) -> ([x], [])
        | (l1, l2) -> (l1, x :: l2)

let dequeue(q) = 
    match q with
    | (_ :: tl, l2) -> if tl = [] && l2 = [] then ([], [])
                      else if tl = [] then (List.rev l2, [])
                      else (tl, l2)
    | ([], []) -> raise (Empty "Queue_double_list: dequeue")

let first(q) =
    match q with
    | (hd :: _, _) -> hd
    | ([], []) -> raise (Empty "Queue_double_list: first")

let isEmpty(q) =
    match q with
    | ([], []) -> true
    | (_, _) -> false