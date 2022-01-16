module Queue_fun: QUEUE_FUN =
struct 
    type 'a t = 'a list
    exception Empty of string

    let empty() = []

    let enqueue(x, q) = q @ [x]

    let dequeue(q) = 
        match q with
            hd :: tl -> tl
        |   [] -> raise (Empty "moudule Queue: dequeue")

    let first(q) = 
        match q with
            hd :: _ -> hd
        |   [] -> raise (Empty "module Queue: first")

    let isEmpty(q) =
        match q with
            hd :: tl -> false
        |   [] -> true
end;;