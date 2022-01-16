type 'a t = { mutable f: int; mutable r: int; arr: 'a option array }
exception Empty of string
exception Full of string

let empty size = { f = 0; r = 0; arr = Array.make(size + 1) None }

let enqueue(x, q) =
    if isFull q
    then raise(Full "module Queue_mut: enqueue")
    else
        begin
            q.arr.(q.r) <- Some x;
            q.r <- (q.r + 1) mod (Array.length q.arr)
        end

let dequeue q =
    if isEmpty q then ()
    else ((q.f <- q.f + 1) mod (Array.length q.arr))

let first q =
    if isEmpty q then raise (Empty "module Queue_mut: first")
    else match q.arr.(q.f) with
         | Some x -> x
         | None -> raise (Empty "module Queue_mut: first")

let isEmpty q =
    q.f = q.r

let isFull(q) =
    ((q.r + 1) mod (Array.length q.arr)) = q.f