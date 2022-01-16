type 'a t = { mutable f: int; mutable r: int; mutable arr: 'a option array }
exception Empty of string
exception Full of string

let empty(size) = { f = 0; r = 0; arr = Array.make(size + 1) None }

let enqueue(x, q) =
    if ((q.r + 1) mod (Array.length q.arr)) = q.f
        then raise(Full "module Queue_mut: enqueue")
    else
        q.arr.(q.r) <- Some x;
        q.r <- (q.r + 1) mod (Array.length q.arr)

let dequeue(q) =
    if q.f != q.r then q.f <- q.f + 1
    else ()

let first(q) =
    if q.f = q.r then raise (Empty "module Queue_mut: first")
    else match q.arr.(q.f) with
         | Some x -> x
         | None -> raise (Empty "module Queue_mut: first")

let isEmpty(q) =
    if q.f = q.r then true
    else false

let isFull(q) =
    if ((q.r + 1) mod (Array.length q.arr)) = q.f then true
    else false