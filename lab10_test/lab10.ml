let sum_arrays_rec matrix =
    let rec sum_rec(index, length, out_array) =
        if index < length then (out_array.(index) <- (Array.fold_left (+) 0 matrix.(index)); sum_rec(index + 1, length, out_array))
        else out_array
    in sum_rec(0, Array.length matrix, Array.make (Array.length matrix) 0)

let sum_arrays_imp matrix =
    let array = Array.make (Array.length matrix) 0 in
    for i = 0 to (Array.length matrix - 1) do
        let sum = ref 0 in
        for j = 0 to (Array.length matrix.(i) - 1) do
            sum := !sum + (matrix.(i)).(j)
        done;
        array.(i) <- !sum
    done;
    array;;
    
sum_arrays_rec([| [|1; 2; 3|]; [|4; 5; 6|]; [|7; 8; 9|] |]);;
sum_arrays_imp([| [|1; 2; 3|]; [|4; 5; 6|]; [|7; 8; 9|] |]);;