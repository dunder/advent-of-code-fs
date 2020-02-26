module Combinatorics

let rec combinations n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs

let rec permutations = function
       | []      -> seq [List.empty]
       | x :: xs -> Seq.collect (insertions x) (permutations xs)
   and insertions x = function
       | []             -> [[x]]
       | (y :: ys) as xs -> (x::xs)::(List.map (fun x -> y::x) (insertions x ys))