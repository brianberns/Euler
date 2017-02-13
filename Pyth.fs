module Pyth

let pthys limit =
    [1..limit-1]
    |> Seq.collect (fun n ->
        [n+1..limit] |> Seq.map (fun m -> (m, n)))
    |> Seq.map (fun (m, n) -> (m*m - n*n, 2*m*n, m*m + n*n))
    |> Seq.sortBy (fun (a, b, c) -> c)
