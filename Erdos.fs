module Erdos

open System

let sqrtInt n =
    if n % 4L > 1L then
        None
    else
        let factors = Prime.factor n
        if factors |> Seq.exists (fun factor -> factor.Exponent % 2 = 1) then
            None
        else
            Some ((1L, factors)
                ||> Seq.fold (fun acc factor ->
                    let factor = { factor with Exponent = factor.Exponent / 2}
                    acc * factor.Value))

// x^2 = c * k^2 + 1
// 1014514229^2 = 42510 * 4920538^2 + 1
let erdos3() =
    let cMax = 100000L
    let kMax = 10000L
    let c, k, x =
        [| 1L .. cMax |]
            |> Array.Parallel.map (fun c ->
                if (c % 1000L = 0L) then printfn "c: %d" c
                let k, xOpt =
                    seq { kMax .. -1L .. 0L }
                        |> Seq.map (fun k ->
                            let xOpt = sqrtInt (c * k * k + 1L)
                            k, xOpt)
                        |> Seq.find (fun (k, xOpt) -> xOpt |> Option.isSome)
                c, k, xOpt.Value)
            |> Seq.maxBy (fun (c, k, x) -> x)
    printfn "%d^2 = %d * %d^2 + 1" x c k
    c
