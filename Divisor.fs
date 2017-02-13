module Divisor

open Prime

let divisors n =
    let factors = List.toArray (factor n)
    let nDivisors =
        factors
        |> Seq.fold (fun acc factor -> acc * (factor.Exponent + 1)) 1
    [
        for iDivisor = 0 to nDivisors - 1 do
            let tuple =
                factors
                    |> Seq.fold (fun (value, index) factor ->
                        let exp = factor.Exponent + 1
                        let exponent = index % exp
                        let power = { Base = factor.Base; Exponent = exponent }
                        (value * power.Value, index / exp)) (1L, iDivisor)
            yield fst tuple
    ]

let private sumOfDivisors n =
    let divisors = divisors n
    Seq.sum divisors - n

let amicable n =
    let other = sumOfDivisors n
    if (sumOfDivisors other = n) && (other <> n) then
        Some other
    else
        None

type Category =
    | Perfect
    | Abundant
    | Deficient

let categorize n =
    let sum = sumOfDivisors n
    if n > sum then
        Deficient
    else if n < sum then
        Abundant
    else
        Perfect
