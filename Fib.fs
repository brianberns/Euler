module Fib

open System
open System.Numerics

open Utility

let rec private raw n =
    if (n = 0I) then
        0I
    else if (n = 1I) then
        1I
    else
        fib (n - 1I) + fib (n - 2I)
and fib =
    Utility.memoize raw

let fibs =
    Seq.initInfinite (BigInteger >> fib)
