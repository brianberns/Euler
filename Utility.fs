module Utility

open System
open System.Collections.Generic
open System.Numerics

let memoize f =
    let cache = new Dictionary<_, _>()
    fun x ->
        let (present, value) = cache.TryGetValue(x)
        if present then
            value
        else
            let result = f x
            cache.[x] <- result
            result
