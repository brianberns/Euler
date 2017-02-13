module Prime

open System

/// Answers the largest number that has to be considered as a factor of the given number.
let private ceiling n =
    int64 (Math.Ceiling(Math.Sqrt(float n)))

/// Answers a sequence of primes less than or equal to the given number.
let private sieve limit =
    let ceil = ceiling limit
    let rec loop ns =
        if Seq.isEmpty ns then
            Seq.empty
        else
            let next = Seq.head ns
            if next >= ceil then
                ns
            else
                seq {
                    yield next
                    let filtered = ns |> Seq.filter (fun n -> n % next <> 0L)
                    yield! (loop filtered)
                }
    loop (seq { 2L..limit })

let primes =
    printfn "Initializing primes..."
    let result = sieve 2000000L |> Seq.toArray
    printfn "done"
    result

[<StructuredFormatDisplay("{AsString}")>]
type Power =
    {
        Base : int64
        Exponent : int
    }
    override this.ToString() =
        if this.Exponent = 1 then
            this.Base.ToString()
        else
            String.Format("{0}^{1}", this.Base, this.Exponent)
    member this.AsString = this.ToString()
    member this.Value = pown this.Base this.Exponent
    
let factor n =
    let primes =
        let ceil = ceiling n
        primes |> Seq.takeWhile (fun n -> n <= ceil)
    let rec loop n' facts =
        if n' = 1L then
            facts
        else
            let fact =
                match Seq.tryFind (fun p -> n' % p = 0L) primes with
                | Some x -> x
                | None -> n'
            loop (n' / fact) (fact :: facts)
    let facts = loop n []
    Seq.groupBy id facts
        |> Seq.sortBy (fun (fact, _) -> fact)
        |> Seq.map (fun (fact, facts) -> { Base = fact; Exponent = Seq.length facts })
        |> Seq.toList
