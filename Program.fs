module Program

open System
open System.Numerics

open Utility

let init() =
    Prime.primes |> Seq.last

let problem1() =
    [1..999]
        |> List.filter (fun n -> (n % 3) = 0 || (n % 5) = 0)
        |> List.sum

let problem2() =
    Fib.fibs
        |> Seq.takeWhile (fun n -> n < 4000000I)
        |> Seq.filter (fun n -> n % 2I = 0I)
        |> Seq.sum

let problem3() =
    let factors = Prime.factor 600851475143L
    (List.head (List.rev factors)).Base

let problem4() =
    let ns = [100..999]
    let isPalindrome n =
        let digits = Digits.ofInt n
        List.rev digits = digits
    let (_, _, answer) =
        Seq.collect (fun n1 -> Seq.map (fun n2 -> (n1, n2, n1 * n2)) ns) ns
        |> Seq.filter (fun (n1, n2, _) -> n1 >= n2)
        |> Seq.filter (fun (_, _, prod) -> isPalindrome prod)
        |> Seq.maxBy (fun (_, _, prod) -> prod)
    answer

let problem5() =
    [1L..20L]
        |> Seq.map Prime.factor
        |> Seq.collect id
        |> Seq.groupBy (fun power -> power.Base)
        |> Seq.map (fun (b, powers) ->
            powers |> Seq.maxBy (fun power -> power.Exponent))
        |> Seq.map (fun power -> power.Value)
        |> Seq.fold (*) 1L

let problem6() =
    let ns = [1..100]
    let sumOfSquares =
        List.sumBy (fun n -> n * n) ns
    let squareOfSums =
        let sum = List.sum ns
        sum * sum
    Math.Abs(sumOfSquares - squareOfSums)

let problem7() =
    Prime.primes.[10000]

let problem8() =
    let str = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    let len = 5
    [0 .. str.Length - len]
        |> Seq.map (
            (fun idx -> str.Substring(idx, len)) >> 
            (Seq.map (fun c -> int c - int '0')) >> 
            (Seq.fold (*) 1))
        |> Seq.max

let problem9() =
    let triples = Pyth.pthys 1000
    let (a, b, c) = Seq.find (fun (a, b, c) -> a + b + c = 1000) triples
    a * b * c

let problem10() =
    Prime.primes
        |> Seq.takeWhile (fun n -> n <= 2000000L)
        |> Seq.sum

let triangle n =
    n * (n + 1) / 2
let triangles =
    Seq.initInfinite id
    |> Seq.skip 1
    |> Seq.map triangle

let problem12() =
    let count n =
        let factors = Prime.factor n
        factors |> Seq.fold (fun acc power -> acc * (power.Exponent + 1)) 1
    triangles |> Seq.find (fun n -> (count (int64 n)) > 500)

let rec collatz n =
    seq {
        yield n
        if n > 1L then
            let next = 
                if n % 2L = 0L then
                    n / 2L
                else
                    3L * n + 1L
            yield! collatz next
    }

let problem14() =
    [1L..999999L]
        |> Seq.maxBy (collatz >> Seq.length)

let problem15() =
    let rec raw (x, y) =
        match (x, y) with
            | (0L, ay) -> 1L
            | (ax, 0L) -> 1L
            | (ax, ay) -> routes (ax - 1L, ay) + routes (ax, ay - 1L)
    and routes =
        Utility.memoize raw
    routes (20L, 20L)

let problem16() =
    let n = pown 2I 1000
    let digits = Digits.ofBigInt n
    Seq.sum digits

let problem21() =
    [2L..9999L]
    |> Seq.map (fun n -> (n, Divisor.amicable n))
    |> Seq.sumBy (fun (n, amic) ->
        match amic with
            | Some _ -> n
            | None -> 0L)

let problem23() =
    let limit = 28123L
    let abundant =
        [2L..limit]
            |> Seq.filter (fun n -> Divisor.categorize n = Divisor.Abundant)
            |> Set.ofSeq
    [1L..limit]
        |> Seq.filter (fun n->
            let matches =
                abundant
                    |> Seq.filter (fun a1 ->
                        let a2 = n - a1
                        abundant.Contains a2)
            Seq.isEmpty matches)
        |> Seq.sum

let rec permute items =
    if Seq.isEmpty items then
        seq { yield Seq.empty }
    else
        items
            |> Seq.collect (fun item ->
                let others = items |> Seq.filter (fun it -> it <> item)
                (permute others) |> Seq.map (fun perm ->
                    seq {
                        yield item
                        yield! perm
                    }))

let problem24() =
    let perms = permute ['0'..'9']
    let perm = List.ofSeq(Seq.nth 999999 perms)
    String.init perm.Length (fun i -> sprintf "%c" perm.[i])

let problem25() =
    let numDigits = Digits.ofBigInt >> Seq.length
    let (iTerm, _) =
        Seq.initInfinite BigInteger
        |> Seq.map (fun i -> (i, Fib.fib i))
        |> Seq.find (fun (i, fib) -> numDigits fib >= 1000)
    iTerm

let problem26() =
    let period n =
        let nBig = BigInteger(int n)
        [1..n-1]
        |> Seq.tryFind (fun k ->
            ((pown 10I k) - 1I) % nBig = 0I)  // period of n's reciprocal is the smallest k such that n divides 10^k - 1
    Prime.primes
        |> Seq.takeWhile (fun n -> n < 999L)
        |> Seq.maxBy (fun n ->
            match period (int n) with
                | Some n -> n
                | None -> 0)

let testAll() =
    let test func n expected =
        printf "problem #%A: " n
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        let actual = func()
        if actual <> expected then
            printfn "Expected %A but received %A" expected actual
        stopwatch.Stop()
        printfn "%A" stopwatch.Elapsed
    test init 0 1999993L
    test problem1 1 233168
    test problem2 2 4613732I
    test problem3 3 6857L
    test problem4 4 906609
    test problem5 5 232792560L
    test problem6 6 25164150
    test problem7 7 104743L
    test problem8 8 40824
    test problem9 9 31875000
    test problem10 10 142913828922L
    test problem12 12 76576500
    test problem14 14 837799L
    test problem15 15 137846528820L
    test problem16 16 1366I
    test problem21 21 31626L
    test problem23 23 4179871L
    test problem24 24 "2783915460"
    test problem25 25 4782I
    test problem26 26 983L

[<EntryPoint>]
let main args =
    printfn "%A" (testAll())
    0
