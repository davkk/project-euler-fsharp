(*
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
*)

// let isDivisibleByRange range x =
//     let result = range |> Seq.tryFind (fun i -> x % i <> 0)

//     match result with
//     | Some _ -> false
//     | None -> true

// (*) 20
// |> Seq.initInfinite
// |> Seq.skip 1
// |> Seq.filter (isDivisibleByRange [ 2..20 ])
// |> Seq.head


let isPrime number =
    let sq = number |> float |> sqrt |> int

    seq { 2..sq }
    |> Seq.exists (fun i -> number % i = 0)
    |> not

let primes limit = seq { 2..limit } |> Seq.filter isPrime

let smallestMultiple (limit: int) (numbers: int seq) =
    numbers
    |> Seq.fold
        (fun product prime ->
            product
            * (float prime
               ** (floor ((float limit |> log) / (float prime |> log)))))
        1.

let solve limit =
    primes limit |> smallestMultiple limit |> int

solve 20
