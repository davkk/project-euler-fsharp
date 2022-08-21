(*
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
*)


let isPrime number =
    if number &&& 1 = 0 then
        false
    else
        let sq = number |> float |> sqrt |> int

        seq { 3..2..sq }
        |> Seq.exists (fun i -> number % i = 0)
        |> not

let primes =
    (+) 2 // start from 2
    |> Seq.initInfinite
    |> Seq.filter isPrime
    |> Seq.map int64

primes |> Seq.takeWhile ((>) 2000000L) |> Seq.sum
