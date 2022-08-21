let isPrime number =
    if number &&& 1 = 0 then
        false
    else
        let sq = number |> float |> sqrt |> int

        seq { 3..2..sq }
        |> Seq.exists (fun i -> number % i = 0)
        |> not

let primes = (+) 1 |> Seq.initInfinite |> Seq.filter isPrime

primes |> Seq.item 10001
