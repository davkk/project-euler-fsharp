(*
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
*)

let isPrime (number: bigint) =
    let sq = number |> float |> sqrt |> bigint

    seq { 2I .. sq }
    |> Seq.exists (fun i -> number % i = 0I)
    |> not

// Euclidean Algorithm for finding the Greatest Common Divisor
let gcd (numbers: bigint * bigint) =
    let numberList = [ fst numbers; snd numbers ]
    let n1 = numberList |> List.min
    let n2 = numberList |> List.max

    let rec gcd' (x: bigint) (y: bigint) =
        let modulo = x % y

        match modulo with
        | _ when modulo = 0I -> y
        | _ -> gcd' y modulo

    gcd' n1 n2

// Pollard's rho algorithm
let rec pra (n: bigint) (x: bigint) (y: bigint) (d: bigint) =
    let g (t: bigint) = (t * t + 1I) % n

    match d with
    | _ when d = 1I -> pra n (g x % n) (g (g y % n) % n) (gcd (abs (x - y), n))
    | _ when d = n -> pra n (x + 1I) (y + 2I) 1I
    | _ -> d


let rec factorize (number: bigint) =
    seq {
        match number with
        | _ when number < 2I -> yield! Seq.empty
        | _ when number |> isPrime -> yield number
        | _ ->
            let divisor = pra number 2I 2I 1I

            if number % divisor = 0I then
                yield divisor
                yield! factorize (number / divisor)
    }

factorize 213122112213321312321312I |> Seq.max
