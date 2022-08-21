(*
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
*)

let isPrime (number: int64) =
    if number &&& 1 = 0 then
        false
    else
        let sq = number |> float |> sqrt |> int64

        seq { 3L .. 2L .. sq }
        |> Seq.exists (fun i -> number % i = 0L)
        |> not

// Euclidean Algorithm for finding the Greatest Common Divisor
let gcd (numbers: int64 * int64) =
    let numberList = [ fst numbers; snd numbers ]
    let n1 = numberList |> List.min
    let n2 = numberList |> List.max

    let rec gcd' (x: int64) (y: int64) =
        let modulo = x % y

        match modulo with
        | _ when modulo = 0L -> y
        | _ -> gcd' y modulo

    gcd' n1 n2

// Pollard's rho algorithm
let rec pra (n: int64) (x: int64) (y: int64) (d: int64) =
    let g (t: int64) = (t * t + 1L) % n

    match d with
    | _ when d = 1L -> pra n (g x % n) (g (g y % n) % n) (gcd (abs (x - y), n))
    | _ when d = n -> pra n (x + 1L) (y + 2L) 1L
    | _ -> d


let rec factorize (number: int64) =
    seq {
        match number with
        | _ when number < 2L -> yield! Seq.empty
        | _ when number |> isPrime -> yield number
        | _ ->
            let divisor = pra number 2L 2L 1L

            if number % divisor = 0L then
                yield divisor
                yield! factorize (number / divisor)
    }

factorize 600851475143L |> Seq.max
