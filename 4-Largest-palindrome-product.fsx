(*
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
*)

let isSquare N =
    let sq = N |> float |> sqrt

    sq % 1. = 0.

let rec fermatFactor N =
    let start = N |> float |> sqrt |> ceil

    let a =
        (+) (int start)
        |> Seq.initInfinite
        |> Seq.map float
        |> Seq.find (fun i -> i * i - N |> isSquare)
        |> float

    let b2 = a * a - N

    (a - sqrt b2, a + sqrt b2)

let isPalidrome (N: int) =
    let number = N |> string |> Seq.map char
    let reverse = number |> Seq.rev

    number
    |> Seq.compareWith Operators.compare reverse = 0

let hasClean3DigitFactors (N: int) =
    let (a, b) = fermatFactor N
    let lenA = a |> string |> String.length
    let lenB = b |> string |> String.length

    a % 1. = 0. && b % 1. = 0. && lenA = 3 && lenB = 3

seq { 100 * 100 .. 999 * 999 }
|> Seq.rev
|> Seq.filter isPalidrome
|> Seq.filter hasClean3DigitFactors
|> Seq.head
