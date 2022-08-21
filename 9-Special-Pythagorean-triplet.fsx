(*
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
*)

let triplet =
    let rec findTriplet' m =
        let n =
            seq { 1 .. m - 1 }
            |> Seq.tryFind (fun n -> m * (m + n) = 500)

        match n with
        | Some n -> m * m - n * n, 2 * m * n, m * m + n * n
        | None -> findTriplet' (m + 1)

    findTriplet' 1

let a, b, c = triplet

a * b * c
