(*
The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
*)

let (|Even|Odd|) x = if x &&& 1L = 0 then Even else Odd

let collatz start =
    let rec collatz' sum n =
        match n with
        | 1L -> sum + 1L
        | Even -> collatz' (sum + 1L) (n / 2L)
        | Odd -> collatz' (sum + 1L) (3L * n + 1L)

    collatz' 0L start

#time

seq { 1L .. 1000000L }
|> Seq.map (fun i -> i, collatz i)
|> Seq.maxBy snd
|> fst

#time
