(*
 * The sum of the squares of the first ten natural numbers is,

1^2 + 2^2 + ... + 10^2 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)^2 = 55^2 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
*)

open Core.Std

let diff_squares a b =
    let sum_of_squares a b =
        List.range ~stop:`inclusive a b |> List.fold ~init:0 ~f:(fun acc x -> acc + x * x) in
    let square_of_sum a b =
        let sum = List.range ~stop:`inclusive a b |> List.fold ~init:0 ~f:(fun acc x -> acc + x)
        in sum * sum
    in square_of_sum a b - sum_of_squares a b
