(*
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
*)

open Core.Std

let factors n =
    let rec go n d =
        if n = 1 then [] else
            if n mod d = 0 then
                d :: go (n / d) d
            else
                go n (d + 1)
    in go n 2

let prime_factors_range a b =
    Sequence.range a b |> Sequence.map ~f:factors |> Sequence.to_list

let count_occurences =
    List.fold ~init:Int.Map.empty ~f:(fun t s ->
        let count =
            match Map.find t s with
            | None -> 0
            | Some x -> x
        in Map.add t s (count + 1)
    )

let merge_taking_largest a b =
    Map.merge a b ~f:(fun ~key values ->
        match values with
        | `Both (a, b)       -> Some (max a b)
        | `Left v | `Right v -> Some v
    )

let expand lst =
    List.map ~f:(fun x ->
        match x with
        | (v,f) -> List.init f ~f:(fun x -> v)
    ) lst

let () =
    let result = prime_factors_range 1 21
        |> List.map ~f:count_occurences
        |> List.reduce_exn ~f:merge_taking_largest
        |> Map.to_alist
        |> expand
        |> List.concat
        |> List.reduce_exn ~f:( * ) in
    printf "%d\n" result
