(*
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
*)

let factors n =
    let rec go n d =
        if n = 1 then [] else
            if n mod d = 0 then
                d :: go (n / d) d
            else
                go n (d + 1)
    in go n 2

let () =
    let facs = factors 600851475143 in
    List.fold_right max facs 0 |> Printf.printf "%d\n"
