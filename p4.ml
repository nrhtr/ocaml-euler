(*
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
*)

open Core.Std

let is_palindrome s =
    String.rev s = s

let _ =
    let max = ref 0 in
    for i = 100 to 999 do
        for j = 100 to 999 do
            let p = i * j in
            if p > !max && is_palindrome (string_of_int p) then
                max := p
        done
    done;
    print_int !max; print_newline ()
