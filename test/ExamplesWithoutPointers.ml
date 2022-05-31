let test x = Format.printf "%a@." Objdump.pp x

type t = A | B of int | C

type _ u = ..
type _ u += D : int u
type _ u += E : 'a -> 'a u

type v = ..
type v += U : 'a u -> v

let () = test ()
let () = test A
let () = test (B 100)
let () = test C
let () = test 42
let () = test false
let () = test (-1)
let () = test None
let () = test (Some (Some (Some (Some (Some (Some (Some (Some 10))))))))
let () = test (Failure "str")
let () = test (Invalid_argument "str")
let () = test "a very very very very very very very long string with \x012 and \" and ' and \\"
let () = test (("fst", "snd"), 2, 3.0, ())
let () = test 3.14
let () = test (2.18, "a")
let () = test (U D)
let () = test (U (E 10))
let () = let m = lazy (1 + 2) in let _ = Lazy.force m in test m
let () = let rec m = lazy (test m) in Lazy.force m
let () = test [|1; 2; 3|]
let () = test [|1.0; 2.0; 3.0|]
let () = test [1; 2; 3]
let () = test Float.nan
let () = test 10l
let () = test 20L
let () = test 30n
