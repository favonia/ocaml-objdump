let test x = Format.printf "%a@." Objdump.pp x

let () = test (fun () -> 1)
let () = test (("fst", "snd"), 2, 3.0, (), fun () -> ())
let f x = (42, fun () -> x)
let () = test (f 1000)

let rec f x = g x
and g x = f x

let () = test f
let () = test g

type _ Effect.t += F : unit Effect.t
let () = Effect.Deep.try_with Effect.perform F
    { effc = fun (type a) (e : a Effect.t) ->
          match e with
          | F -> Some (fun (k : (a, _) Effect.Deep.continuation) -> test k; Effect.Deep.continue k ())
          | _ -> None }
