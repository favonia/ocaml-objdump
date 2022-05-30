# 🖨️ Printing OCaml Runtime Representations

A small library I wrote when hacking the OCaml compiler.

## Caveats

- ⚠️ `forcing`, `cont`, `closure`, and `infix` blocks are only partially implemented because much information is intentionally not documented.
- ⚠️ The code was only tested with the latest versions of OCaml.

## API and Example Code

This library only exposes an ugly printer [pp] that dumps the runtime representation of any OCaml value.

```ocaml
(* This line prints out [(42, ()) is represented as variant0(int(42),int(0))] *)
let () = Format.printf "@[(42, ())@ is@ represented@ as@ %a@]@." Objdump.pp (42, ())
```
