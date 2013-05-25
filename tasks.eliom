open Base

type t = Task.t list
let init () =
  []

let add x xs  =
  x :: xs

let demo =
  init ()
  +> add (Task.create "Buy milk")
  +> add (Task.create "Prove theorem")
  +> add (Task.create "Install OCaml")
