open Base

type t = Task.t list
let init () =
  []

let add x xs  =
  x :: xs

let demo =
  init ()
