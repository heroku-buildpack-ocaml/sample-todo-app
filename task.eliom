{shared{
  type t = {
    id : int;
    name : string
  }
}}

let create ~id ~name =
  { id; name }
