open Base
module Lwt_thread = struct
    include Lwt
    include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)

let table = <:table< task(
  id integer NOT NULL,
  name text NOT NULL,
  is_done boolean NOT NULL
) >>

let task_table = <:table< task(
  name text NOT NULL,
  is_done boolean NOT NULL
) >>

let get_db : unit -> unit Lwt_PGOCaml.t Lwt.t =
  let db_handler =
    ref None in
  fun () ->
    match !db_handler with
    | Some h -> Lwt.return h
    | None -> Lwt_PGOCaml.connect ~database:"test" ~user:"postgres" ()

let incomplete () =
  let open Lwt in
  get_db ()
  >>= fun dbh ->
    Lwt_Query.view dbh
      <:view< {name = task.name; id = task.id} order by task.id desc |
               task in $table$;
               task.is_done = false >>
    >>= fun results ->
    Lwt.return @@ List.map begin fun obj ->
      let id =
        Int32.to_int @@ Sql.get obj#id
      in
      let name =
        Sql.get obj#name
      in
      Task.create ~id ~name
    end results

let add { Task.name; _ } =
  let open Lwt in
  get_db () >>= fun dbh ->
    Lwt_Query.query dbh
    <:insert< $task_table$ :=
      { name = $string:name$; is_done = false } >>

let done_it { Task.id; _ } =
  let open Lwt in
  get_db () >>= fun dbh ->
    Lwt_Query.query dbh
      <:update< t in $table$
                := { is_done = true }
                | t.id = $int32:Int32.of_int id$ >>
