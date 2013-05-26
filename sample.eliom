{shared{
  open Eliom_lib
  open Eliom_content
  open Base
}}

module Sample_app =
  Eliom_registration.App (
    struct
      let application_name = "sample"
    end)

let main_service =
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let add name =
  if name = "" then
    Lwt.return None
  else
    let open Lwt in
    Db.add (Task.create ~id:~-1 ~name)
    >> Db.incomplete ()
    >>= function
      | [] -> Lwt.return None
      | x :: _ -> Lwt.return (Some x)

let add_service =
  Eliom_registration.Html5.register_service
    ~path:["add"]
    ~get_params:Eliom_parameter.unit
    (fun _ _ -> raise Eliom_common.Eliom_Wrong_parameter)

let add_service =
  Eliom_registration.Redirection.register_post_service
    ~post_params:(Eliom_parameter.string "name")
    ~fallback:add_service
    (fun () name ->
      let open Lwt in
      add name
      >> Lwt.return main_service)

let api_add_service =
  Eliom_registration.Ocaml.register_service
    ~path:["api"; "add"]
    ~get_params:Eliom_parameter.unit
    (fun _ _ -> add "")

let api_add_service =
  Eliom_registration.Ocaml.register_post_service
    ~fallback:api_add_service
    ~post_params:(Eliom_parameter.string "name")
    (fun () name -> add name)

let done_service =
  Eliom_registration.Html5.register_service
    ~path:["done"]
    ~get_params:Eliom_parameter.unit
    (fun _ _ -> raise Eliom_common.Eliom_Wrong_parameter)

let done_service =
  Eliom_registration.Redirection.register_post_service
    ~post_params:(Eliom_parameter.int "id")
    ~fallback:done_service
    (fun () id ->
      let open Lwt in
      Db.done_it (Task.create ~id ~name:"-")
      >> Lwt.return main_service)

(* main service *)
let task_form =
  let open Html5.D in
  post_form ~xhr:false ~service:add_service begin fun name -> [
    div ~a:[ a_class ["input-append"]] [
      input  ~a:[ a_placeholder "task name" ] ~input_type:`Text ~name ();
      button ~a:[ a_class [ "btn"; "btn-primary"]]
             ~button_type:`Submit
        [ pcdata "Add" ]
      ]]
    end ()

{shared{
  let task_elt ?(anime=false) { Task.name; id } =
    let open Html5.D in
    let class_ =
      if anime then
        [ "new" ]
      else
        []
    in
    tr ~a:[ a_class class_ ]
       [td [ pcdata name];
        td [
          post_form ~service:%done_service begin fun name->
            [ int_input ~input_type:`Hidden
                        ~name
                        ~value:id
                        ();
              button ~button_type:`Submit ~a:[ a_class ["btn"]]
                [ pcdata "DONE" ]]
          end () ]]
}}

{client{
  let submit ?use_capture target =
    Lwt_js_events.make_event (Dom.Event.make "submit") ?use_capture target
  let submits ?use_capture t =
    Lwt_js_events.seq_loop submit ?use_capture t
}}

let init tbody = {unit{
  let dom =
    Eliom_content.Html5.To_dom.of_form %task_form
  in
  let name =
    match Js.Opt.to_option @@ dom##elements##namedItem (Js.string "name") with
    | Some elt -> Js.Unsafe.coerce elt
    | None -> assert false
  in
  Eliom_lib.jsdebug name;
  Lwt.async begin fun () ->
    submits dom begin fun e _ ->
      Lwt_js_events.preventDefault e;
      Eliom_client.call_caml_service
        ~service:%api_add_service
        () (name##value)
      >>= begin function
        | Some task ->
            let dom =
              Eliom_content.Html5.To_dom.of_tbody %tbody
            in
            let () =
              Dom.insertBefore dom
                               (Eliom_content.Html5.To_dom.of_node @@ task_elt ~anime:true task)
                               dom##firstChild
            in
            Lwt.return ()
        | None ->
            Eliom_lib.debug "something error";
            Lwt.return ()
      end
      >> Lwt.return ()
    end
  end
}}

let () =
  Sample_app.register
    ~service:main_service
    (fun () () ->
      let open Lwt in
      Db.incomplete () >>= fun tasks ->
        let task_table =
          Html5.D.tbody @@ List.map task_elt tasks
        in
        let () =
          ignore @@ init task_table
        in
        Lwt.return (Eliom_tools.F.html
          ~title:"Simple Todo tool: Eliom on Heroku sample"
          ~css:[
            ["css";"bootstrap.min.css"];
            ["css";"main.css"] ]
          Html5.F.(body [
            div ~a:[ a_class [ "container" ]] [
              header [
                div ~a:[ a_class [ "hero-unit" ]] [
                  h1 [ pcdata "Simple Todo tool" ];
                  p  [ pcdata "Sample for Eliom on Heroku" ]
                ]];
              article [
                h2 [ pcdata "TODOs" ];
                task_form;
                tablex
                  ~a:[ a_class [ "table"; "table-striped" ]]
                  ~thead:(thead [tr [ th [ pcdata "TODO" ]; th [] ]]) @@
                  [ task_table ]];
              article [
                h2 [ pcdata "About this" ];
                p  [ pcdata "This is ";
                    Raw.a ~a:[ a_href @@
                      Xml.uri_of_string "https://github.com/mzp/heroku-buildpack-ocsigen"
                    ] [
                      pcdata "heroku-buildpack-ocsigen"
                    ];
                    pcdata " sample."]
              ]
        ]])))

