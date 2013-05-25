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

let add_service_fallback =
  Eliom_registration.Html5.register_service
  ~path:["add"]
  ~get_params:Eliom_parameter.unit
  (fun _ _ ->
    raise Eliom_common.Eliom_Wrong_parameter)

let add_service = Eliom_registration.Redirection.register_post_service
  ~post_params:(Eliom_parameter.string "name")
  ~fallback:add_service_fallback
  (fun () name ->
    let open Lwt in
    Db.add (Task.create ~id:~-1 ~name)
    >> Lwt.return main_service)

let () =
  Sample_app.register
    ~service:main_service
    (fun () () ->
      let open Lwt in
      Db.incomplete () >>= fun tasks ->
      Lwt.return
        (Eliom_tools.F.html
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
                 post_form ~service:add_service begin fun name -> [
                   div ~a:[ a_class ["input-append"]] [
                     input  ~a:[ a_placeholder "task name" ] ~input_type:`Text ~name ();
                     button ~a:[ a_class [ "btn" ]] ~button_type:`Submit [ pcdata "Add" ]
                     ]]
                 end ();
                 tablex
                   ~a:[ a_class [ "table"; "table-striped" ]]
                   ~thead:(thead [tr [ th [ pcdata "TODO" ]; th [] ]]) @@
                   [ tbody @@ List.map (fun { Task.name } ->
                       tr [ td [ pcdata name];
                            td [ Raw.a ~a:[ a_class ["btn"]] [ pcdata "DONE" ] ]])
                       tasks
               ]];
               article [
                 h2 [ pcdata "About this" ];
                 p [ pcdata "This is ";
                     Raw.a ~a:[ a_href @@
                       Xml.uri_of_string "https://github.com/mzp/heroku-buildpack-ocsigen"
                     ] [
                       pcdata "heroku-buildpack-ocsigen"
                     ];
                     pcdata " sample."]
               ]
           ]])))

