open Dream

let hello_handler _request = html "Hello from ocaml"

let () =
  Dream.run ~port:8080
    (Dream.router
       ([ get "/" hello_handler ] @ Cineai_routes.User_routes.user_api_routes ()))
