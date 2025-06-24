open Dream

let json_response (status_code : int) (json_body : Yojson.Safe.t) :
    Dream.response Lwt.t =
  json ~code:status_code (Yojson.Safe.to_string json_body)

let create_user_handler request =
  let%lwt body_string = body request in
  match Yojson.Safe.from_string body_string with
  | json_payload -> (
      match Cineai_user.User.user_of_json json_payload with
      | Ok { name; email; _ } ->
          let new_user = Cineai_user.User.add_user name email in
          json_response 201 (Cineai_user.User.user_to_json new_user)
      | Error msg ->
          log "Error: Failed to create user: %s" msg;
          json_response 400
            (`Assoc
               [
                 ("error", `String (Printf.sprintf "Invalid user data: %s " msg));
               ])
      | exception Yojson.Json_error msg ->
          log "Error parsing json: %s" msg;
          json_response 500
            (`Assoc [ ("error", `String "Internal server error") ]))

let get_all_users_handler _request =
  let all_users = Cineai_user.User.get_all () in
  let users = List.map Cineai_user.User.user_to_json all_users in
  json_response 200 (`List users)

let user_api_routes () =
  [
    Dream.post "/users" create_user_handler;
    Dream.get "/users" get_all_users_handler;
  ]
