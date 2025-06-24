type user = { id : int; name : string; email : string }

let next_user_id = ref 0

let generate_user_id () =
  incr next_user_id;
  !next_user_id

let users : user list ref = ref []

let user_to_json (user : user) : Yojson.Safe.t =
  `Assoc
    [
      ("id", `Int user.id);
      ("name", `String user.name);
      ("email", `String user.email);
    ]

let user_of_json (json : Yojson.Safe.t) : (user, string) result =
  try
    let open Yojson.Safe.Util in
    let id_opt = json |> member "id" |> to_int_option in
    let name = json |> member "name" |> to_string in
    let email = json |> member "email" |> to_string in
    let final_id =
      match id_opt with Some i -> i | None -> generate_user_id ()
    in
    Ok { id = final_id; name; email }
  with _ -> Error "Unknown error"

(* Create new user *)
let add_user (name : string) (email : string) : user =
  let new_user = { id = generate_user_id (); name; email } in
  users := new_user :: !users;
  new_user

(* Get all users *)
let get_all () : user list = !users
