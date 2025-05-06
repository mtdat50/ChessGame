open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic
open Chess

(* Settings *)
let port = 8080
let static_dir = "static"

(* Helper function to read the entire body of a request *)
let read_body req =
  Cohttp_lwt.Body.to_string req >>= fun body ->
  Lwt.return body

(* Helper function to parse JSON body *)
let parse_json_body req =
  read_body req >>= fun body ->
  try
    let json = Yojson.Basic.from_string body in
    Lwt.return (Some json)
  with _ ->
    Lwt.return None

(* Handle static file requests *)
let handle_static_file path =
  let file_path = Filename.concat static_dir path in
  
  (* Basic security check - prevent path traversal *)
  if String.contains path '/' || String.contains path '\\' then
    Server.respond_string ~status:`Bad_request ~body:"Bad path" ()
  else
    try
      let ic = open_in_bin file_path in
      let content_length = in_channel_length ic in
      let content = really_input_string ic content_length in
      close_in ic;
      
      (* Set content type based on file extension *)
      let content_type =
        if Filename.check_suffix file_path ".html" then "text/html"
        else if Filename.check_suffix file_path ".js" then "application/javascript"
        else if Filename.check_suffix file_path ".css" then "text/css"
        else if Filename.check_suffix file_path ".png" then "image/png"
        else if Filename.check_suffix file_path ".jpg" || Filename.check_suffix file_path ".jpeg" then "image/jpeg"
        else "application/octet-stream"
      in
      
      let headers = Header.init_with "Content-Type" content_type in
      Server.respond_string ~headers ~status:`OK ~body:content ()
    with
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
        Server.respond_string ~status:`Not_found ~body:"File not found" ()
    | e ->
        Server.respond_string ~status:`Internal_server_error ~body:(Printexc.to_string e) ()

(* API handlers *)
let handle_new_game _req _body =
  let game = GameState.create () in
  let response = `Assoc [
    ("game_id", `String game.id);
    ("position", position_to_json game.position)
  ] in
  Server.respond_string ~status:`OK ~body:(to_string response) ()

let handle_get_game req _body =
  let uri = Request.uri req in
  match Uri.get_query_param uri "id" with
  | None ->
      Server.respond_string ~status:`Bad_request ~body:"Missing game ID" ()
  | Some game_id ->
      match GameState.get game_id with
      | None ->
          Server.respond_string ~status:`Not_found ~body:"Game not found" ()
      | Some game ->
          let response = `Assoc [
            ("game_id", `String game.id);
            ("position", position_to_json game.position);
            ("history_length", `Int (List.length game.history))
          ] in
          Server.respond_string ~status:`OK ~body:(to_string response) ()

let handle_make_move req body =
  parse_json_body body >>= function
  | None ->
      Server.respond_string ~status:`Bad_request ~body:"Invalid JSON body" ()
  | Some json ->
      let uri = Request.uri req in
      match Uri.get_query_param uri "id" with
      | None ->
          Server.respond_string ~status:`Bad_request ~body:"Missing game ID" ()
      | Some game_id ->
          match GameState.get game_id with
          | None ->
              Server.respond_string ~status:`Not_found ~body:"Game not found" ()
          | Some game ->
              match move_of_json json with
              | None ->
                  Server.respond_string ~status:`Bad_request ~body:"Invalid move format" ()
              | Some move ->
                  let legal_moves = get_legal_moves game.position in
                  if List.exists (fun m -> 
                      m.from_row = move.from_row && 
                      m.from_col = move.from_col && 
                      m.to_row = move.to_row && 
                      m.to_col = move.to_col &&
                      m.promotion = move.promotion
                  ) legal_moves then
                    (* Move is legal *)
                    let new_position = apply_move game.position move in
                    let updated_game = GameState.update game_id new_position in
                    let response = `Assoc [
                      ("game_id", `String updated_game.id);
                      ("position", position_to_json updated_game.position);
                      ("move", move_to_json move);
                      ("legal", `Bool true)
                    ] in
                    Server.respond_string ~status:`OK ~body:(to_string response) ()
                  else
                    (* Move is not legal *)
                    let response = `Assoc [
                      ("game_id", `String game_id);
                      ("position", position_to_json game.position);
                      ("move", move_to_json move);
                      ("legal", `Bool false)
                    ] in
                    Server.respond_string ~status:`Bad_request ~body:(to_string response) ()

let handle_get_moves req _body =
  let uri = Request.uri req in
  match Uri.get_query_param uri "id" with
  | None ->
      Server.respond_string ~status:`Bad_request ~body:"Missing game ID" ()
  | Some game_id ->
      match GameState.get game_id with
      | None ->
          Server.respond_string ~status:`Not_found ~body:"Game not found" ()
      | Some game ->
          let legal_moves = get_legal_moves game.position in
          let moves_json = `List (List.map move_to_json legal_moves) in
          let response = `Assoc [
            ("game_id", `String game_id);
            ("moves", moves_json)
          ] in
          Server.respond_string ~status:`OK ~body:(to_string response) ()

(* Main request handler *)
let request_handler _conn req body =
  let uri = Request.uri req in
  let path = Uri.path uri in
  let meth = Request.meth req in

  match meth, path with
  | `GET, "/" ->
      handle_static_file "index.html"
  | `GET, path when String.length path > 8 && String.sub path 0 8 = "/static/" ->
      (* Sanitize path by removing /static/ prefix and any parent directory traversal *)
      let cleaned_path = Filename.basename (String.sub path 8 (String.length path - 8)) in
      handle_static_file cleaned_path
  | `GET, "/api/new" ->
      handle_new_game req body
  | `GET, "/api/game" ->
      handle_get_game req body
  | `POST, "/api/move" ->
      handle_make_move req body
  | `GET, "/api/moves" ->
      handle_get_moves req body
  | _ ->
      Server.respond_string ~status:`Not_found ~body:"Not found" ()

(* Initialize and start the server *)
let start_server () =
  (* Set up random seed *)
  Random.self_init ();
  
  (* Periodically clean up old games *)
  let _ =
    let open Lwt in
    let rec clean_loop () =
      Lwt_unix.sleep 3600.0 >>= fun () ->
      GameState.clean_old_games ();
      clean_loop ()
    in
    Lwt.async clean_loop
  in
  
  (* Create static directory if it doesn't exist *)
  if not (Sys.file_exists static_dir) then
    Unix.mkdir static_dir 0o755;
  
  (* Start server *)
  let server = Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback:request_handler ()) in
  
  Printf.printf "Server running at http://localhost:%d\n%!" port;
  Printf.printf "To play chess, open your browser at http://localhost:%d\n%!" port;
  
  server

(* Entry point *)
let () =
  Lwt_main.run (start_server ())
