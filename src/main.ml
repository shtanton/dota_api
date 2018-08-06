open Lwt.Infix

let headers =
  Cohttp.Header.(init () |> fun h -> add_list h [
    ("Access-Control-Allow-Origin", "http://localhost:8080");
    ("Access-Control-Allow-Headers", "content-type");
    ("Content-Type", "application/json");
  ])
let schema = Graphql_lwt.Schema.(schema ~mutations:Item.mutation Item.query)

let ctx _ = Database.pool

let execute_query ctx variables query =
  match Graphql_parser.parse query with
  | Ok doc ->
    Graphql_lwt.Schema.execute schema ctx ~variables doc
  | Error err -> Lwt.return (Error (`String err))

let static_file_response file =
  match Assets.read file with
  | Some body -> Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
  | None -> Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"" ()

let execute_request req body =
  Cohttp_lwt.Body.to_string body >>= fun body' ->
  let json = Yojson.Basic.from_string body' in
  let query = Yojson.Basic.(json |> Util.member "query" |> Util.to_string) in
  let variables =
    try Yojson.Basic.Util.(json |> member "variables" |> to_assoc)
    with _ -> [] in
  execute_query (ctx req) (variables :> (string * Graphql_parser.const_value) list) query >>= function
  | Ok data ->
    let body = Yojson.Basic.to_string data in
    print_endline "REQUEST IS GUD";
    Cohttp_lwt_unix.Server.respond_string ~headers ~status:`OK ~body ()
  | Error err ->
    print_endline "ERROR";
    let body = Yojson.Basic.to_string err in
    Cohttp_lwt_unix.Server.respond_string ~status:`Internal_server_error ~body ()

let callback _ (req: Cohttp.Request.t) body =
  let reqPath = Cohttp.Request.uri req |> Uri.path in
  let parts = Str.(split (regexp "/") reqPath) in
  match req.meth, parts with
  | `GET, ["graphql"] -> static_file_response "index.html"
  | `POST, ["graphql"] -> execute_request req body
  | `OPTIONS, ["graphql"] -> Cohttp_lwt_unix.Server.respond_string ~status:`OK ~headers ~body:"" ()
  | _, ["graphql"] -> print_endline "BAD METHOD"; Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"" ()
  | _ -> print_endline "BAD URL"; Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"" ()

let () =
  Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port 3000)) (Cohttp_lwt_unix.Server.make ~callback ())
  |> Lwt_main.run
