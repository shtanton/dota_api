open Lwt.Infix

type error =
  | Database_error of string

let connection_url = "mariadb://root:root@localhost:3306/dota"

let pool =
  match Caqti_lwt.connect_pool ~max_size:10 (Uri.of_string connection_url) with
  | Ok pool -> pool
  | Error err -> failwith (Caqti_error.show err)

let or_error m =
  m >|= function
    | Ok _ as a -> a
    | Error e -> Error (Database_error (Caqti_error.show e))
