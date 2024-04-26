open Lwt.Infix

type config = { db_uri : string }
type t = { sqlconn : Caqti_lwt.connection }

let default_db_uri = "sqlite3:db.sqlite"
let getenv var default = Sys.getenv_opt var |> Option.value ~default
let env_config = { db_uri = getenv "DB_URI" default_db_uri }
let test_config run_id = { db_uri = "sqlite3:file:cachedb_" ^ run_id ^ "?mode=memory&cache=shared" }

let conn connection_string =
  (* TODO :: use logger *)
  print_endline ("Connecting to: " ^ connection_string);

  let connection_uri = Uri.of_string connection_string in
  Lwt_main.run @@ (Caqti_lwt.connect connection_uri >>= Caqti_lwt.or_fail)

let init config =
  let sqlconn = conn config.db_uri in
  { sqlconn }
