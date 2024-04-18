open Lwt.Syntax
open Lwt.Infix

module type Db = Caqti_lwt.CONNECTION

module Q = struct
  open Caqti_request.Infix
  module T = Caqti_type

  let create_todo_table =
    ( "todos_table_creation",
      (T.unit ->. T.unit)
      @@ "CREATE TABLE IF NOT EXISTS todos (id INTEGER PRIMARY KEY AUTOINCREMENT, title  \
          VARCHAR(20)  NOT NULL,  description VARCHAR(100) NOT NULL,  completed_at DATETIME)" )

  let migrations = [ create_todo_table ]
end

let execute (module Db : Db) =
  let rec loop = function
    | [] -> Lwt.return_unit
    | (id, query) :: xs ->
        print_endline ("Running migration: " ^ id);
        let* _ = Db.exec query () in
        loop xs
  in
  loop Q.migrations

let run connection_string =
  let connection_uri = Uri.of_string connection_string in
  Lwt_main.run @@ (Caqti_lwt.connect connection_uri >>= Caqti_lwt.or_fail >>= execute)
