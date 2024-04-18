open Lwt.Syntax
open Lwt.Infix

module type Db = Caqti_lwt.CONNECTION

module Q = struct
  open Caqti_request.Infix
  module T = Caqti_type

  let create_todo_table =
    ( "create | todos_table",
      (T.unit ->. T.unit)
      (*up*)
      @@ "CREATE TABLE IF NOT EXISTS todos (id INTEGER PRIMARY KEY AUTOINCREMENT, title  \
          VARCHAR(20)  NOT NULL,  description VARCHAR(100) NOT NULL,  completed_at DATETIME);",
      (*down*)
      (T.unit ->. T.unit) @@ "DROP TABLE IF EXISTS todos;" )

  let migrations = [ create_todo_table ]
end

module Executor = struct
  type direction = Up | Down [@@deriving show]

  let migrate drop (module Db : Db) =
    let rec loop direction = function
      | [] -> Lwt.return_unit
      | (id, up, down) :: xs ->
          print_endline ("Running migration: " ^ id ^ " | " ^ show_direction direction);

          let command = match direction with Up -> up | Down -> down in
          let* _ = Db.exec command () in

          loop direction xs
    in

    if drop then loop Down (List.rev Q.migrations) >>= fun _ -> loop Up Q.migrations
    else loop Up Q.migrations
end

let run ?(down_all = false) connection_string =
  let connection_uri = Uri.of_string connection_string in
  Lwt_main.run
  @@ (Caqti_lwt.connect connection_uri >>= Caqti_lwt.or_fail >>= Executor.migrate down_all)
