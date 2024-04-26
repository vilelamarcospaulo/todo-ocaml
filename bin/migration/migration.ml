let connection_string = "sqlite3:todo.sqlite"

let _ =
  let db_conn = Lwt_main.run @@ Todo.Migration.conn connection_string in
  Todo.Migration.run db_conn
