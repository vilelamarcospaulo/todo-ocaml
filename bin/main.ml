let sql_uri = "sqlite3:db.sqlite"

let _ =
  Dream.run
  @@ Dream.logger
  @@ Dream.sql_pool sql_uri
  @@ Dream.sql_sessions
  @@ Dream.router [
    Dream.get "/" (fun _ -> Dream.html "Hello, world!");
  ]

