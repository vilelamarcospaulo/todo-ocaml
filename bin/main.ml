let sql_uri = "sqlite3:db.sqlite"
let routes = Todo.Http.routes
let _ = Dream.run @@ Dream.logger @@ Dream.sql_pool sql_uri @@ routes
