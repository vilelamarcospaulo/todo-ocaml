type t_error = DBError of string | InvalidJson | RecordNotFound

let to_response = function
  | InvalidJson -> Dream.json ~code:400 "Invalid request"
  | DBError s ->
      Dream.error (fun log -> log "[DBError] :: %s " s);
      Dream.json ~code:500 "Internal dabase error"
  | RecordNotFound -> Dream.json ~code:404 "Record not found"
