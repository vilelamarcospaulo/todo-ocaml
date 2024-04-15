open Lwt.Syntax
open Lwt.Infix

let sql_uri = "sqlite3:db.sqlite"

type _ Todo.error += InvalidJson

(* maybe we can use some combination of monads Lwt and Result to remove this custom impl*)
(* ps: this assumes all processing steps are Lwt.t and will always return a Result (a, e) *)
let _bind (promisse : ('a, 'e) result Lwt.t) (f : 'a -> ('b, 'e) result Lwt.t) =
  promisse >>= fun result ->
  match result with Ok x -> f x | Error e -> Lwt.return_error e

let ( >>@ ) = _bind

let from_body req ty_of_yojson =
  let* body = Dream.body req in
  let json = body |> Yojson.Safe.from_string in
  try Lwt.return_ok @@ ty_of_yojson json
  with Failure _ -> Lwt.return_error InvalidJson

let to_json todo_item = todo_item |> Todo.yojson_of_t |> Yojson.Safe.to_string

let to_resp result ~to_json =
  match result with
  | Ok result -> Dream.json ~code:200 (to_json result)
  | Error e -> (
      (* add some log with the received params on this *)
      match e with
      | InvalidJson -> Dream.json ~code:400 "Invalid request"
      | Todo.DBError s ->
          Dream.error (fun log -> log "[DBError] :: %s " s);
          Dream.json ~code:500 "Internal dabase error"
      | Todo.RecordNotFound -> Dream.json ~code:404 "Record Not found"
      | _ -> Dream.json ~code:500 "Internal server error")

let create_todo_handler req db =
  from_body req Todo.t_new_item_of_yojson
  >>@ Todo.create_item db >>= to_resp ~to_json

let query_todo_hander _req id db =
  Todo.by_id db (int_of_string id) >>= to_resp ~to_json

let _ =
  Dream.run @@ Dream.logger @@ Dream.sql_pool sql_uri
  @@ Dream.router
       [
         Dream.get "/" (fun _ -> Dream.html "Hello, world!");
         Dream.post "/todos" (fun req ->
             Dream.sql req @@ create_todo_handler req);
         Dream.get "/todos/:id" (fun req ->
             Dream.sql req @@ query_todo_hander req (Dream.param req "id"));
       ]
