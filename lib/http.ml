open Lwt.Syntax
open Lwt.Infix
module Err = Server_error

(* maybe we can use some combination of monads Lwt and Result to remove this custom impl*)
(* ps: this assumes all processing steps are Lwt.t and will always return a Result (a, e) *)
let _bind (promisse : ('a, 'e) result Lwt.t) (f : 'a -> ('b, 'e) result Lwt.t) =
  promisse >>= function Ok x -> f x | Error e -> Lwt.return_error e

let ( >>@ ) = _bind

let _result_bind (promisse : ('a, 'e) result Lwt.t) (f : 'a -> ('b, 'e) result) =
  promisse >|= fun x -> Result.bind x f

let ( >> ) = _result_bind

let from_json ty_of_yojson str =
  try Result.Ok (str |> Yojson.Safe.from_string |> ty_of_yojson)
  with _ -> Result.error Err.InvalidJson

let to_json yojson_of_t value =
  try Result.ok (value |> yojson_of_t |> Yojson.Safe.to_string)
  with _ -> Result.error Err.InvalidJson

let from_body req ty_of_yojson =
  let* body = Dream.body req in
  Lwt.return @@ from_json ty_of_yojson body

let json_response result =
  match result with Ok result -> Dream.json ~code:200 result | Error e -> Err.to_response e

let create_todo_handler req db =
  from_body req Item.t_new_item_of_yojson
  >>@ Item.create db
  >> to_json Item.yojson_of_item
  >>= json_response

let query_todo_handler _req db =
  Item.query db >> to_json (Sql.Page.yojson_of_page Item.yojson_of_item) >>= json_response

let fetch_todo_handler id _req db =
  Item.by_id db id >> to_json Item.yojson_of_item >>= json_response

let delete_todo_handler id _req db =
  Item.delete_by_id db id >> to_json Item.yojson_of_item >>= json_response

let routes =
  Dream.router
    [
      Dream.get "/" (fun _ -> Dream.html "Todo's API");
      Dream.get "/todos" (fun req -> Dream.sql req @@ query_todo_handler req);
      Dream.post "/todos" (fun req -> Dream.sql req @@ create_todo_handler req);
      Dream.get "/todos/:id" (fun req ->
          let id = Dream.param req "id" in
          Dream.sql req @@ fetch_todo_handler (int_of_string id) req);
      Dream.delete "/todos/:id" (fun req ->
          let id = Dream.param req "id" in
          Dream.sql req @@ delete_todo_handler (int_of_string id) req);
    ]
