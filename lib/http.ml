open Lwt.Syntax
open Lwt.Infix
module Err = Server_error

(* maybe we can use some combination of monads Lwt and Result to remove this custom impl*)
(* ps: this assumes all processing steps are Lwt.t and will always return a Result (a, e) *)
let _bind (promisse : ('a, 'e) result Lwt.t) (f : 'a -> ('b, 'e) result Lwt.t) =
  promisse
  >>= fun result ->
  match result with Ok x -> f x | Error e -> Lwt.return_error e

let ( >>@ ) = _bind

let from_body req ty_of_yojson =
  let* body = Dream.body req in
  try Lwt.return_ok @@ (body |> Yojson.Safe.from_string |> ty_of_yojson)
  with Failure _ -> Lwt.return_error Err.InvalidJson

let to_json yojson_of_t value =
  try Lwt.return_ok @@ (value |> yojson_of_t |> Yojson.Safe.to_string)
  with Failure _ -> Lwt.return_error Err.InvalidJson

let to_resp result =
  match result with
  | Ok result -> Dream.json ~code:200 result
  | Error e -> Err.to_response e

let create_todo_handler req db =
  from_body req Item.t_new_item_of_yojson
  >>@ Item.create_item db
  >>@ to_json Item.yojson_of_t
  >>= to_resp

let query_todo_handler id _req db =
  Item.by_id db id >>@ to_json Item.yojson_of_t >>= to_resp

let routes =
  Dream.router
    [
      Dream.get "/" (fun _ -> Dream.html "Todo's");
      Dream.post "/todos" (fun req -> Dream.sql req @@ create_todo_handler req);
      Dream.get "/todos/:id" (fun req ->
          let id = Dream.param req "id" in
          Dream.sql req @@ query_todo_handler (int_of_string id) req);
    ]
