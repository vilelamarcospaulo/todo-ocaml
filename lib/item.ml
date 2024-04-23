open Lwt.Syntax
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Server_error

module type DB = Caqti_lwt.CONNECTION

type t = { id : int; title : string; description : string; completedAt : int option }
[@@deriving yojson]

type t_new_item = { title : string; description : string } [@@deriving yojson]

module Q = struct
  open Caqti_request.Infix
  module T = Caqti_type

  (* To handle a query with more than 4 fields check this issue *)
  (* The caqti recommends to use tuple of tuples to resolve this issue *)
  (* https://github.com/paurkedal/ocaml-caqti/issues/100 *)

  let reg_item =
    (T.tup2 T.string T.string ->! T.tup4 T.int T.string T.string (T.option T.ptime))
    @@ "INSERT INTO todos (title, description) VALUES (?, ?) RETURNING id, title, description, \
        completed_at;"

  let all = (T.unit ->! T.int) @@ "SELECT count(id) FROM todos;"

  let by_id =
    (T.int ->! T.tup4 T.int T.string T.string (T.option T.ptime))
    @@ "SELECT id, title, description, completed_at FROM todos WHERE id = ?;"

  let delete_by_id =
    (T.int ->! T.tup4 T.int T.string T.string (T.option T.ptime))
    @@ "DELETE FROM todos WHERE id = ? RETURNING id, title, description, completed_at;"

  let tuple_to_t (id, title, description, completed_at_as_time) =
    let time_to_int v =
      match v with Some x -> Ptime.to_span x |> Ptime.Span.to_int_s | None -> None
    in
    let completedAt = time_to_int completed_at_as_time in

    { id; title; description; completedAt }
end

let create_item (module Db : DB) (new_item : t_new_item) =
  let* result = Db.find Q.reg_item (new_item.title, new_item.description) in
  match result with
  | Ok row -> Lwt.return_ok @@ Q.tuple_to_t row
  | Error err -> Lwt.return_error @@ DBError (Caqti_error.show err)

let by_id (module Db : DB) id =
  let* result = Db.find_opt Q.by_id id in
  match result with
  | Error err -> Lwt.return_error @@ DBError (Caqti_error.show err)
  | Ok row -> (
      match row with
      | Some row -> Lwt.return_ok @@ Q.tuple_to_t row
      | None -> Lwt.return_error RecordNotFound)

let delete_by_id (module Db : DB) id =
  let* result = Db.find_opt Q.delete_by_id id in
  match result with
  | Error err -> Lwt.return_error @@ DBError (Caqti_error.show err)
  | Ok row -> (
      match row with
      | Some row -> Lwt.return_ok @@ Q.tuple_to_t row
      | None -> Lwt.return_error RecordNotFound)
