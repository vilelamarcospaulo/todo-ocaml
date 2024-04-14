open Lwt.Syntax
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module type DB = Caqti_lwt.CONNECTION

type t = {
  id : int;
  title : string;
  description : string;
  completedAt : int option;
}
[@@deriving yojson]

type t_new_item = { title : string; description : string } [@@deriving yojson]
type _ error = ..
type _ error += DBError of string

module Q = struct
  open Caqti_request.Infix
  module T = Caqti_type

  (* To handle a query with more than 4 fields check this issue *)
  (* The caqti recommends to use tuple of tuples to resolve this issue *)
  (* https://github.com/paurkedal/ocaml-caqti/issues/100 *)

  let reg_item =
    T.tup2 T.string T.string
    ->! T.tup4 T.int T.string T.string (T.option T.ptime)
    @@ "INSERT INTO todos (title, description) VALUES (?, ?) RETURNING id, \
        title, description, completed_at"

  let tuple_to_t (id, title, description, completed_at_as_time) =
    let time_to_int v =
      match v with
      | Some x -> Ptime.to_span x |> Ptime.Span.to_int_s
      | None -> None
    in
    let completedAt = time_to_int completed_at_as_time in

    { id; title; description; completedAt }
end

let create_item (module Db : DB) new_item =
  let* result = Db.find Q.reg_item (new_item.title, new_item.description) in
  match result with
  | Ok row -> Lwt.return_ok @@ Q.tuple_to_t row
  | Error err -> Lwt.return_error @@ DBError (Caqti_error.show err)
