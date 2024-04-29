open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type item = { id : int; title : string; description : string; completedAt : int option }
[@@deriving yojson]

type t_new_item = { title : string; description : string } [@@deriving yojson]

module Q = struct
  open Caqti_request.Infix
  module T = Caqti_type

  module type DB = Caqti_lwt.CONNECTION

  type t = item
  type tuple_of_t = int * string * string * Ptime.t option
  type new_reg_parameters = string * string
  type 'a query_result = ('a, Caqti_error.call_or_retrieve) result Lwt.t

  let all =
    (T.unit ->* T.tup4 T.int T.string T.string (T.option T.ptime))
    @@ "SELECT id, title, description, completed_at FROM todos;"

  let query (module Db : DB) (page_params : Sql.Page.page_params_opt) =
    match page_params with
    | None -> Db.collect_list all ()
    | Some { page; size } ->
        let offset = (page - 1) * size in
        let limit = size in
        let catqi_query =
          (T.tup2 T.int T.int ->* T.tup4 T.int T.string T.string (T.option T.ptime))
          @@ "SELECT id, title, description, completed_at FROM todos LIMIT ? OFFSET ?;"
        in

        Db.collect_list catqi_query (limit, offset)

  let query_count (module Db : DB) =
    Db.find ((T.unit ->! T.int) @@ "SELECT count(id) FROM todos;") ()

  let create (module Db : DB) new_item =
    let caqti_query =
      (T.tup2 T.string T.string ->! T.tup4 T.int T.string T.string (T.option T.ptime))
      @@ "INSERT INTO todos (title, description) VALUES (?, ?) RETURNING id, title, description, \
          completed_at;"
    in

    Db.find caqti_query new_item

  let get_by_id (module Db : DB) id =
    let caqti_query =
      (T.int ->! T.tup4 T.int T.string T.string (T.option T.ptime))
      @@ "SELECT id, title, description, completed_at from todos WHERE id = ?;"
    in

    Db.find_opt caqti_query id

  let del_by_id (module Db : DB) id =
    let caqti_query =
      (T.int ->! T.tup4 T.int T.string T.string (T.option T.ptime))
      @@ "DELETE FROM todos WHERE id = ? RETURNING id, title, description, completed_at;"
    in

    Db.find_opt caqti_query id

  let tuple_to_t (id, title, description, completed_at_as_time) =
    let time_to_int v =
      match v with Some x -> Ptime.to_span x |> Ptime.Span.to_int_s | None -> None
    in
    let completedAt = time_to_int completed_at_as_time in

    { id; title; description; completedAt }
end

module Repo = Sql.RepoMod (Q)

let create db new_item = Repo.create db (new_item.title, new_item.description)
let query = Repo.query
let by_id = Repo.get_by_id
let delete_by_id = Repo.del_by_id
