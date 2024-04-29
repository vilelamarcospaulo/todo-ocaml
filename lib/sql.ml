open Lwt.Infix
open Server_error
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(*binds to setup a caqti result to a monad*)
let caqti_bind promisse f =
  promisse
  >|= function Ok row -> f row | Error err -> Result.Error (DBError (Caqti_error.show err))

let ( >>@ ) = caqti_bind
let ( >>! ) promisse f = promisse >>@ fun x -> Result.Ok (f x)

let ( >>? ) promisse f =
  promisse >>@ function Some x -> Result.Ok (f x) | None -> Result.Error RecordNotFound

let ( >>* ) promisse f = promisse >>@ fun x -> Result.Ok (List.map f x)
let _bind result f = Result.map f result
let ( >> ) = _bind

module Page = struct
  type page_params = { page : int; size : int } [@@deriving yojson]
  type page_params_opt = page_params option [@@deriving yojson]

  type current_page = { page : int; size : int; total_items : int; total_pages : int }
  [@@deriving yojson]

  type 'a page = { data : 'a list; page : current_page option } [@@deriving yojson]

  let paginated total_items (current_page : page_params) data =
    let total_pages = total_items / current_page.size in
    let page = { page = current_page.page; size = current_page.size; total_items; total_pages } in
    let page = Some page in

    { data; page }

  let not_paginated data = { data; page = None }
end

module type QueryModule = sig
  type t
  type tuple_of_t
  type new_reg_parameters
  type 'a query_result = ('a, Caqti_error.call_or_retrieve) result Lwt.t

  (* To handle a query with more than 4 parameters check this issue *)
  (* The caqti recommends to use tuple of tuples to do it *)
  (* https://github.com/paurkedal/ocaml-caqti/issues/100 *)

  val query_count : Caqti_lwt.connection -> int query_result
  val query : Caqti_lwt.connection -> Page.page_params_opt -> tuple_of_t list query_result
  val create : Caqti_lwt.connection -> new_reg_parameters -> tuple_of_t query_result
  val get_by_id : Caqti_lwt.connection -> int -> tuple_of_t option query_result
  val del_by_id : Caqti_lwt.connection -> int -> tuple_of_t option query_result
  val tuple_to_t : tuple_of_t -> t
end

module type DB = Caqti_lwt.CONNECTION

module RepoMod =
functor
  (Q : QueryModule)
  ->
  struct
    open Lwt.Syntax

    let create db new_item = Q.create db new_item >>! Q.tuple_to_t

    let query db page =
      let* data = Q.query db page >>* Q.tuple_to_t in

      match page with
      | None -> Lwt.return (data >> Page.not_paginated)
      | Some current_page ->
          let* total_items = Q.query_count db in
          let total_items = Result.get_ok total_items in

          Lwt.return (data >> Page.paginated total_items current_page)

    let get_by_id db id = Q.get_by_id db id >>? Q.tuple_to_t
    let del_by_id db id = Q.del_by_id db id >>? Q.tuple_to_t
  end
