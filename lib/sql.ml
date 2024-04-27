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
let _bind promisse f = promisse >|= Result.map f
let ( >> ) = _bind

module Page = struct
  type page_params = { page : int; per_page : int } [@@deriving yojson]
  type page_params_opt = page_params option [@@deriving yojson]
  type 'a page = { data : 'a list; total_items : int; page : page_params_opt } [@@deriving yojson]

  let paginate page data = { data; total_items = List.length data; page }
end

module type QueryModule = sig
  type t
  type tuple_of_t
  type new_reg_parameters
  type 'a query_result = ('a, Caqti_error.call_or_retrieve) result Lwt.t

  (* To handle a query with more than 4 parameters check this issue *)
  (* The caqti recommends to use tuple of tuples to do it *)
  (* https://github.com/paurkedal/ocaml-caqti/issues/100 *)

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
    let create db new_item = Q.create db new_item >>! Q.tuple_to_t
    let query ?page db = Q.query db page >>* Q.tuple_to_t >> Page.paginate page
    let get_by_id db id = Q.get_by_id db id >>? Q.tuple_to_t
    let del_by_id db id = Q.del_by_id db id >>? Q.tuple_to_t
  end
