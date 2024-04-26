type config = { db_uri : string }
type t = { sqlconn : Caqti_lwt.connection }

val env_config : config
val test_config : string -> config
val init : config -> t
