type error =
  | Database_error of string

val or_error: ('a, Caqti_error.t) result Lwt.t -> ('a, error) result Lwt.t

val pool: (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t
