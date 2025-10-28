val get_capabilities : unit -> string

val handler : Http.Request.t -> Unix.file_descr -> Http_svr.reqd -> unit
