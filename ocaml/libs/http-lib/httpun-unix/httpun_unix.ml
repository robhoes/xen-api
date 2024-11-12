module D = Debug.Make (struct let name = "gluten_unix" end)

open D

module Server = struct
  let create_connection_handler ?(config = Httpun.Config.default)
      ~request_handler ~error_handler client_addr socket =
    let create_connection =
      Httpun.Server_connection.create ~config
        ~error_handler:(error_handler client_addr)
    in
    debug "Httpun_unix.Server.create_connection_handler" ;
    Gluten_unix.Server.create_upgradable_connection_handler
      ~read_buffer_size:config.read_buffer_size
      ~protocol:(module Httpun.Server_connection)
      ~create_protocol:create_connection ~request_handler client_addr socket
end
