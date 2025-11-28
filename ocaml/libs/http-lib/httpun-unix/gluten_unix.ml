module D = Debug.Make (struct let name = "gluten_unix" end)

open D
module Buffer = Gluten.Buffer

module Proxy = struct
  open Angstrom
  
  module AU = Unbuffered

  let is_cr = function '\r' -> true | _ -> false

  let eol = string "\r\n" <?> "eol"

  let proxy = string "PROXY " <?> "PROXY"

  let proxy_header =
    proxy *> take_till is_cr <* eol <* commit

  let rec transition buf off len = function
    | AU.Done (consumed, value) ->
      debug "PROXY: %s (%d)" value consumed ;
      consumed, Some value
    | AU.Fail (consumed, _marks, msg) ->
      debug "NO PROXY: %s (%d)" msg consumed ;
      consumed, None
    | AU.Partial { committed; continue } ->
      (* The first iteration will always hit this, as AU.parse did
         not get buf yet. *)
      transition buf off len (continue buf ~off ~len AU.Incomplete)

  let parse_proxy buf off len =
    transition buf off len (AU.parse proxy_header)
end

module IO_loop = struct
  let writev socket iovecs =
    try
      let lenv =
        List.fold_left
          (fun acc {Faraday.buffer; off; len} ->
            let _ : int = Unix.write_bigarray socket buffer off len in
            acc + len
          )
          0 iovecs
      in
      `Ok lenv
    with End_of_file -> `Closed

  let read_once socket buffer =
    Buffer.put
      ~f:(fun buf ~off ~len k ->
        let n = Unix.read_bigarray socket buf off len in
        match n with 0 -> raise End_of_file | _ -> k n
      )
      buffer
      (fun _ -> ())

  let read socket buffer =
    match read_once socket buffer with
    | r ->
        r
    | exception
        (Unix.Unix_error (ENOTCONN, _, _) | Unix.Unix_error (EBADF, _, _)) ->
        raise End_of_file

  let shutdown socket cmd =
    try Unix.shutdown socket cmd
    with Unix.Unix_error (ENOTCONN, _, _) | Unix.Unix_error (EBADF, _, _) ->
      ()

  let close socket =
    try Unix.close socket
    with Unix.Unix_error (ENOTCONN, _, _) | Unix.Unix_error (EBADF, _, _) ->
      ()

  let start :
      type t.
         (module Gluten.RUNTIME with type t = t)
      -> read_buffer_size:int
      -> t
      -> Unix.file_descr
      -> string option Atomic.t
      -> unit =
   fun (module Runtime) ~read_buffer_size t socket proxy ->
    let write_closed = ref false in
    let read_buffer = Buffer.create read_buffer_size in
    let checked_proxy = ref false in
    let check_proxy () =
      if not !checked_proxy then (
        debug "checking PROXY" ;
        (* Read and parse PROXY header, if present *)
        (* Assumption: the buffer is large enough to fit the entire PROXY header
           and the first read op from the socket will read the whole header. *)
        let _ : int = Buffer.get read_buffer ~f:(fun buf ~off ~len ->
            let consumed, proxy' = Proxy.parse_proxy buf off len in
            Atomic.set proxy proxy' ;
            consumed
        ) in
        checked_proxy := true
      )
    in
    let rec read_loop () =
      let rec read_loop_step () =
        match Runtime.next_read_operation t with
        | `Read ->
            debug "read request" ;
            ( match read socket read_buffer with
            | _n ->
                check_proxy () ;
                let (_ : int) =
                  Buffer.get read_buffer ~f:(fun buf ~off ~len ->
                      Runtime.read t buf ~off ~len
                  )
                in
                ()
            | exception End_of_file ->
                debug "read EOF" ;
                let (_ : int) =
                  Buffer.get read_buffer ~f:(fun buf ~off ~len ->
                      Runtime.read_eof t buf ~off ~len
                  )
                in
                ()
            ) ;
            read_loop_step ()
        | `Yield ->
            Runtime.yield_reader t read_loop
        | `Close -> (
            match read socket read_buffer with
            | _n ->
                (* discard *)
                ()
            | exception (End_of_file as exn) -> (
                shutdown socket Unix.SHUTDOWN_RECEIVE ;
                match !write_closed with
                | true ->
                    (* If the write loop has finished, the loop is closing
                       cleanly. We don't need to do anything else. *)
                    ()
                | false ->
                    (* If the write loop hasn't yet finished, but we got EOF from
                       read (i.e. socket closed), we want to feed EOF to the
                       writer here so that we can terminate cleanly. *)
                    Runtime.report_exn t exn
              )
          )
      in
      match read_loop_step () with
      | () ->
          ()
      | exception exn ->
          debug "%s" (Printexc.to_string exn) ;
          Runtime.report_exn t exn
    in
    let rec write_loop () =
      let rec write_loop_step () =
        match Runtime.next_write_operation t with
        | `Write io_vectors ->
            let write_result = writev socket io_vectors in
            Runtime.report_write_result t write_result ;
            write_loop_step ()
        | `Yield ->
            Runtime.yield_writer t write_loop
        | `Close _ ->
            write_closed := true ;
            shutdown socket Unix.SHUTDOWN_SEND
      in
      match write_loop_step () with
      | () ->
          ()
      | exception exn ->
          debug "%s" (Printexc.to_string exn) ;
          Runtime.report_exn t exn
    in
    let _ : Thread.t =
      Thread.create
        (fun () ->
          Runtime.yield_writer t write_loop ;
          read_loop () ;
          debug "! closing socket" ;
          close socket
        )
        ()
    in
    ()
end

module Server = struct
  let create_connection_handler ~read_buffer_size ~protocol connection
      _client_addr socket =
    let connection = Gluten.Server.create ~protocol connection in
    let proxy = Atomic.make None in
    IO_loop.start (module Gluten.Server) ~read_buffer_size connection socket proxy

  let create_upgradable_connection_handler ~read_buffer_size ~protocol
      ~create_protocol ~request_handler (client_addr : Unix.sockaddr) socket =
    let proxy = Atomic.make None in
    let connection =
      Gluten.Server.create_upgradable ~protocol ~create:create_protocol
        (request_handler client_addr proxy)
    in
    debug "Gluten_unix.Server.create_upgradable_connection_handler" ;
    IO_loop.start (module Gluten.Server) ~read_buffer_size connection socket proxy
end
