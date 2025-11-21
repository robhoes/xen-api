(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
open Http
open Forkhelpers

module D = Debug.Make (struct let name = "xapi_logs_download" end)

open D

let logs_download_handler (req : Request.t) s reqd =
  debug "running logs-download handler" ;
  Http_svr.respond_with_pipe reqd @@ fun s' send_headers ->
  Xapi_http.with_context "Downloading host logs" req s (fun __context ->
      send_headers [Http.Hdr.connection, "close"; "Cache-Control", "no-cache, no-store"] ;
      debug "sent the http headers" ;
      let pid =
        safe_close_and_exec None (Some s') None [] !Xapi_globs.logs_download []
      in
      waitpid_fail_if_bad_exit pid
  )
