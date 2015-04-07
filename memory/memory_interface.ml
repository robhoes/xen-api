(*
 * Copyright (C) Citrix Systems Inc.
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
(**
 * @group Memory
 *)

let service_name = "memory"
let queue_name = Xcp_service.common_prefix ^ service_name
let json_path = "/var/xapi/memory.json"
let xml_path = "/var/xapi/memory"

type reservation_id = string

exception Cannot_free_this_much_memory of (int64 * int64)
exception Domains_refused_to_cooperate of (int list)
exception Unknown_reservation of (reservation_id)
exception No_reservation
exception Invalid_memory_value of (int64)

type debug_info = string

type session_id = string

external get_diagnostics: debug_info -> string = ""

external login: debug_info -> string -> session_id = ""

external reserve_memory: debug_info -> session_id -> int64 -> reservation_id = ""

external reserve_memory_range: debug_info -> session_id -> int64 -> int64 -> reservation_id * int64 = ""

external delete_reservation: debug_info -> session_id -> reservation_id -> unit = ""

external transfer_reservation_to_domain: debug_info -> session_id -> reservation_id -> int -> unit = ""

external query_reservation_of_domain: debug_info -> session_id -> int -> reservation_id = ""

external balance_memory: debug_info -> unit = ""

external get_host_reserved_memory: debug_info -> int64 = ""

external get_host_initial_free_memory: debug_info -> int64 = ""

type domain_zero_policy =
   | Fixed_size of int64 (** it will never be ballooned *)
   | Auto_balloon of int64 * int64 (** it will auto-balloon over a range *)

external get_domain_zero_policy: debug_info -> domain_zero_policy = ""

