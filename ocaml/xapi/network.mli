module Net :
	sig
		val reopen_logs : 'a -> bool
		val clear_state : 'a -> unit
		val reset_state : 'a -> unit
		val set_gateway_interface :
			string -> name:Network_interface.iface -> unit
		val set_dns_interface :
			string -> name:Network_interface.iface -> unit
		module Interface :
			sig
				val get_all : string -> 'a -> string list
				val exists : string -> name:Network_interface.iface -> bool
				val get_mac : string -> name:Network_interface.iface -> string
				val is_up : string -> name:Network_interface.iface -> bool
				val get_ipv4_addr :
					string ->
					name:Network_interface.iface -> (Unix.inet_addr * int) list
				val set_ipv4_conf :
					string ->
					name:Network_interface.iface ->
					conf:Network_interface.ipv4 -> unit
				val get_ipv4_gateway :
					string -> name:Network_interface.iface -> Unix.inet_addr option
				val set_ipv4_gateway :
					string ->
					name:Network_interface.iface ->
					address:Network_interface.Unix.inet_addr -> unit
				val get_ipv6_addr :
					string ->
					name:Network_interface.iface -> (Unix.inet_addr * int) list
				val set_ipv6_conf :
					string ->
					name:Network_interface.iface ->
					conf:Network_interface.ipv6 -> unit
				val get_ipv6_gateway :
					string -> name:Network_interface.iface -> Unix.inet_addr option
				val set_ipv6_gateway :
					string ->
					name:Network_interface.iface ->
					address:Network_interface.Unix.inet_addr -> unit
				val set_ipv4_routes :
					string ->
					name:Network_interface.iface ->
					routes:(Network_interface.Unix.inet_addr * int *
									Network_interface.Unix.inet_addr)
								 list ->
					unit
				val get_dns :
					string ->
					name:Network_interface.iface ->
					Unix.inet_addr list * string list
				val set_dns :
					string ->
					name:Network_interface.iface ->
					nameservers:Network_interface.Unix.inet_addr list ->
					domains:string list -> unit
				val get_mtu : string -> name:Network_interface.iface -> int
				val set_mtu :
					string -> name:Network_interface.iface -> mtu:int -> unit
				val set_ethtool_settings :
					string ->
					name:Network_interface.iface ->
					params:(string * string) list -> unit
				val set_ethtool_offload :
					string ->
					name:Network_interface.iface ->
					params:(string * string) list -> unit
				val is_connected : string -> name:Network_interface.iface -> bool
				val is_physical : string -> name:Network_interface.iface -> bool
				val is_vif_front : string -> name:Network_interface.iface -> bool
				val get_pci_bus_path :
					string -> name:Network_interface.iface -> string
				val bring_up : string -> name:Network_interface.iface -> unit
				val bring_down : string -> name:Network_interface.iface -> unit
				val is_persistent :
					string -> name:Network_interface.iface -> bool
				val set_persistent :
					string -> name:Network_interface.iface -> value:bool -> unit
				val make_config :
					string ->
					?conservative:bool ->
					config:(Network_interface.iface *
									Network_interface.interface_config_t)
								 list ->
					'a -> unit
				val rename :
					string ->
					name:Network_interface.iface ->
					new_name:Network_interface.iface -> unit
				val set_driver_domain :
					string ->
					name:Network_interface.iface ->
					uuid:Network_interface.domain -> unit
			end
		module Bridge :
			sig
				val get_all : string -> 'a -> string list
				val get_bond_links_up :
					string -> name:Network_interface.port -> int
				val create :
					string ->
					?vlan:Network_interface.bridge * int ->
					?mac:string ->
					?other_config:(string * string) list ->
					name:Network_interface.bridge -> 'a -> unit
				val destroy :
					string ->
					?force:bool -> name:Network_interface.bridge -> 'a -> unit
				val get_kind : string -> 'a -> Network_interface.kind
				val get_ports :
					string ->
					name:Network_interface.bridge -> (string * string list) list
				val get_all_ports :
					string -> ?from_cache:bool -> 'a -> (string * string list) list
				val get_bonds :
					string ->
					name:Network_interface.bridge -> (string * string list) list
				val get_all_bonds :
					string -> ?from_cache:bool -> 'a -> (string * string list) list
				val is_persistent :
					string -> name:Network_interface.bridge -> bool
				val set_persistent :
					string -> name:Network_interface.bridge -> value:bool -> unit
				val get_vlan :
					string ->
					name:Network_interface.bridge -> (string * int) option
				val add_port :
					string ->
					?bond_mac:string ->
					bridge:Network_interface.bridge ->
					name:Network_interface.port ->
					interfaces:Network_interface.iface list ->
					?bond_properties:(string * string) list -> 'a -> unit
				val remove_port :
					string ->
					bridge:Network_interface.bridge ->
					name:Network_interface.port -> unit
				val get_interfaces :
					string -> name:Network_interface.bridge -> string list
				val get_fail_mode :
					string ->
					name:Network_interface.bridge ->
					Network_interface.fail_mode option
				val make_config :
					string ->
					?conservative:bool ->
					config:(Network_interface.bridge *
									Network_interface.bridge_config_t)
								 list ->
					'a -> unit
				val set_driver_domain :
					string ->
					name:Network_interface.bridge ->
					uuid:Network_interface.domain -> unit
			end
	end

val transform_networkd_exn : [ `PIF ] API.Ref.t -> (unit -> 'a) -> 'a

val driver_domain_devs :
	__context:Context.t -> (string * string) list ->
	(int * (int * int * int * int)) list

val get_bond : API.pIF_t -> [ `Bond ] API.Ref.t option

val get_vlan : API.pIF_t -> API.ref_VLAN option

val get_tunnel : API.pIF_t -> [ `tunnel ] API.Ref.t option

val get_pif_type :
	API.pIF_t ->
		[> `bond_pif of [ `Bond ] API.Ref.t
			| `phy_pif
			| `tunnel_pif of [ `tunnel ] API.Ref.t
			| `vlan_pif of API.ref_VLAN ]

val get_base_pif :
	__context:Context.t -> [ `PIF ] API.Ref.t -> [ `PIF ] API.Ref.t list

val get_driver_domain :
	__context:Context.t -> [ `PIF ] API.Ref.t -> [ `VM ] API.Ref.t option
