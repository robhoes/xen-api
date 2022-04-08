exception Library_not_loaded of string
exception Symbol_not_loaded of string
type interface
type device
type enable_state = Disabled | Enabled
type memory_info = { total : int64; free : int64; used : int64; }
type pci_info = {
  bus_id : string;
  domain : int32;
  bus : int32;
  device : int32;
  pci_device_id : int32;
  pci_subsystem_id : int32;
}
type utilization = { gpu : int; memory : int; }
type pgpu_metadata = string
type vgpu_metadata = string
type vgpu_instance = int
type vm_domid = string
type vgpu_uuid = string
type vgpu_compatibility_t
type vm_compat = None | Cold | Hybernate | Sleep | Live
type pgpu_compat_limit = None | HostDriver | GuestDriver | GPU | Other
val library_open : unit -> interface
val library_close : interface -> unit
val init : interface -> unit
val shutdown : interface -> unit
val device_get_count : interface -> int
val device_get_handle_by_index : interface -> int -> device
val device_get_handle_by_pci_bus_id : interface -> string -> device
val device_get_memory_info : interface -> device -> memory_info
val device_get_pci_info : interface -> device -> pci_info
val device_get_temperature : interface -> device -> int
val device_get_power_usage : interface -> device -> int
val device_get_utilization_rates : interface -> device -> utilization
val device_set_persistence_mode :
  interface -> device -> enable_state -> unit
val device_get_pgpu_metadata : interface -> device -> pgpu_metadata
val pgpu_metadata_get_pgpu_version : pgpu_metadata -> int
val pgpu_metadata_get_pgpu_revision : pgpu_metadata -> int
val pgpu_metadata_get_pgpu_host_driver_version : pgpu_metadata -> string
val device_get_active_vgpus : interface -> device -> vgpu_instance list
val vgpu_instance_get_vm_domid : interface -> vgpu_instance -> vm_domid
val vgpu_instance_get_vgpu_uuid :
  interface -> vgpu_instance -> vgpu_uuid
val get_vgpu_metadata : interface -> vgpu_instance -> vgpu_metadata
val get_pgpu_vgpu_compatibility :
  interface -> vgpu_metadata -> pgpu_metadata -> vgpu_compatibility_t
val vgpu_compat_get_vm_compat : vgpu_compatibility_t -> vm_compat list
val vgpu_compat_get_pgpu_compat_limit :
  vgpu_compatibility_t -> pgpu_compat_limit list
val get_vgpus_for_vm : interface -> device -> vm_domid -> vgpu_instance list
val get_vgpu_for_uuid :
  interface -> vgpu_uuid -> vgpu_instance list -> vgpu_instance list
