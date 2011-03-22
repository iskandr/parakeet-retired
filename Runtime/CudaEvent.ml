module CuEvent = Int64

external cuda_create_event : unit -> CuEvent.t = "ocaml_cuda_create_event"
external cuda_destroy_event : CuEvent.t -> unit = "ocaml_cuda_destroy_event"
external cuda_record_event : CuEvent.t -> unit = "ocaml_cuda_record_event"
external cuda_stop_event_and_get_elapsed_time :
  CuEvent.t -> CuEvent.t -> float = "ocaml_cuda_stop_event_and_get_elapsed_time"
