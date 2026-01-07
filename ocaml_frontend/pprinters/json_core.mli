(** JSON serialization for Core AST - mirrors pp_core.ml structure *)

open Core

(** Configuration module type *)
module type CONFIG =
sig
  (** Show functions from #include files *)
  val show_include: bool
end

(** Output module type *)
module type JSON_CORE =
sig
  (** Serialize a generic Core file to JSON.
      This function handles all top-level elements:
      - main function symbol
      - tag definitions (struct/union)
      - stdlib functions (if enabled)
      - implementation constants
      - global variables
      - functions (respecting show_include config)
      - external symbol map
      - function info map
   *)
  val json_file: ('a, 'b) generic_file -> Yojson.Safe.t
end

module Make (C : CONFIG) : JSON_CORE

(** Only includes definitions from the main file *)
module Basic : JSON_CORE

(** Includes all definitions including from headers *)
module All : JSON_CORE