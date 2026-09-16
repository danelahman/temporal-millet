module type S = sig
  type t

  val compare : t -> t -> int
  val fresh : string -> t

  val fresh_synthetic : string -> t
  (** A symbol the compiler invented rather than read from the source, such as a
      variable the desugarer binds to hoist a subcomputation. Error messages
      must never name one. *)

  val is_synthetic : t -> bool
  (** Whether the symbol came from {!fresh_synthetic}, directly or through
      {!refresh}. *)

  val refresh : t -> t
  val print : t -> Format.formatter -> unit
  val string_of : t -> string
end

module Make () : S = struct
  (* Identity is the [id] alone: two symbols made from the same name are
     distinct, and refreshing one keeps its name and its provenance. *)
  type t = { id : int; name : string; synthetic : bool }

  let compare s1 s2 = Int.compare s1.id s2.id
  let count = Atomic.make (-1)
  let next_id () = Atomic.fetch_and_add count 1 + 1
  let fresh name = { id = next_id (); name; synthetic = false }
  let fresh_synthetic name = { id = next_id (); name; synthetic = true }
  let is_synthetic s = s.synthetic
  let refresh s = { s with id = next_id () }
  let print s ppf = Format.fprintf ppf "%s" s.name
  let string_of s = s.name
end
