module type Keyable =
  sig
    include Base.Hashtbl.Key.S
    val to_string : t -> string
  end
module Make :
  functor (K : Keyable) (V : Keyable) ->
    sig
      type t
      val of_array : sorted:(K.t * V.t) array -> int -> t
      val enq : t -> K.t -> V.t -> unit
      val deq : t -> V.t option
      val contains_value : t -> V.t -> bool
      val change : t -> V.t -> (K.t option -> K.t option) -> unit
    end
