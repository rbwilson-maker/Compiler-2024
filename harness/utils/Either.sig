signature EITHER =
sig
  datatype ('a, 'b) t = ERR of 'a | OK of 'b
  val map : ('a1 -> 'a2) -> ('b1 -> 'b2) -> ('a1, 'b1) t -> ('a2, 'b2) t
  val partition : ('a -> ('b, 'c) t) -> 'a list -> 'b list * 'c list
  val toValue : (JSON.value, JSON.value) t -> JSON.value
  val fromValue : JSON.value -> (JSON.value, JSON.value) t
end
