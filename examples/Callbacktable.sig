signature Callbacktable =
sig
    type key = int
    type 'a t
    val new : int -> 'a t
    val insert : 'a t -> key * 'a -> unit
    val peek : 'a t -> key -> 'a option
    val remove : 'a t -> key -> unit
end
