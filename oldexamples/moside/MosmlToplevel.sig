signature MosmlToplevel =
sig
    type t = {send : string -> string,
	      quit : unit   -> unit}

    val newFull : string -> string list -> string * t
    val new     : string list -> string * t
end
