(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

structure Util =
struct

    local
	fun str _ nil _ _ = ""
	  | str p (h::t) sep needSep =
	    let val s = p h ^ (str p t sep true)
	    in  if needSep then sep ^ s else s
	    end
    in
	fun stringSep start finish sep p l = 
	    start ^ (str p l sep false) ^ finish
    end (* local *)

    fun memoize cmp f =
        let open Splaymap
	    val dict = ref (mkDict cmp)
	in  fn k => case peek(!dict,k) of
			SOME x => x
		      | NONE => let val x = f k
				    val _ = dict := insert(!dict,k,x)
				in  x end
	end

end (* structure Util *)
