(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004  *)

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

    fun optionCmp cmp (x, y) =
	case (x, y) of
	    (NONE, NONE) => EQUAL
	  | (NONE, SOME y) => LESS
	  | (SOME x, NONE) => GREATER
	  | (SOME x, SOME y) => cmp (x, y)

    fun listCmp cmp (xs, ys) =
	let fun loop [] [] = EQUAL
              | loop [] (y::ys) = LESS
              | loop (x::xs) [] = GREATER
	      | loop (x::xs) (y::ys) = 
		let val ord = cmp (x,y)
		in  if ord = EQUAL then loop xs ys else ord
		end
	in  loop xs ys
	end

    fun splitWhile p l =
	let fun loop [] acc = (rev acc, [])
	      | loop (x::xs) acc = if p x then loop xs (x::acc)
				   else (rev acc, x::xs)
	in  loop l []
	end

    fun abort code = 
	let val msg = "Internal error: " ^ Int.toString code
	in  raise Fail msg end

end (* structure Util *)
