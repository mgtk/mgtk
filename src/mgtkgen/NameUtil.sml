(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

structure NameUtil :> NameUtil =
struct

    type name = string list (* path *) * string list (* base type name *)

(*old
    fun remove prefix name =
	let val l = String.size prefix
	in  if String.isPrefix prefix name then String.extract(name, l, NONE)
	    else name
	end
old*)

    (* Utility functions - handling strings *)
    val toLower = (String.map Char.toLower)
    val toUpper = (String.map Char.toUpper)

    fun takeWhile p xs acc =
	let fun loop [] acc = (acc,[])
	      | loop (x::xs) acc = if p x then loop xs (x :: acc) 
				   else (acc, x :: xs)
	in  loop xs acc
	end

    fun separate_words sep word =
	let val word = explode word
	    val _ = if null word then raise Fail("separate_words: empty word")
		    else ()
	    fun addSep l = if null l then l else sep :: l

	    fun split [] acc = rev acc
	      | split (c::cs) acc =
		if Char.isUpper c 
		then let val (upper,cs') = takeWhile Char.isUpper cs []
		     in  case upper of
			 (* Make sure that single capitalizations get
			    treated correctly. The Gtk convention is as
			    follows:
			       PrefixXName => Prefix, XName
			       PrefixXYName => Prefix, XY, Name
                         *)
			     [] => split cs' (c :: addSep acc)
			 |   [c'] => split cs' (c' :: c :: addSep acc)
			 |   upper =>
				 if length cs' > 0 
				 then split cs' (hd upper :: sep :: (tl upper @ c :: addSep acc))
				 else rev (upper @ (c :: addSep acc))
		     end
		else split cs (c :: acc)
		        
	    val word' = split word []
	in  implode word'
	end

    fun id x = x
    fun nameToString (path,base) = 
	Util.stringSep "" (Util.stringSep "." "" "." id base) "." id path

end (* structure NameUtil *)