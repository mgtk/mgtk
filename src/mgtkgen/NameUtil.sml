(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

structure NameUtil :> NameUtil =
struct

    (* Utility functions - handling type names *)
    fun removePrefix name =
	if String.isPrefix "Gtk" name then String.extract(name, 3, NONE)
	else name
    fun remove_prefix name =
	if String.isPrefix "gtk_" name then String.extract(name, 4, NONE)
	else name
    fun remove_PREFIX name =
	if String.isPrefix "GTK_" name then String.extract(name, 4, NONE)
	else name
    val toLower = (String.map Char.toLower)
    val toUpper = (String.map Char.toUpper)

    fun takeWhile p xs acc =
	let fun loop [] acc = (acc,[])
	      | loop (x::xs) acc = if p x then loop xs (x :: acc) 
				   else (acc, x :: xs)
	in  loop xs acc
	end

    fun separateWords sep typExp =
	let val typExp = explode typExp
	    val _ = if null typExp then raise Fail("separateWords: empty type expression")
		    else ()
	    fun find saw_C [] = []
              | find saw_C (c::cs) =
		if saw_C then if c = #"L" orelse c = #"T" 
			      then Char.toLower c :: find false cs
			      else c :: find false cs
		else c :: find (c = #"C") cs

	    fun find [] acc = rev acc
	      | find (c::cs) acc =
		case c::cs of
		   #"C":: #"L":: #"i":: #"s":: #"t":: cs => find cs (#"t":: #"s":: #"i":: #"l" :: #"C":: acc)
		 | #"C":: #"T":: #"r":: #"e":: #"e":: cs => find cs (#"e":: #"e":: #"r":: #"t" :: #"C":: acc)
		 | _ => find cs (c :: acc)

	    fun addSep l = if null l then l else sep :: l
	    fun split [] acc = rev acc
	      | split (c::cs) acc =
		if Char.isUpper c 
		then let val (upper,cs') = takeWhile Char.isUpper cs []
		     in  if length upper > 0
			 then if length cs' > 0 
			      then split cs' (hd upper :: sep :: (tl upper @ c :: addSep acc))
			      else rev (upper @ (c :: addSep acc))
			 else split cs' (c :: addSep acc)
		     end
		else split cs (c :: acc)
		        
	    val typExp' = find typExp []
	    val typExp'' = split typExp' []
	    val typExp''' = map Char.toLower typExp''
	in  implode typExp'''
	end

end (* structure NameUtil *)