structure Name :> NAME = struct

    (* utilities *)
    fun takeWhile p xs acc =
        let fun loop [] acc = (rev acc,[])
              | loop (x::xs) acc = if p x then loop xs (x :: acc) 
                                   else (rev acc, x :: xs)
        in  loop xs acc
        end
    fun separateWords word =
        let val word = explode word
            val _ = if null word then raise Fail("separate_words: empty word")
                    else ()
	    fun add [] rest = rest
	      | add cur rest = cur :: rest
            fun split [] (cur,rest) = List.map (implode o rev) 
		                               (rev (add cur rest))
              | split (c::cs) (acc as (cur, rest)) =
                if Char.isUpper c 
                then let val (upper,cs') = takeWhile Char.isUpper cs []
                     in  case upper of
                         (* Make sure that single capitalizations get
                            treated correctly. The Gtk convention is as
                            follows:
                               PrefixXName => Prefix, XName
                               PrefixXYName => Prefix, XY, Name
                         *)
                             [] => split cs' ([c], add cur rest)
                         |   [c'] => split cs' ([c', c], add cur rest)
                         |   upper =>
				 let val upper' = rev upper
				     val next = hd upper'
				     val word = tl upper' @ [c]
				 in  if length cs' > 0 
				     then split cs' ([next], add word (add cur rest))
				     else split cs' (upper' @ [c], add cur rest)
				 end
                     end
                else split cs (c :: cur, rest)
                        
            val words = split word ([],[])
	    fun coalesce words =
		let fun loop [] = []
		      | loop (x::"Type"::xs) = (x^"Type") :: loop xs
		      | loop (x::xs) = x :: loop xs
		in  loop words end
        in  coalesce words
        end

    val separateUnderscores = String.tokens (fn c=> #"_"=c)

    (* names *)
    type name = {path: string list, fullpath: string list, base: string list}

    fun equal (n1, n2) =
	#path n1 = #path n2 andalso
	#fullpath n1 = #fullpath n2 andalso
	#base n1 = #base n2

    fun toString name = 
	(case #path name of
	     [] => ""
	   | p => Util.stringSep "" "." "." (fn s=>s) p
        ) ^ Util.stringSep "" "" "" (fn s=>s) (#base name)

    fun fromString base = (* FIXME *)
	{path=[],fullpath=[],base=separateWords base}

    fun fromPaths (fullpath,path,base) = {fullpath=fullpath,path=path,base=base}

    val getPath = #path
    val getFullPath = #fullpath
    val getBase = #base

    val toLower = String.map Char.toLower
    val toUpper = String.map Char.toUpper
    fun capitalize "" = ""
      | capitalize s  = 
	String.str(Char.toUpper (String.sub (s,0))) ^ String.extract(s,1,NONE)

    fun separate s = Util.stringSep "" "" s (fn s=>s)
    val dotSep = separate "." (* Util.stringSep "" "" "." (fn s=>s) *)
    val undSep = separate "_" (* Util.stringSep "" "" "_" (fn s=>s) *)
    val noSep  = String.concat
    fun combine sep (path,base) = if String.size path = 0 then base
				  else path ^ sep ^ base
	
    fun underscored name = 
	let val (path,base) = (getPath name, getBase name)
	in  combine "." (dotSep path, toLower(undSep base))
	end

    fun asModule name =
	let val (path,base) = (getPath name, getBase name)
	in  combine "" (noSep path, noSep(map capitalize base))
	end
    val asEnum = underscored
    val asMethod = underscored
    val asField = underscored
    val asSignal = underscored
    fun asType name =
	let val (path,base) = (getPath name, getBase name)
	in  noSep(map toLower base)
	end

    fun asCName sep trans name =
	let val (path,base) = (getFullPath name, getBase name)
	in  combine sep (trans(separate sep path), trans(separate sep base))
	end

(*
    fun asEnum' name =
	let val (path,base) = (getFullPath name, getBase name)
	in  combine "_" (toLower(undSep path), toLower(undSep base))
	end
*)
    val asCEnum = asCName "_" toLower
    val asCMethod = asCName "_" toLower

end (* structure Name *)