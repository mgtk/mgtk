structure Name :> NAME = struct

    (* utilities *)
    fun takeWhile p xs acc =
        let fun loop [] acc = (rev acc,[])
              | loop (x::xs) acc = if p x then loop xs (x :: acc) 
                                   else (rev acc, x :: xs)
        in  loop xs acc
        end

    fun coalesce words =
	let fun loop [] = []
	      (* Special-case some Gtk name patterns *)
	      | loop ("get"::"type"::xs) = "get"::"type" :: loop xs
	      | loop ("get"::"buffer"::xs) = "get"::"buffer" :: loop xs
	      | loop ("Window"::"Type"::xs) = "Window"::"Type" :: loop xs
	      | loop (x::"Type"::xs) = (x^"Type") :: loop xs
	      | loop (x::"type"::xs) = (x^"type") :: loop xs
	      | loop (x::"Buffer"::xs) = (x^"Buffer") :: loop xs
	      | loop (x::"buffer"::xs) = (x^"buffer") :: loop xs
	      | loop (x::"Renderer"::xs) = (x^"Renderer") :: loop xs
	      | loop (x::"renderer"::xs) = (x^"renderer") :: loop xs
	      | loop (x::"Iter"::xs) = (x^"Iter") :: loop xs
	      | loop (x::"iter"::xs) = (x^"iter") :: loop xs
	      | loop ("Accel"::"Group"::xs) = ("AccelGroup") :: loop xs
	      | loop ("accel"::"group"::xs) = ("accelgroup") :: loop xs
	      | loop ("Text"::"View"::xs) = ("TextView") :: loop xs
	      | loop ("text"::"view"::xs) = ("textview") :: loop xs
	      | loop ("Text"::"Child"::"Anchor"::xs) = ("TextChildAnchor") :: loop xs
	      | loop ("text"::"child"::"anchor"::xs) = ("textchildanchor") :: loop xs
	      | loop ("Text"::"Mark"::xs) = ("TextMark") :: loop xs
	      | loop ("text"::"mark"::xs) = ("textmark") :: loop xs
	      | loop ("Tree"::"Model"::xs) = ("TreeModel") :: loop xs
	      | loop ("tree"::"model"::xs) = ("treemodel") :: loop xs
	      | loop ("Tree"::"View"::"Column"::xs) = ("TreeViewColumn") :: loop xs
	      | loop ("tree"::"view"::"column"::xs) = ("treeviewcolumn") :: loop xs
	      | loop ("Tree"::"View"::xs) = ("TreeView") :: loop xs
	      | loop ("tree"::"view"::xs) = ("treeview") :: loop xs
	      | loop ("Tree"::"Selection"::xs) = ("TreeSelection") :: loop xs
	      | loop ("tree"::"selection"::xs) = ("treeselection") :: loop xs
	      | loop ("Progress"::"Bar"::xs) = ("ProgressBar") :: loop xs
	      | loop ("progress"::"bar"::xs) = ("progressbar") :: loop xs
	      | loop ("Rc"::"Style"::xs) = "RcStyle" :: loop xs
	      | loop (x::xs) = x :: loop xs
	in  loop words end

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
        in  split word ([],[])
        end

    val separateUnderscores = String.tokens (fn c=> #"_"=c)

    val separateWords = coalesce o separateWords
    val separateUnderscores = coalesce o separateUnderscores

    (* names *)
    type name = {path: string list, fullpath: string list, base: string list}

    fun equal (n1: name, n2: name) =
	#path n1 = #path n2 andalso
	#fullpath n1 = #fullpath n2 andalso
	#base n1 = #base n2

    fun toString (name:name) = 
	(case #path name of
	     [] => ""
	   | p => Util.stringSep "" "." "." (fn s=>s) p
        ) ^ Util.stringSep "" "" "" (fn s=>s) (#base name)

    fun toString' (name:name) = 
	Util.stringSep "(" ")" "." (fn s=>s) (#path name)
        ^ Util.stringSep "[" "]" "." (fn s=>s) (#fullpath name)
        ^ Util.stringSep "<" ">" "." (fn s=>s) (#base name)

    (* FIXME: Do we wish to allow names to be broken apart? *)
    fun pp name = Pretty.ppString(toString name) 
    fun pp' name = Pretty.ppString(toString' name)

        
(*
    fun compare (n1, n2) =
	case Util.listCmp String.compare (#path n1, #path n2) of
	    EQUAL => Util.listCmp String.compare(#base n1, #base n2)
	  | order => order

    (* FIXME *)
*)
(*
    fun compare (n1, n2) = Util.listCmp String.compare (#base n1, #base n2)
*)

    fun compare (n1:name, n2:name) = 
	let fun tos ns = Util.stringSep "" "" "" (fn s=>s) ns
	in  String.compare(tos (#fullpath n1 @ #path n1 @ #base n1), 
			   tos (#fullpath n2 @ #path n2 @ #base n2))
	end

    fun fromString base = (* FIXME *)
	{path=[],fullpath=[],base=separateWords base}

    fun fromPaths (fullpath,path,base) = {fullpath=fullpath,path=path,base=base}
    val getPath : name -> string list = #path
    val getFullPath : name -> string list = #fullpath
    val getBase : name -> string list = #base


    (* make sure a name gets output as a valid ML name *)
    fun mlify name =
	let val (f,p,b) = (getFullPath name, getPath name, getBase name)
	    fun ify "Type" = "type_t" (* FIXME *)
	      | ify n = n
	in  fromPaths(map ify f, map ify p, map ify b) end

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
	
    fun prune (path,base) =
	case (path,base) of (* FIXME big time! *)
	    (p::prest,b::brest) => if p = b then ([],base)
				   else (path,base)
	  | _ => (path,base)

    fun underscored name = 
	let val (path,base) = prune (getPath name, getBase name)
	in  combine "." (dotSep path, toLower(undSep base))
	end
    fun asModule name =
	let val (path,base) = prune (getPath name, getBase name)
	in  combine "" (noSep path, noSep(map capitalize base))
	end
    fun asSMLName sep trans name =
	let val (path,base) = prune (getPath name, getBase name)
	in  combine sep (trans(separate sep path), trans(separate sep base))
	end

    val asEnum = underscored o mlify
    val asEnumConst = asSMLName "_" toUpper
    val asBoxed = underscored
    val asMethod = underscored
    val asField = underscored
    val asSignal = underscored
    fun asType name =
	let val (path,base) = prune (getPath name, getBase name)
	in  noSep(map toLower base)
	end

    fun asCName sep trans name =
	let val (path,base) = prune (getFullPath name, getBase name)
	in  combine sep (trans(separate sep path), trans(separate sep base))
	end

(*
    fun asEnum' name =
	let val (path,base) = (getFullPath name, getBase name)
	in  combine "_" (toLower(undSep path), toLower(undSep base))
	end
*)
    local 
	fun ify [] acc = rev acc
	  | ify ("CellRenderer"::rest) acc = ify rest ("Renderer"::"Cell"::acc)
	  | ify ("TextBuffer"::rest) acc = ify rest ("Buffer"::"Text"::acc)
	  | ify ("accelgroup"::rest) acc = ify rest ("group"::"accel"::acc)
	  | ify ("AccelGroup"::rest) acc = ify rest ("Group"::"Accel"::acc)
	  | ify ("textview"::rest) acc = ify rest ("view"::"text"::acc)
	  | ify ("TextView"::rest) acc = ify rest ("View"::"Text"::acc)
	  | ify ("textchildanchor"::rest) acc = ify rest ("anchor"::"child"::"text"::acc)
	  | ify ("TextChildAnchor"::rest) acc = ify rest ("Anchor"::"Child"::"Text"::acc)
	  | ify ("textmark"::rest) acc = ify rest ("mark"::"text"::acc)
	  | ify ("TextMark"::rest) acc = ify rest ("Mark"::"Text"::acc)
	  | ify ("withbuffer"::rest) acc = ify rest ("buffer"::"with"::acc)
	  | ify ("shadowtype"::rest) acc = ify rest ("type"::"shadow"::acc)
	  | ify ("childtype"::rest) acc = ify rest ("type"::"child"::acc)
	  | ify ("columntype"::rest) acc = ify rest ("type"::"column"::acc)
	  | ify ("getiter"::rest) acc = ify rest ("iter"::"get"::acc)
	  | ify ("setbuffer"::rest) acc = ify rest ("buffer"::"set"::acc)
	  | ify ("storagetype"::rest) acc = ify rest ("type"::"storage"::acc)
	  | ify ("curvetype"::rest) acc = ify rest ("type"::"curve"::acc)
	  | ify ("toiter"::rest) acc = ify rest ("iter"::"to"::acc)
	  | ify ("selectioniter"::rest) acc = ify rest ("iter"::"selection"::acc)
	  | ify ("childiter"::rest) acc = ify rest ("iter"::"child"::acc)
	  | ify ("modeliter"::rest) acc = ify rest ("iter"::"model"::acc)
	  | ify ("enditer"::rest) acc = ify rest ("iter"::"end"::acc)
	  | ify ("startiter"::rest) acc = ify rest ("iter"::"start"::acc)
	  | ify ("unselectiter"::rest) acc = ify rest ("iter"::"unselect"::acc)
	  | ify ("selectiter"::rest) acc = ify rest ("iter"::"select"::acc)
	  | ify ("converteriter"::rest) acc = ify rest ("iter"::"converter"::acc)
	  | ify ("convertiter"::rest) acc = ify rest ("iter"::"convert"::acc)
	  | ify ("storeiter"::rest) acc = ify rest ("iter"::"store"::acc)
	  | ify ("treemodel"::rest) acc = ify rest ("model"::"tree"::acc)
	  | ify ("TreeModel"::rest) acc = ify rest ("Model"::"Tree"::acc)
	  | ify ("treeviewcolumn"::rest) acc = ify rest ("column"::"view"::"tree"::acc)
	  | ify ("TreeViewColumn"::rest) acc = ify rest ("Column"::"View"::"Tree"::acc)
	  | ify ("treeview"::rest) acc = ify rest ("view"::"tree"::acc)
	  | ify ("TreeView"::rest) acc = ify rest ("View"::"Tree"::acc)
	  | ify ("treeselection"::rest) acc = ify rest ("selection"::"tree"::acc)
	  | ify ("TreeSelection"::rest) acc = ify rest ("Selection"::"Tree"::acc)
	  | ify ("progressbar"::rest) acc = ify rest ("bar"::"progress"::acc)
	  | ify ("ProgressBar"::rest) acc = ify rest ("Bar"::"Progress"::acc)
	  | ify (w::rest) acc = ify rest (w::acc)	    
    in
    fun cify (name:name) =
	fromPaths(ify (#fullpath name) [],
		  ify (#path name) [],
		  ify (#base name) [])
    end (* local *)

    val asCType = asCName "" capitalize
    val asCEnum = asCName "_" toLower
    val asCBoxed = asCName "" (fn s=>s)
    val asCFunc = (asCName "_" toLower) o cify
    val asCEnumConst = asCName "_" toUpper
    fun asCStub name = "mgtk_" ^ asCName "_" toLower (cify name)
    fun asCGetEnum name = "mgtk_get_" ^ asCName "_" toLower name

end (* structure Name *)