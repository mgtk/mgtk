(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

structure DependencyReorder :> DEPENDENCY_REORDER = struct

    structure A = AST
    structure T = Type
    structure M = Splaymap
    structure S = Splayset

    val empty = M.mkDict String.compare
    val emptySet = S.empty String.compare
    fun lookup deps name =
	case M.peek(deps,name) of
	    NONE => emptySet
	  | SOME d => d
    fun add ((name,d),deps) = M.insert(deps,name,S.add(lookup deps name,d))
    fun addList ((name,ds),deps) = M.insert(deps,name,S.addList(lookup deps name, ds))
	    
    fun dependencies module =
	let fun loop (A.Module{name,info,members}, deps) = 
		List.foldl (loop' name) deps members
	    and loop' name (A.Sub module, deps) = loop (module, deps)
	      | loop' name (A.Member{name=n,info}, deps) =
		let val freeTyNames = T.freeTyNames
		    fun get (A.Method ty) = freeTyNames ty
		      | get (A.Field ty) = freeTyNames ty
		      | get (A.Object(ty,parent,impl)) = 
			ty :: (case parent of NONE => impl | SOME p => p::impl)
		      | get (A.Boxed _) = []
		      | get (A.Enum _ ) = []
(*		      | get (A.Signal ty) = freeTyNames ty*)
		      | get (A.Signal ty) = []
		    val ds = List.map (fn dep => (name,dep)) (get info)
		in  addList ((name,get info), deps) end
	in  loop (module, empty) end
					
    fun build module =
	let fun loop (m as A.Module{name,info,members}, map) =
		M.insert(List.foldl loop' map members, name, SOME m)
	    and loop' (A.Sub module, map) = loop(module,map)
	      | loop' (A.Member({name,info=A.Enum _}), map) = 
		M.insert(map, name, NONE) (* Enums should be known, but should
					     not reflect dependencies ? *)
	      | loop' (A.Member _, map) = map
	    val empty = M.mkDict String.compare
	    fun addList map = List.foldl (fn ((x,k),m) => M.insert(m,x,k)) map
	    val initial = addList empty
	           [ ("GObject",NONE)
                   , ("GType",NONE)
                   ]
	in  loop(module, initial) end

    fun reorder module =
	let 
	    val module = A.map (fn i => (ref false,i), fn i => i) module
	    val map = build module
	    fun findModule name = M.find(map,name)
	    val deps = dependencies module
	    fun loop (A.Module{name,info,members=[]}) = []
	      | loop (A.Module{name,info=(done,info),members}) =
		if !done then []
		else let val _ = done := true
			 val ds = S.listItems(lookup deps name)
		     in  List.concat (List.map loop'' ds) @
			 [A.Module{name=name,info=info,
				   members=List.concat(List.map loop' members)}]
		     end
	    and loop' (A.Sub module) = List.map A.Sub (loop module)
	      | loop' (A.Member{name,info}) = [A.Member{name=name,info=info}]
	    and loop'' name = 
		(case findModule name of
		     SOME module => loop module
		   | NONE => []
		) handle M.NotFound => (MsgUtil.warning("Unbound dependency ("^name^")\n") ; [] )
	in  case loop module of
		[] => Util.abort 54321
	      | [m] => m
	      | ms =>
		let val _ = MsgUtil.warning "Toplevel module depends on:\n"
		    val deps = Util.stringSep "  {" "}\n" "," 
					      (fn (A.Module{name,...}) => name)
					      ms
		    val _ = MsgUtil.warning deps
		in  hd(rev ms)
		end
	end

end (* structure DependencyReorder *)