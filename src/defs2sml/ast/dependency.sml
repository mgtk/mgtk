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
    fun add deps (name,d) = M.insert(deps,name,S.add(lookup deps name,d))
    fun addSet deps (name,d) = M.insert(deps,name,S.union(lookup deps name,d))
    fun addList ((name,ds),deps) = M.insert(deps,name,S.addList(lookup deps name, ds))
    fun union (s,s') = S.union(s,s')

    fun showdeps deps = Util.stringSep "{" "}\n" "," (fn s=>s) deps
    fun showdepset deps = showdeps (S.listItems deps)

    fun dependencies module =
	let fun loop (A.Module{name,info,members}, deps as (depmap,depset)) = 
		let val (map,ds) = List.foldl loop' (depmap,emptySet) members
		    val ds = S.difference(ds,S.singleton String.compare name)
		in  (addSet map (name,ds), union(depset,ds))
		end
	    and loop' (A.Sub module, deps) = loop (module, deps)
	      | loop' (A.Member{name=n,info}, deps as (depmap,depset)) =
		let val freeTyNames = T.freeTyNames
		    fun get (A.Method ty) = freeTyNames ty
		      | get (A.Field ty) = freeTyNames ty
		      | get (A.Object(ty,parent,impl)) = 
			ty :: (case parent of NONE => impl | SOME p => p::impl)
		      | get (A.Boxed _) = []
		      | get (A.Enum _ ) = []
(*		      | get (A.Signal ty) = freeTyNames ty*)
		      | get (A.Signal ty) = []
		in  (depmap, S.addList(depset, get info)) end
	in  #1(loop (module, (empty,emptySet))) end
					
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
	    val A.Module{name,info=(done,info),members} = module
	in  A.Module{name=name,info=info,
		     members=List.concat(List.map loop' members)}
	end

    (* For debugging: *)
    local open Pretty in
    val reorder = fn module =>
        let val pps = Pretty.ppString
	    val pp = AST.ppAst pps (Type.pp pps (fn _ => empty))
	    val module' = reorder module
	in  if Debug.included "DependencyReorder.debug_deps" then
		( print("Before reordering defs:\n")
		; ppPlain (pp module) TextIO.stdOut
		; print("After reordering defs:\n")
		; ppPlain (pp module') TextIO.stdOut)
	    else ()
          ; module'
        end
    end (* local *)

end (* structure DependencyReorder *)


val _ = Debug.add {name="DependencyReorder.debug_deps",
		   short_option="dep",long_option=SOME("debug-dependency"),
		   included=false}