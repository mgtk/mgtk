structure ResolveNames :> ResolveNames =
struct

    val toLower = String.map Char.toLower

    fun toName ret split current name = 
	let val splits = split name
(*
	    fun return full [] = (rev full,[],[])
	      | return full [p] = (rev full,[],[p])
	      | return full path =
		let val reversed = rev path
		in (rev full, rev(tl reversed), [hd reversed]) end
	    fun loop (e::p,e'::p') acc = 
		if toLower e = toLower e' then loop (p,p') acc
		else (rev acc, e'::p')
	      | loop ([], p') acc = (rev acc, p')
	      | loop (_, []) acc = ([],rev acc) (* FIXME *)
*)
	    val show_path = Util.stringSep "{" "}" "-" (fn s => s)

	    fun return full path = ret (rev full, path)
	    fun loop (e::p, e'::p') full = 
		if Name.toLower e = Name.toLower e' then loop (p,p') (e::full)
		else return full (e'::p')
	      | loop ([], p') full = return full p'
	      | loop (p, []) full = return (p@full) []
	    val (full,path,base) = loop (current, splits) []
        in  (full,path,base)
	end

    open AST

    type 'a ty = ('a,'a) Type.ty
    type name = Name.name

    structure Set = Splayset

    fun resolve module =
	let 
	    val show_path = Util.stringSep "{" "}" "-" (fn s => s)
		    
	    fun compare (n1, n2) = 
		let fun tos ns = Util.stringSep "" "" "" (fn s=>s) ns
		in  String.compare(tos(Name.getFullPath n1 @ Name.getBase n1), 
				   tos(Name.getFullPath n2 @ Name.getBase n2))
		end
	    val seenbefore = ref (Set.empty Name.compare)
	    fun add name = seenbefore := Set.add(!seenbefore,name)
	    val _ = add (Name.fromString "GObject")
	    fun lookup tyname current (full,base) =
		if Set.member(!seenbefore, Name.fromPaths(full,[],base))
		then (full,base,[])
		else (full,[], base)
	    fun id (full,base) = (full,[],base)
	    fun splitfcn info = 
                case info of
                    Method _ => Name.separateUnderscores
	          | Field _ => (fn s => [s])
		  | i => Name.separateWords

	    fun resMod (parent,current) m =
	        case m of
		    Module{name,members,info} =>
			let val (full,path,base) = toName id Name.separateWords current name
			    val current' = Name.separateWords name
			    val name' = Name.fromPaths(full,path,base)
			    val _ = MsgUtil.debug("  module: " ^ name ^ " (at " ^ show_path current ^ ") -> " ^ Name.toString' name' ^"\n")
			in  Module{name=name',
				   members=List.map (resMem (current,current')) members,
				   info=SOME name'}
			before
			    add name'
			end
	    and resMem (parent,current) m =
		case m of
		    Sub m => Sub(resMod (current,current) m)
	          | Member{name,info} => 
			let val (full,path,base) = toName id (splitfcn info) current name
			    val name' = Name.fromPaths(full,path,base)
			    val _ = MsgUtil.debug("  member: " ^ name ^ " (at " ^ show_path current ^ ") -> " ^ Name.toString' name')
			in  Member{name=name',
				   info= resMemInfo (parent,current) info}
			end
	    and resMemInfo (parent,current) (Enum(flags, enums)) =
		let fun res e = 
			let val (f,p,b) = toName id (Name.separateUnderscores) current e
			in  Name.fromPaths(f,p,b) end
		in  Enum(flags,List.map res enums) end
	      | resMemInfo path (Method ty) = Method(resType path ty)
	      | resMemInfo path (Field ty) = Field(resType path ty)
	      | resMemInfo path (Signal ty) = Signal(resType path ty)
	      | resMemInfo path (Boxed funcs) = Boxed funcs
	      | resMemInfo path (Object obj) = Object(resObject path obj)
	    and resType (parent,current) ty =
		let fun demod_ty (Type.Tname _, tyname) = 
			let val (f,p,b) = toName (lookup tyname current) Name.separateWords current tyname
			in  Name.fromPaths(f,p,b) end
		      | demod_ty (Type.Base _, tyname) = Name.fromPaths([],[],[tyname])
		      | demod_ty (_,_) = raise Fail("resType: shouldn't happen")
		    fun demod_def (ty,d) = Name.fromPaths(toName id Name.separateUnderscores current d)
		in  Type.mapiv demod_ty demod_def ty end
	    and resObject (parent,current) (ty,typarent,impl) =
		let fun demod n =
		    let val (f,p,b) = toName (lookup n current) Name.separateWords parent n
		    in  Name.fromPaths(f,p,b) end
		in  (demod ty, Option.map demod typarent, List.map demod impl)
		end
	in  resMod ([],[]) module
	end

    fun modularize module =
	let fun modMod m =
		case m of 
		    Module{name={path,base,fullpath},members,info} =>
		       Module{name=Name.fromPaths (fullpath,[],base),
			      members=List.map (modMem) members,
			      info=info}
	    and modMem m =
		case m of
		    Sub m => Sub(modMod m)
		  | Member{name={path,base,fullpath},info} =>
			Member{name=Name.fromPaths(fullpath,[],base),info=info}
	in  modMod module
	end

    (* For debugging: *)
    val resolve = fn module => 
        let val module' = resolve module
	    val pp = AST.ppAst Name.pp (Type.pp Name.pp' Name.pp')
	in  if Debug.included "ResolveNames.debug_resolve_names" then
		( print("After resolving names:\n")
		; Pretty.ppPlain (pp module') TextIO.stdOut )
	    else ()
          ; module'
        end

end (* structure ResolveNames *)

val _ = Debug.add {name="ResolveNames.debug_resolve_names",
		   short_option="drn",long_option=SOME("debug-resolve-names"),
		   included=false}