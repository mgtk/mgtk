structure ParseUtils :> ParseUtils = 
struct

    type pos = AST.pos
    type declaration = AST.declaration
    type name = NameUtil.name
    type texp = TypeExp.texp
    type inherits = TypeExp.inherits
	
    structure TE = TypeExp
    structure TI = TypeInfo
    structure NU = NameUtil

    (* Name resolution *)
    local
	type type_info = TE.texp
	exception Find
	val (table: (string, type_info) Polyhash.hash_table) = 
	    Polyhash.mkPolyTable (*(hash, compare)*) (99, Find)
	(* insert some primitive types in the symbol table *)
	val _ = app (fn n => Polyhash.insert table (n,TE.PRIMTYPE n))
	            ["none","int","uint","float","bool","string",
		     "static_string","GtkType"
                    ]
	val _ = app (Polyhash.insert table)
	            [("GtkObject",TE.WIDGET((["Gtk"],["Object"]),
					    TE.INH_ROOT)),
		     ("GtkWidget",TE.WIDGET((["Gtk"],["Widget"]),
					    TE.INH_FROM(["Gtk"],["Object"])))
		    ]
    in  fun insert name exp = (Polyhash.insert table (name, exp); exp)
	fun insertTypeName tName = insert tName (TE.PRIMTYPE tName)
	fun insertFlag flag (path,base) = 
	    insert flag (TE.FLAG ((path,base),false))
	fun insertEnum enum (path,base) = 
	    insert enum (TE.FLAG ((path,base),true))
	fun insertWidget wid (path,base,inh) = 
	    insert wid (TE.WIDGET ((path,base),inh))
	fun insertPointer box (path,base,inh) = 
	    insert box (TE.POINTER ((path,base),inh))
	fun lookup tName =
	    ((Polyhash.find table tName)
	     handle Find => Util.notFound("unbound type name: " ^ tName))
	fun mkTypeExp name = lookup name
    end

    (* Modules *)
    val current: string list ref = ref []
    fun split namePath = 
	let val (path',base') = 
	    let fun loop 0 (_,p') acc = (rev acc,p')
		  | loop d (e::p,e'::p') acc = 
		       if NU.toLower e = NU.toLower e' 
		       then loop (d-1) (p,p') (e::acc) 
		       else (rev acc, p')
		  | loop d ([], p') acc = (rev acc, p')
		  | loop d (_, []) acc = raise Fail "split: current path is longer than name"
	    in  loop 1 (!current, namePath) []
	    end
	in  (path', base')
	end

    fun combine sep path = Util.stringSep "" "" sep (fn s=>s) path
    fun splitWords name = 
	let val name' = NU.separate_words #" " name
	    val namePath = String.tokens Char.isSpace name'
	in  split namePath
	end
    fun splitUnderscores name =
	let val namePath = String.tokens (fn c=> #"_"=c) name
	    val (path,base) = split namePath
	in  split namePath
	end	

    fun declareModule path = current := path
    val splitWidgetName = splitWords
    val splitBoxedName  = splitWords
    val splitFlagName   = splitWords
    val splitEnumName   = splitFlagName
    val splitFunName    = splitUnderscores

    (* Construct abstract syntax *)
    type parlist = (TE.texp * string) list
    fun splitList p l =
	let fun f (elem, (acc1,acc2)) =
	        if p elem then (elem::acc1,acc2)
		else (acc1, elem::acc2)
	in  List.foldr f ([],[]) l
	end
    fun separateParams (retType, params: parlist) =
	let fun mkTuple ([], retType) = retType              | mkTuple (pars, retType) =
	        let val tOut = if TI.isVoidType retType then pars
			       else retType :: pars
		in  case tOut of
		       nil => raise Fail ("mkTuple: has to return *something*")
		     | [t] => t
                     | ts => TE.TUPLE ts
		end
	    val (outPars, pars) = splitList (TI.isOutputType o #1) params
	in  (outPars, pars, mkTuple (map #1 outPars, retType))
	end
    fun mkFunType (retType, []) = 
	raise Fail ("mlFunType: no parameters")
      | mkFunType (retType, params) =
	let val (outPars, pars, retType) = separateParams (retType, params)
	in  TE.ARROW(pars, outPars, params, retType)
	end

    fun mkModuleDecl (pos, (name, NONE)) =
	let val _ = declareModule [name]
	in  AST.MODULE_DECL (pos, true, [name])
        end
      | mkModuleDecl (pos, (name, SOME sub)) =
	let val _ = declareModule [sub,name]
	in  AST.MODULE_DECL (pos, true, [sub,name])
	end

    fun mkWidgetDecl (pos, ((name, inherits), fields)) = 
	let val (mPath,mBase) = splitWidgetName name
	    val _ = declareModule (mPath @ mBase)
	    val tExp = insertWidget name (mPath,mBase,inherits)
	in  [AST.MODULE_DECL (pos, false, mPath @ mBase),
             AST.OBJECT_DECL (pos, tExp, fields)
            ]
	end
    fun mkFunDecl (pos, ((name, typeExp), params)) =
	let val dummyPair = (TE.PRIMTYPE "none", "dummy")
	    val params' = List.filter (not o TI.isNullType') params
	    val params'' = if null params' then [dummyPair] else params'
	    val retType = typeExp
	    val shortType = if List.length params' = List.length params then NONE
			    else SOME (mkFunType (retType, params''))
	    val funname = splitFunName name
	in  AST.FUNCTION_DECL (pos, funname,
			       AST.FUNTYPE(mkFunType (retType, params),shortType))
	end
    fun mkFlagsDecl false (pos, (flagName, cs)) = 
	let val (path, base) = splitFlagName flagName
	    val tExp = insertFlag flagName (path,base)
	    val cs' = map splitUnderscores cs
	in  AST.FLAGS_DECL(pos, tExp, cs')
	end
      | mkFlagsDecl true (pos, (enumName, cs)) =
	let val (path, base) = splitEnumName enumName
	    val tExp = insertEnum enumName (path,base)
	    val cs' = map splitUnderscores cs
	in  AST.FLAGS_DECL(pos, tExp, cs')
	end
    fun mkBoxedDecl (pos, (((name, inherits), names), size)) =
	let val (path, base) = splitBoxedName name
	    val tExp = insertPointer name (path,base,inherits)
	in  AST.BOXED_DECL (pos, tExp, names)
	end
    fun mkSignalDecl (pos, ((wid,signal),cbType)) = 
	(AST.SIGNAL_DECL (pos, wid, signal, cbType))

    fun mkWidgetInherits NONE = TE.INH_ROOT
      | mkWidgetInherits (SOME inh) = TE.INH_FROM (splitWidgetName inh)

    fun mkBoxedInherits NONE = NONE
      | mkBoxedInherits (SOME(NONE)) = SOME(TE.INH_ROOT)
      | mkBoxedInherits (SOME(SOME inh)) = 
	SOME(TE.INH_FROM (splitBoxedName inh))

    fun mkCBType (retType, pars) = TE.ARROW(pars, [], pars, retType)

    val mkSng = fn e => [e]
    val mkModuleDecl  = mkSng o mkModuleDecl
    val mkFunDecl     = mkSng o mkFunDecl
    val mkFlagsDecl   = fn b => mkSng o (mkFlagsDecl b)
    val mkBoxedDecl   = mkSng o mkBoxedDecl
    val mkSignalDecl  = mkSng o mkSignalDecl

end (* structure ParseUtils *)