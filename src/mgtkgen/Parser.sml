(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

(* Parser for GTK .defs files
 *
 * Henning Niss, February 2000
 *)


structure Parser =
struct

    structure TE = TypeExp
    structure TI = TypeInfo

    open Parsercomb

    infix 6 $-- --$ #-- --#
    infix 5 --
    infix 3 >> >>*
    infix 2 >>=
    infix 0 ||

    (* character categories *)
    fun quoteSymb c = c = #"\""
    val letterSymb = Char.isAlpha
    val digitSymb  = Char.isDigit
    fun wordSymb #"-" = true
      | wordSymb #"_" = true
      | wordSymb c = letterSymb c orelse digitSymb c

    (* lexer like definitions *)
    val comment = skipWS (";" $-- (getChars0 (fn c=>not(c= #"\n"))) --$ "\n")
    fun skipComment pf = repeat0 comment #-- skipWS pf
    fun skipCommentWS pf = skipComment pf

    val $ = skipCommentWS o Parsercomb.$

    val equals = $ "="
    val fields = $ "fields"
    val defMdl = $ "define-module" || $ "module"
    val defObj = $ "define-object"
    val defFnc = $ "define-func"
    val defFlags = $ "define-flags"
    val defEnum =  $ "define-enum"
    val defBoxed = $ "define-boxed"
    val defSignal = $ "define-signal"
    val listQual = $ "list"

    (* word actually allows slightly more than we want --- words
       should not begin with a digit, underscore, or hypen. *)
    val word   = skipCommentWS (getChars1 wordSymb)
    val string = skipCommentWS ("\"" $-- (getChars0 (not o quoteSymb)) --$ "\"")

    val openParen  = skipCommentWS ($# #"(")
    val closeParen = skipCommentWS ($# #")")

    fun parens pf  = skipCommentWS (openParen #-- pf --# closeParen)
    fun parens' pf = skipCommentWS (withPos (openParen #-- pf --# closeParen))

    (* modules *)
    local
	val current: string list ref = ref []
    in  
	fun declareModule path = (current := path;
				  !current)
	fun mkModulePath name = 
	    let val name' = NameUtil.separate_words #" " name
		val namePath = String.tokens Char.isSpace name'
		val path' = let fun loop 0 _ acc = rev acc
				  | loop d ([],[]) acc = rev acc
                                  | loop d (e::p,e'::p') acc = if e=e' then loop (d-1) (p,p') (e::acc) 
							     else rev acc
		                  | loop d ([], _) acc = rev acc
                                  | loop d (_, []) acc = rev acc
			    in  loop 1 (!current, namePath) []
			    end
		val pathStr = Util.stringSep "" "" "" (fn s=>s) path'
		val base = NameUtil.remove pathStr name
(*
		val _ = print ("path= " ^ pathStr ^ ", base=" ^ base ^ "\n")
*)
	    in  path' @ [base]
	    end
    end


    (* name resolution *)
    local
	type type_info = TE.long_texp
	exception Find
	val (table: (string, type_info) Polyhash.hash_table) = 
	    Polyhash.mkPolyTable (*(hash, compare)*) (99, Find)
	(* insert some primitive types in the symbol table *)
	val _ = app (fn n => Polyhash.insert table (n,TE.LONG([],TE.PRIMTYPE n)))
	            ["none","int","uint","float","bool","string",
		     "static_string","GtkType"
                    ]
	val _ = app (Polyhash.insert table)
	            [("GtkObject",TE.LONG([],TE.WIDGET("GtkObject",NONE))),
		     ("GtkWidget",TE.LONG([],TE.WIDGET("GtkWidget",SOME "GtkObject")))
		    ]
    in  fun mkLong constr tExp = TE.LONG([], constr tExp)

	fun insert tName func = Polyhash.insert table (tName, func)
	fun insertTypeName tName = insert tName (mkLong TE.PRIMTYPE tName)
	fun insertFlag tName = insert tName (mkLong TE.FLAG (tName,false))
	fun insertEnum tName = insert tName (mkLong TE.FLAG (tName,true))
	fun insertWidget wid inh = insert wid (mkLong TE.WIDGET (wid,SOME inh))
	fun insertPointer box inh = insert box (mkLong TE.POINTER (box,inh))
	fun lookup tName =
	    ((Polyhash.find table tName)
	     handle Find => Util.notFound("unbound type name: " ^ tName))
	fun mkTypeExp name = lookup name
    end

    (* construct abstract syntax *)
    type parlist = (TE.long_texp * string) list
    fun splitList p l =
	let fun f (elem, (acc1,acc2)) =
	        if p elem then (elem::acc1,acc2)
		else (acc1, elem::acc2)
	in  List.foldr f ([],[]) l
	end
    fun separateParams (retType, params: parlist) =
	let fun mkTuple ([], retType) = retType
              | mkTuple (pars, retType) =
	        let val tOut = if TI.isVoidType retType then pars
			       else retType :: pars
		in  case tOut of
		       nil => raise Fail ("mkTuple: has to return *something*")
		     | [t] => t
                     | ts => TE.LONG([], TE.TUPLE ts)
		end
	    val (outPars, pars) = splitList (TI.isOutputType o #1) params
	in  (outPars, pars, mkTuple (map #1 outPars, retType))
	end
    fun mkFunType (retType, []) = 
	raise Fail ("mlFunType: no parameters")
      | mkFunType (retType, params) =
	let val (outPars, pars, retType) = separateParams (retType, params)
	in  TE.LONG([], TE.ARROW(pars, outPars, params, retType))
	end

    fun mkModuleDecl (pos, (name, NONE)) =
	let val path = declareModule [name]
	in  AST.MODULE_DECL (pos, true, path)
        end
      | mkModuleDecl (pos, (name, SOME sub)) =
	let val path = declareModule [sub,name]
	in  AST.MODULE_DECL (pos, true, path)
	end

    fun mkObjectDecl (pos, (((name, inherits), fields))) = 
	let val path = declareModule (mkModulePath name)
	in  insertWidget name inherits
	  ; [AST.MODULE_DECL (pos, false, path),
             AST.OBJECT_DECL (pos, TE.LONG([],TE.WIDGET(name,SOME inherits)),fields)
            ]
	end

    fun mkFunctionDecl (pos, ((name, typeExp), params)) =
	let val dummyPair = (TE.LONG([], TE.PRIMTYPE "none"), "dummy")
	    val params' = List.filter (not o TI.isNullType') params
	    val params'' = if null params' then [dummyPair] else params'
	    val retType = typeExp
	    val shortType = if List.length params' = List.length params then NONE
			    else SOME (mkFunType (retType, params''))
	in  AST.FUNCTION_DECL (pos, name, AST.FUNTYPE(mkFunType (retType, params),shortType))
	end
    fun mkFlagsDecl false (pos, (flagName, cs)) = 
	( insertFlag flagName
	; AST.FLAGS_DECL(pos, TE.LONG([], TE.FLAG(flagName, false)), cs)
        )
      | mkFlagsDecl true (pos, (flagName, cs)) =
	( insertEnum flagName
	; AST.FLAGS_DECL(pos, TE.LONG([], TE.FLAG(flagName, true)), cs)
        )
    fun mkBoxedDecl (pos, (((name, inherits), names), size)) =
	( insertPointer name inherits
	; AST.BOXED_DECL (pos, TE.LONG([], TE.POINTER(name,inherits)), names)
	)
    fun mkSignalDecl (pos, ((name,signal),cbType)) = 
	(AST.SIGNAL_DECL (pos, mkTypeExp name, signal, cbType))

    fun mkBoxedInherits NONE = NONE
      | mkBoxedInherits (SOME(NONE)) = SOME(TE.INH_ROOT)
      | mkBoxedInherits (SOME(SOME inh)) = SOME(TE.INH_FROM inh)

    fun mkCBType (retType, pars) = 
	TE.LONG ([], TE.ARROW(pars, [], pars, retType))

    fun ensureNonEmpty [] = [(TE.LONG ([], TE.PRIMTYPE "none"), "dummy")]
      | ensureNonEmpty pars = pars

    datatype type_flag = NULL_TYPE | OUTPUT_TYPE
    fun toType ((x1,x2),SOME NULL_TYPE) = (TE.LONG ([],TE.OPTION x1),x2)
      | toType ((x1,x2),SOME OUTPUT_TYPE) = (TE.LONG ([],TE.OUTPUT x1), x2)
      | toType ((x1,x2), NONE) = (x1, x2)

    (* various *)
    val typeExp =  (word >> mkTypeExp)
                || (parens (word --# listQual) >> ((mkLong TE.LIST) o mkTypeExp))

    (* parameters *)
    val default = parens (equals #-- string)
    val flag =  (parens ($ "null-ok") >> (fn _ => NULL_TYPE))
             || (parens ($ "output")  >> (fn _ => OUTPUT_TYPE))
    val par = parens (typeExp -- word)
    val parWithOptDefault = parens 
	(   typeExp
	 -- word
	 -- (optional flag)
	--# (optional default)
	)   >> toType
    val parList = parens (repeat0 par)
    val parDefaultList = parens (repeat0 parWithOptDefault) >> ensureNonEmpty 

    (* modules *)
    val mdlDecl = parens'
        (   defMdl
        #-- word
         -- optional (parens ($"submodule-of" #-- word))
        )

    (* objects *)
    val inherits = parens word
    val fieldList = parens (fields #-- repeat1 par) >> op::
    val objDecl = parens' 
	(   defObj
	#-- word
	 -- inherits
	 -- optional fieldList
	)

    (* functions *)
    val fncDecl = parens' 
	(   defFnc
	#-- word
	 -- typeExp
	 -- parDefaultList
	)

    (* enums/flags *)
    val constr = parens (word #-- word)
    val constructors = repeat1 constr >> (op ::)
    val enumDecl = parens' (defEnum #-- word -- constructors)
    val flagsDecl = parens' (defFlags #-- word -- constructors)

    (* boxed types *)
    val size = string 
    val inherits = optional (parens (optional word)) >> mkBoxedInherits
    val boxedDecl = parens' 
	(   defBoxed 
	#-- word
	 -- inherits
	 -- repeat0 word
	 -- optional size
	)

    (* signals *)
    val cbType = parens (typeExp -- parList) >> mkCBType
    val signalName = (string >> (fn n => [n]))
                  || (parens (string -- string) >> (fn (n,p) => [p,n]))
    val signalDecl = parens' 
	(   defSignal
	#-- word
	 -- signalName
	 -- optional cbType
	)

    (* the various forms of declarations *)
    val mkSng = fn e => [e]
    val decl' = (mdlDecl >> (mkSng o mkModuleDecl))
             || (objDecl >> mkObjectDecl) 
             || (fncDecl >> (mkSng o mkFunctionDecl))
             || (enumDecl >> (mkSng o mkFlagsDecl true))
             || (flagsDecl >> (mkSng o mkFlagsDecl false))
             || (boxedDecl >> (mkSng o mkBoxedDecl))
             || (signalDecl >> (mkSng o mkSignalDecl))

(*
    fun definePred (Lexer.WORD (pos, str)) = String.isPrefix "define" str
      | definePred _ = false

    val decl = (decl' || (skipN definePred 2 #-- decl))
*)
    val decl = decl'

    val decls =   (repeat1 decl >> (List.concat o op::))
	      --# (getChars0 Char.isSpace) (* remove remaining whitespace *)

end (* structure Parser *)

(*

   [decls stream] parses stream as a sequence (one or more) of
   declarations. This is the topmost entry to the parser (and
   the only one documented here). The output is a list of declarations
   (see AST).

*)
