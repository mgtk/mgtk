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

    (* lexer like definitions *)
    val comment = skipWS (";" $-- (getChars0 (fn c=>not(c= #"\n"))) --$ "\n")
    fun skipComment pf = repeat0 comment #-- skipWS pf
    fun skipCommentWS pf = skipWS (skipComment pf)

    val $ = skipCommentWS o Parsercomb.$

    val equals = $ "="
    val fields = $ "fields"
    val defObj = $ "define-object"
    val defFnc = $ "define-func"
    val defFlags = $ "define-flags"
    val defEnum =  $ "define-enum"
    val defBoxed = $ "define-boxed"
    val defSignal = $ "define-signal"
    val listQual = $ "list"

    fun quoteSymb c = c = #"\""
    val letterSymb = Char.isAlpha
    val digitSymb  = Char.isDigit
    fun wordSymb #"-" = true
      | wordSymb #"_" = true
      | wordSymb c = letterSymb c orelse digitSymb c

    (* word actually allows slightly more than we want --- words
       should not begin with a digit, underscore, or hypen. *)
    val word   = skipCommentWS (getChars1 wordSymb)
    val string = skipCommentWS ("\"" $-- (getChars0 (not o quoteSymb)) --$ "\"")

    val openParen  = skipCommentWS ($# #"(")
    val closeParen = skipCommentWS ($# #")")

    fun parenthesized pf = skipCommentWS (openParen #-- pf --# closeParen)
    fun parenthesized' pf = skipCommentWS (withPos (openParen #-- pf --# closeParen))

    (* name resolution *)
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
	            [("GtkObject",TE.WIDGET("GtkObject",NONE)),
		     ("GtkWidget",TE.WIDGET("GtkWidget",SOME "GtkObject"))
		    ]
    in  fun insert tName func = Polyhash.insert table (tName, func)
	fun insertTypeName tName = insert tName (TE.PRIMTYPE tName)
	fun insertFlag tName = insert tName (TE.FLAG(tName,false))
	fun insertEnum tName = insert tName (TE.FLAG(tName,true))
	fun insertWidget wid inh = insert wid (TE.WIDGET(wid,SOME inh))
	fun insertPointer box inh = insert box (TE.POINTER(box,inh))
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

    fun mkObjectDecl (pos, (((name, inherits), fields))) = 
	( insertWidget name inherits
        ; AST.OBJECT_DECL (pos, TE.LONG([],TE.WIDGET(name,SOME inherits)),fields)
        )
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
	(AST.SIGNAL_DECL (pos, TE.LONG([],mkTypeExp name), signal, cbType))

    fun mkBoxedInherits NONE = NONE
      | mkBoxedInherits (SOME(NONE)) = SOME(TE.INH_ROOT)
      | mkBoxedInherits (SOME(SOME inh)) = SOME(TE.INH_FROM inh)

    fun mkCBType (retType, pars) = 
	TE.LONG ([], TE.ARROW(pars, [], pars, retType))

    fun ensureNonEmpty [] = [(TE.LONG ([], TE.PRIMTYPE "none"), "dummy")]
      | ensureNonEmpty pars = pars

    fun singleton texp = ([], texp)
    val mkLong = TE.LONG o singleton

    datatype type_flag = NULL_TYPE | OUTPUT_TYPE
    fun toType ((x1,x2),SOME NULL_TYPE) = (TE.LONG ([],TE.OPTION x1),x2)
      | toType ((x1,x2),SOME OUTPUT_TYPE) = (TE.LONG ([],TE.OUTPUT x1), x2)
      | toType ((x1,x2), NONE) = (x1, x2)

    (* various *)
    val typeExp =  (word >> (mkLong o mkTypeExp))
                || (parenthesized (word --# listQual) >> (mkLong o TE.LIST o mkLong o mkTypeExp))

    (* parameters *)
    val default = parenthesized (equals #-- string)
    val flag =  (parenthesized ($ "null-ok") >> (fn _ => NULL_TYPE))
             || (parenthesized ($ "output")  >> (fn _ => OUTPUT_TYPE))
    val par = parenthesized (typeExp -- word)
    val parWithOptDefault = parenthesized 
	(   typeExp
	 -- word
	 -- (optional flag)
	--# (optional default)
	)   >> toType
    val parList = parenthesized (repeat0 par)
    val parDefaultList = parenthesized (repeat0 parWithOptDefault) >> ensureNonEmpty 

    (* objects *)
    val inherits = parenthesized word
    val fieldList = parenthesized (fields #-- repeat1 par) >> op::
    val objDecl = parenthesized' 
	(   defObj
	#-- word
	 -- inherits
	 -- optional fieldList
	)

    (* functions *)
    val fncDecl = parenthesized' 
	(   defFnc
	#-- word
	 -- typeExp
	 -- parDefaultList
	)

    (* enums/flags *)
    val constr = parenthesized (word #-- word)
    val constructors = repeat1 constr >> (op ::)
    val enumDecl = parenthesized' (defEnum #-- word -- constructors)
    val flagsDecl = parenthesized' (defFlags #-- word -- constructors)

    (* boxed types *)
    val size = string 
    val inherits = optional (parenthesized (optional word)) >> mkBoxedInherits
    val boxedDecl = parenthesized' 
	(   defBoxed 
	#-- word
	 -- inherits
	 -- repeat0 word
	 -- optional size
	)

    (* signals *)
    val cbType = parenthesized (typeExp -- parList) >> mkCBType
    val signalName = (string >> (fn n => [n]))
                  || (parenthesized (string -- string) >> (fn (n,p) => [p,n]))
    val signalDecl = parenthesized' 
	(   defSignal
	#-- word
	 -- signalName
	 -- optional cbType
	)

    (* the various forms of declarations *)
    val decl' = (fncDecl >> mkFunctionDecl)
             || (objDecl >> mkObjectDecl) 
             || (enumDecl >> mkFlagsDecl true)
             || (flagsDecl >> mkFlagsDecl false)
             || (boxedDecl >> mkBoxedDecl)
             || (signalDecl >> mkSignalDecl)

(*
    fun definePred (Lexer.WORD (pos, str)) = String.isPrefix "define" str
      | definePred _ = false

    val decl = (decl' || (skipN definePred 2 #-- decl))
*)
    val decl = decl'

    val decls =   (repeat1 decl >> op::)
	      --# (getChars0 Char.isSpace) (* remove remaining whitespace *)

end (* structure Parser *)

(*

   [decls stream] parses stream as a sequence (one or more) of
   declarations. This is the topmost entry to the parser (and
   the only one documented here). The output is a list of declarations
   (see AST).

*)
