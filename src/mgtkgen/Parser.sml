(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

(* Parser for GTK .defs files
 *
 * Henning Niss, February 2000
 *)


structure Parser =
struct

    open Combinators

    infix 6 $--
    infix 6 --$
    infix 5 --
    infix 3 >>
    infix 0 ||


    val openParen = Combinators.openParen (* for clarity *)
    val closeParen = Combinators.closeParen (* for clarity *)

    fun parenthesized pf = openParen $-- pf --$ closeParen

    (* a version of parenthesized that includes position information *)
    fun openParen' tokenStream =
	case Lexer.get tokenStream of
	    SOME (Lexer.LPAREN pos, tokenStream') => (pos, tokenStream')
	  | SOME (_, tokenStream') => raise SyntaxError ("expected `('", tokenStream)
	  | NONE => raise SyntaxError ("Unexpected end of file", tokenStream)

    fun closeParen' tokenStream =
	case Lexer.get tokenStream of
	    SOME (Lexer.RPAREN pos, tokenStream') => (pos, tokenStream')
	  | SOME (_, tokenStream') => raise SyntaxError ("expected `)'", tokenStream)
	  | NONE => raise SyntaxError ("Unexpected end of file", tokenStream)

    fun startPos (s,e) = s
    fun endPos (s,e) = e
    fun parenthesized' pf stream = 
	let val (pos1, stream1) = openParen' stream
	    val (tok,  stream2) = pf stream1
	    val (pos2, stream3) = closeParen' stream2
	in  (((startPos pos1, endPos pos2), tok), stream3)
	end

    val equals = Combinators.equals (* for clarity *)

    val fields = $$ "fields"
    val defObj = $$ "define-object"
    val defFnc = $$ "define-func"
    val defFlags = $$ "define-flags"
    val defEnum =  $$ "define-enum"
    val defBoxed = $$ "define-boxed"
    val defSignal = $$ "define-signal"
    val listQual = $$ "list"

    (* name resolution *)
    local
	type type_info = string -> AST.texp
	exception Find
	val (table: (string, type_info) Polyhash.hash_table) = 
	    Polyhash.mkPolyTable (*(hash, compare)*) (99, Find)
	(* insert some primitive types in the symbol table *)
	val _ = app (fn n => Polyhash.insert table (n,AST.TYPENAME))
	            ["none","int","uint","float","bool","string",
		     "static_string", "GtkObject", "GtkWidget",
		     "GtkGtkType"
		    ]
    in  fun insert tName func = Polyhash.insert table (tName, func)
	fun insertTypeName tName = insert tName AST.TYPENAME
	fun insertFlag tName = insert tName (fn n => AST.FLAG(n,false))
	fun insertEnum tName = insert tName (fn n => AST.FLAG(n,true))
	fun lookup tName =
	    ((Polyhash.find table tName)
	     handle Find => Util.notFound("unbound type name: " ^ tName))
	fun mkTypeExp name = (lookup name) name
    end

    (* construct abstract syntax *)
    fun mkObjectDecl (pos, (((name, inherits), fields))) = 
	(insertTypeName name; AST.OBJECT_DECL (pos, name,inherits,fields))
    fun mkFunctionDecl (pos, ((name, typeExp), parameters)) =
	(AST.FUNCTION_DECL (pos, name, typeExp, parameters))
    fun mkFlagsDecl false (pos, (flagName, cs)) = 
	( insertFlag flagName
	; AST.FLAGS_DECL(pos, AST.LONG([], AST.FLAG(flagName, false)), cs)
        )
      | mkFlagsDecl true (pos, (flagName, cs)) =
	( insertEnum flagName
	; AST.FLAGS_DECL(pos, AST.LONG([], AST.FLAG(flagName, true)), cs)
        )
    fun mkBoxedDecl (pos, ((name, names), size)) =
	(insertTypeName name; AST.BOXED_DECL (pos, name, names, size))
    fun mkSignalDecl (pos, ((name,signal),cbType)) = 
	(AST.SIGNAL_DECL (pos, name, signal, cbType))

    fun mkFunType (retType, pars) = AST.LONG ([], AST.ARROW(pars, retType))

    fun ensureNonEmpty [] = [(AST.LONG ([], AST.TYPENAME "none"), "dummy")]
      | ensureNonEmpty pars = pars

    fun singleton texp = ([], texp)
    val mkLong = AST.LONG o singleton

    (* functions *)
    val typeExp =  (word >> (mkLong o mkTypeExp))
                || (parenthesized (word --$ listQual) >> (mkLong o AST.LIST o mkLong o mkTypeExp))
    val parenName = parenthesized word
    val par = parenthesized (typeExp -- word)
    val constr = parenthesized (word $-- word)
    val constructors = constr -- (repeat constr) >> (op ::)

    datatype type_flag = NULL_TYPE | OUTPUT_TYPE
    fun flag toks = 
	let val (fl, toks') = parenthesized word toks
	in  case fl of
	      "null-ok" => (NULL_TYPE, toks')
	    | "output"  => (OUTPUT_TYPE, toks')
            | _ => raise SyntaxError("unknown flag", toks)
	end

    fun toType ((x1,x2),SOME NULL_TYPE) = (AST.LONG ([],AST.OPTION x1),x2)
      | toType ((x1,x2),SOME OUTPUT_TYPE) = (AST.LONG ([],AST.OUTPUT x1), x2)
      | toType ((x1,x2), NONE) = (x1, x2)

    val default = parenthesized (equals $-- string)
    val parWithOptDefault = 
	parenthesized (typeExp -- word -- (optional flag) --$ (optional default))
        >> toType

    val parList = parenthesized (repeat par)
    val parDefaultList = 
	 parenthesized (repeat parWithOptDefault) >> ensureNonEmpty 

    (* objects *)
    val inherits = parenthesized word
    val fieldList = parenthesized (fields $-- repeat par)


    (* declarations *)
    val objDecl = parenthesized' (defObj $-- word -- inherits -- optional fieldList)
    val fncDecl = parenthesized' (defFnc $-- word -- typeExp -- parDefaultList)
    val enumDecl = parenthesized' (defEnum $-- word -- constructors)
    val flagsDecl = parenthesized' (defFlags $-- word -- constructors)

    val size = Combinators.string 
    val boxedDecl = parenthesized' (defBoxed $-- word -- repeat word -- optional size)

    val cbType = parenthesized (typeExp -- (parList >> map #1))
                 >> mkFunType

    val signalName = (string >> (fn n => [n]) )
                  || (parenthesized (string -- string) >> (fn (n,p) => [p,n]))
    val signalDecl = parenthesized' (defSignal $-- word -- signalName --
				     optional cbType)

    val decl' = (fncDecl >> mkFunctionDecl)
             || (objDecl >> mkObjectDecl) 
             || (enumDecl >> mkFlagsDecl true)
             || (flagsDecl >> mkFlagsDecl false)
             || (boxedDecl >> mkBoxedDecl)
             || (signalDecl >> mkSignalDecl)

    fun definePred (Lexer.WORD (pos, str)) = String.isPrefix "define" str
      | definePred _ = false

    fun decl stream = (decl' || (skipN definePred 2 $-- decl)) stream

    val decls = decl -- (repeat decl) >> op::

end (* structure Parser *)

(*

   [decls stream] parses stream as a sequence (one or more) of
   declarations. This is the topmost entry to the parser (and
   the only one documented here). The output is a list of declarations
   (see AST).

*)
