(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

(* Parser for GTK .defs files
 *
 * Henning Niss, February 2000
 *)


structure Parser =
struct

    structure TE = TypeExp
    structure PU = ParseUtils

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

    fun ensureNonEmpty [] = [(TE.PRIMTYPE "none", "dummy")]
      | ensureNonEmpty pars = pars

    datatype type_flag = NULL_TYPE | OUTPUT_TYPE
    fun toType ((x1,x2),SOME NULL_TYPE) = (TE.OPTION x1,x2)
      | toType ((x1,x2),SOME OUTPUT_TYPE) = (TE.OUTPUT x1, x2)
      | toType ((x1,x2), NONE) = (x1, x2)

    (* various *)
    val typeExp =  (word >> PU.mkTypeExp)
                || (parens (word --# listQual) >> (TE.LIST o PU.mkTypeExp))

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

    (* widgets *)
    val inherits = optional (parens word) >> PU.mkWidgetInherits
    val fieldList = parens (fields #-- repeat1 par) >> op::
    val widDecl = parens' 
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
    val inherits = optional (parens (optional word)) >> PU.mkBoxedInherits
    val boxedDecl = parens' 
	(   defBoxed 
	#-- word
	 -- inherits
	 -- repeat0 word
	 -- optional size
	)

    (* signals *)
    val cbType = parens (typeExp -- parList) >> PU.mkCBType
    val signalName = (string >> (fn n => ([],[n])))
                  || (parens (string -- string) >> (fn (n,p) => ([p],[n])))
    val signalDecl = parens' 
	(   defSignal
	#-- typeExp
	 -- signalName
	 -- optional cbType
	)

    (* the various forms of declarations *)
    val decl' = (mdlDecl >> PU.mkModuleDecl)
             || (widDecl >> PU.mkWidgetDecl) 
             || (fncDecl >> PU.mkFunDecl)
             || (enumDecl >> PU.mkFlagsDecl true)
             || (flagsDecl >> PU.mkFlagsDecl false)
             || (boxedDecl >> PU.mkBoxedDecl)
             || (signalDecl >> PU.mkSignalDecl)

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
