structure DefsParse = struct

    val pathList = ref ["api"]

    open Parsercomb Defs
    infix 7 |>
    infix 6 $-- --$ #-- --#
    infix 5 -- unless    
    infix 3 >> >>*
    infix 2 >>=
    infix 0 ||

    fun id x = x
    fun tagFn tag (name,attribs) = (name, tag, attribs)

    fun toStream file =
	let open TextIO
	    val _ = print("Reading " ^ file ^ "...")
	    val inp = FileUtils.openIn (!pathList) file
	    val sub = Substring.all(inputAll inp) before closeIn inp
	    val par = stream Substring.getc sub
	    val _ = print " done!\n"
	in  par
	end

    fun getLine strm0 =
	let fun loop strm acc =
		case getItem strm of
		    SOME(c, rest) => if c = #"\n" 
				     then let val l = String.implode (rev (#"\n"::acc))
					  in  SOME(Substring.all l, rest)
					  end
				     else loop rest (c::acc)
		  | NONE => NONE
	in  loop strm0 []
	end

    (* PRE-LEXING *)
    fun qc c = c = #"\""
    val string = "\"" $-- getChars0 (fn c=>not(qc c)) --$ "\""
    val incl = 
	("(" $-- skipWS ("include" $-- skipWS (string --# skipWS ($")"))))
    
    fun includer (orig_src: char stream) : char stream =
	let fun get (stk, src, NONE) = 
		(case getLine src of
		     SOME(l,rest) => (case incl (stream Substring.getc l) of
					  NONE => get (stk, rest, SOME l)
					| SOME(file,_) => get (rest::stk, toStream file, NONE)
                                     )
		   | NONE => (case stk of [] => NONE | s::ss => get(ss,s,NONE))
                )
	      | get (stk, src, SOME line) =
		(case Substring.getc line of
		     SOME(c,rest) => SOME(c,(stk,src,SOME rest))
		   | NONE => get (stk, src, NONE)
                )
	in  stream get ([], orig_src, NONE)
	end

    fun show p src =
	let val g = getItem src
	    val _ = case g of
			SOME(item,_) => p item
		      | NONE => ()
	in  g
	end

    (* LEXING *)
    datatype token = WORD_T of string | STRING_T of string | LPAR_T | RPAR_T
                   | EOF_T

    fun showToken (WORD_T w) = "!" ^ w ^ "!"
      | showToken (STRING_T s) = "\"" ^ s ^ "\""
      | showToken LPAR_T = "<"
      | showToken RPAR_T = ">"
      | showToken EOF_T = "<eof>"

    fun wordOf (WORD_T n) = n
      | wordOf _ = raise Fail("wordOf: not a word")
    fun stringOf (STRING_T s) = s
      | stringOf _ = raise Fail("stringOf: not a string")
    fun isWord (WORD_T _) = true
      | isWord _ = false
    fun isString (STRING_T _) = true
      | isString _ = false

    fun wc #"-" = true
      | wc #"_" = true
      | wc #"#" = true (* FIXME: Do we really want this? Yup, truth values *)
      | wc c = Char.isAlphaNum c
    fun qc c = c = #"\""
    val word       = getChars1 wc
    val quotedWord = "\"" $-- getChars1 (fn c=>not(qc c) andalso wc c) --$ "\""
    val string     = "\"" $-- getChars0 (not o qc) --$ "\""
    val white = getChar Char.isSpace
    fun token is = 
        (   $"("        |> LPAR_T
        ||  $")"        |> RPAR_T
	||  word        >> WORD_T
        ||  string      >> STRING_T
        ||  $"'("       |> LPAR_T (* FIXME: Is this what we want? *)
        ||  white #-- token
        ||  comment
        ||  eof            EOF_T
        ) is
    and comment is = 
	(";" $-- getChars0 (fn c=>not(c= #"\n")) --$ "\n" #-- token) is
    and lexer is = stream token is

(*
    val lexer = stream (show (TextIO.print o showToken)) o lexer
*)

    (* PARSING *)
    infix 6 &-- --&
    fun & s = getElem (fn (WORD_T n) => s = n | _ => false) >> wordOf
    fun (n &-- pf) = & n -- pf >> #2
    fun (pf --& n) = pf -- & n >> #1

    infix 6 %-- --%
    fun (t %-- pf) = getLit t -- pf >> #2
    fun (pf --% t) = pf -- getLit t >> #1

    fun parens pf = LPAR_T %-- pf --% RPAR_T
    val name = getElem isWord >> wordOf
    val defName = name >> (fn n => (TextIO.print(n^"\n"); n))
    val string = getElem isString >> stringOf

    (* Type expressions and lists of types and names *)
    val typeID = string
    val typeExp = string

    val nullOk = parens (&"null-ok") |> NullOk
    val default = parens ("default" &-- string) >> Default
    val typeName = (* too liberal since we use it also for fields *)
	parens (typeExp -- string -- optional nullOk -- optional default)
        >> (fn (((te,na),nu),d)=>(te,na,List.mapPartial id (nu::d::[])))
    val typeNameList = repeat1 typeName >> (op ::)
    
    (* Objects and boxed types *)
    val objAttrib =
        (   parens ("in-module" &-- string)         >> (SOME o Module)
        ||  parens ("parent" &-- string)            >> (SOME o Parent)
        ||  parens ("c-name" &-- string)            >> (SOME o CName)
        ||  parens ("gtype-id" &-- typeID)          >> (SOME o TypeID)
        ||  parens ("fields" &-- typeNameList)      >> (SOME o Fields)
	||  parens ("implements" &-- string)        |> NONE
        )
    val objAttribs = repeat1 objAttrib >> (List.mapPartial id o (op::))
    val object = parens ( (&"define-object" || &"define-interface") 
                          #-- defName -- objAttribs) >> tagFn Object

    val boxAttrib =
        (   parens ("in-module" &-- string)         >> Module
        ||  parens ("c-name" &-- string)            >> CName
        ||  parens ("gtype-id" &-- typeID)          >> TypeID
        ||  parens ("copy-func" &-- string)         >> CopyFunc
        ||  parens ("release-func" &-- string)      >> ReleaseFunc
        ||  parens ("fields" &-- typeNameList)      >> Fields
        )
    val boxAttribs = repeat1 boxAttrib >> (op::)
    val boxed = parens ( (&"define-boxed" || &"define-pointer")
                         #-- defName -- boxAttribs) >> tagFn Boxed

    (* Enums and flags *)
    val value = parens (string -- string)
    val valueList = repeat1 value >> (op::)
    val enuAttrib =
	(   parens ("in-module" &-- string)         >> Module
        ||  parens ("c-name" &-- string)            >> CName
        ||  parens ("gtype-id" &-- typeID)          >> TypeID
        ||  parens ("values" &-- valueList)         >> Values
        )
    val enuAttribs = repeat1 enuAttrib >> (op::)
    val enum = parens ( (&"define-enum" || &"define-flags")
                        #-- defName -- enuAttribs) >> tagFn Enum

    (* Function like things *)
    val truth = &"#t" |> true || &"#f" |> false
    val funAttrib = (* too liberal since it is used for all func things *)
        (   parens ("c-name" &-- string)            >> CName
        ||  parens ("is-constructor-of" &-- name)   >> Constructor
        ||  parens ("of-object" &-- string)         >> OfObject
        ||  parens ("return-type" &-- typeExp)      >> ReturnType
        ||  parens ("parameters" &-- typeNameList)  >> Params
	||  parens ("deprecated" &-- string)        |> Deprecated
	||  parens ("varargs" &-- truth)            >> Varargs
	||  parens ("caller-owns-return" &-- truth) >> CallerOwnsReturn
        )
    val funAttribs = repeat1 funAttrib >> (op::)
    val function = parens ("define-function" &-- defName -- funAttribs) >> tagFn Function
    val method   = parens ("define-method"   &-- defName -- funAttribs) >> tagFn Method

    val definitions = repeat1 (function || method || object || boxed || enum )
                          --% EOF_T
                      >> (op::)

    fun parse file =
	let val stream = toStream file
	    val included = includer stream
	    val lexed = lexer included
	in  #1 (Option.valOf(definitions lexed))
	end

end (* structure DefsParse *)