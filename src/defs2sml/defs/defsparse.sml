structure DefsParse = struct

    val pathList = ref []

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

    (* BASIC THINGS *)
    fun wc #"(" = false
      | wc #")" = false
      | wc #"\"" = false
      | wc #";" = false
      | wc #"'" = false
      | wc c = not(Char.isSpace c)
    fun qc #"\"" = true
      | qc _ = false
    val word       = getChars1 wc
    val quotedWord = "\"" $-- getChars1 (not o qc) --$ "\""

    (* PRE-LEXING *)

(*  pyGtk
    val incl = 
	("(" $-- skipWS ("include" $-- skipWS (string --# skipWS ($")"))))
*)

(*  GtkMM
*)
    val incl = 
	("(" $-- skipWS ("include" $-- skipWS (word --# skipWS ($")"))))
    
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
	let val i = getItem src
	in  Option.app (p o #1) i
          ; i
        end

    (* LEXING *)
    datatype token = WORD_T of string | LPAR_T | RPAR_T | EOF_T

    fun wordOf (WORD_T n) = n
      | wordOf _ = raise Fail("wordOf: not a word")
    fun isWord (WORD_T _) = true
      | isWord _ = false

    val white = getChar Char.isSpace
    fun token is = 
        (   $"("        |> LPAR_T
        ||  $")"        |> RPAR_T
	||  word        >> WORD_T
	||  quotedWord  >> WORD_T
        ||  $"'("       |> LPAR_T (* FIXME: Is this what we want? *)
        ||  white #-- token
        ||  comment
        ||  eof            EOF_T
        ) is
    and comment is = 
	(";" $-- getChars0 (fn c=>not(c= #"\n")) --$ "\n" #-- token) is
    and lexer is = stream token is

    (* PARSING *)
    (* % t and derivatives --- get token t *)
    infix 6 %-- --%
    fun (t %-- pf) = getLit t -- pf >> #2
    fun (pf --% t) = pf -- getLit t >> #1

    (* & s and derivatives --- get word token with lexeme s *)
    infix 6 &-- --&
    fun & s = getElem (fn (WORD_T n) => s = n | _ => false) >> wordOf
    fun (n &-- pf) = & n -- pf >> #2
    fun (pf --& n) = pf -- & n >> #1

    fun parens pf = LPAR_T %-- pf --% RPAR_T
    val word = getElem isWord >> wordOf
    val defWord = word >> (fn n => (TextIO.print(n^"\n"); n))

    (* Type expressions and lists of types and names *)
    val typeID = word
    val typeExp = word

    val nullOk = parens (&"null-ok") |> NullOk
    val default = parens ("default" &-- word) >> Default
    val typeName = (* too liberal since we use it also for fields *)
	parens (typeExp -- word -- optional nullOk -- optional default)
        >> (fn (((te,na),nu),d)=>(te,na,List.mapPartial id (nu::d::[])))
    val typeNameList = repeat0 typeName
    
    (* Objects and boxed types *)
    val objAttrib =
        (   parens ("in-module" &-- word)           >> (SOME o Module)
        ||  parens ("parent" &-- word)              >> (SOME o Parent)
        ||  parens ("c-name" &-- word)              >> (SOME o CName)
        ||  parens ("gtype-id" &-- typeID)          >> (SOME o TypeID)
        ||  parens ("fields" &-- typeNameList)      >> (SOME o Fields)
	||  parens ("implements" &-- word)          |> NONE
        )
    val objAttribs = repeat1 objAttrib >> (List.mapPartial id o (op::))
    val object = parens ( (&"define-object" || &"define-interface") 
                          #-- defWord -- objAttribs) >> tagFn Object

    val boxAttrib =
        (   parens ("in-module" &-- word)           >> Module
        ||  parens ("c-name" &-- word)              >> CName
        ||  parens ("gtype-id" &-- typeID)          >> TypeID
        ||  parens ("copy-func" &-- word)           >> CopyFunc
        ||  parens ("release-func" &-- word)        >> ReleaseFunc
        ||  parens ("fields" &-- typeNameList)      >> Fields
        )
    val boxAttribs = repeat1 boxAttrib >> (op::)
    val boxed = parens ( (&"define-boxed" || &"define-pointer")
                         #-- defWord -- boxAttribs) >> tagFn Boxed

    (* Enums and flags *)
    val value = parens (word -- word)
    val valueList = repeat1 value >> (op::)
    val enuAttrib =
	(   parens ("in-module" &-- word)           >> Module
        ||  parens ("c-name" &-- word)              >> CName
        ||  parens ("gtype-id" &-- typeID)          >> TypeID
        ||  parens ("values" &-- valueList)         >> Values
        )
    val enuAttribs = repeat1 enuAttrib >> (op::)
    val enum = parens ( (&"define-enum" || &"define-flags")
                        #-- defWord -- enuAttribs) >> tagFn Enum

    (* Function like things *)
    val truth = &"#t" |> true || &"#f" |> false
    val funAttrib = (* too liberal since it is used for all func things *)
        (   parens ("c-name" &-- word)              >> CName
        ||  parens ("is-constructor-of" &-- word)   >> Constructor
        ||  parens ("of-object" &-- word)           >> OfObject
        ||  parens ("return-type" &-- typeExp)      >> ReturnType
        ||  parens ("parameters" &-- typeNameList)  >> Params
	||  parens ("deprecated" &-- word)          |> Deprecated
	||  parens ("varargs" &-- truth)            >> Varargs
	||  parens ("caller-owns-return" &-- truth) >> CallerOwnsReturn
        )
    val funAttribs = repeat1 funAttrib >> (op::)
    val function = parens ("define-function" &-- defWord -- funAttribs) >> tagFn Function
    val method   = parens ("define-method"   &-- defWord -- funAttribs) >> tagFn Method

    (* Signals *)
    val when = (&"unknown" |> Unknown || &"last" |> Last || &"first" |> First)
    val sigAttrib =
        (   parens ("of-object" &-- word)           >> OfObject
        ||  parens ("return-type" &-- word)         >> ReturnType
        ||  parens ("when" &-- when)                >> When
        ||  parens ("parameters" &-- typeNameList)  >> Params
        )
    val sigAttribs = repeat1 sigAttrib >> (op::)
    val signal = parens ("define-signal" &-- defWord -- sigAttribs) >> tagFn Signal

    (* Misc *)
    val property = parens ("define-property" &-- word 
                           -- repeat0 (parens (word -- word)))

    val definitions = repeat1 (  function >> SOME || method >> SOME 
                              || object >> SOME || boxed >> SOME
                              || enum >> SOME || signal >> SOME
                              || property |> NONE)
                          --% EOF_T
                      >> (List.mapPartial id o (op::))

    fun parse file =
	let val stream = toStream file
	    val included = includer stream
	    val lexed = lexer included
	in  #1 (Option.valOf(definitions lexed))
	end

end (* structure DefsParse *)