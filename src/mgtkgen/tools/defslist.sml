exception Error of string

fun say msg = (TextIO.output(TextIO.stdErr,msg);TextIO.flushOut TextIO.stdErr)

type decl = AST.declaration

fun parse file =
    case Parse.parse Parser.decls file of
	(msg, SOME decls) => decls
      | (msg, NONE) => raise Error("Syntax Error: " ^ msg)

(* auxillaries for command line parsing *)
val inFile: string ref = ref ""
val sorted: bool ref = ref false
val sortFunc: (decl * decl -> General.order) ref = ref AST.declOrder
val filterFunc: (decl -> bool) ref = ref (fn _ => true)

(* auxillary functions used in ArgParse.parse below *)
fun inputFile fName = inFile := fName
fun setSorted () = sorted := true
fun useNames () = sortFunc := AST.nameOrder
fun useDecls () = sortFunc := AST.declOrder

fun listFunctions () = filterFunc := AST.isFunction
fun listWidgets () = filterFunc := AST.isWidget
fun listEnums () = filterFunc := AST.isEnum
fun listBoxeds () = filterFunc := AST.isBoxed
fun listSignals () = filterFunc := AST.isSignal

fun showVersion () =
    ( say ("defslist --- listing .defs files (March 2000).\n")
    ; say ("(c) Henning Niss and Ken Friis-Larsen\n")
    ; raise Error("")
    )

fun showUsage msg =
    ( say (msg ^ "\n")
    ; say ("Usage: defslist [options] <file>\n")
    ; say ("  where options can be\n\n")
    ; say ("  -s / --sorted\n")
    ; say ("  -n / --sort-on-names\n")
    ; say ("  -d / --sort-on-decls\n\n")
    ; say ("  -f / --list-only-funcs\n")
    ; say ("  -w / --list-only-widgets\n")
    ; say ("  -b / --list-only-boxeds\n")
    ; say ("  -e / --list-only-enums\n\n")
    ; say ("  -V / --version\n")
    ; raise Error("")
    )

fun main () =
    let 
	val _ = 
	    (ArgParse.parse 
                       [("-s",            ArgParse.Unit setSorted),
			("--sorted",      ArgParse.Unit setSorted),
			("-n",            ArgParse.Unit useNames),
			("--sort-on-names", ArgParse.Unit useNames),
			("-d",            ArgParse.Unit useDecls),
			("--sort-on-decls", ArgParse.Unit useDecls),
			("-f",                ArgParse.Unit listFunctions),
			("--list-only-funcs", ArgParse.Unit listFunctions),
			("-w",                ArgParse.Unit listWidgets),
			("--list-only-widgets", ArgParse.Unit listWidgets),
			("-b",                ArgParse.Unit listBoxeds),
			("--list-only-boxeds", ArgParse.Unit listBoxeds),
			("-e",                ArgParse.Unit listEnums),
			("--list-only-enums", ArgParse.Unit listEnums),
			("-V",            ArgParse.Unit showVersion),
			("--version",     ArgParse.Unit showVersion)
		       ] inputFile)
                       handle ArgParse.Bad msg => showUsage msg

        val _ = if !inFile = "" then raise Error("No input file specified!")
		else ()

	val decls = parse (!inFile)

	fun show decl = 
	        if (!filterFunc) decl 
		then SOME(AST.nameOf decl ^ ": " ^ AST.typeOf decl ^ "\n")
		else NONE
	fun printOption NONE = ()
          | printOption (SOME s) = print s
	val _ = map (printOption o show) 
	            (if !sorted then Listsort.sort (!sortFunc) decls
		     else decls)

    in  ()
    end
    handle Error msg => say (msg ^ "\n")
         | Fail msg => say ("Fail: "^msg^"\n")

val _ = main ()


