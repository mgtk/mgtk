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

(* auxillary functions used in ArgParse.parse below *)
fun inputFile fName = inFile := fName
fun setSorted () = sorted := true
fun useNames () = sortFunc := AST.nameOrder
fun useDecls () = sortFunc := AST.declOrder

fun showVersion () =
    ( say ("defssort --- listing .defs files (March 2000).\n")
    ; say ("(c) Henning Niss and Ken Friis-Larsen\n")
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
			("-V",            ArgParse.Unit showVersion),
			("--version",     ArgParse.Unit showVersion)
		       ] inputFile)
                       handle ArgParse.Bad msg => raise Error msg

        val _ = if !inFile = "" then raise Error("No input file specified!")
		else ()

	val decls = parse (!inFile)

	fun show decl = AST.nameOf decl ^ ": " ^ AST.typeOf decl ^ "\n"
	val _ = map (print o show) 
	            (if !sorted then Listsort.sort (!sortFunc) decls
		     else decls)

    in  ()
    end
    handle Error msg => say (msg ^ "\n")
         | Fail msg => say ("Fail: "^msg^"\n")

val _ = main ()


