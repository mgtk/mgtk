exception Error of string

fun say msg = (TextIO.output(TextIO.stdErr,msg);TextIO.flushOut TextIO.stdErr)

type decl = AST.declaration

fun parse file =
    case Parse.parse Parser.decls file of
	(msg, SOME decls) => decls
      | (msg, NONE) => raise Error("Syntax Error: " ^ msg)

(* auxillaries for command line parsing *)
val inFile: string ref = ref ""
val fnName: string ref = ref ""

(* auxillary functions used in ArgParse.parse below *)
fun inputFile fName = inFile := fName
fun findName name = fnName := name

fun showVersion () =
    ( say ("defsfind --- find declarations in .defs files (March 2000).\n")
    ; say ("(c) Henning Niss and Ken Friis-Larsen\n")
    ; raise Error("")
    )

fun main () =
    let 
	val _ = 
	    (ArgParse.parse 
                       [("-d",            ArgParse.String findName),
			("--decl-name",   ArgParse.String findName),
			("-V",            ArgParse.Unit showVersion),
			("--version",     ArgParse.Unit showVersion)
		       ] inputFile)
                       handle ArgParse.Bad msg => raise Error msg

        val _ = if !inFile = "" then raise Error("No input file specified!")
		else ()

	val decls = parse (!inFile)

	fun show decl = let val pos as (p1, p2) = AST.posOf decl
			in  TextIO.print ("Found " ^ AST.nameOf decl ^ 
					  " in " ^ (!inFile) ^ " at ")
			  ; TextIO.print ("pos: (" ^ Int.toString p1 ^ ", " ^ 
					             Int.toString p2 ^ ")\n")
                          ; TextIO.print ("--------------------------------------------------\n")
                          ; TextIO.print (Util.extractSource (!inFile) pos ^ "\n")
                          ; TextIO.print ("--------------------------------------------------\n")
			end
	fun find decl = if AST.nameOf decl = (!fnName)
	                then show decl
			else ()
	val _ = map find decls 

    in  ()
    end
    handle Error msg => say (msg ^ "\n")
         | Fail msg => say ("Fail: "^msg^"\n")

val _ = main ()


