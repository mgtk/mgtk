exception Error of string

fun say msg = (TextIO.output(TextIO.stdErr,msg);TextIO.flushOut TextIO.stdErr)

(* auxillaries for command line parsing *)
val inFile: string ref = ref ""

fun parse file =
    case Parse.parse Parser.decls file of
	(msg, SOME decls) => decls
      | (msg, NONE) => raise Error("Syntax Error: " ^ msg)

(* auxillary functions used in ArgParse.parse below *)
fun inputFile fName = inFile := fName

fun showVersion () =
    ( say ("defsstat --- statistics on .defs files (March 2000).\n")
    ; say ("(c) Henning Niss and Ken Friis-Larsen\n")
    ; raise Error("")
    )

fun main () =
    let 
	val _ = 
	    (ArgParse.parse 
                       [("-V",            ArgParse.Unit showVersion),
			("--version",     ArgParse.Unit showVersion)
		       ] inputFile)
                       handle ArgParse.Bad msg => raise Error msg

        val _ = if !inFile = "" then raise Error("No input file specified!")
		else ()

	val decls = parse (!inFile)
	val init_stat = {objects=0,functions=0,enums=0,signals=0,boxed=0}
	fun incObj {objects=ob,functions=f,enums=e,signals=s,boxed=b} =
	    {objects=ob+1,functions=f,enums=e,signals=s,boxed=b}
	fun incFnc {objects=ob,functions=f,enums=e,signals=s,boxed=b} =
	    {objects=ob,functions=f+1,enums=e,signals=s,boxed=b}
	fun incEnm {objects=ob,functions=f,enums=e,signals=s,boxed=b} =
	    {objects=ob,functions=f,enums=e+1,signals=s,boxed=b}
	fun incSig {objects=ob,functions=f,enums=e,signals=s,boxed=b} =
	    {objects=ob,functions=f,enums=e,signals=s+1,boxed=b}
	fun incBxd {objects=ob,functions=f,enums=e,signals=s,boxed=b} =
	    {objects=ob,functions=f,enums=e,signals=s,boxed=b+1}
	fun count (decl, acc) =
	    case decl of
		AST.OBJECT_DECL _ => incObj acc
              | AST.FUNCTION_DECL _ => incFnc acc
              | AST.FLAGS_DECL _ => incEnm acc
              | AST.SIGNAL_DECL _ => incSig acc
              | AST.BOXED_DECL _ => incBxd acc
	val stat = foldl count init_stat decls

	val indent = "  "
	fun showInt cat i = 
	    print (indent ^ StringCvt.padLeft #" " 4 (Int.toString i) ^ " " ^ cat ^ "\n")

	val _ = ( print ("Statistics for " ^ !inFile ^ ":\n")
                ; showInt "declarations" (List.length decls)
                ; print (indent ^ "--------------------\n")
		; showInt "objects" (#objects stat)
		; showInt "functions" (#functions stat)
		; showInt "signals" (#signals stat)
		; showInt "enumerations" (#enums stat)
		; showInt "boxed types" (#boxed stat)
                )

    in  ()
    end
    handle Error msg => say (msg ^ "\n")
         | Fail msg => say ("Fail: "^msg^"\n")

val _ = main ()


