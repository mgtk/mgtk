(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

(* MAIN
 *
 * Author: Henning Niss
 *)

val setStringOption = State.setStringOption State.COMMAND_LINE
val setBoolOption =   State.setBoolOption   State.COMMAND_LINE
val addStringOption = State.addStringOption
val addBoolOption   = State.addBoolOption
val getStringOption = State.getStringOption
val getBoolOption   = State.getBoolOption


exception Error of string

fun say msg = (TextIO.output(TextIO.stdErr,msg);TextIO.flushOut TextIO.stdErr)

(* declare some options *)
val _ = ( addStringOption "inFile" NONE
	; addStringOption "outFile" NONE
        ; addStringOption "headFile" NONE
        ; addStringOption "footFile" NONE
        ; addBoolOption "insertEnd" false
        ; addStringOption "target" (SOME "C")
        )

fun copyStream os is =TextIO.output (os, TextIO.inputAll is)

fun parse () =
    case Parse.parse Parser.decls (valOf (getStringOption "inFile")) of
	(msg, SOME decls) => decls
      | (SOME msg, _) => raise Error("Error: " ^ msg)
      | (NONE,NONE) => Util.shouldntHappen "Parse.parse returned (N,N)"

fun openOut () =
    case getStringOption "outFile" of
	SOME fName => ((TextIO.openOut fName) 
	               handle General.Io _ => raise Error("Couldn't open "^fName))
      | NONE => TextIO.stdOut

fun closeOut stream =
    case getStringOption "outFile" of
	SOME fName => TextIO.closeOut stream
      | NONE => ()

fun insert (outstream, file) =
    let val is = TextIO.openIn file
    in  (copyStream outstream is)
	before
	(TextIO.closeIn is)
    end
    handle General.Io _ => raise Error("Couldn't open "^file)

fun insertHead outstream =
    case getStringOption "headFile" of
	SOME file => insert (outstream, file)
    |   NONE => ()

fun insertFoot outstream =
    let val insertEnd = getBoolOption "insertEnd"
    in  case getStringOption "footFile" of
	   SOME file => if insertEnd 
			then raise Error("Both --end_footer and --footer specified")
			else insert (outstream, file)
	 | NONE => if insertEnd then TextIO.output(outstream, "end\n")
		   else ()
    end

fun translate (outstream, decls) =
    Translate.translate outstream decls

fun message (outstream, func) =
    func outstream

(* auxillary functions used in ArgParse.parse below *)
fun headerFile fName = setStringOption "headFile" (SOME fName)
fun noHeaderFile () = setStringOption "headFile" NONE
fun footerFile fName     = 
    ( setStringOption "footFile" (SOME fName)
    ; setBoolOption "insertEnd" false)
fun endFooter () =
    ( setBoolOption "insertEnd" true
    ; setStringOption "footFile" NONE)
fun noFooter () =
    ( setBoolOption "insertEnd" false
    ; setStringOption "footFile" NONE)

fun generateStructure () = 
    ( setStringOption "target" (SOME "SML")
    ; endFooter ()
    ; headerFile "header.sml")
fun generateSignature () = 
    ( setStringOption "target" (SOME "SIG")
    ; endFooter ()
    ; headerFile "header.sig")
fun generateC ()         = 
    ( setStringOption "target" (SOME "C")
    ; headerFile "header.c")

fun showVersion () =
    ( say ("defs2sml --- stub file generator (Jul 11). ")
    ; say ("(c) Henning Niss and Ken Friis-Larsen\n")
    ; raise Error("")
    )

fun showFunctionality () =
    ( say ("functionality: ")
    ; case getStringOption "headFile" of SOME fName => say(fName ^ "+") 
                                       | NONE => ()
    ; case getStringOption "target" of SOME str => say("to"^str^"(" )
                                     | NONE => raise Error "No target specified."
    ; say (valOf(getStringOption "inFile") ^ ")")
    ; case getStringOption "footFile" of SOME fName => say("+" ^ fName) 
                                       | NONE => ()
    ; say (" -> ")
    ; case getStringOption "outFile" of SOME fName => say(fName) 
                                      | NONE => say ("stdout")
    ; say ("\n")
    )
fun main () =
    let 
	val _ = 
	    (ArgParse.parse 
                       [("-sml",          ArgParse.Unit generateStructure),
			("-sig",          ArgParse.Unit generateSignature),
			("-c",            ArgParse.Unit generateC),
			("-h",            ArgParse.String headerFile),
			("--header",      ArgParse.String headerFile),
                        ("--no-header",   ArgParse.Unit noHeaderFile),
			("-f",            ArgParse.String footerFile),
			("--footer",      ArgParse.String footerFile),
			("--end-footer",  ArgParse.Unit endFooter),
			("-end",          ArgParse.Unit endFooter),
			("--no-footer",   ArgParse.Unit noFooter),
			("-o",            ArgParse.String (setStringOption "outFile" o SOME)),
			("-V",            ArgParse.Unit showVersion),
			("--version",     ArgParse.Unit showVersion)
		       ] (setStringOption "inFile" o SOME))
                       handle ArgParse.Bad msg => raise Error msg

	val _ = case (getStringOption "inFile") of
	           NONE => raise Error("No input file specified!")
		|  _ => ()

	val decls = parse ()
	val _ = showFunctionality () (* this has to come after the parsing
				        since the .defs file may contain options
				     *)
	val outstream = openOut ()

        val _ = Messages.copyright outstream
        val _ = Messages.autogenerated outstream
	val _ = insertHead outstream
        val _ = Messages.autostart outstream
        val _ = translate (outstream, decls)
        val _ = insertFoot outstream
	val _ = closeOut outstream
    in  ()
    end
    handle Error msg => say (msg ^ "\n")
         | Fail msg => say ("Fail: "^msg^"\n")
         | exn => Util.explain exn

val _ = main ()
