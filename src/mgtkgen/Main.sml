(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

(* MAIN
 *
 * Author: Henning Niss
 *)


exception Error of string

fun say msg = (TextIO.output(TextIO.stdErr,msg);TextIO.flushOut TextIO.stdErr)

(* auxillaries for command line parsing *)
val inFile: string ref = ref ""
val outFile: string option ref = ref NONE
val headFile: string option ref = ref NONE
val footFile: string option ref = ref NONE
val insertEnd: bool ref = ref false
val verbose : bool ref = ref false
val target = ref AST.C

fun copyStream os is =TextIO.output (os, TextIO.inputAll is)

fun parse file =
    case Parse.parse Parser.decls file of
	(msg, SOME decls) => decls
      | (msg, NONE) => raise Error("Syntax Error: " ^ msg)

fun openOut file =
    case file of
	SOME fName => ((TextIO.openOut fName) 
	               handle General.Io _ => raise Error("Couldn't open "^fName))
      | NONE => TextIO.stdOut

fun closeOut (file, stream) =
    case file of
	SOME fName => TextIO.closeOut stream
      | NONE => ()

fun insert (outstream, file) =
    case file of
	SOME fName => (let val is = TextIO.openIn fName
		       in  (copyStream outstream is)
			   before
			   (TextIO.closeIn is)
		       end
		       handle General.Io _ => raise Error("Couldn't open "^fName))
      | NONE => ()

fun insertFoot (outstream, file) =
    case file of
	SOME _ => if !insertEnd then raise Error("Both --end_footer and --footer specified")
		  else insert (outstream, file)
      | NONE => if !insertEnd then TextIO.output(outstream, "end\n")
		else ()

fun translate (outstream, decls) =
    Translate.translate outstream (!target) decls

fun message (outstream, func) =
    func (!target) outstream

fun chat msg =
    if !verbose then say (msg)
    else ()

fun phase (msg, func) = (chat msg; func ())

(* auxillary functions used in ArgParse.parse below *)
fun generateStructure () = 
    ( chat "  generating ML structure\n"
    ; target := AST.SML
    ; insertEnd := true
    ; headFile := SOME("header.sml"))
fun generateSignature () = 
    ( chat "  generating ML signature\n"
    ; target := AST.SIG
    ; insertEnd := true
    ; headFile := SOME("header.sig"))
fun generateC ()         = 
    ( chat "  generating C code\n"
    ; target := AST.C
    ; headFile := SOME("header.c"))
fun setVerbose ()        = verbose := true
fun outputFile fName     = 
    ( chat ("  output file is: " ^ fName ^ "\n")
    ; outFile := SOME fName)
fun headerFile fName     = 
    ( chat ("  header file is: " ^ fName ^ "\n")
    ; headFile := SOME fName)
fun noHeaderFile () =
    ( chat ("  no header file\n")
    ; headFile := NONE)
fun footerFile fName     = 
    ( chat ("  footer file is: " ^ fName ^ "\n")
    ; footFile := SOME fName
    ; insertEnd := false)
fun endFooter () =
    ( chat ("  footer is `end´\n")
    ; insertEnd := true
    ; footFile := NONE)
fun noFooter () =
    ( chat ("  no footer\n")
    ; insertEnd := false
    ; footFile := NONE)
fun inputFile  fName     = 
    ( chat ("  input file is: " ^ fName ^ "\n")
    ; inFile := fName)
fun showVersion () =
    ( say ("mgtk generator (Feb 25). ")
    ; say ("(C) Henning Niss and Ken Friis-Larsen\n")
    ; raise Error("")
    )

fun showFunctionality () =
    ( say ("functionality: ")
    ; case !headFile of SOME fName => say(fName ^ "+") | NONE => ()
    ; case !target of
         AST.SML => say ("toSML(")
       | AST.SIG => say ("toSIG(")
       | AST.C   => say ("toC(")
    ; say (!inFile ^ ")")
    ; case !footFile of SOME fName => say("+" ^ fName) | NONE => ()
    ; say (" -> ")
    ; case !outFile of SOME fName => say(fName) | NONE => say ("stdout")
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
			("-o",            ArgParse.String outputFile),
			("-V",            ArgParse.Unit showVersion),
			("--version",     ArgParse.Unit showVersion),
			("-v",            ArgParse.Unit setVerbose),
			("--verbose",     ArgParse.Unit setVerbose)
		       ] inputFile)
                       handle ArgParse.Bad msg => raise Error msg

        val _ = if !inFile = "" then raise Error("No input file specified!")
		else ()
	val _ = showFunctionality ()
	val decls = phase ("Parsing " ^ !inFile ^ "\n", fn () => parse (!inFile))
	val outstream = openOut (!outFile)

        val _ = phase ("Copyright message\n", fn () => message (outstream,Messages.copyright))
        val _ = phase ("Autogenerated message\n", fn () => message (outstream,Messages.autogenerated))
	val _ = phase ("Inserting header\n", fn () => insert (outstream, !headFile))
        val _ = phase ("Autogeneration start message\n", fn () => message (outstream,Messages.autostart))
        val _ = phase ("Translating\n", fn () => translate (outstream, decls))
        val _ = phase ("Inserting footer\n", fn () => insertFoot (outstream,!footFile))
	val _ = closeOut (!outFile, outstream)
    in  ()
    end
    handle Error msg => say (msg ^ "\n")
         | Fail msg => say ("Fail: "^msg^"\n")
         | exn => Util.explain exn

val _ = main ()
