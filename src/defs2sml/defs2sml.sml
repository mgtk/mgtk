(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

fun main () =
    let val file = ref NONE
	fun setFile f = file := SOME f
	fun getFile () = case !file of NONE=>raise Fail "no file" | SOME f=>f
	val outFile = ref NONE
	fun setOutFile f = outFile := SOME f
	val outFileSetup =  fn () =>
	    case !outFile of 
		NONE => (fn () => TextIO.stdOut, fn () => ())
	      | SOME f => let val os = TextIO.openOut f
			  in (fn () => os, fn () => TextIO.closeOut os) end
		
	val args = [("-I", ArgParse.String DefsParse.addPath),
		    ("-o", ArgParse.String setOutFile)
                   ]
	val _ = ArgParse.parse args setFile
	val _ = DefsParse.addPath (#dir (Path.splitDirFile (getFile())))
	val (getOutFile,closeOutFile) = outFileSetup()

	val defs = (MsgUtil.print "Parsing (defs)..."; 
		    DefsParse.parseFile (getFile ()) 
		    before
		    MsgUtil.close "done\n")
	val _ = MsgUtil.print ("Defs file with " ^ Int.toString (List.length defs) ^ " definitions\n")

	val api = (ResolveTypes.resolve o ResolveNames.resolve)
		      (FromDefs.fromDefs "Gtk" defs)

	val (modules,values) = AST.fold (fn (mn,(m,v)) => (m+1,v), fn (mn,(m,v)) => (m,v+1)) (0,0) api
	val _ = MsgUtil.close ("  corresponding to " ^ Int.toString modules ^ "(sub)modules with " ^ Int.toString values ^ " values\n")

	val api' = GenSML.generate api
	val _ = GenSML.print (getOutFile()) api'

	val api'' = GenC.generate api
		    handle (exn as Fail m) => (TextIO.output(TextIO.stdOut, "Caught Fail(" ^ m ^ ")\n"); raise exn)
	val _ = GenC.print (getOutFile()) api''

    in  closeOutFile()
    end

val _ = main ()
