(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

fun main () =
    let 

	(* file ops *)
	fun ext f e = OS.Path.joinBaseExt {base=f,ext=SOME e}
	fun getExt f = #ext(OS.Path.splitBaseExt f)
	fun isFile f = OS.FileSys.fileSize f > 0 
		       handle SysErr _ => false
	fun unlink f = OS.FileSys.remove f 
	               handle SysErr(m,_) => TextIO.output(TextIO.stdErr, "Couldn't unlink " ^ f ^ "("^m^")\n")
	fun copyFile os file =
	    let val is = TextIO.openIn file
		val s = TextIO.inputAll is
	    in  TextIO.output(os, s) 
              ; TextIO.closeIn is
	    end


	(* preambles, copyrights, and stiff *)
	val cPreamble = ref NONE
	fun setCPreamble f = cPreamble := SOME f

	val smlPreamble = ref NONE
	fun setSMLPreamble f = smlPreamble := SOME f

        (* input and output files *)
	val file = ref NONE
	fun setFile f = file := SOME f
	fun getFile () = case !file of NONE=>raise Fail "no file" | SOME f=>f

	val smlOutFile = ref NONE
	fun setSMLOutFile f = ( if isFile f then unlink f else ()
		              ; smlOutFile := SOME f )

	val cOutFile = ref NONE
	fun setCOutFile f = ( if isFile f then unlink f else ()
                            ; cOutFile := SOME f )

	fun setOutFileBase f = 
	    case getExt f of
		NONE => ( setSMLOutFile(ext f "sml")
                        ; setCOutFile(ext f "c") )
	      | SOME _ => ( setSMLOutFile f ; setCOutFile f )

	val outFileSetup =  fn outFile =>
	    case !outFile of 
		NONE => (fn () => TextIO.stdOut, fn () => ())
	      | SOME f => let val os = TextIO.openAppend f
			  in (fn () => os, fn () => TextIO.closeOut os) end
		
	val args = [("-I",  ArgParse.String DefsParse.addPath),
		    ("-so", ArgParse.String setSMLOutFile),
		    ("-o",  ArgParse.String setOutFileBase),
		    ("-co", ArgParse.String setCOutFile),
		    ("-bo", ArgParse.String setOutFileBase),
		    ("-cp", ArgParse.String setCPreamble),
		    ("-sp", ArgParse.String setSMLPreamble)
                   ]
	val _ = ArgParse.parse args setFile
	val _ = DefsParse.addPath (#dir (Path.splitDirFile (getFile())))

	val defs = (MsgUtil.print "Parsing (defs)..."; 
		    DefsParse.parseFile (getFile ()) 
		    before
		    MsgUtil.close "done\n")
	val _ = MsgUtil.print ("Defs file with " ^ Int.toString (List.length defs) ^ " definitions\n")


	val api = FromDefs.fromDefs "Gtk" defs
	val exclude = Splayset.addList(Splayset.empty String.compare,
				       ["gtk_accel_group_connect",
					"gtk_accel_group_connect_by_path",
					"gtk_accel_group_disconnect",
					"_gtk_accel_group_attach",
					"_gtk_accel_group_detach",
					"_gtk_accel_group_reconnect",
					"_gtk_accel_map_init",
					"_gtk_accel_map_add_group",
					"_gtk_accel_map_remove_group",
					"_gtk_accel_path_is_valid",
					"gtk_accel_group_from_accel_closure",
					"_gtk_button_box_child_requisition",
					"_gtk_bindings_activate_event",
					"__gtk_binding_reset_parsed",
					"_gtk_scale_format_value",

					 "gtk_accel_group_find"
                                       , "PrivateFlags"
				       ])
	fun member set elem = Splayset.member(set,elem)
	val api = AST.filteri (not o member exclude o #1) api

	val api = ResolveTypes.resolve (ResolveNames.resolve api)

	val typeinfo = TypeInfo.build api

	val (modules,values) = AST.fold (fn (mn,(m,v)) => (m+1,v), fn (mn,(m,v)) => (m,v+1)) (0,0) api
	val _ = MsgUtil.close ("  corresponding to " ^ Int.toString modules ^ "(sub)modules with " ^ Int.toString values ^ " values\n")


	val _ = MsgUtil.print "Generating SML code ..."
	val (getOutFile,closeOutFile) = outFileSetup smlOutFile
	val api' = GenSML.generate typeinfo api
	val _ = GenSML.print (!smlPreamble) (getOutFile()) api'
        val _ = closeOutFile()
	val _ = MsgUtil.close "done"

	val _ = MsgUtil.print "Generating C code ..."
	val (getOutFile,closeOutFile) = outFileSetup cOutFile
	val api'' = GenC.generate typeinfo api
		    handle (exn as Fail m) => (TextIO.output(TextIO.stdOut, "Caught Fail(" ^ m ^ ")\n"); raise exn)
	val _ = Option.app (copyFile (getOutFile())) (!cPreamble)
	val _ = GenC.print typeinfo (getOutFile()) api''
        val _ = closeOutFile()
	val _ = MsgUtil.close "done"

    in  ()
    end

val _ = main ()
