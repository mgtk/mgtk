(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

structure MosmlTypeInfo = TypeInfo(structure Prim = MosmlPrimTypes)
structure MLtonTypeInfo = TypeInfo(structure Prim = MLtonPrimTypes)
structure MosmlPrims = MosmlPrims(structure TypeInfo = MosmlTypeInfo)
structure MLtonPrims = MLtonPrims(structure TypeInfo = MLtonTypeInfo)
structure GenSMLMosml = GenSML(structure Prims = MosmlPrims)
structure GenSMLMLton = GenSML(structure Prims = MLtonPrims)
structure GenCMosml = GenCMosml(structure TypeInfo = MosmlPrims.TypeInfo)
structure GenCMLton = GenCMLton(structure TypeInfo = MLtonPrims.TypeInfo)

fun main () =
    let 

	fun inc r () = r := !r + 1

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

	val toplevel = ref NONE
	fun setToplevel tl = toplevel := SOME tl

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

	val metadataFile = ref NONE
	fun setMetaFile f = metadataFile := SOME f
	fun getMetaFile () = !metadataFile

	val verbosity = ref 0

	val forMLton = ref false
	val sep_struct = ref false
	val named_sigs = ref true

	val args = [ ("-I",  ArgParse.String DefsParse.addPath)
		   , ("-so", ArgParse.String setSMLOutFile)
		   , ("-o",  ArgParse.String setOutFileBase)
		   , ("-co", ArgParse.String setCOutFile)
		   , ("-bo", ArgParse.String setOutFileBase)
		   , ("-cp", ArgParse.String setCPreamble)
		   , ("-sp", ArgParse.String setSMLPreamble)
		   , ("-tl", ArgParse.String setToplevel)
		   , ("-q",  ArgParse.Unit   MsgUtil.quiet)
		   , ("-v",  ArgParse.Unit   (inc verbosity))
		   , ("--mlton",  ArgParse.Unit   (fn () => forMLton := true))
		   , ("--separate-struct",  ArgParse.Unit   (fn () => sep_struct := true))
		   , ("--anonymous-sigs",  ArgParse.Unit   (fn () => named_sigs := false))
		   , ("-as",  ArgParse.Unit   (fn () => named_sigs := false))
		   , ("--metadata-file", ArgParse.String setMetaFile)
                   ] @ Debug.argparse ()
	val _ = ArgParse.parse args setFile
	val _ = DefsParse.addPath (#dir (Path.splitDirFile (getFile())))
	val _ = case !verbosity of
		    0 => MsgUtil.quiet()
		  | 1 => MsgUtil.verbose()
		  | 2 => MsgUtil.Verbose()
		  | _ => MsgUtil.Debug()
	val _ = if !named_sigs andalso !sep_struct then
		    MsgUtil.warning("Warning: could lead to SML code that does not conform to the standard.")
		else ()
        (* 1. Parse *)
	val defs = (MsgUtil.print "Parsing (defs)..."; 
		    DefsParse.parseFile (getFile ()) 
		    before
		    MsgUtil.close "done\n")
	val _ = MsgUtil.print ("Defs file with " ^ Int.toString (List.length defs) ^ " definitions\n")
	val metadata =
	    case getMetaFile () of 
		NONE => []
	      | SOME f => ( MsgUtil.print "Parsing (metadata defs)..."
		          ; DefsParse.parseMetadataFile f
			    before
			    MsgUtil.close "done\n"
                          )

	val toplevel = case !toplevel of
			   NONE => Name.capitalize(Path.base(Path.file (getFile())))
			 | SOME tl => tl

        (* 2. Build modules and exclude items*)
	val api = FromDefs.fromDefs toplevel defs metadata
	val exclude = 
	    let fun read file =
		    let val is = TextIO.openIn file
		    in  TextIO.inputAll is
			before TextIO.closeIn is
		    end
		val contents = read "lib/excludes.txt"
		val excludes = Splayset.addList
		       (Splayset.empty String.compare,
			String.tokens Char.isSpace contents)
	    in  excludes
	    end
(*
	val _ = TextIO.print("Excluding: " ^ Util.stringSep "{" "}\n" ", " (fn s=>s) (Splayset.listItems exclude))
*)
	fun member set elem = Splayset.member(set,elem)
	fun filter (name,info) = not(member exclude name)
        (* Since the toplevel module will be Gtk, and this is presumably
           not filtered away, valOf below should always work. *)
	val api = Option.valOf(AST.filteri (filter,filter) api)

        (* 3. Change the order of the entries in the file to move 
              all non-module stuff up before modules. *)
	fun order api =
	    let fun oModule (AST.Module{name,members,info}) =
		    let val (mods,non) = 
			    List.partition (fn (AST.Sub _) => true
					     | _ => false) members
		    in  AST.Module{members=non @ (List.map oMember mods),
				   name=name,info=info}
		    end
		and oMember (AST.Sub module) = AST.Sub(oModule module)
		  | oMember (m as AST.Member _) = m
	    in  oModule api
	    end
	val api = order api

	val debug = fn module => 
        let val pp = AST.ppAst Name.pp (Type.pp Name.pp Name.pp)
	in  if Debug.included "ResolveTypes.debug_resolve_types" then
		( print("After resolving types and names:\n")
		; Pretty.ppPlain (pp module) TextIO.stdOut )
	    else ()
        end

        (* 4. Resolve types and names *)
	val api = ResolveNames.resolve (ResolveTypes.resolve api)
	val _ = debug api (* resolvetypes only knows how to handle
			     stringe'd versions of ASTs *)

	val (modules,values) = AST.fold (fn (mn,(m,v)) => (m+1,v), fn (mn,(m,v)) => (m,v+1)) (0,0) api
	val _ = MsgUtil.close ("  corresponding to " ^ Int.toString modules ^ "(sub)modules with " ^ Int.toString values ^ " values\n")

	val _ =
	    if !forMLton then
		let
	(* 5. Generate code ... *)
        (*    ... SML *)
	val _ = MsgUtil.print "Generating SML code ..."
	val typeinfo = MLtonPrims.TypeInfo.build api
	val (getOutFile,closeOutFile) = outFileSetup smlOutFile
	val topdec = GenSMLMLton.translate (!named_sigs) typeinfo api
	val _ = GenSMLMLton.print (!smlPreamble) (!sep_struct) 
				  (getOutFile()) topdec
        val _ = closeOutFile()
	val _ = MsgUtil.close "done"

	val _ = MsgUtil.print "Generating C code ..."
	val (getOutFile,closeOutFile) = outFileSetup cOutFile
	val api'' = GenCMLton.generate typeinfo api
		    handle (exn as Fail m) => (TextIO.output(TextIO.stdOut, "Caught Fail(" ^ m ^ ")\n"); raise exn)
	val _ = Option.app (copyFile (getOutFile())) (!cPreamble)
	val _ = GenCMLton.print (getOutFile()) api''
        val _ = closeOutFile()
	val _ = MsgUtil.close "done"
		in () end
        
	    else let
	(* 5. Generate code ... *)
        (*    ... SML *)
	val _ = MsgUtil.print "Generating SML code ..."
	val typeinfo = MosmlPrims.TypeInfo.build api
	val (getOutFile,closeOutFile) = outFileSetup smlOutFile
	val topdec = GenSMLMosml.translate (!named_sigs) typeinfo api
	val _ = GenSMLMosml.print (!smlPreamble) (!sep_struct)
				  (getOutFile()) topdec
        val _ = closeOutFile()
	val _ = MsgUtil.close "done"

        (*    ... C *)
	val _ = MsgUtil.print "Generating C code ..."
	val (getOutFile,closeOutFile) = outFileSetup cOutFile
	val api'' = GenCMosml.generate typeinfo api
		    handle (exn as Fail m) => (TextIO.output(TextIO.stdOut, "Caught Fail(" ^ m ^ ")\n"); raise exn)
	val _ = Option.app (copyFile (getOutFile())) (!cPreamble)
	val _ = GenCMosml.print (getOutFile()) api''
        val _ = closeOutFile()
	val _ = MsgUtil.close "done"
		in () end

    in  ()
    end

val _ = main ()
