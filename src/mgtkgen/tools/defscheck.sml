exception Error of string

fun say msg = (TextIO.output(TextIO.stdErr,msg);TextIO.flushOut TextIO.stdErr)

type decl = AST.declaration

fun parse file =
    case Parse.parse Parser.decls file of
	(msg, SOME decls) => decls
      | (msg, NONE) => raise Error("Syntax Error: " ^ msg)

(* auxillaries for command line parsing *)
val inFile: string ref = ref ""

(* auxillary functions used in ArgParse.parse below *)
fun inputFile fName = inFile := fName

fun showVersion () =
    ( say ("defscheck --- check .defs files for consistency (June 2000).\n")
    ; say ("(c) Henning Niss and Ken Friis-Larsen\n")
    ; raise Error("")
    )

fun showUsage msg =
    ( say (msg ^ "\n")
    ; say ("Usage: defscheck [options] <file>\n")
    ; say ("  where options can be\n\n")
    ; say ("  -V / --version\n")
    ; raise Error("")
    )

fun main () =
    let 
	val _ = 
	    (ArgParse.parse 
                       [("-V",            ArgParse.Unit showVersion),
			("--version",     ArgParse.Unit showVersion)
		       ] inputFile)
                       handle ArgParse.Bad msg => showUsage msg

        val _ = if !inFile = "" then raise Error("No input file specified!")
		else ()

	val decls = parse (!inFile)

	exception Find
	val (hash: (string, {used:bool,defined:bool}) Polyhash.hash_table) = 
	    Polyhash.mkPolyTable (*(hash, compare)*) (99, Find)
	fun lookupType tName =
	    (Polyhash.find hash tName)
	    handle Find => ( print("unbound type name: " ^ tName)
		           ; raise Fail "unbound type"
			   )
	fun insertType tName = 
	    Polyhash.insert hash (tName, {used=false, defined=false})
	fun setDefined tName =
	    let val {used,defined} = lookupType tName
            in  Polyhash.insert hash (tName, {used=used,defined=true})
	    end
	fun setUsed tName =
	    let val {used,defined} = lookupType tName
            in  Polyhash.insert hash (tName, {used=true,defined=defined})
	    end

	fun insertDecl (AST.OBJECT_DECL (_, widget, _, _)) = insertType widget
          | insertDecl (AST.FLAGS_DECL (_, enum, _)) = insertType enum
          | insertDecl (AST.BOXED_DECL (_, boxed, _,_)) = insertType boxed
          | insertDecl _ = ()
	val _ = List.app insertDecl decls

	exception NotAName
	fun extractName (AST.LONG(path, AST.TYPENAME name)) = name
          | extractName _ = raise NotAName
	fun processDecl (AST.FUNCTION_DECL(_, name, AST.LONG(path, resExp), pars)) =
	    let val _ = 
		case resExp of 
		    AST.TUPLE args =>
			((List.app (setDefined o extractName) args)
			 handle NotAName => print ("Not a valid argument type: " ^ name ^ "\n")
			)
		  | AST.TYPENAME tName => setDefined tName
		  | _ => print ("Not a valid return type: " ^ name ^ "\n")
			 val _ = List.app (setUsed o extractName) args
	    in  ()
	    end
          | processDecl _ = ()
	val _ = List.app processDecl decls

	fun usedp (typeName, {used,defined}) = used
	fun definedp (typeName, {used,defined}) = defined
	val notUsed = Polyhash.copy hash
	val _ = Polyhash.filter (not o usedp) notUsed
	val notDefined = Polyhash.copy hash
	val _ = Polyhash.filter (not o definedp) notDefined

	val _ = ( print "Unused types:\n"
                ; Polyhash.apply (print o #1) notUsed
                )
        val _ = ( print "Undefined types:\n"
                ; Polyhash.apply (print o #1) notDefined
                )

    in  ()
    end
    handle Error msg => say (msg ^ "\n")
         | Fail msg => say ("Fail: "^msg^"\n")

val _ = main ()


