fun main () =
    let val file = ref NONE
	fun addPath p = DefsParse.pathList := p :: !(DefsParse.pathList)
	fun setFile f = file := SOME f
	fun getFile () = case !file of NONE=>raise Fail "no file" | SOME f=>f
	val args = [("-I", ArgParse.String addPath)]
	val _ = ArgParse.parse NONE args setFile
	val _ = addPath (#dir (Path.splitDirFile (getFile())))

	val defs = (TextIO.print "Parsing (defs)\n"; 
		    DefsParse.parse (getFile ()) )
	val _ = List.app (fn (name,_,_) => TextIO.print(name^"\n")) defs
    in  ()
    end

val _ = main ()
