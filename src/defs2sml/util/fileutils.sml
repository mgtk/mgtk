(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

functor FileUtils(type instream val openIn: string -> instream) :>
    sig
	val openIn: string list -> string -> instream
    end =
struct

    val openIn = fn pathList => fn file =>
	let fun err () = 
		raise IO.Io{cause=SysErr("No such file or directory", NONE),
			    function="FileUtils.openIn",name=file}
	    fun try path = openIn(Path.joinDirFile{dir=path,file=file})
	    fun loop [] = err ()
	      | loop (p::ps) = try p handle _ => loop ps

	in  loop (Path.currentArc :: pathList)
	end

end (* structure FileUtils *)
