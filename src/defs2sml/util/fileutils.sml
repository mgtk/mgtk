(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

structure FileUtils = struct

    fun openIn pathList file =
	let fun try path =
		TextIO.openIn(Path.joinDirFile{dir=path,file=file})
	    fun loop [] = raise IO.Io{cause = SysErr("No such file or directory", NONE), function="FileUtils.openIn", name=file}
	      | loop (p::ps) = try p handle IO.Io _ => loop ps
	in  loop (Path.currentArc :: pathList)
	end

end (* structure FileUtils *)