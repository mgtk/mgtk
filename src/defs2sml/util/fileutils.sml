structure FileUtils = struct

    fun openIn pathList file =
	let fun try path =
		TextIO.openIn(Path.joinDirFile{dir=path,file=file})
	    fun loop [] = raise IO.Io{cause = SysErr("No such file or directory", NONE), function="FileUtils.openIn", name=file}
	      | loop (p::ps) = try p handle IO.Io _ => loop ps
	in  loop (Path.currentArc :: pathList)
	end

end (* structure FileUtils *)