val defs = (TextIO.print "Parsing (defs)\n"; DefsParse.parse "api/gtk.defs")
val _ = List.app (fn (name,_,_) => TextIO.print(name^"\n")) defs
