(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

signature DEFSPARSE =  sig
    val addPath : string -> unit (* add a path to the list of directories
                                    searched when expanding includes *)
    val parseFile : string -> Defs.definition list
    val parseMetadataFile : string -> Defs.metadata list

end
