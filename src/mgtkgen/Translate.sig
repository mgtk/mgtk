(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

signature Translate =
sig

    val translate: TextIO.outstream -> AST.declaration list -> unit
end

(*

   [translate outstream decls] generates code from the list of
   declarations decls and outputs the result on the output stream
   outstream. The type of the generated code depends on the target
   option (either C, SIG, SML; see Main.sml).

*)