(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

(* PARSER BUILDER
 *
 * This structure binds the functions in the lexer and the
 * parser together.
 *
 * Author: Henning Niss
 *)

structure Parse =
struct

    fun parse pf filename =
	let val dev = TextIO.openIn filename
	    val ss  = Substring.all(TextIO.inputAll dev) 
		      before TextIO.closeIn dev
	    val stream = Parsercomb.stream Substring.getc ss
	in  case pf stream of
	        SOME(res, strm) => 
		    (case Parsercomb.getItem strm of
			SOME (c,strm') => (* stream is non empty *)
			    (SOME ("The whole file didn't parse: "^
				   "last declaration " ^ AST.nameOf(hd(rev res))),
			     NONE)
                       | NONE => (NONE, SOME res)
		    )
	      | NONE => (SOME "Syntax error", NONE)
	end

end (* structure Parse *)

(*

   [parse pf fname] read a file and parse it according to
   the pf parser. If any tokens remain on the input stream
   it is considered an error. This is probably NOT what one
   would expect; but it helps me debug the code
  
*)