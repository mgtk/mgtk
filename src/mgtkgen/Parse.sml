(* mgtkgen --- generate wrapper code from .defs file.                       *)
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
  
    fun inputFile filename =
	let val inStream = TextIO.openIn filename
	    val fileContents = TextIO.inputAll inStream 
		               before TextIO.closeIn inStream
	    fun readChar i = 
		SOME ((String.sub (fileContents, i), i), i+1)
		handle Subscript => (TextIO.closeIn inStream; NONE)
	in  Stream.mkStream readChar 0
	end

    local
	fun str _ nil _ _ = ""
	  | str p (h::t) sep needSep =
	    let val s = p h ^ (str p t sep true)
	    in  if needSep then sep ^ s else s
	    end
    in
	fun stringSep start finish sep p l = 
	    start ^ (str p l sep false) ^ finish
    end (* local *)

    fun showRest msg tokenStream =
	let val list = #1(Stream.getItems 10 tokenStream)
	    val strList = stringSep msg "" ", "  Lexer.toString list
	in  strList
	end

    fun parse pf fname = 
	let val tokenStream = Lexer.scan(inputFile fname)
	    val (res, tokenStream') = pf tokenStream
	in  if Stream.null tokenStream'
	    then ("Okay", SOME res)
	    else (showRest "Still some left:\n" tokenStream', NONE)
	end handle Combinators.SyntaxError (msg, fresult) => 
	                 (showRest msg fresult, NONE)

    local 
	fun pos strm = Lexer.posOf (#1(Stream.getItem strm))
    in 
	fun parse' pf fname =
	    let val tokenStream = Lexer.scan(inputFile fname)
		val (res, tokenStream') = pf tokenStream
	    in  if Stream.null tokenStream'
		then (NONE, SOME res)
		else (SOME (pos tokenStream'), NONE)
	    end handle Combinators.SyntaxError (msg, fresult) => 
		         (SOME (pos fresult), NONE)
    end

end (* structure Parse *)

(*
   [inputFile filename] reads a file and creates a character
   stream for the file contents. This could probably be 
   improved to only read a chunk each time a new character
   is requested.
  
   [stringSep start finish sep p l] converts the list
   l to a string by prepending start, seperating elements
   by sep, and appending finish. Each element is converted
   to a string by p.
  
   [showRest msg tokenStream] a simple debug routine showing
   what remains on the input stream after parsing has consumed
   the part is recognizes.
  
   [parse pf fname] read a file and parse it according to
   the pf parser. If any tokens remain on the input stream
   it is considered an error. This is probably NOT what one
   would expect; but it helps me debug the code

   [parse' pf fname] as above, but returns an option, option
   pair with the first component indicating error (with a
   position), and the second component the parsed declarations
   (on success).
  
*)