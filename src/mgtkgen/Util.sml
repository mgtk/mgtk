(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

structure Util =
struct

    (* some useful exceptions *)
    exception NotImplemented of string
    exception ShouldntHappen of string
    exception NotFound of string

    (* convenience functions to raise exceptions *)
    fun notFound msg = raise NotFound msg
    fun notImplemented msg = raise NotImplemented msg
    fun shouldntHappen msg = raise ShouldntHappen msg

    (* print an exception with suitable explanation *)
    fun explain (Fail msg) = TextIO.output(TextIO.stdOut, msg ^ "\n")
      | explain (NotFound msg) = TextIO.output(TextIO.stdOut, msg ^ "\n")
      | explain (NotImplemented msg) = TextIO.output(TextIO.stdOut, "Not implemented: " ^ msg ^ "\n")
      | explain (ShouldntHappen msg) = TextIO.output(TextIO.stdOut, "Shouldn't happen: " ^ msg ^ "\n")

      | explain (List.Empty) = TextIO.output(TextIO.stdOut, "Empty list\n")

      | explain (General.Bind) = TextIO.output(TextIO.stdOut, "Bind\n")
      | explain (General.Match) = TextIO.output(TextIO.stdOut, "Match\n")
      | explain (General.Interrupt) = TextIO.output(TextIO.stdOut, "Interrupt\n")

      | explain (General.Subscript) = TextIO.output(TextIO.stdOut, "Subscript\n")
      | explain (General.Size) = TextIO.output(TextIO.stdOut, "Size\n")

      | explain (General.Overflow) = TextIO.output(TextIO.stdOut, "Overflow\n")
      | explain (General.Div) = TextIO.output(TextIO.stdOut, "Div\n")
      | explain (General.Domain) = TextIO.output(TextIO.stdOut, "Domain\n")

      | explain (General.Chr) = TextIO.output(TextIO.stdOut, "Chr\n")
      | explain (General.Ord) = TextIO.output(TextIO.stdOut, "Ord\n")

      | explain (General.Io {function,...}) = TextIO.output(TextIO.stdOut, "Input/Output exception: " ^ function ^ "\n")
      | explain (General.SysErr (c,_)) = TextIO.output(TextIO.stdOut, "System Error exception: " ^ c ^ "\n")
      | explain (General.Out_of_memory) = TextIO.output(TextIO.stdOut, "no more memory\n")
      | explain (General.Invalid_argument s) = TextIO.output(TextIO.stdOut, "invalid argument: " ^ s ^ "\n")
(*      | explain (General.Graphic_failure s) = TextIO.output(TextIO.stdOut, "graphic failure: " ^ s ^ "\n")
*)
      | explain exn = (TextIO.output(TextIO.stdOut, "Uncaught exception\n"); raise exn)

    (* extend the messages of the exception with a string *)
    fun extend (NotFound msg) str = raise NotFound(str ^ msg)
      | extend (NotImplemented msg) str = raise NotImplemented(str ^ msg)
      | extend (Fail msg) str = raise Fail(str ^ msg)
      | extend (ShouldntHappen msg) str = raise ShouldntHappen(str ^ msg)
      | extend exn str = raise exn


    fun extractSource file (p1,p2) =
	let
	    val inputStream = TextIO.openIn file
	    val fileContents = TextIO.inputAll inputStream
	    val _ = TextIO.closeIn inputStream

	    val text = String.extract (fileContents, p1, SOME (p2-p1))
	in  text
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

end (* structure Util *)

(*

   The exceptions [NotFound msg], [ShouldntHappen msg], and
   [NotImplemented msg] are some ``standard'' exceptions used
   throughout the program.

   [notFound msg] convenience for NotFound

   [shouldntHappen msg] convenience for ShouldntHappen

   [notImplemented msg] convenience for NotImplemented

   [explain exn] print the exception exn with a suitable explanation.

   [extend exn] extend the message (the string argument) of a few
   exceptions with another string.

   [extractSource file (pos1,pos2)] cut out the text of the file file
   from position pos1 (an integer) to pos2 and return this as a string.

   [stringSep start finish sep p l] convert the list l to a string by
   applying p to each element, prepending start, appending finish, 
   and separating each element by sep.

*)
