(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

(* LEXER SPECIFICATION
 *
 * This is a very simple lexer based on the lexer in Paulson's
 * book. It could probably be improved a lot.
 *
 * Author: Henning Niss
 *)

structure Lexer :> Lexer =
struct

    type charStream = (char * int) Stream.stream

    type pos = int * int
    fun lastPos p ([]: (char * int) list) = p
      | lastPos p l = #2(hd (rev l))

    datatype token =
	LPAREN of pos
      | RPAREN of pos
      | EQUALS of pos
      | WORD of pos * string
      | STRING of pos * string
    type tokenStream = token Stream.stream

    fun toString (LPAREN _) = "("
      | toString (RPAREN _) = ")"
      | toString (EQUALS _) = "="
      | toString (WORD(_, s)) = s
      | toString (STRING(_, s)) = "\"" ^ s ^ "\""

    fun whitespaceChar #" " = true
      | whitespaceChar #"\r" = true
      | whitespaceChar #"\n" = true
      | whitespaceChar _ = false
    val letterChar = Char.isAlpha
    val digitChar = Char.isDigit
    fun newlineChar ch = ch = #"\n"

    fun symbolChar #"(" = true
      | symbolChar #")" = true
      | symbolChar #"=" = true
      | symbolChar _ = false
    fun symbol p (#"(", cs) = (LPAREN(p,p+1), cs)
      | symbol p (#")", cs) = (RPAREN(p,p+1), cs)
      | symbol p (#"=", cs) = (EQUALS(p,p+1), cs)
      | symbol p _ = raise Fail("oops")

    val wordStartChar = letterChar
    fun wordChar #"-" = true
      | wordChar #"_" = true
      | wordChar c = letterChar c orelse digitChar c
    fun word p (c, str: charStream) =
	let val (wrd, str') = Stream.getItemsUntil (not o wordChar o #1) str
	in  (implode (c :: (map #1 wrd)), lastPos p wrd, str')
	end

    fun stringStartChar c = c = #"\""
    fun string p (str: charStream) =
	let val (s, str') = Stream.getItemsUntil (stringStartChar o #1) str
	    val (_, str'') = Stream.getItem str' (* remove the " *)
	in  (implode (map #1 s), lastPos p s, str'')
	end

    fun untilNewline (str: charStream) =
	let val (_, str') = Stream.getItemsUntil (newlineChar o #1) str
	    val (_, str'') = Stream.getItem str' (* remove the \n *)
	in  str''
	end

    fun scanning str =
	if Stream.null str then NONE
	else let val ((c,p), str) = Stream.getItem str
	     in  if wordStartChar c
		 then let val (id, p', str') = word p (c, str)
		      in  SOME (WORD((p,p'),id), str')
		      end
		 else if symbolChar c
		      then let val (sy, str') = symbol p (c, str)
			   in  SOME (sy, str')
			   end
		 else if stringStartChar c
		      then let val (s, p', str') = string p str
			   in  SOME (STRING((p+1,p'),s), str')
			   end
		 else if c = #";"
		      then let val str' = untilNewline str
			   in  scanning str'
			   end
		 else (* skip spaces, line breaks, strange characters *)
		      scanning str
	     end

    val scan = Stream.mkStream scanning

    val get = Stream.get

end (* structure Lexer *)