(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

signature Lexer =
sig

    type pos = int * int
    datatype token =
	LPAREN of pos
      | RPAREN of pos 
      | EQUALS of pos
      | WORD of pos * string
      | STRING of pos * string
    type tokenStream = token Stream.stream

    val toString: token -> string
    val scan: (char * int) Stream.stream -> tokenStream
    val get: tokenStream -> (token * tokenStream) option

end

(*

   Structure Lexer implements a simple lexical analyzer.

   [toString token] makes a string representation of a token.

   [scan instream outstream] converts a character stream instream to a
   token stream outstream by collecting input characters into tokens.

   [get stream] returns the first token on the stream and the rest of
   the stream if a token is available, otherwise NONE.

*)

