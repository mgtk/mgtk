(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

signature Combinators =
sig

    type token = Lexer.token
    type tokenStream = Lexer.tokenStream
    type 'a parser = tokenStream -> 'a * tokenStream

    exception SyntaxError of string * tokenStream

    val empty : 'a list parser

    val -- : 'a parser * 'b parser -> ('a * 'b) parser
    val $-- : 'a parser * 'b parser -> 'b parser
    val --$ : 'a parser * 'b parser -> 'a parser
    val || : 'a parser * 'a parser -> 'a parser
    val >> : 'a parser * ('a -> 'b) -> 'b parser

    val repeat : 'a parser -> ('a list) parser
    val optional : 'a parser -> ('a option) parser

    val skip : (token -> bool) -> (token list) parser
    val skipN : (token -> bool) -> int -> (token list) parser

    val $ : token -> token parser
    val $$ : string -> string parser

    val openParen : unit parser
    val closeParen : unit parser
    val equals : unit parser
    val word : string parser
    val string : string parser

end

(*

   Structure Combinators is a library of parser combinators. A parser
   is a function taking a token stream and returning a result and a
   new token stream from which the consumed tokens have been removed.
   A parser pf raises SyntaxError if the token stream cannot legally
   be parsed by pf.

   [empty] is a parser that accepts the empty string (and consumes no
   input).

   [pfa -- pfb] returns a parser that parses first a valid pfa string,
   then a valid pfb string. It returns a pair of the results returned
   by pfa and pfb.

   [pfa $-- pfb] as for -- pfa pfb, but discards the result returned
   by pfa.

   [pfa --$ pfb] as for -- pfa pfb, but discards the result returned
   by pfb.

   [pfa || pfb] returns a parser that parses either a valid pfa string
   or a valid pfb string. It returns the result returned by the
   successfull parser.

   [pf >> f] returns a parser that when a successfull pf parse has
   been obtained, applies f to the result returned.

   [repeat pf] returns a parser that accepts zero or more occurrences
   of valid pf strings. Returns the list of results returned by
   pf. This corresponds to (pf -- repeat pf >> op:: || empty) .

   [optional pf] returns a parser that accepts zero or one occurrences
   of a valid pf string. Returns an option indicating which
   alternative was taken. This corresponds to 
     pf >> SOME || empty >> none .

   [$ token] is a parser that accepts exactly the token token.

   [$$ string] is a parser that accepts exactly the string string.

   [openParen] a parser that accepts a LPAREN token (see
   Lexer.sig). This is somewhat ugly for a general library of parser
   combinators.

   [closeParen] a parser that accepts a RPAREN token (see
   Lexer.sig). This is somewhat ugly for a general library of parser
   combinators.

   [equals] a parser that accepts a EQUALS token (see Lexer.sig). This
   is somewhat ugly for a general library of parser combinators.

   [word] a parser that accepts a WORD token (see Lexer.sig). This is
   somewhat ugly for a general library of parser combinators.

   [string] a parser that accepts a STRING token (see Lexer.sig). This
   is somewhat ugly for a general library of parser combinators.

   [skip p] a parser that skips tokens until the second time a token
   satisfies p. The parser returns a list of the skipped tokens (in
   reverse). We use the second time, since the typical use of skip
   is in a combination (p1 || skip p2) where p1 and p2 match the same
   initial segment.

   [skipN p n] a parser that behaves like skip (above), but inserts
   the last n skipped tokens in front of the stream. A more compositional
   approach would be to separate the ``unget'' functionality from the
   skip functionality, but we haven't got a such a composition combinator.

*)