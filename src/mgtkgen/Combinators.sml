(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

(* PARSER COMBINATORS
 *
 * Author: Henning Niss
 *         (based on ideas by Fritz Henglein and Larry Paulson's book).
 *)

structure Combinators :> Combinators =
struct

(* Infix declarations *)

infix 6 $--
infix 6 --$
infix 5 --
infix 3 >>
infix 0 ||

(* Parser combinators *)

type token = Lexer.token
type tokenStream = Lexer.tokenStream
type 'a parser = tokenStream -> 'a * tokenStream

exception SyntaxError of string * tokenStream

fun (pf1 || pf2) tokenStream =
    pf1 tokenStream handle SyntaxError _ => 
	pf2 tokenStream

fun (pf1 -- pf2) tokenStream = 
    let val (res1, tokenStream1) = pf1 tokenStream
	val (res2, tokenStream2) = pf2 tokenStream1
    in  ((res1, res2), tokenStream2)
    end 

fun (pf1 $-- pf2) tokenStream = 
    let val (_, tokenStream1) = pf1 tokenStream
    in  pf2 tokenStream1
    end 

fun (pf1 --$ pf2) tokenStream = 
    let val (res1, tokenStream1) = pf1 tokenStream
	val (_, tokenStream2) = pf2 tokenStream1
    in  (res1, tokenStream2)
    end 

fun (pf >> f) tokenStream =
    let val (res, tokenStream') = pf tokenStream
    in  (f res, tokenStream') 
    end 

fun empty tokenStream = (nil, tokenStream)

fun repeat pf tokenStream = 
    let fun rep (accum, tokenStream) =
            let val (res, tokenStream') = pf tokenStream
	    in  rep (res :: accum, tokenStream') 
	    end handle SyntaxError _ => (rev accum, tokenStream)
    in  rep (nil, tokenStream)
    end

fun optional pf tokenStream = 
    let val (res, tokenStream') = pf tokenStream
    in  (SOME res, tokenStream')
    end handle SyntaxError _ => (NONE, tokenStream)

(* Scanner constructors *)

fun $ tok tokenStream = 
    case Lexer.get tokenStream of
	SOME (res as (tok', tokenStream')) => 
	    if tok = tok' then res 
	    else raise SyntaxError (Lexer.toString tok ^ " expected", tokenStream)
      | NONE => raise SyntaxError ("Unexpected end of file (1)", tokenStream)

fun openParen tokenStream =
    case Lexer.get tokenStream of
	SOME (Lexer.LPAREN _, tokenStream') => ((), tokenStream')
      | SOME (_, tokenStream') => raise SyntaxError ("expected `('", tokenStream)
      | NONE => raise SyntaxError ("Unexpected end of file (2)", tokenStream)

fun closeParen tokenStream =
    case Lexer.get tokenStream of
	SOME (Lexer.RPAREN _, tokenStream') => ((), tokenStream')
      | SOME (_, tokenStream') => raise SyntaxError ("expected `)'", tokenStream)
      | NONE => raise SyntaxError ("Unexpected end of file (3)", tokenStream)

fun equals tokenStream =
    case Lexer.get tokenStream of
	SOME (Lexer.EQUALS _, tokenStream') => ((), tokenStream')
      | SOME (_, tokenStream') => raise SyntaxError ("expected `)'", tokenStream)
      | NONE => raise SyntaxError ("Unexpected end of file (4)", tokenStream)

fun word tokenStream =
    case Lexer.get tokenStream of
	SOME (Lexer.WORD(_, word), tokenStream') => (word, tokenStream')
      | SOME (_, tokenStream') => raise SyntaxError ("name expected", tokenStream)
      | NONE => raise SyntaxError ("Unexpected end of file (5)", tokenStream)

fun string tokenStream =
    case Lexer.get tokenStream of
	SOME(Lexer.STRING(_, str), tokenStream') => (str, tokenStream')
      | SOME (_, tokenStream') => raise SyntaxError ("string expected",tokenStream)
      | NONE => raise SyntaxError ("Unexpected end of file (6)", tokenStream)

fun $$ s tokenStream =
    case word tokenStream of
	(s', tokenStream') =>
	    if s = s' then (s, tokenStream')
	    else raise SyntaxError (s ^ " expected", tokenStream)

end (* structure Combinators *)
