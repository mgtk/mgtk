(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)
(* This code: (c) Fritz Henglein.                                           *)

(* STREAM IMPLEMENTATION.
 * Canonical implemenation of lazy streams
 *
 * Author: Fritz Henglein.
 *)

structure Stream :> Stream =
struct

    datatype 'a closureRep = 
	VAL of 'a
      | CLOS of unit -> 'a
	
    type 'a closure = 'a closureRep ref

    fun value (v: 'a ): 'a closure = ref (VAL v)
    fun delay (f: unit -> 'a): 'a closure = ref (CLOS f)

    fun force (cl: 'a closure): 'a =
	case !cl of
	    VAL v => v
	  | CLOS f => 
		let val v = f() in 
		    cl := VAL v; v 
		end

    (* types *)
    datatype 'a streamVal = 
	STREAM of ('a * 'a stream) option
    withtype 'a stream = 'a streamVal closure

    (* exceptions *)
    exception EndOfStream

    (* conversions *)
    fun put (p: ('a * 'a stream) option): 'a stream =
	value (STREAM p)

    fun get (s: 'a stream): ('a * 'a stream) option = 
	let val STREAM strV = force s
	in strV
	end

    (* general constructors and destructors *)
    fun mkStream (f: 'b -> ('a * 'b) option) (src: 'b): 'a stream =
	delay (fn () => 
	          case f src of
		      NONE => STREAM NONE
		    | SOME (x, y) => STREAM (SOME (x, mkStream f y)))

    fun foldLazy (f: ('a * 'b closure) option -> 'b) (s: 'a stream)
	         : 'b closure =
	delay (fn () =>
	          case get s of
		      NONE => f NONE
		    | SOME (x, s') => f (SOME (x, foldLazy f s')))
	
    fun foldr (f: ('a * 'b) option -> 'b) (s: 'a stream): 'b =
	case get s of
	    NONE => f NONE
	  | SOME (x, s') => f (SOME (x, foldr f s'))
		
    fun foldl (f: ('a * 'b) option -> 'b) (s: 'a stream): 'b =
	let fun fold (b, s) =              
	    case get s of
                NONE => b
              | SOME (x, s') => 
		    fold (f (SOME (x, b)), s')
	in  fold (f NONE, s)
	end

    (* constructors *)
    fun eos (): 'a stream = 
	put NONE

    fun putItem (p: 'a * 'a stream): 'a stream = 
	put (SOME p)

    fun putItems (l: 'a list, s: 'a stream) =
	List.foldr putItem s l

    fun fromList (l: 'a list) =
	mkStream List.getItem l

    (* destructors *)
    fun getItem (s: 'a stream): 'a * 'a stream =
	case get s of
	    SOME p => p
	  | NONE => raise EndOfStream
  
    fun getItems (n: int) (s: 'a stream): 'a list * 'a stream =
	let fun getN (left, right, k) =
	    if k = 0 then (rev left, right)
	    else case get right of
		NONE => (rev left, right)
	      | SOME (a, ar) => 
		    getN (a :: left, ar, k-1)
	in       
	    if n < 0 then raise Size
	    else getN (nil, s, n)
	end
  
    fun getItemsUntil (f: 'a -> bool) (s: 'a stream): 'a list * 'a stream = 
	let fun getUntil (accum, s) = 
	    case get s of
		NONE => (rev accum, s)
	      | SOME (a, ar) => 
		    if f a then (rev accum, s)
		    else getUntil (a :: accum, ar)
	in getUntil (nil, s)
	end  

    fun toList s =
	rev (foldl (fn NONE => nil | SOME (x, l) => x :: l) s)

    (* transformers *) 
    fun map f = 
	foldLazy (fn NONE => STREAM NONE 
                   | SOME (a, bstr) => STREAM (SOME (f a, bstr)))

    fun app f = 
	foldl (fn NONE => () | SOME (x, _) => f x)

    fun filter f = 
	foldLazy (fn NONE => STREAM NONE
                   | p as (SOME (a, astr)) => 
		      if f a then STREAM p
		      else force astr)

    fun appendTo (s'': 'a stream) =
	foldLazy (fn NONE => force s'' 
                   | p as (SOME _) => STREAM p)

    fun concat (sl: 'a stream stream) = 
	foldLazy (fn NONE => STREAM NONE
                   | SOME (se, sr) =>
		      force (appendTo sr se)) sl
 
    (* observers *)
    fun null s = 
	case get s of 
	    NONE => true
	  | SOME _ => false

end


