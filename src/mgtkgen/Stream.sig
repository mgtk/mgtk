(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)
(* This code: (c) Fritz Henglein.                                           *)

signature Stream =
sig

    type 'a stream

    exception EndOfStream

    val put: ('a * 'a stream) option -> 'a stream
    val get: 'a stream -> ('a * 'a stream) option

    val mkStream: ('b -> ('a * 'b) option) -> 'b -> 'a stream

(*
    val foldLazy: (('a * 'b Closure.closure) option -> 'b) -> 'a stream 
                  -> 'b Closure.closure
*)
    val foldr: (('a * 'b) option -> 'b) -> 'a stream -> 'b
    val foldl: (('a * 'b) option -> 'b) -> 'a stream -> 'b

    val eos: unit -> 'a stream
    val putItem: 'a * 'a stream -> 'a stream
    val putItems: 'a list * 'a stream -> 'a stream
    val fromList: 'a list -> 'a stream

    val getItem: 'a stream -> 'a * 'a stream
    val getItems: int -> 'a stream -> 'a list * 'a stream
    val getItemsUntil: ('a -> bool) -> 'a stream -> 'a list * 'a stream 
    val toList: 'a stream -> 'a list

    val map: ('a -> 'b) -> 'a stream -> 'b stream
    val app: ('a -> unit) -> 'a stream -> unit 
    val filter: ('a -> bool) -> 'a stream -> 'a stream
    val appendTo: 'a stream -> 'a stream -> 'a stream
    val concat: 'a stream stream -> 'a stream
 
    val null: 'a stream -> bool

end


(*
   Type ['a stream] represents streams with elements of type 'a.

   Exception [EndOfStream] is raised by getItem if the stream is
   empty.

   [put (elem, stream)] prepends the element elem onto the stream
   stream. The element is an option --- NONE signifies end-of-stream.

   [get stream] returns the first element of the stream if it exists,
   NONE otherwise, and the remaining part of the stream.

   [mkStream f elem] constructs a (lazy) stream from elem and
   f. Whenever a new element is needed f is called (initially with
   elem, later with the second part of the return value of the last
   call to f).

   [foldr f stream]

   [foldl f stream]

   [eos ()] constructs an empty stream.

   [putItem (elem, stream)] prepends the element elem onto the stream
   stream.

   [putItem (elems, stream)] preprends the list of elements elems onto
   the stream stream.

   [fromList elems] constructs a stream from the list of elements elenms.

   [getItem stream] returns the first element of the stream and the
   remaining part of the stream; raises EndOfStream if no element
   exists.

   [getItems n stream] returns the first n elements of the stream and
   the remaining part of the stream. If n elements are not available
   the function returns as many elements as possible. Raises Size if
   n < 0.

   [getItemsUntil p stream] returns an initial segment of the stream
   of such that the next element satisfies p (and the remaining part
   of the stream). The element satisfying p is not part of the
   segment.

   [toList stream] returns a list of the elements of the stream stream.

   [map f stream]

   [app f stream]

   [filter f stream]

   [appendTo stream1 stream2] appends the stream stream2 onto stream1.

   [concat stream] For a stream of streams stream, returns the
   ``flattened'' stream.

   [null stream] returns true if the stream stream is empty.

*)
