(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

signature AST =
sig

    type pos = Lexer.pos

    datatype target = SIG | SML | C

    datatype texp = 
	TYPENAME of string
      | TUPLE of long_texp list
      | ARROW of long_texp list * long_texp
      | OPTION of long_texp
      | OUTPUT of long_texp
      | FLAG of string * bool (* is this an enum? *)
      | LIST of long_texp
    and long_texp = LONG of string list (* path to the type *)
                          * texp (* the type itself *)

    val toString: long_texp -> string

    type constructor = string
    type parameter = long_texp * string

    datatype declaration =
	OBJECT_DECL of pos * string * string * (parameter list option)
      | FUNCTION_DECL of pos * string * long_texp * (parameter list)
      | FLAGS_DECL of pos * long_texp * constructor list
      | BOXED_DECL of pos * string * (string list) * string option
      | SIGNAL_DECL of pos * string * string list * long_texp option

    val isWidget: declaration -> bool
    val isFunction: declaration -> bool
    val isEnum: declaration -> bool
    val isFlags: declaration -> bool
    val isBoxed: declaration -> bool
    val isSignal: declaration -> bool

    val nameOf: declaration -> string
    val signalOf: string list -> string
    val typeOf: declaration -> string
    val posOf: declaration -> pos

    val typeClass: long_texp -> string

    val equal: declaration * declaration -> bool
    val nameOrder: declaration * declaration -> order
    val declOrder: declaration * declaration -> order

end

(*

   Type [target] is a type for making it possible to make target
   specific functions. It has the obvious interpretation.

   Type [long_texp] is the type of values returned by the parser
   for parts of the .defs file corresponding to types.

   Type [declaration] (and it's auxiallary types [constructor] and
   [parameter]) is the type of declarations in the .defs file.

   [typeClass typExp] returns a string explaining the ``kind'' of the
   type expression typExp.

   [toString texp] returns a string representation of the long
   type expression texp.

   [isXXX decl] returns true if decl is an XXX, where XXX can be
   Widget, Function, Enum, Boxed, or Signal.

   [posOf decl] returns the position of the declaration in the .defs
   file.

   [nameOf decl] returns the name of the object being declared by the
   declaration decl.

   [signalOf signal] returns the Gtk-signal part of the signal signal.

   [typeOf decl] returns a string describing the type of declaration.

   [equal (decl1, decl2)] returns true if the two declarations decl1
   and decl2 are identical; false otherwise.

   [nameOrder (decl1, decl2)] compare decl1 and decl2 based on the
   name they declare.

   [declOrder (decl1, decl2)] compare decl1 and decl2 based on the
   type of declaration (objects < functions < signals < enums < boxeds)
   and the name being declared.

*)
