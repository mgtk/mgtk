(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

signature AST =
sig

    type texp = TypeExp.texp
    type name = NameUtil.name

    type pos = int * int

    type parameter = texp * string

    datatype funtype =
	FUNTYPE of texp (* ``normal'' parameters *)
	         * texp option (* short parameters *)
    datatype declaration =
	MODULE_DECL of pos * bool (* explicit? *) * string list
      | OBJECT_DECL of pos * texp * (parameter list option)
      | FUNCTION_DECL of pos * name * funtype
      | FLAGS_DECL of pos * texp * name list
      | BOXED_DECL of pos * texp * string list (* ref/unref function names *)
      | SIGNAL_DECL of pos * texp * name * texp option

    val isWidget: declaration -> bool
    val isFunction: declaration -> bool
    val isEnum: declaration -> bool
    val isFlags: declaration -> bool
    val isBoxed: declaration -> bool
    val isSignal: declaration -> bool

    val nameOf: declaration -> string
    val typeOf: declaration -> string
    val posOf: declaration -> pos

    val equal: declaration * declaration -> bool
    val nameOrder: declaration * declaration -> order
    val declOrder: declaration * declaration -> order

end

(*

   Type [declaration] (and it's auxiallary types [parameter] and
   [funtype]) is the type of declarations in the .defs file. Type
   [parameter] specifies parameters (function arguments and fields of
   widgets). A parameter consists of a type expression and a
   name. Type [funtype] specifies function types; the optional part is
   included if the function takes arguments with defaults.

   Type [pos] represents positions in the input file; use
   Util.extractSource to extract the referenced part.

   [isXXX decl] returns true if decl is an XXX, where XXX can be
   Widget, Function, Enum, Flags, Boxed, or Signal.

   [nameOf decl] returns the name of the object being declared by the
   declaration decl.

   [typeOf decl] returns a string describing the type of declaration.

   [posOf decl] returns the position of the declaration in the .defs
   file.

   [equal (decl1, decl2)] returns true if the two declarations decl1
   and decl2 are identical; false otherwise.

   [nameOrder (decl1, decl2)] compare decl1 and decl2 based on the
   name they declare.

   [declOrder (decl1, decl2)] compare decl1 and decl2 based on the
   type of declaration 
        (objects < functions < signals < enums/flags < boxeds)
   and the name being declared.

*)
