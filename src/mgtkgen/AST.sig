(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

signature AST =
sig

    type pos = Lexer.pos

    datatype target = SIG | SML | C

    datatype type_expression = 
	TYPENAME of string
      | TUPLE of type_expression list
      | ARROW of type_expression list * type_expression
      | OPTION of type_expression
      | OUTPUT of type_expression

    type constructor = string (* nick *) * string (* constructor *)
    type parameter = type_expression * string

    datatype declaration =
	OBJECT_DECL of pos * string * string * (parameter list option)
      | FUNCTION_DECL of pos * string * type_expression * (parameter list)
      | FLAGS_DECL of pos * string * constructor list
      | BOXED_DECL of pos * string * (string list) * string option
      | SIGNAL_DECL of pos * string * string * type_expression option


    val nameOf: declaration -> string
    val typeOf: declaration -> string
    val posOf: declaration -> pos

    val typeClass: type_expression -> string

    val equal: declaration * declaration -> bool
    val nameOrder: declaration * declaration -> order
    val declOrder: declaration * declaration -> order

end

(*

   Type [target] is a type for making it possible to make target
   specific functions. It has the obvious interpretation.

   Type [type_expression] is the type of values returned by the parser
   for parts of the .defs file corresponding to types.

   Type [declaration] (and it's auxiallary types [constructor] and
   [parameter]) is the type of declarations in the .defs file.

   [typeClass typExp] returns a string explaining the ``kind'' of the
   type expression typExp.

   [posOf decl] returns the position of the declaration in the .defs
   file.

   [nameOf decl] returns the name of the object being declared by the
   declaration decl.

   [typeOf decl] returns a string describing the type of declaration.

   [equal (decl1, decl2)] returns true if the two declarations decl1
   and decl2 are identical; false otherwise.

   [nameOrder (decl1, decl2)] compare decl1 and decl2 based on the
   name the declare.

   [declOrder (decl1, decl2)] compare decl1 and decl2 based on the
   type of declaration (objects < functions < signals < enums < boxeds)
   and the name being declared.

*)
