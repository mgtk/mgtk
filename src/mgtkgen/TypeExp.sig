(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

signature TypeExp =
sig

    type tname = NameUtil.name

    datatype inherits =
	INH_ROOT (* base of inheritance hierarchy *)
      | INH_FROM of tname (* inherits from tname *)

    datatype texp = 
	PRIMTYPE of string (* no path is necessary here *)
      | WIDGET of tname * inherits (* parent type *)
      | FLAG of tname * bool (* is this an enum? *)
      | POINTER of tname * inherits option (* parent type *)
      | TUPLE of texp list
      | ARROW of (texp * string) list (* parameters *) 
               * (texp * string) list (* output parameters *) 
               * (texp * string) list (* all parameters *)
	       * texp (* return type *)
      | OPTION of texp
      | OUTPUT of texp
      | LIST of texp
      | ARRAY of texp * bool (* include length? *)

    val toString: texp -> string
    val typeKind: texp -> string

    val equal_tname: tname * tname -> bool
    val equal_texp: texp * texp -> bool

end

(*

   Type [tname] represents type names. Type names are simply names
   as described in NameUtil.

   Type [inherits] specifies an inheritance hierarchy for widgets and
   pointer types. INH_ROOT represents the top of the hierarchy (with
   no parents), and INH_FROM(parent) a type inheriting from parent.

   Type [texp] specifies type expressions. Most of these are
   self-explanatory. The following describe the remaning ones 
   - PRIMTYPE represents "built-in" types such as integers and
     strings;
   - ARROW represents function types (notice the three types of
     parameter lists: the first component is the list of parameters
     when all output parameters have been removed, the second is the
     list of parameters, and the third is the complete list of
     parameters).

   [toString texp] returns a string representation of the type
   expression texp.

   [typeKind texp] returns a string describing the type expression
   texp --- sort of like the kind of the expression.

   [equal_tname (tname1,tname2)] returns true if the two type
   names tname1 and tname2 are equal.

   [equal_texp (texp1,texp2)] returns true if the two type expressions
   texp1 and texp2 are equal.

*)
