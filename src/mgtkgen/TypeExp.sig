signature TypeExp =
sig

    datatype inherits =
	INH_ROOT (* base of inheritance hierarchy *)
      | INH_FROM of string (* inherits from string *)

    datatype texp = 
	PRIMTYPE of string
      | TUPLE of long_texp list
      | ARROW of (long_texp * string) list (* parameters *) 
               * (long_texp * string) list (* output parameters *) 
               * (long_texp * string) list (* all parameters *)
	       * long_texp (* return type *)
      | OPTION of long_texp
      | OUTPUT of long_texp
      | FLAG of string * bool (* is this an enum? *)
      | WIDGET of string * string option (* parent type *)
      | POINTER of string * inherits option (* parent type *)
      | LIST of long_texp
    and long_texp = LONG of string list (* path to the type *)
                          * texp (* the type itself *)

    val toString: long_texp -> string
    val typeClass: long_texp -> string

    val equal_long_texp: long_texp * long_texp -> bool

    val widgetOf: long_texp -> string
    val boxedOf: long_texp -> string
    val flagOf: long_texp -> string

end

(*

   Type [long_texp] is the type of values returned by the parser
   for parts of the .defs file corresponding to types.

   [toString texp] returns a string representation of the long
   type expression texp.

   [typeClass texp] returns a string describing the type expression
   texp --- sort of like the kind of the expression.

*)