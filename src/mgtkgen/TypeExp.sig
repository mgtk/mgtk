signature TypeExp =
sig

    type tname = NameUtil.name

    datatype inherits =
	INH_ROOT (* base of inheritance hierarchy *)
      | INH_FROM of tname (* inherits from tname *)

    datatype texp = 
	PRIMTYPE of string (* no path is necessary here *)
      | TUPLE of texp list
      | ARROW of (texp * string) list (* parameters *) 
               * (texp * string) list (* output parameters *) 
               * (texp * string) list (* all parameters *)
	       * texp (* return type *)
      | OPTION of texp
      | OUTPUT of texp
      | FLAG of tname * bool (* is this an enum? *)
      | WIDGET of tname * inherits (* parent type *)
      | POINTER of tname * inherits option (* parent type *)
      | LIST of texp

    val toString: texp -> string
    val typeClass: texp -> string

    val equal_name: tname * tname -> bool
    val equal_texp: texp * texp -> bool

    val widgetOf: texp -> string
    val boxedOf: texp -> string
    val flagOf: texp -> string

end

(*

   Type [long_texp] is the type of values returned by the parser
   for parts of the .defs file corresponding to types.

   [toString texp] returns a string representation of the long
   type expression texp.

   [typeClass texp] returns a string describing the type expression
   texp --- sort of like the kind of the expression.

*)