(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

signature SML_TYPE = sig

    type tyvar = string
    type tyname = string list

    datatype ty =
	IntTy 
      | CharTy
      | UnitTy
      | BoolTy
      | RealTy
      | StringTy
      | TyVar of tyvar
      | TupTy of ty list
      | ArrowTy of ty list * ty
      | TyApp of ty list * tyname
      | RefTy of ty

    val mkTuple : ty list -> ty

    val eqTyName : tyname * tyname -> bool
    val eqTyVar  :  tyvar * tyvar  -> bool
    val equal    :     ty * ty     -> bool

    val ppTyName : tyname Pretty.pp
    val ppTyVar  :  tyvar Pretty.pp
    val pp       :     ty Pretty.pp

    val toString : ty -> string

end (* signature SMLType *)