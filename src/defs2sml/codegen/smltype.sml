(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

structure SMLType = struct

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

    fun show_tname tname = Util.stringSep "" "" "." (fn s=>s) tname

    fun show ty =
	case ty of
	    IntTy => "int"
	  | CharTy => "char"
	  | StringTy => "string"
	  | UnitTy => "unit"
	  | BoolTy => "bool"
	  | RealTy => "real"
	  | TyVar a => a
	  | TupTy tys => Util.stringSep "" "" " * " show tys
	  | ArrowTy (tys,ty2) =>Util.stringSep "" " -> " " -> " show tys ^ show ty2
	  | TyApp([],tname) => show_tname tname
	  | TyApp([ty],tname) => show ty ^ " " ^ show_tname tname
	  | TyApp(tys,tname) => Util.stringSep "(" (") "^show_tname tname) "," show tys
    val toString = show

end (* structure SMLType *)

