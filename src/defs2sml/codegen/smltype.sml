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
	  | TyVar a => a
	  | TupTy tys => Util.stringSep "" "" " * " show tys
	  | ArrowTy (tys,ty2) =>
	    if length tys <= 5 
	    then Util.stringSep "" " -> " " -> " show tys ^ show ty2
	    else Util.stringSep "" " -> " " * " show tys ^ show ty2
	  | TyApp([],tname) => show_tname tname
	  | TyApp([ty],tname) => show ty ^ " " ^ show_tname tname
	  | TyApp(tys,tname) => Util.stringSep "(" (") "^show_tname tname) "," show tys
    val toString = show

    (* FIXME: How about a hash table *)
    fun baseType "guint" = IntTy (* FIXME *)
      | baseType "uint"  = IntTy
      | baseType "int"   = IntTy
      | baseType "char"  = CharTy
      | baseType "bool"  = BoolTy
      | baseType t = raise Fail("Unrecognized base type: " ^ t)

    fun fromType fresh ty =
	case ty of
	    (* recognize standard "patterns" *)
	    Type.Ptr(Type.Base "char") => StringTy

            (* then the general stuff *)
	  | Type.Void => UnitTy
	  | Type.Base t => baseType t
	  | Type.Tname n => TyApp([fresh()],(*Name.getPath n @*) Name.getBase n@["t"])
(*
	  | Type.Tname n => TyApp([TyVar "'a"],Name.getPath n @ ["t"])
*)
	  | Type.Const ty => fromType fresh ty
	  | Type.Ptr ty => fromType fresh ty
	  | Type.Func(pars,ret) => 
	       ArrowTy(List.map (fromType fresh o #2) pars, fromType fresh ret)

	  | Type.Arr(len,ty) => TyVar(toString(fromType fresh ty)^"["^(case len of NONE => "" | SOME l => Int.toString l)^"]")

    fun fromTypeSeq () = 
	let val no = ref 0
	    val orda = Char.ord #"a"
	    fun next () = if(!no<26) then (TyVar("'"^Char.toString(Char.chr(!no+orda)))
				           before
					   no := !no + 1)
			  else raise Fail("Not implemented: fromTypeSeq.next()")
	in  fromType next end

    fun primtypeFromType ty =
	case ty of 
	    Type.Ptr(Type.Base "char") => StringTy
	  | Type.Void => UnitTy
	  | Type.Base t => baseType t
	  | Type.Tname n => TyApp([],["cptr"])
	  | Type.Const ty => primtypeFromType ty
	  | Type.Ptr ty => TyApp([],["cptr"])
	  | Type.Func(pars,ret) => 
	       ArrowTy(List.map (primtypeFromType o #2) pars, primtypeFromType ret)
	  | Type.Arr(len,ty) => TyApp([],["..."]) (* FIXME *)

end (* structure SMLType *)

