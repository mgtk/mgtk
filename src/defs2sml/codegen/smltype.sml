(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

structure SMLType :> SML_TYPE = struct

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

    fun mkTuple [ty] = ty
      | mkTuple tys = TupTy tys

    fun eqStr (s,s') = case String.compare(s,s') of
			   EQUAL => true
			 | _ => false
    fun eqTyVar (tv,tv') = eqStr(tv,tv')
    fun eqTyName (tn,tn') = List.all eqStr (ListPair.zip(tn,tn'))
    fun equal (t,t') =
	case (t,t') of
	    (IntTy, IntTy) => true
	  | (CharTy, CharTy) => true
	  | (UnitTy, UnitTy) => true
	  | (BoolTy, BoolTy) => true
	  | (RealTy, RealTy) => true
	  | (StringTy, StringTy) => true
	  | (TyVar tv, TyVar tv') => eqTyVar(tv,tv')
	  | (TupTy ts, TupTy ts') => eqlists (ts, ts')
	  | (ArrowTy(ts,t), ArrowTy(ts',t')) => 
	      eqlists (ts,ts') andalso equal (t,t')
	  | (RefTy t, RefTy t') => equal (t,t')
	  | (TyApp(ts,tn), TyApp(ts',tn')) => 
	      eqlists (ts,ts') andalso eqTyName(tn,tn')
	  | _ => false
    and eqlists (ts,ts') = List.all equal (ListPair.zip (ts,ts'))

    local open Pretty
    in
    fun ppTyName tname = clist "#." ppString tname
    fun ppTyVar  tyvar = ppString tyvar
    fun parens my safe tree = if my<=safe then bracket "(#)" tree else tree
    fun pp safe ty =
	case ty of
	    IntTy => ppString "int"
	  | CharTy => ppString "char"
	  | StringTy => ppString "string"
	  | UnitTy => ppString "unit"
	  | BoolTy => ppString "bool"
	  | RealTy => ppString "real"
	  | TyVar a => ppString a
	  | TupTy tys => parens 2 safe (ilist " #* " (pp 2) tys)
	  | ArrowTy (tys, ty) => 
	      parens 1 safe (ppBinary(ilist " #-> " (pp 1) tys, "->", pp 1 ty))
	  | TyApp([],["void"]) => ppString "unit" (* FIXME: HACK *)
	  | TyApp([],tname) => ppTyName tname
	  | TyApp([ty],tname) => pp 2 ty ++ ppTyName tname
	  | TyApp(tys,tname) =>
	      bracket "(#)" (clist ",#" (pp 2) tys) ++ ppTyName tname
	  | RefTy ty => pp safe ty +^ " ref"
    end (* local *)
    val pp = fn ty => pp 0 ty

    val toString = Pretty.ppToString o pp

end (* structure SMLType *)

