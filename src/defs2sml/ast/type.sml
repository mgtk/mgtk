(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

structure Type :> Type = struct

    datatype 'n ty =
	Void
      | Base of 'n
      | Tname of 'n
      | Ptr of 'n ty
      | Const of 'n ty
      | Arr of int option * 'n ty
      | Func of (string * 'n ty) list * 'n ty

    fun mapi (f: ('n1 ty * 'n1) -> 'n2) (ty: 'n1 ty) : 'n2 ty =
	case ty of
	    Void => Void
	  | Ptr ty => Ptr(mapi f ty)
	  | Const ty => Const(mapi f ty)
	  | Arr(i,ty) => Arr(i,mapi f ty)
	  | Func(pars,ret) => Func(List.map (fn (p,t) => (p,mapi f t)) pars,
				   mapi f ret)
	  | Base n => Base (f(ty,n))
	  | Tname n => Tname (f(ty, n))
    fun map f ty = mapi (fn (_,n) => f n) ty

    val toUpper = String.map Char.toUpper
    fun show show_tname ty =
	let fun shw ty =
		case ty of
		    Void => "VOID"
		  | Base n => toUpper(show_tname n)
		  | Tname n => show_tname n
		  | Ptr ty => shw ty ^ " ref"
		  | Const ty => shw ty ^ " const"
		  | Arr(i, ty) => 
		      shw ty ^ " array" ^ 
		               (case i of NONE => ""
					| SOME l => "["^Int.toString l^"]")
		  | Func(pars, ty) => 
		      Util.stringSep "{" "}" "*" 
				     (fn (p,t) => p^":"^shw t)
				     pars
	                ^ "-> " ^ shw ty
	in  shw ty
	end


    fun getParams(Func(pars,_)) = pars
      | getParams _ = raise Fail("getParams: Not a function type")

    fun getRetType(Func(_,ret)) = ret
      | getRetType _ = raise Fail("getRetType: Not a function type")

end (* structure Type *)
