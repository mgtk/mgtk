(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

structure Type :> TYPE = struct

    datatype ('n, 'v) ty =
	Void
      | Base of 'n
      | Tname of 'n
      | Ptr of ('n,'v) ty
      | Const of ('n,'v) ty
      | Arr of int option * ('n,'v) ty
      | Func of (string * ('n,'v) ty) list * ('n,'v) ty
      | WithDefault of ('n,'v) ty * 'v

    fun mapiv f g ty =
	case ty of
	    Void => Void
	  | Ptr ty => Ptr(mapiv f g ty)
	  | Const ty => Const(mapiv f g ty)
	  | Arr(i,ty) => Arr(i,mapiv f g ty)
	  | Func(pars,ret) => Func(List.map (fn (p,t) => (p,mapiv f g t)) pars,
				   mapiv f g ret)
	  | Base n => Base (f(ty,n))
	  | Tname n => Tname (f(ty, n))
 	  | WithDefault(ty, d) => WithDefault(mapiv f g ty, g(ty,d))

    fun mapi f ty = mapiv f (fn (_,d) => d) ty
    fun map f ty = mapi (fn (_,n) => f n) ty

    val toUpper = String.map Char.toUpper
    fun show show_tname show_default ty =
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
		  | WithDefault(ty,d) => 
		      "<" ^ shw ty ^ "> [" ^ show_default d ^ "]"
	in  shw ty
	end

    local open Pretty
    in
      fun parens my safe tree = if my<=safe then bracket "(#)" tree else tree
      fun pp ppn ppv ty =
	  let fun p safe ty =
		  case ty of
		      Void => ppString "void"
		    | Base n => ppn n
		    | Tname n => ppn n
		    | Ptr ty => p safe ty +^ " ref"
		    | Const ty => p safe ty +^ " const"
		    | Arr(i,ty) =>
		        (p safe ty +^ " array") ++
			  (case i of NONE => empty | 
				     SOME l => "[" ^+ ppInt l +^ "]")
		    | Func(pars,ty) =>
		        let fun f (par,ty) = ppBinary(ppString par,":",p 2 ty)
			in
			    parens 1 safe (ppBinary(ilist " #* " f pars, "->",
						    p 1 ty))
			end
		    | WithDefault(ty,d) =>
		        ppBinary(p safe ty, "with", ppv d)
	  in  p 0 ty end
    end (* local *)

    fun getParams(Func(pars,_)) = pars
      | getParams _ = raise Fail("getParams: Not a function type")

    fun getRetType(Func(_,ret)) = ret
      | getRetType _ = raise Fail("getRetType: Not a function type")

end (* structure Type *)
