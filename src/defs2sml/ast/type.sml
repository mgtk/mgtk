(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

structure Type :> TYPE = struct

    datatype pass = OUT | INOUT (* no need to have IN as that is the default *)

    datatype ('n, 'v) ty =
	Void
      | Base of 'n
      | Tname of 'n
      | Ptr of ('n,'v) ty
      | Const of ('n,'v) ty
      | Arr of int option * ('n,'v) ty
      | Func of (string * ('n,'v) ty) list * ('n,'v) ty
      | WithDefault of ('n,'v) ty * 'v
      | Output of pass * ('n,'v) ty

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
	  | Output(p, ty) => Output(p, mapiv f g ty)

    fun mapi f ty = mapiv f (fn (_,d) => d) ty
    fun map f ty = mapi (fn (_,n) => f n) ty

    local open Pretty
    in
      fun parens my safe tree = if my<=safe then bracket "(#)" tree else tree
      fun pp ppn ppv ty =
	  let fun pppass OUT = ppString "out" 
		| pppass INOUT = ppString "in/out"
	      fun p safe ty =
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
		    | Output(pass,ty) =>
		        p safe ty ++ ("[" ^+ pppass pass +^ "]")
	  in  p 0 ty end
    end (* local *)

    fun getParams(Func(pars,_)) = pars
      | getParams _ = raise Fail("getParams: Not a function type")

    fun getRetType(Func(_,ret)) = ret
      | getRetType _ = raise Fail("getRetType: Not a function type")

end (* structure Type *)
