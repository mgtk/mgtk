(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

structure Type :> Type = struct

    datatype 'n ty =
	Void
      | Base of string
      | Tname of 'n
      | Ptr of 'n ty
      | Const of 'n ty
      | Arr of int option * 'n ty
      | Func of (string * 'n ty) list * 'n ty

    fun map (f: 'n1 -> 'n2) (ty: 'n1 ty) : 'n2 ty =
	case ty of
	    Void => Void
	  | Base str => Base(str)
	  | Ptr ty => Ptr(map f ty)
	  | Const ty => Const(map f ty)
	  | Arr(i,ty) => Arr(i,map f ty)
	  | Func(pars,ret) => Func(List.map (fn (p,t) => (p,map f t)) pars,
				   map f ret)
	  | Tname n => Tname (f n)

    fun show show_tname ty =
	let fun shw ty =
		case ty of
		    Void => "VOID"
		  | Base b => String.map Char.toUpper b
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

end (* structure Type *)
