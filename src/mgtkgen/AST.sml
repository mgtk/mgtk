(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

(* DATATYPES ABSTRACTING THE PARSE PHASE
 *
 * Author: Henning Niss
 *)

structure AST :> AST =
struct

    type pos = Lexer.pos

    datatype target = SIG | SML | C

    datatype type_expression = 
	TYPENAME of string
      | TUPLE of type_expression list
      | ARROW of type_expression list * type_expression
      | OPTION of type_expression
      | OUTPUT of type_expression

    fun typeClass (TYPENAME _) = "type name"
      | typeClass (TUPLE _) = "tuple"
      | typeClass (ARROW _) = "arrow"
      | typeClass (OPTION _) = "option"
      | typeClass (OUTPUT _) = "output"
	
    type constructor = string (* nick *) * string (* constructor *)
    type parameter = type_expression * string

    datatype declaration =
	OBJECT_DECL of pos * string * string * (parameter list option)
      | FUNCTION_DECL of pos * string * type_expression * (parameter list)
      | FLAGS_DECL of pos * string * constructor list
      | BOXED_DECL of pos * string * (string list) * string option
      | SIGNAL_DECL of pos * string * string * type_expression option
	
    fun nameOf (OBJECT_DECL (_, obj, _, _)) = obj
      | nameOf (FUNCTION_DECL (_, func, _, _)) = func
      | nameOf (FLAGS_DECL (_, flag, _)) = flag
      | nameOf (BOXED_DECL (_,typ, _, _)) = typ
      | nameOf (SIGNAL_DECL (_, widget, signal, _)) = signal

    fun typeOf (OBJECT_DECL _) = "object"
      | typeOf (FUNCTION_DECL _) = "function"
      | typeOf (FLAGS_DECL _) = "enum"
      | typeOf (BOXED_DECL _) = "boxed"
      | typeOf (SIGNAL_DECL _) = "signal"

    fun posOf (OBJECT_DECL (p,_,_,_)) = p
      | posOf (FUNCTION_DECL (p,_,_,_)) = p
      | posOf (FLAGS_DECL (p, _, _)) = p
      | posOf (BOXED_DECL (p,_,_,_)) = p
      | posOf (SIGNAL_DECL (p,_,_,_)) = p

    local
	fun equal_list eq ([], []) = true
	  | equal_list eq (x::xs, y::ys) = 
	    eq(x,y) andalso equal_list eq (xs, ys)
	  | equal_list eq _ = false

	fun equal_opt eq (NONE, NONE) = true
	  | equal_opt eq (SOME x, SOME y) = eq (x,y)
	  | equal_opt eq _ = false

	fun equal_texp (TYPENAME name1, TYPENAME name2) = name1 = name2
	  | equal_texp (TUPLE texps1, TUPLE texps2) = 
	    equal_texp_list (texps1, texps2)
	  | equal_texp (ARROW(args1,ret1),ARROW(args2,ret2)) =
	    equal_texp_list (args1, args2) andalso equal_texp (ret1, ret2)
	  | equal_texp (OPTION texp1, OPTION texp2) = equal_texp (texp1, texp2)
	  | equal_texp (OUTPUT texp1, OUTPUT texp2) = equal_texp (texp1, texp2)
	  | equal_texp _ = false
	and equal_texp_list (texps1, texps2) = 
	    equal_list equal_texp (texps1,texps2)
	fun equal_par ((typExp1, name1), (typExp2, name2)) = 
	    typExp1 = typExp2 andalso name1 = name2
	fun equal_pars (pars1, pars2) = equal_list equal_par (pars1, pars2)
	fun equal_pars_opt (pars1, pars2) = 
	    equal_opt (equal_list equal_par) (pars1, pars2)

    in (* local *)

	fun equal (OBJECT_DECL(_,obj1,inh1,fields1), OBJECT_DECL(_,obj2,inh2,fields2)) =
	    obj1 = obj2 andalso inh1 = inh2 andalso equal_pars_opt (fields1, fields2)
	  | equal (FUNCTION_DECL(_,func1,typExp1,pars1), FUNCTION_DECL(_,func2,typExp2,pars2)) =
	    func1 = func2 andalso typExp1 = typExp2 andalso equal_pars (pars1, pars2)
	  | equal (FLAGS_DECL(_,flag1,pars1), FLAGS_DECL(_,flag2,pars2)) =
	    flag1 = flag1 andalso equal_pars (pars1, pars2)
	  | equal (BOXED_DECL(_,typ1,funcs1,_), BOXED_DECL(_,typ2,funcs2,_)) =
	    typ1 = typ2 andalso equal_list (op =) (funcs1, funcs2)
	  | equal (SIGNAL_DECL(_,signal1,wid1,cbType1), SIGNAL_DECL(_,signal2,wid2,cbType2)) =
	    signal1 = signal2 andalso wid1 = wid2 andalso equal_opt equal_texp (cbType1, cbType2)
	  | equal _ = false
	    
    end (* local *)


    fun nameOrder (d1, d2) = String.compare (nameOf d1, nameOf d2)
    fun declOrder (d1, d2) =
	case (d1, d2) of
	    (OBJECT_DECL _, OBJECT_DECL _) =>
		nameOrder (d1, d2)
	  | (OBJECT_DECL _, _) => LESS
	  | (FUNCTION_DECL _, OBJECT_DECL _) => GREATER
	  | (FUNCTION_DECL _, FUNCTION_DECL _) =>
		nameOrder (d1, d2)
	  | (FUNCTION_DECL _, _) => LESS
	  | (SIGNAL_DECL _, OBJECT_DECL _) => GREATER
	  | (SIGNAL_DECL _, FUNCTION_DECL _) => GREATER
	  | (SIGNAL_DECL _, SIGNAL_DECL _) =>
		nameOrder (d1, d2)
	  | (SIGNAL_DECL _, _) => LESS
	  | (FLAGS_DECL _, OBJECT_DECL _) => GREATER
	  | (FLAGS_DECL _, FUNCTION_DECL _) => GREATER
	  | (FLAGS_DECL _, SIGNAL_DECL _) => GREATER
	  | (FLAGS_DECL _, FLAGS_DECL _) => 
		nameOrder (d1, d2)
	  | (FLAGS_DECL _, _) => LESS
	  | (BOXED_DECL _, BOXED_DECL _) =>
		nameOrder (d1, d2)
	  | (BOXED_DECL _, _) => GREATER

end (* structure AST *)
