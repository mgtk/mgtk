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

    datatype texp = 
	TYPENAME of string
      | TUPLE of long_texp list
      | ARROW of long_texp list * long_texp
      | OPTION of long_texp
      | OUTPUT of long_texp
      | LIST of long_texp
    and long_texp = LONG of string list * texp

    fun typeClass' (TYPENAME _) = "type name"
      | typeClass' (TUPLE _) = "tuple"
      | typeClass' (ARROW _) = "arrow"
      | typeClass' (OPTION _) = "option"
      | typeClass' (OUTPUT _) = "output"
      | typeClass' (LIST _) = "list"
    fun typeClass (LONG (path, texp)) = typeClass' texp

    fun texpToString (TYPENAME s) = s
      | texpToString (TUPLE longs) = 
	Util.stringSep "(" ")" " * " toString longs
      | texpToString (ARROW (args, res)) = 
	(Util.stringSep "[" "]" " * " toString args) ^ " -> " ^ toString res
      | texpToString (OPTION long) = toString long ^ " option"
      | texpToString (OUTPUT long) = toString long ^ " output"
      | texpToString (LIST long) = toString long ^ " list"
    and toString (LONG ([], texp)) = texpToString texp
      | toString (LONG (path, texp)) = 
	(Util.stringSep "" "." "." (fn s=>s) path) ^ texpToString texp

    type constructor = string (* nick *) * string (* constructor *)
    type parameter = long_texp * string

    datatype declaration =
	OBJECT_DECL of pos * string * string * (parameter list option)
      | FUNCTION_DECL of pos * string * long_texp * (parameter list)
      | FLAGS_DECL of pos * string * constructor list
      | BOXED_DECL of pos * string * (string list) * string option
      | SIGNAL_DECL of pos * string * string list * long_texp option

    fun isWidget (OBJECT_DECL _) = true
      | isWidget _ = false
    fun isFunction (FUNCTION_DECL _) = true
      | isFunction _ = false
    fun isEnum (FLAGS_DECL _) = true
      | isEnum _ = false
    fun isBoxed (BOXED_DECL _) = true
      | isBoxed _ = false
    fun isSignal (SIGNAL_DECL _) = true
      | isSignal _ = false

    fun signalOf [n] = n
      | signalOf [p,n] = n
      | signalOf _ = Util.shouldntHappen "not a signal"

    fun nameOf (OBJECT_DECL (_, obj, _, _)) = obj
      | nameOf (FUNCTION_DECL (_, func, _, _)) = func
      | nameOf (FLAGS_DECL (_, flag, _)) = flag
      | nameOf (BOXED_DECL (_,typ, _, _)) = typ
      | nameOf (SIGNAL_DECL (_, widget, signal, _)) = signalOf signal

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
	    equal_long_texp_list (texps1, texps2)
	  | equal_texp (ARROW(args1,ret1),ARROW(args2,ret2)) =
	    equal_long_texp_list (args1, args2) andalso equal_long_texp (ret1, ret2)
	  | equal_texp (OPTION texp1, OPTION texp2) = equal_long_texp (texp1, texp2)
	  | equal_texp (OUTPUT texp1, OUTPUT texp2) = equal_long_texp (texp1, texp2)
	  | equal_texp (LIST texp1, LIST texp2) = equal_long_texp (texp1,texp2)
	  | equal_texp _ = false
	and equal_long_texp (LONG (path1, texp1), LONG (path2, texp2)) = 
	    equal_list (op =) (path1, path2) andalso equal_texp (texp1, texp2)
	and equal_texp_list (texps1, texps2) = 
	    equal_list equal_texp (texps1,texps2)
	and equal_long_texp_list (texps1, texps2) = 
	    equal_list equal_long_texp (texps1,texps2)
	fun equal_par ((typExp1, name1), (typExp2, name2)) = 
	    equal_long_texp (typExp1, typExp2) andalso name1 = name2
	fun equal_pars (pars1, pars2) = equal_list equal_par (pars1, pars2)
	fun equal_pars_opt (pars1, pars2) = 
	    equal_opt (equal_list equal_par) (pars1, pars2)
	fun equal_cons ((_,cons1), (_,cons2)) = cons1 = cons2
    in (* local *)

	fun equal (OBJECT_DECL(_,obj1,inh1,fields1), OBJECT_DECL(_,obj2,inh2,fields2)) =
	    obj1 = obj2 andalso inh1 = inh2 andalso equal_pars_opt (fields1, fields2)
	  | equal (FUNCTION_DECL(_,func1,typExp1,pars1), FUNCTION_DECL(_,func2,typExp2,pars2)) =
	    func1 = func2 andalso typExp1 = typExp2 andalso equal_pars (pars1, pars2)
	  | equal (FLAGS_DECL(_,flag1,cons1), FLAGS_DECL(_,flag2,cons2)) =
	    flag1 = flag1 andalso equal_list equal_cons (cons1,cons2)
	  | equal (BOXED_DECL(_,typ1,funcs1,_), BOXED_DECL(_,typ2,funcs2,_)) =
	    typ1 = typ2 andalso equal_list (op =) (funcs1, funcs2)
	  | equal (SIGNAL_DECL(_,wid1,signal1,cbType1), SIGNAL_DECL(_,wid2,signal2,cbType2)) =
	    wid1 = wid2 andalso equal_list (op =) (signal1,signal2) andalso equal_opt equal_long_texp (cbType1, cbType2)
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
