(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

(* DATATYPES ABSTRACTING THE PARSE PHASE
 *
 * Author: Henning Niss
 *)

structure AST :> AST =
struct

    type tname = TypeExp.tname
    type texp = TypeExp.texp

    type name = NameUtil.name

    datatype target = SIG | SML | C

    type constructor = name
    type parameter = texp * string

    type pos = int * int
    datatype funtype =
	FUNTYPE of texp  (* ``normal'' parameters *)
	         * texp option (* short parameters *)
    datatype declaration =
	MODULE_DECL of pos * bool (* explicit? *) * string list
      | OBJECT_DECL of pos * texp * (parameter list option)
      | FUNCTION_DECL of pos * name * funtype
      | FLAGS_DECL of pos * texp * constructor list
      | BOXED_DECL of pos * texp * string list
      | SIGNAL_DECL of pos * texp * name * texp option

    fun isWidget (OBJECT_DECL _) = true
      | isWidget _ = false
    fun isFunction (FUNCTION_DECL _) = true
      | isFunction _ = false
    fun isFlags (FLAGS_DECL _) = true
      | isFlags _ = false
    val isEnum = isFlags
    fun isBoxed (BOXED_DECL _) = true
      | isBoxed _ = false
    fun isSignal (SIGNAL_DECL _) = true
      | isSignal _ = false

    fun signalOf (path,base) = Util.stringSep "" "" "" (fn s=>s) (path@base)

    fun nameOf (MODULE_DECL (_,_,path)) = Util.stringSep "" "" "." (fn s=>s) path
      | nameOf (OBJECT_DECL (_, obj, _)) = TypeExp.widgetOf obj
      | nameOf (FUNCTION_DECL (_, funname, _)) = NameUtil.nameToString funname
      | nameOf (FLAGS_DECL (_, flag, _)) = TypeExp.flagOf flag
      | nameOf (BOXED_DECL (_, pointer, _)) = TypeExp.boxedOf pointer
      | nameOf (SIGNAL_DECL (_, widget, signal, _)) = signalOf signal

    fun typeOf (MODULE_DECL _) = "module"
      | typeOf (OBJECT_DECL _) = "object"
      | typeOf (FUNCTION_DECL _) = "function"
      | typeOf (FLAGS_DECL _) = "flags/enum"
      | typeOf (BOXED_DECL _) = "boxed"
      | typeOf (SIGNAL_DECL _) = "signal"

    fun posOf (MODULE_DECL(p, _,_)) = p
      | posOf (OBJECT_DECL (p,_,_)) = p
      | posOf (FUNCTION_DECL (p,_,_)) = p
      | posOf (FLAGS_DECL (p, _, _)) = p
      | posOf (BOXED_DECL (p,_,_)) = p
      | posOf (SIGNAL_DECL (p,_,_,_)) = p

    local
	fun equal_list eq ([], []) = true
	  | equal_list eq (x::xs, y::ys) = 
	    eq(x,y) andalso equal_list eq (xs, ys)
	  | equal_list eq _ = false

	fun equal_opt eq (NONE, NONE) = true
	  | equal_opt eq (SOME x, SOME y) = eq (x,y)
	  | equal_opt eq _ = false

	fun equal_pair eq ((x1,y1),(x2,y2)) = eq (x1,y1) andalso eq (y1,y2)

	fun equal_par ((typExp1, name1), (typExp2, name2)) = 
	    TypeExp.equal_texp (typExp1, typExp2) andalso name1 = name2
	fun equal_pars (pars1, pars2) = equal_list equal_par (pars1, pars2)
	fun equal_pars_opt (pars1, pars2) = 
	    equal_opt (equal_list equal_par) (pars1, pars2)
    in (* local *)

	fun equal (MODULE_DECL (_,exp1,path1), MODULE_DECL(_,exp2,path2)) =
	    exp1=exp2 andalso equal_list (op=) (path1,path2)
	  | equal (OBJECT_DECL(_,wid1,fields1), OBJECT_DECL(_,wid2,fields2)) =
	    TypeExp.equal_texp (wid1,wid2) andalso equal_pars_opt (fields1,fields2)
	  | equal (FUNCTION_DECL(_,name1,funtype1), FUNCTION_DECL(_,name2,funtype2)) =
	    TypeExp.equal_name (name1,name2) andalso equal_funtype (funtype1, funtype2)
	  | equal (FLAGS_DECL(_,flag1,cons1), FLAGS_DECL(_,flag2,cons2)) =
	    TypeExp.equal_texp (flag1,flag2) andalso equal_list (op =) (cons1,cons2)
	  | equal (BOXED_DECL(_,typ1,funcs1), BOXED_DECL(_,typ2,funcs2)) =
	    TypeExp.equal_texp (typ1,typ2) andalso TypeExp.equal_texp (typ1,typ2)
	    andalso equal_list (op =) (funcs1, funcs2)
	  | equal (SIGNAL_DECL(_,wid1,signal1,cbType1), SIGNAL_DECL(_,wid2,signal2,cbType2)) =
	    TypeExp.equal_texp (wid1,wid2) andalso TypeExp.equal_name (signal1,signal2) andalso equal_opt TypeExp.equal_texp (cbType1, cbType2)
	  | equal _ = false
	and equal_funtype (FUNTYPE (long1,short1), FUNTYPE(long2,short2)) =
	    TypeExp.equal_texp (long1,long2) andalso equal_opt TypeExp.equal_texp (short1,short2)
    end (* local *)


    fun nameOrder (d1, d2) = String.compare (nameOf d1, nameOf d2)
    fun declOrder (d1, d2) =
	case (d1, d2) of
            (MODULE_DECL (_,_,p1), MODULE_DECL (_,_,p2)) =>
		let fun loop ([], []) = EQUAL
                      | loop ([], _ ) = LESS
                      | loop (_,  []) = GREATER
                      | loop (x::xs,y::ys) = if x=y then loop (xs,ys)
					     else String.compare(x,y)
		in  loop (p1,p2)
		end
          | (MODULE_DECL _, _) => LESS
          | (OBJECT_DECL _, MODULE_DECL _) => GREATER
	  | (OBJECT_DECL _, OBJECT_DECL _) =>
		nameOrder (d1, d2)
	  | (OBJECT_DECL _, _) => LESS
          | (FUNCTION_DECL _, MODULE_DECL _) => GREATER
	  | (FUNCTION_DECL _, OBJECT_DECL _) => GREATER
	  | (FUNCTION_DECL _, FUNCTION_DECL _) =>
		nameOrder (d1, d2)
	  | (FUNCTION_DECL _, _) => LESS
          | (SIGNAL_DECL _, MODULE_DECL _) => GREATER
	  | (SIGNAL_DECL _, OBJECT_DECL _) => GREATER
	  | (SIGNAL_DECL _, FUNCTION_DECL _) => GREATER
	  | (SIGNAL_DECL _, SIGNAL_DECL _) =>
		nameOrder (d1, d2)
	  | (SIGNAL_DECL _, _) => LESS
          | (FLAGS_DECL _, MODULE_DECL _) => GREATER
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
