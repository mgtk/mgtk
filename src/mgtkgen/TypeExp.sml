(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

structure TypeExp :> TypeExp =
struct

    structure NU = NameUtil

    type tname = NU.name

    datatype inherits =
	INH_ROOT (* base of inheritance hierarchy *)
      | INH_FROM of tname (* inherits from tname *)

    datatype texp = 
	PRIMTYPE of string (* no path is necessary here *)
      | TUPLE of texp list
      | ARROW of (texp * string) list (* parameters *) 
               * (texp * string) list (* output parameters *) 
               * (texp * string) list (* all parameters *)
	       * texp (* return type *)
      | OPTION of texp
      | OUTPUT of texp
      | FLAG of tname * bool (* is this an enum? *)
      | WIDGET of tname * inherits (* parent type *)
      | POINTER of tname * inherits option (* parent type *)
      | LIST of texp
      | ARRAY of texp * bool (* include length? *)

    fun typeKind (PRIMTYPE _) = "type name"
      | typeKind (TUPLE _) = "tuple"
      | typeKind (ARROW _) = "arrow"
      | typeKind (OPTION _) = "option"
      | typeKind (OUTPUT _) = "output"
      | typeKind (FLAG _) = "flag/enum"
      | typeKind (POINTER _) = "pointer"
      | typeKind (WIDGET _) = "widget"
      | typeKind (LIST _) = "list"
      | typeKind (ARRAY _) = "array"

    fun toString (PRIMTYPE s) = s
      | toString (TUPLE args) = 
	Util.stringSep "(" ")" " * " toString args
      | toString (ARROW (args, _, _, res)) = 
	(Util.stringSep "[" "]" " * " (toString o #1) args) ^ " -> " ^ toString res
      | toString (OPTION texp) = toString texp ^ " option"
      | toString (OUTPUT texp) = toString texp ^ " output"
      | toString (FLAG (name,false)) = NU.combine "_" name ^ " flag"
      | toString (FLAG (name,true)) =  NU.combine "_" name ^ " enum"
      | toString (POINTER (name,inherits)) = NU.combine "_" name ^ " boxed"
      | toString (WIDGET (name,parent)) = NU.combine "" name ^ " widget"
      | toString (LIST texp) = toString texp ^ " list"
      | toString (ARRAY(texp,b)) = toString texp ^ " array" ^
				   (if b then "" else "-")

    fun equal_list eq ([], []) = true
      | equal_list eq (x::xs, y::ys) = 
	eq(x,y) andalso equal_list eq (xs, ys)
      | equal_list eq _ = false

    fun equal_opt eq (NONE, NONE) = true
      | equal_opt eq (SOME x, SOME y) = eq (x,y)
      | equal_opt eq _ = false

    fun equal_inherits (INH_ROOT, INH_ROOT) = true
      | equal_inherits (INH_FROM p1, INH_FROM p2) = p1=p2
      | equal_inherits _ = false

    fun equal_tname ((path1,base1),(path2,base2)) =
	equal_list (op=) (path1,path2) andalso equal_list (op=) (base1,base2)
    fun equal_texp (PRIMTYPE name1, PRIMTYPE name2) = name1=name2
      | equal_texp (TUPLE texps1, TUPLE texps2) = 
	equal_texp_list (texps1, texps2)
      | equal_texp (ARROW(args1,outs1,cmp1,ret1),ARROW(args2,outs2,cmp2,ret2)) =
	equal_par_list (args1, args2) andalso equal_par_list (outs1,outs2) andalso equal_par_list (cmp1,cmp2)
	andalso equal_texp (ret1, ret2)
      | equal_texp (OPTION texp1, OPTION texp2) = equal_texp (texp1, texp2)
      | equal_texp (OUTPUT texp1, OUTPUT texp2) = equal_texp (texp1, texp2)
      | equal_texp (FLAG (name1,em1), FLAG (name2,em2)) = 
	equal_tname (name1,name2) andalso em1=em2
      | equal_texp (WIDGET(name1, par1), WIDGET(name2,par2)) =
	equal_tname (name1, name2) andalso equal_inherits (par1,par2)
      | equal_texp (POINTER (name1,inh1), POINTER (name2,inh2)) =
	equal_tname (name1, name2) andalso equal_opt equal_inherits (inh1,inh2)
      | equal_texp (LIST texp1, LIST texp2) = equal_texp (texp1,texp2)
      | equal_texp (ARRAY(texp1,b1),ARRAY(texp2,b2)) = 
	equal_texp (texp1,texp2) andalso b1 = b2
      | equal_texp _ = false
    and equal_par ((texp1, name1), (texp2,name2)) =
	equal_texp (texp1, texp2) andalso name1=name2
    and equal_texp_list (texps1, texps2) = 
	equal_list equal_texp (texps1,texps2)
    and equal_par_list (pars1, pars2) =
	equal_list equal_par (pars1,pars2)

end (* structure TypeExp *)