structure TypeExp :> TypeExp =
struct

    datatype inherits =
	INH_ROOT (* base of inheritance hierarchy *)
      | INH_FROM of string (* inherits from string *)

    datatype texp = 
	PRIMTYPE of string
      | TUPLE of long_texp list
      | ARROW of (long_texp * string) list (* parameters *) 
               * (long_texp * string) list (* output parameters *) 
               * (long_texp * string) list (* all parameters *)
	       * long_texp (* return type *)
      | OPTION of long_texp
      | OUTPUT of long_texp
      | FLAG of string * bool (* is this an enum? *)
      | WIDGET of string * string option (* parent *)
      | POINTER of string * inherits option (* parent *)
      | LIST of long_texp
    and long_texp = LONG of string list * texp

    fun typeClass' (PRIMTYPE _) = "type name"
      | typeClass' (TUPLE _) = "tuple"
      | typeClass' (ARROW _) = "arrow"
      | typeClass' (OPTION _) = "option"
      | typeClass' (OUTPUT _) = "output"
      | typeClass' (FLAG _) = "flag/enum"
      | typeClass' (POINTER _) = "pointer"
      | typeClass' (WIDGET _) = "widget"
      | typeClass' (LIST _) = "list"
    fun typeClass (LONG (path, texp)) = typeClass' texp

    fun texpToString (PRIMTYPE s) = s
      | texpToString (TUPLE longs) = 
	Util.stringSep "(" ")" " * " toString longs
      | texpToString (ARROW (args, _, _, res)) = 
	(Util.stringSep "[" "]" " * " (toString o #1) args) ^ " -> " ^ toString res
      | texpToString (OPTION long) = toString long ^ " option"
      | texpToString (OUTPUT long) = toString long ^ " output"
      | texpToString (FLAG (name,false)) = name ^ " flag"
      | texpToString (FLAG (name,true)) = name ^ " enum"
      | texpToString (POINTER (boxed,inherits)) = boxed
      | texpToString (WIDGET (name,parent)) = name ^ " widget"
      | texpToString (LIST long) = toString long ^ " list"
    and toString (LONG ([], texp)) = texpToString texp
      | toString (LONG (path, texp)) = 
	(Util.stringSep "" "." "." (fn s=>s) path) ^ texpToString texp


    fun widgetOf (LONG(_, WIDGET(wName,_))) = wName
      | widgetOf _ = Util.shouldntHappen "widgetOf: not a widget"
    fun flagOf (LONG(_, FLAG(fName,_))) = fName
      | flagOf _ = Util.shouldntHappen "flagOf: not a flag"
    fun boxedOf (LONG(_, POINTER (boxed,inherits))) = boxed
      | boxedOf _ = Util.shouldntHappen "boxedOf: not a pointer"


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

    fun equal_texp (PRIMTYPE name1, PRIMTYPE name2) = name1=name2
      | equal_texp (TUPLE texps1, TUPLE texps2) = 
	equal_long_texp_list (texps1, texps2)
      | equal_texp (ARROW(args1,outs1,cmp1,ret1),ARROW(args2,outs2,cmp2,ret2)) =
	equal_par_list (args1, args2) andalso equal_par_list (outs1,outs2) andalso equal_par_list (cmp1,cmp2)
	andalso equal_long_texp (ret1, ret2)
      | equal_texp (OPTION texp1, OPTION texp2) = equal_long_texp (texp1, texp2)
      | equal_texp (OUTPUT texp1, OUTPUT texp2) = equal_long_texp (texp1, texp2)
      | equal_texp (FLAG (flag1,enum1), FLAG (flag2,enum2)) = 
	flag1=flag2 andalso enum1=enum2
      | equal_texp (WIDGET(widget1, parent1), WIDGET(widget2,parent2)) =
	widget1=widget2 andalso parent1=parent2
      | equal_texp (POINTER (boxed1,inherits1), POINTER (boxed2,inherits2)) =
	boxed1=boxed2 andalso equal_opt equal_inherits (inherits1,inherits2)
      | equal_texp (LIST texp1, LIST texp2) = equal_long_texp (texp1,texp2)
      | equal_texp _ = false
    and equal_long_texp (LONG (path1, texp1), LONG (path2, texp2)) = 
	equal_list (op =) (path1, path2) andalso equal_texp (texp1, texp2)
    and equal_par ((texp1, name1), (texp2,name2)) =
	equal_long_texp (texp1, texp2) andalso name1=name2
    and equal_texp_list (texps1, texps2) = 
	equal_list equal_texp (texps1,texps2)
    and equal_long_texp_list (texps1, texps2) = 
	equal_list equal_long_texp (texps1,texps2)
    and equal_par_list (pars1, pars2) =
	equal_list equal_par (pars1,pars2)

end (* structure TypeExp *)