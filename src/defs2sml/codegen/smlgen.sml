(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

signature PRIMITIVES = sig
    structure TypeInfo : TypeInfo
    val mkMethodName : Name.name -> string
    val ccall : string -> int -> SMLType.ty -> TinySML.exp
    val getEnumsTy : int -> SMLType.ty
    val getEnums : string -> Name.name list -> TinySML.exp

    val mkToFunc: unit -> TinySML.exp
    val callStub : TypeInfo.typeinfo -> string  -> (Name.name, Name.name) Type.ty
		-> (TinySML.exp * (Name.name, TinySML.exp) Type.ty) list -> TinySML.exp
    val strHeader : TinySML.dec list
end (* signature PRIMTIVES *)

functor MosmlPrims(structure TypeInfo : TypeInfo) :> PRIMITIVES 
= struct
    structure TypeInfo = TypeInfo
    open TinySML SMLType
    val mkMethodName = Name.asCStub
    fun ccall name args ty =
	let val app = if args > max_curried then "app1" 
		      else "app" ^ Int.toString args
	in  App(Var app, [App(Var "symb", [Str name])])
	end
    fun getEnumsTy num =
	ArrowTy([UnitTy], TupTy(List.tabulate(num, fn _ => IntTy)))
    fun getEnums enum consts =
	App(Var("get_" ^ enum ^ "_"), [Unit])

    fun mkToFunc () = 
	App(Var"inherit", [Unit,Fn("()",App(Var"repr",[Var"obj"]))])

    fun callStub tinfo name ret pars = 
	let fun default (par,t as Type.WithDefault(ty, v)) = 
		App(Var"getOpt", [Tup[par, v]])
	      | default (par,ty) = par
	in  App(Var(name^"_"),
		if List.length pars > max_curried
		then [Tup(List.map default pars)]
		else List.map default pars
            )
	end

    val strHeader =
        [ OpenDec["Dynlib"]
        , TypeDec(([],["cptr"]), SOME(TyApp([],["GObject.cptr"])))
        , ValDec(VarPat"repr", NONE, Var("GObject.repr"))
        , ValDec(VarPat"symb", NONE, Var("GtkBasis.symb"))
        , CommentDec NONE
        ]
end (* structure MosmlPrims *)

functor MLtonPrims(structure TypeInfo : TypeInfo) :> PRIMITIVES 
= struct
    structure TypeInfo = TypeInfo
    open TinySML SMLType
    val mkMethodName = Name.asCFunc
    fun ccall name args ty = Import(name, ty)
    fun getEnumsTy num =
	ArrowTy([TupTy(List.tabulate(num, fn _ => RefTy IntTy))], UnitTy)
    fun getEnums enum consts =
	let val fresh = List.tabulate(List.length consts, fn i => "x"^Int.toString i)
	    fun refe e = App(Long(Name.fromString "ref"), [e])
	    fun deref e = App(Var("!"), [e])
	in  Let(ValDec(TupPat(List.map VarPat fresh), NONE, 
			Tup(List.map (fn _ => refe(Const "0")) fresh)),
		SeqExp [ App(Var("get_"^enum^"_"), [Tup(List.map Var fresh)])
		       , Tup(List.map (deref o Var) fresh)
                       ]
		)
	end
    val unWrap = fn (t,e) => e
    local open TinySML in  
        fun toPrimString e = App(Var("CString.fromString"), [e])
    end

    fun mkToFunc () = 
	App(Var"inherit", 
	    [Unit,Fn("()",App(Var"GObject.withPtr",
			      [Tup[Var"obj",Fn("obj",Var"obj")]]))])

    fun callStub tinfo name ret pars = 
	let val fresh = ref 0
	    val next = fn () => (!fresh before fresh := !fresh + 1)
	    fun mk (Var v) = v
	      | mk e = "v"^Int.toString(next())
	    fun unwrapper (Type.WithDefault _) = "GObject.withOpt"
	      | unwrapper (Type.Ptr ty)        = unwrapper ty
	      | unwrapper _                    = "GObject.withPtr"
	    fun f ((Var "GObject.null",_), c) = c (* hack to avoid unwrapping with withPtr *)
	      | f ((e,ty), c) =
		if TypeInfo.isWrapped tinfo ty
		then let val v = mk e
		     in  App(Var(unwrapper ty), [Tup[e, Fn(v, c)]])
		     end
		else c
	    fun g (e, Type.WithDefault(ty,def)) = 
		if TypeInfo.isWrapped tinfo ty
		then (* already handled by withOpt above *) e
		else (if TypeInfo.isString tinfo ty
		      then toPrimString
		      else fn e => e) 
		     (App(Var"getOpt", [Tup[e, def]]))
	      | g (e, ty) = 
		if TypeInfo.isString tinfo ty
		then toPrimString e
		else e
	    val stub = App(Var(name^"_"), [Tup(List.map g pars)])
	    val call = if TypeInfo.isString tinfo ret
		       then Let(ValDec(VarPat"t",NONE,stub),
				App(Var"CString.toString",[Var"t"]))
		       else stub
	in  List.foldr f call pars
	end

    val strHeader = 
        [
          TypeDec(([],["cptr"]), SOME(TyApp([],["GObject.cptr"])))
	]
end (* structure MLtonPrims *)

functor GenSML(structure Prims : PRIMITIVES)
	: GEN_SML where type typeinfo = Prims.TypeInfo.typeinfo =
struct

    (* convenience *)
    open TinySML
    structure TypeInfo = Prims.TypeInfo
    structure P = Prims
    structure TI = TypeInfo

    infix ++
    fun d1 ++ d2 = SeqDec[d1,d2]

    infix **
    fun s1 ** s2 = SeqSpec[s1,s2]

    open SMLType
    infix --> ==>
    fun ty1 --> ty2 = ArrowTy([ty1], ty2)
    fun tys ==> ty2 = ArrowTy(tys, ty2)

    type typeinfo = TypeInfo.typeinfo

    fun print preamble sep_struct os (TinySML.StrDec(strid,sigopt,decs)) =
	let open Pretty
	    val device = plainOutput ( "(*" , "*)" )
	    fun ppPreamble (SOME file) =
		let val is = TextIO.openIn file
		    fun chop l = 
			let val n = String.size l
			in  if String.sub(l,n-1) = #"\n" 
			    then String.extract(l,0,SOME(n-1))
			    else l
			end
		    fun loop acc = 
			if TextIO.endOfStream is then rev acc
			else loop (chop(TextIO.inputLine is) :: acc)
		in  List.map ppString (loop []) 
	        before
		    TextIO.closeIn is
		end
	      | ppPreamble NONE = [empty]
	    val preamble = ppPreamble preamble
	    val body = clist "#" (forcemulti o TinySML.ppTopDec) decs
	    val sigcons = case sigopt of 
			      NONE => empty
			    | SOME sigexp => ":> " ^+ TinySML.ppSigExp sigexp
	    val res =
		if sep_struct then
		    close(1,"end")
		      (break(1,4)( ("structure " ^+ ppString strid ++ sigcons) +^ " = struct"
                                 , clist "#" forcemulti 
				      (preamble @ (List.map ppTopDec decs)) )
                      )
		else
		    clist "#" forcemulti
		       (preamble @ (List.map ppTopDec decs))
	in  ppPrint res device os
	end
      | print preamble sep_struct os _ = raise Fail("print: not a strdec")

    (* code generation *)
    type name = Name.name
    type ty = (name,name) Type.ty

    fun transCValue tinfo (ty, value) =
	let val v = Name.toString value (* FIXME: trim the string here *)
	in  if String.size v = 0 then raise Fail("Unrecognized C value: "^Name.toString' value)
	    else if P.TypeInfo.isString tinfo ty andalso v = "NULL" 
	         then Const "\"\"" (* converted to a primitive string in
                                      callStub *)
	    else if v = "NULL" then Var("GObject.null")
	    else if v = "TRUE" then Const("true")
	    else if v = "FALSE" then Const("false")
	    else if String.sub(v,0) = #"-" 
	    then Const("~"^String.extract(v,1,NONE))
	    else Const v
	end

    exception Skip of string
    fun trans tinfo (name,member) =
	case member of
            (* METHODS 
 
               Prototypical declaration

                   val new_with_label_: string -> cptr
                       = app1(symb"mgtk_gtk_button_new_with_label")
                   val new_with_label: string -> base t
                       = fn label => makeBut(new_with_label_ label)
            *)
	    AST.Method ty => 
		let val cname = P.mkMethodName name
		    val name = Name.asMethod name
		    val parsty = Type.getParams ty
		    val (pars,tys) = ListPair.unzip parsty
		    val ret     = Type.getRetType ty
		    fun ubnd n =
			raise Skip("Unbound type name: "^Name.toString' n)

                    (* convenience *)
		    val parFn = fn f => f tinfo o #2
		    val isOut = TI.isOutput (fn _ => true)
		    val isOutOut = TI.isOutput (fn Type.OUT => true
						 | Type.INOUT => false)

                    (* parameter list helpers *)
		    fun var (par,ty) = (Var par, ty)
		    fun prim (par,ty) = 
			(TypeInfo.toPrimValue tinfo ty par, ty)
			handle TypeInfo.Unbound n => ubnd n
		    fun trans (par, ty) = 
			(par, Type.mapiv (fn (ty,n)=>n) (transCValue tinfo) ty)
		    fun default (_, t as Type.WithDefault(ty,v)) = 
			(transCValue tinfo (ty,v), ty)
		                      (* the second ty above is correct here to
				         avoid extra calls to withPtr *)
		      | default (p, ty) = prim (p, ty)

                    (* type-based helpers *)
		    val seqfrom = TypeInfo.toSMLTypeSeq tinfo
		    val fromtype = fn ty => seqfrom ty 
				      handle TypeInfo.Unbound n => ubnd n
		    val fromtypeclosed = 
			fn ty => TypeInfo.toSMLType tinfo (fn _ => "base") ty
			   handle TypeInfo.Unbound n => ubnd n
		    val primfrom = fn ty => TypeInfo.toPrimType tinfo ty
				      handle TypeInfo.Unbound n => ubnd n
		    fun fromprim ty e =
			TypeInfo.fromPrimValue tinfo ty e
			handle TypeInfo.Unbound n => ubnd n

                    (* generate code that handles output parameters 
                       - returns a "wrapped" stub call and the list
                         of parameters to be used for the exported
                         version of the function *)
		    fun mkOutputStub with_outputs ret call =
			if with_outputs then
			    let val outputs = List.filter (parFn isOut) parsty

				val non = List.filter (not o parFn isOutOut) parsty
				fun refe e = App(Long(Name.fromString "ref"), [e])
				fun deref e = App(Var("!"), [e])
				fun inout (v,t) =
				    if TypeInfo.isOutput (fn Type.INOUT => true | Type.OUT => false) tinfo t
				    then refe (Var v)
				    else refe (Const "0")
				val ret = (case ret of Type.Void => []
						     | ty => [ty])
					  @ (List.map #2 outputs)
			    in  (Let(ValDec(TupPat(List.map (VarPat o #1) outputs), NONE, 
					     Tup(List.map inout outputs)),
				     SeqExp[ call
					   , Tup(List.map (deref o Var o #1) outputs)
					   ]
				     ),
				 non, ret)
			    end
			else 
			    (call, parsty, [ret])

		    val with_defaults =	List.exists (parFn TI.isDefault) parsty
		    val with_outputs =  List.exists (parFn isOut)        parsty

		    val stub = P.callStub tinfo name ret
			                  (map (trans o prim o var) parsty)

		    val (stub, outparsty, outret) = 
			mkOutputStub with_outputs ret stub

		    val params_ml = List.map (fromtype o #2) outparsty
		    val ret_ml    = SMLType.mkTuple(List.map fromtypeclosed outret)
		    val ty_ml     = SMLType.ArrowTy(params_ml, ret_ml)

		    val primty = primfrom ty

		    val fromtype' = TypeInfo.toSMLTypeSeq tinfo
		    fun fromtype ty = 
			fromtype' ty handle TypeInfo.Unbound n => ubnd n

		    val defpars = List.map (trans o default o var) parsty
		    val defparams = 
			List.map (fromtype o #2) 
				 (List.filter (not o (TypeInfo.isDefault tinfo) o #2) parsty)
		    val defparams = if List.length defparams = 0 then
					[SMLType.UnitTy]
				    else defparams
		    val defpars' = List.map #1 (List.filter (not o (TypeInfo.isDefault tinfo) o #2) parsty)
		    val defpars' = if List.length defpars' = 0 then
				        ["dummy"]
				   else defpars'
		    val (stubcall',_,_) = mkOutputStub with_outputs ret (P.callStub tinfo name ret defpars)
		    val ty'_ml = SMLType.ArrowTy(defparams, ret_ml)
		in  {stru = 
                       ValDec(VarPat(name^"_"), SOME primty,
			       P.ccall cname (List.length parsty) primty)
                 ++    ValDec(VarPat name, SOME ty_ml,
			       List.foldr Fn (fromprim ret stub) 
					  (List.map #1 outparsty))
                 ++ (if with_defaults then 
		      ValDec(VarPat(name^"'"), SOME ty'_ml,
			     List.foldr Fn (fromprim ret stubcall') defpars')
		     else EmptyDec),
		     sign = ValSpec(VarPat name, ty_ml) 
		         ** (if with_defaults then
				 ValSpec(VarPat(name^"'"), ty'_ml)
			     else EmptySpec)
                    }
		end

            (* ENUMS 
 
               Prototypical declaration

                  type gdk_image_type = int
                  val get_gdk_image_type_: unit -> int * int * int
                      = app1(symb"mgtk_get_gdk_image_type")
                  val (GDK_IMAGE_NORMAL,GDK_IMAGE_SHARED,GDK_IMAGE_FASTEST)
                      = get_gdk_image_type_ ()
            *)
	  | AST.Enum(flag,consts) => 
	        let val cname = Name.asCEnum name
		    val name = Name.asEnum name
		    val primty = P.getEnumsTy (List.length consts)
		    val specs = List.map (fn c => ValSpec(VarPat(Name.asEnumConst c),TyApp([],[name]))) consts
		in  {stru=SeqDec(
	            TypeDec(([],[name]), SOME IntTy)
                 :: 
                  [ ValDec(VarPat("get_" ^ name ^ "_"), SOME primty,
			   P.ccall ("mgtk_get_"^cname) 1 primty)
		  , ValDec(TupPat(List.map (VarPat o Name.asEnumConst) consts), NONE,
			   P.getEnums name consts)
                  ]),
		     sign=SeqSpec(TypeSpec(([],[name]), NONE) :: specs)
                    }
		end

	  | AST.Boxed funcs =>
	        let val name = Name.asBoxed name
		in  {stru=TypeDec(([],[name]), SOME(TyApp([],["GObject.cptr"]))),
		     sign=TypeSpec(([],[name]),NONE)
                    }
		end
          | AST.Field ty => 
		let val name = Name.asField name
		in  {stru=ValDec(VarPat ("get_"^name), NONE, 
				 P.ccall ("mgtk_get_"^name) 1 IntTy),
		     sign=EmptySpec}
		end

(*
   Generate in the structure:

     local open Signal infix --> in
     fun delete_event_sig f = 
         signal "delete_event" true (unit --> return_bool) f
     fun destroy_sig f = 
         signal "destroy" false (void --> return_void) f
     end

   In the signature

     val delete_event_sig : (unit -> bool) -> 'a t Signal.signal
     val destroy_sig      : (unit -> unit) -> 'a t Signal.signal

*)
	  | AST.Signal ty =>
	        let val name = Name.asSignal name
		    fun toTypeExp (SMLType.ArrowTy([ty],ret)) =
			(* signal types have this particular shape *)
			let fun loop (SMLType.ArrowTy(args,ret)) =
			       Util.stringSep "" "" " --> " loop args
			       ^ " --> " ^ "return_" ^ loop ret
			      | loop (SMLType.TupTy _) = Util.abort 452346
			      | loop (SMLType.RefTy _) = Util.abort 452348
			      | loop (SMLType.TyApp([],["void"])) = "void"
			      | loop ty = SMLType.toString ty
			in  loop ty end
		      | toTypeExp _ = Util.abort 443535
		    val ty = TypeInfo.toSignalType tinfo ty
		    val args = [Str name, Const "false", 
				Const("("^toTypeExp ty^")"), Var "f"]
		    fun toUnderscore #"-" = #"_"
		      | toUnderscore ch = ch
		    val name' = (String.map toUnderscore name)^"_sig"
		in  {stru=LocalDec(OpenDec ["Signal"] ++ 
					   InfixDec(SOME Right, ["-->"]),
				   ValDec(VarPat(name'), SOME ty, 
					  Fn("f", App(Var("signal"), args)))),
		     sign=ValSpec(VarPat(name'), ty)}
		end

    fun header tinfo (name, info) =
	let val (typ,parent,impl) = 
		case info of
		    SOME (n,parent,impl) => 
		       (Name.asType n,
			case parent of SOME(p) => p
				     (* assume that objects without a parent
				        derives from GObject *)
		                     | _ => Name.fromString "GObject",
			impl
                       )
		  | _ => raise Skip("No type information")

	    val base_t = "base"
	    val witness_t = typ^"_t"
	    val parent_t = Name.asModule parent ^ ".t"

	    val type_t = "t"
	    fun pRef id = Name.asModule parent ^ "." ^ id (* FIXME: Using names instead *)

	    fun f (i,a) = TyApp([a], [Name.asModule i ^ ".t"])
	    val path = List.foldl f (TyApp([TyVar "'a"], [witness_t])) impl
	in  {stru=SeqDec(P.strHeader)
		  ++ TypeDec(([],[base_t]),SOME UnitTy)
		  ++ TypeDec((["'a"],[witness_t]),SOME UnitTy)
		  ++ TypeDec((["'a"],[type_t]), SOME(TyApp([path],[parent_t])))
(*
   fun inherit w con = 
       let val con = let val ptr = con () in fn () => ptr end
           val editableWitness = Editable.inherit () con
           val cellEditableWitness = CellEditable.inherit editableWitness con
       in  Widget.inherit cellEditableWitness con
       end
*)
         ++ FunDec("inherit",[VarPat "w",VarPat "con"],
		    (* 'a -> GObject.constructor -> 'a t *)
		    NONE,
		    Let(SeqDec (
			   ValDec(VarPat "con", NONE, Let(ValDec(VarPat "ptr",NONE,App(Var "con",[Unit])),Fn("()",Var"ptr")))
                        :: ValDec(VarPat "witness", NONE, Unit)
			:: List.map (fn i => ValDec(VarPat "witness", NONE, App(Var(Name.asModule i ^ ".inherit"), [Var "witness", Var "con"]))) impl
                        ),
			App(Var(pRef "inherit"),[Var "witness",Var "con"])))
	 ++ FunDec("make"(*^Name.asModule name*),[VarPat"ptr"],NONE,
		    App(Var("inherit"),[Unit,Fn("()",Var"ptr")]))
	 ++ FunDec("to"^Name.asModule name, [VarPat "obj"],
	         SOME(TyApp([TyVar "'a"],["t"]) --> TyApp([TyVar "base"],["t"])),
		 P.mkToFunc ())
         ++ CommentDec NONE,
	     sign=  TypeSpec(([],[base_t]),NONE)
                 ** TypeSpec((["'a"],[witness_t]),NONE)
                 ** TypeSpec((["'a"],[type_t]), SOME(TyApp([path],[parent_t])))
                 ** FunSpec("inherit", 
		      [TyVar "'a", TyApp([],["GObject","constructor"])] ==> TyApp([TyVar"'a"],["t"]))
                 ** FunSpec("to"^Name.asModule name, TyApp([TyVar "'a"],["t"]) --> TyApp([TyVar "base"],["t"]))
             }
	end
	    handle Skip msg => {stru=EmptyDec,sign=EmptySpec}

    local 
	open AST TinySML
	val trans = fn tinfo => fn (name,member) => trans tinfo (name,member)
		   handle Skip msg => ( TextIO.output(TextIO.stdErr,
		       "Error translating " ^ Name.toString name ^ ": " ^msg^"\n")
                     ; {stru=EmptyDec,sign=EmptySpec} )

	fun decUnzip ls = 
	    let fun loop [] (a1,a2) = {stru=List.concat(rev a1), sign=SeqSpec(rev a2)}
		  | loop ({stru,sign}::ds) (a1,a2) = 
		    loop ds (stru::a1,sign::a2)
	    in  loop ls ([],[]) end

	fun transMod named_sigs tinfo (Module{name,members,info}) =
	    let val {stru=headstr,sign=headsig} = header tinfo (name,info)
		val name = Name.asModule name
		val contents = List.map (transMem named_sigs tinfo) members
		val {stru=contstr,sign=contsig} = decUnzip contents
		val sigexp = SigBasic(SeqSpec[headsig,contsig])
		val decs = CoreDec(headstr) :: contstr
	    in  
		if named_sigs then
		    { stru = SigDec(name, sigexp) ::
			     StrDec(name, SOME(SigId name), decs) ::
			     [] ,
		      sign = EmptySpec }
		else
		    { stru = [ StrDec(name, SOME(sigexp), decs) ],
		      sign = EmptySpec }
	    end
	and transMem named_sigs tinfo (Sub module) = 
	    transMod named_sigs tinfo module
	  | transMem named_sigs tinfo (Member{name,info}) =
	    let val {stru,sign} = trans tinfo (name,info)
	    in  { stru = [ CoreDec(stru) ], sign = sign } end

    in  fun translate named_sigs tinfo (Module{name,members,info}) =
	    let val name = Name.asModule name
		val head = CoreDec(SeqDec P.strHeader)
		val contents = List.map (transMem named_sigs tinfo) members
		val {stru=contstr,sign=contsig} = decUnzip contents
	    in  StrDec(name, NONE, head :: contstr)
	    end
    end

end (* structure GenSML *)
