(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

signature PRIMITIVES = sig
    structure TypeInfo : TypeInfo
    val mkMethodName : Name.name -> string
    val ccall : string -> int -> SMLType.ty -> TinySML.exp
    val getEnumsTy : int -> SMLType.ty
    val getEnums : string -> Name.name list -> TinySML.exp
    val unWrap: (Name.name, Name.name) Type.ty * TinySML.exp -> TinySML.exp
    val toPrimString : TinySML.exp -> TinySML.exp
    val callStub : TypeInfo.typeinfo -> string  -> (Name.name, Name.name) Type.ty
		-> (TinySML.exp * (Name.name, Name.name) Type.ty) list -> TinySML.exp
    val strHeader : TinySML.decl list
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

    fun unWrap (Type.WithDefault(ty,v),e) = App(Var"Option.map",[Var "repr", e])
      | unWrap (t,e) = App(Var "repr", [e])
    val toPrimString = fn e => e

    fun callStub tinfo name ret pars = 
	App(Var(name^"_"),
	    if List.length pars > max_curried
	    then [Tup(List.map #1 pars)]
	    else List.map #1 pars
           )

    val strHeader =
        [ Open["Dynlib"]
        , TypeDecl(([],["cptr"]), Some(TyApp([],["GObject.cptr"])))
        , ValDecl(VarPat"repr", None, Var("GObject.repr"))
        , ValDecl(VarPat"symb", None, Var("GtkBasis.symb"))
        , Comment NONE
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
	in  Let(ValDecl(TupPat(List.map VarPat fresh), None, 
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

    fun callStub tinfo name ret pars = 
	let fun f ((Var v,t), c) =
		if TypeInfo.isWrapped tinfo t
		then App(Var"GObject.withPtr", [Tup[Var v, Fn(v, c)]])
		else c
	      | f ((Const v,t), c) = c
	      | f ((e,t), c) = c
		(* FIXME:
		if TypeInfo.isWrapped tinfo t then raise Fail("oops")
		else c
                *)
	    val stub = App(Var(name^"_"), [Tup(List.map #1 pars)])
	    val call = if TypeInfo.isString tinfo ret
		       then Let(ValDecl(VarPat"t",None,stub),
				App(Var"CString.toString",[Var"t"]))
		       else stub
	in  List.foldr f call pars
	end

    val strHeader = 
        [
          TypeDecl(([],["cptr"]), Some(TyApp([],["GObject.cptr"])))
	]
end (* structure MLtonPrims *)

functor GenSML(structure Prims : PRIMITIVES)
	: GEN_SML where type typeinfo = Prims.TypeInfo.typeinfo =
struct

    (* convenience *)
    open TinySML
    structure TypeInfo = Prims.TypeInfo

    infix ++
    fun d1 ++ d2 = Some(SeqDecl[d1,d2])

    open SMLType
    infix --> ==>
    fun ty1 --> ty2 = ArrowTy([ty1], ty2)
    fun tys ==> ty2 = ArrowTy(tys, ty2)

    type typeinfo = TypeInfo.typeinfo
    type 'a incl = 'a TinySML.incl
    type sml_info = TinySML.decl

    datatype module_info = STRUCTURE of string * string option (* sig? *)
			 | SIGNATURE of string
    fun isStrMode (STRUCTURE _) = true
      | isStrMode _ = false
    fun isSigMode (SIGNATURE _) = true
      | isSigMode _ = false
    fun showModInfo (STRUCTURE(name,sign)) = 
	"structure " ^ name ^ (case sign of SOME s => " :> " ^s | NONE => "")
      | showModInfo (SIGNATURE name) = "signature " ^ name
    fun showModBegin (STRUCTURE _) = "struct"
      | showModBegin (SIGNATURE _) = "sig"

    val toString = TinySML.toString (isStrMode, isSigMode)	    
    fun print preamble sep_struct os module =
	let fun dump s = TextIO.output(os, s)
	    fun spaces n = String.implode(List.tabulate(n,fn _ => #" "))
	    fun dump_preamble indent (SOME file) =
		let val is = TextIO.openIn file
		    val indent = spaces indent
		    fun loop () = 
			if TextIO.endOfStream is then ()
			else ( dump (indent ^ TextIO.inputLine is)
                             ; loop() )
		in  loop () ; TextIO.closeIn is end
	      | dump_preamble indent NONE = ()
	    fun indent cols str = spaces cols ^ str
	    fun print_member mode indent (AST.Member{name,info}) = 
		dump (toString mode (spaces indent) info)
	      | print_member mode indent (AST.Sub module) = 
		print_module indent module
	    and print_module indent (AST.Module{name,members,info}) =
		( dump(spaces indent ^ showModInfo info ^ " = " ^ showModBegin info ^ "\n")
		; List.app (print_member info (indent+4)) members
		; dump(spaces indent ^ "end\n")
		)
	    fun print_toplevel (AST.Module{name,members,info}) =
		if sep_struct then
		    ( dump(spaces 0 ^ showModInfo info ^ " = " ^ 
			   showModBegin info ^ "\n")
                    ;  dump_preamble 4 preamble
                    ; List.app (print_member info 4) members
                    ; dump("end\n")
                    )
		else
		    ( dump_preamble 0 preamble
                    ; List.app (print_member info 0) members
                    )
	in  print_toplevel module
	end

    (* code generation *)
    type name = Name.name
    type ty = (name,name) Type.ty

    fun transCValue tinfo (ty, value) =
	let val v = Name.toString value (* FIXME: trim the string here *)
	in  if String.size v = 0 then raise Fail("Unrecognized C value: "^Name.toString' value)
	    else if Prims.TypeInfo.isString tinfo ty andalso v = "NULL" 
	         then Prims.toPrimString(Const "\"\"")
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
		let val cname = Prims.mkMethodName name
		    val name = Name.asMethod name
		    val parsty = Type.getParams ty
		    val (pars,tys) = ListPair.unzip parsty
		    fun ubnd n =
			raise Skip("Unbound type name: "^Name.toString' n)
		    fun isWidgetType (Type.Tname n) = (* FIXME *)
			(case Name.getFullPath n of "Gtk"::_ => true | _ => false)
		      | isWidgetType (Type.Ptr t) = isWidgetType t
		      | isWidgetType _ = false
		    fun var (par,ty) = (Var par, ty)
		    fun default (par,Type.WithDefault(ty, v)) = 
			(App(Var"getOpt", [Tup[par, transCValue tinfo (ty,v)]]), ty)
		      | default (par,ty) = (par, ty)
		    fun wrap (par,ty) =
			(if TypeInfo.isWrapped tinfo ty
			 then (Prims.unWrap (ty,par),ty)
			 else if TypeInfo.isString tinfo ty
			 then (Prims.toPrimString (par),ty)
			 else (par, ty))
			handle TypeInfo.Unbound n => ubnd n
		    val pars' = List.map (default o wrap o var) parsty
		    val fromtype' = TypeInfo.toSMLTypeSeq tinfo
		    fun fromtype ty = 
			fromtype' ty handle TypeInfo.Unbound n => ubnd n
		    fun primtypeFromType ty = 
			TypeInfo.toPrimType tinfo ty
			handle TypeInfo.Unbound n => ubnd n
		    val params = Type.getParams ty
		    val ret    = Type.getRetType ty
		    val params' = List.map (fromtype o #2) params
		    val ret'    = TypeInfo.toSMLType tinfo (fn _ => "base") ret
				  handle TypeInfo.Unbound n => ubnd n
		    fun isDefault (p, Type.WithDefault _) = true
		      | isDefault _ = false
		    val with_defaults = List.exists isDefault parsty
		    fun fromprim ty e =
			TypeInfo.fromPrimValue tinfo ty e
			handle TypeInfo.Unbound n => ubnd n
		    val primty = primtypeFromType ty
		    val stubcall = Prims.callStub tinfo name ret pars'
		    val fromtype' = TypeInfo.toSMLTypeSeq tinfo
		    fun fromtype ty = 
			fromtype' ty handle TypeInfo.Unbound n => ubnd n

		    fun default (_, Type.WithDefault(ty,v)) = 
			(transCValue tinfo (ty,v), ty)
		      | default (p, ty) = wrap (Var p, ty)
		    val defpars = List.map default parsty
		    val defparams = 
			List.map (fromtype o #2) 
				 (List.filter (not o isDefault) params)
		    val defparams = if List.length defparams = 0 then
					[SMLType.UnitTy]
				    else defparams
		    val defpars' = List.map #1 (List.filter (not o isDefault) params)
		    val defpars' = if List.length defpars' = 0 then
				        ["dummy"]
				   else defpars'
		    val stubcall' = Prims.callStub tinfo name ret defpars
		in  StrOnly(
                       ValDecl(VarPat(name^"_"), Some primty,
			       Prims.ccall cname (List.length pars) primty))
                 ++ Some(
                       ValDecl(VarPat name, Some(SMLType.ArrowTy(params',ret')),
			       List.foldr Fn (fromprim ret stubcall) pars))
                 ++ (if with_defaults then 
		      Some(
			ValDecl(VarPat(name^"'"), 
				Some(SMLType.ArrowTy(defparams, ret')),
				List.foldr Fn (fromprim ret stubcall') defpars'))
		     else None)
		end

            (* ENUMS 
 
               Prototypical declaration

                  type gdk_image_type = int
                  val get_gdk_image_type_: unit -> int * int * int
                      = app1(symb"mgtk_get_gdk_image_type")
                  val (GDK_IMAGE_NORMAL,GDK_IMAGE_SHARED,GDK_IMAGE_FASTEST)
                      = get_gdk_image_type_ ()
            *)
	  | AST.Enum consts => 
	        let val cname = Name.asCEnum name
		    val name = Name.asEnum name
		    val primty = Prims.getEnumsTy (List.length consts)
		    val decs = List.map (fn c => SigOnly(ValDecl(VarPat(Name.asEnumConst c),Some(TyApp([],[name])),Const "1"))) consts
		in  Some(SeqDecl(
	            Some(TypeDecl(([],[name]), StrOnly IntTy))
                 :: decs
                @ [ StrOnly(ValDecl(VarPat("get_" ^ name ^ "_"), Some primty,
			    Prims.ccall ("mgtk_get_"^cname) 1 primty))
		  , StrOnly(ValDecl(TupPat(List.map (VarPat o Name.asEnumConst) consts), None,
			    Prims.getEnums name consts))
                  ]))
		end

	  | AST.Boxed funcs =>
	        let val name = Name.asBoxed name
		in  Some(TypeDecl(([],[name]), StrOnly(TyApp([],["GObject.cptr"]))))
		end
          | AST.Field ty => 
		let val name = Name.asField name
		in  StrOnly(ValDecl(VarPat ("get_"^name), None, 
			    Prims.ccall ("mgtk_get_"^name) 1 IntTy))
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
		    fun show ty =
			let open Type
			    fun loop Void = "void"
			      | loop (Func(args,ret)) = 
				   Util.stringSep "" "" " --> " (loop o #2) args
				   ^ " --> " ^ "return_" ^ loop ret
			      | loop (Base tn) = 
				   (case Name.asType tn of
					"uint" => "int"
				      | ty => ty
                                   )
			      | loop (Tname tn) = 
				   (* FIXME: ? *) "unit"
			      | loop (Ptr ty) = loop ty
			      | loop (Const ty) = loop ty
			      | loop (Arr(i, ty)) = raise Fail("signal (dyn): not impl for "^name)
			in  loop ty end
		    fun toSmlType ty =
			let open Type
			    fun loop Void = UnitTy
			      | loop (Ptr ty) = loop ty
			      | loop (Const ty) = loop ty
			      | loop (Func(args,ret)) = 
				   ArrowTy(List.map (loop o #2) args, loop ret)
			      | loop (Tname tn) = UnitTy
			      | loop (Base tn) =
				   (case Name.asType tn of
					"bool" => BoolTy
				      | "char" => CharTy
				      | "uint" => IntTy
				      | "int" => IntTy
				      | "double" => RealTy
				      | "float" => RealTy
				      | ty => raise Fail("signal: not impl:" ^ty ^ " for " ^name)
                                   )
			      | loop (Arr(i,ty)) = raise Fail("signal (stat): not impl for "^name)
			    val ret = TyApp([TyApp([TyVar "'a"], ["t"])], ["Signal","signal"])
			in  ArrowTy([loop ty], ret) end
		    val args = [Str name, Const "false", 
				Const("("^show ty^")"), Var "f"]
		    fun toUnderscore #"-" = #"_"
		      | toUnderscore ch = ch
		in  Some(Local(StrOnly(Open ["Signal"]) ++ StrOnly(Infix(SOME Right, ["-->"])),
			 Some(ValDecl(VarPat((String.map toUnderscore name)^"_sig"), 
				      Some(toSmlType ty), 
				      Fn("f", App(Var("signal"), args))))))
		end

    fun header tinfo (name, info) =
	let val (typ,parent,impl) = 
		case info of
		    SOME (n,parent,impl) => 
		       (Name.asType n,
			case parent of SOME(p) => p
		                     | _ => raise Skip("No parent"),
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
	in  Some(SeqDecl(List.map StrOnly Prims.strHeader))
         ++ Some(TypeDecl(([],[base_t]),StrOnly UnitTy))
         ++ Some(TypeDecl((["'a"],[witness_t]),StrOnly UnitTy))
         ++ Some(TypeDecl((["'a"],[type_t]), 
		    Some(TyApp([path],[parent_t]))))
(*
   fun inherit w con = 
       let val con = let val ptr = con () in fn () => ptr end
           val editableWitness = Editable.inherit () con
           val cellEditableWitness = CellEditable.inherit editableWitness con
       in  Widget.inherit cellEditableWitness con
       end
*)
         ++ Some(FunDecl("inherit",[VarPat "w",VarPat "con"],
		    (* 'a -> GObject.constructor -> 'a t *)
		    SigOnly([TyVar "'a", TyApp([],["GObject","constructor"])] ==> TyApp([TyVar"'a"],["t"])),
		    Let(SeqDecl (
			   Some(ValDecl(VarPat "con", None, Let(ValDecl(VarPat "ptr",None,App(Var "con",[Unit])),Fn("()",Var"ptr"))))
                        :: Some(ValDecl(VarPat "witness", None, Unit))
			:: List.map (fn i => Some(ValDecl(VarPat "witness", None, App(Var(Name.asModule i ^ ".inherit"), [Var "witness", Var "con"])))) impl
                        ),
			App(Var(pRef "inherit"),[Var "witness",Var "con"]))))
	 ++ StrOnly(FunDecl("make"(*^Name.asModule name*),[VarPat"ptr"],None,
		    App(Var("inherit"),[Unit,Fn("()",Var"ptr")])))
         ++ StrOnly(Comment NONE)
	end
	    handle Skip msg => None (* Was EmptyDecl *)


    local
	open AST 
	val trans = fn tinfo => fn (name,member) => trans tinfo (name,member)
		   handle Skip msg => ( TextIO.output(TextIO.stdErr,
		       "Error translating " ^ Name.toString name ^ ": " ^msg^"\n")
                     ; None)
	fun generate_module tinfo (Module{name,members,info}) = 
	    let val subs = List.concat(List.map (generate_member tinfo) members)
	    in  [ Module{name=name, info=SIGNATURE (Name.asModule name),
			 members=Member{name=Name.fromString "signatureheader",
					info=header tinfo (name,info)}
				 :: subs}
		, Module{name=name, info=STRUCTURE (Name.asModule name, SOME (Name.asModule name)),
			 members=Member{name=Name.fromString "structureheader",
					info=header tinfo (name,info)}
				 :: subs}
		]
	    end
	and generate_member tinfo (Sub module) = 
	    List.map Sub (generate_module tinfo module)
	  | generate_member tinfo (Member{name,info}) = 
	    [Member{name=name,info=trans tinfo (name,info)}]
    in  fun generate tinfo (Module{name,members,info}) = 
	    let fun f d = Member{name = Name.fromString "structureheader",
				 info = StrOnly d}
	    in  Module{name=name,info=STRUCTURE(Name.asModule name, NONE),
		       members=
		             List.map f Prims.strHeader
			   @ List.concat(List.map (generate_member tinfo) members)}
	    end
    end (* local *)

end (* structure GenSML *)
