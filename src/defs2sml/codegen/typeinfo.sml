(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

signature PRIMTYPES = sig
    type ty = SMLType.ty
    val mkArrowTy : ty list * ty -> ty
    val stringTy : bool -> ty
    val unwrap : string option
end (* signature PRIMTYPES *)

structure MosmlPrimTypes : PRIMTYPES = struct
    type ty = SMLType.ty
    fun mkArrowTy(pars,ret) =
	SMLType.ArrowTy(
	   if List.length pars > TinySML.max_curried
	   then [SMLType.TupTy pars]
	   else pars
        ,  ret)
    val stringTy = fn _ => SMLType.StringTy
    val unwrap = SOME "repr"
end (* structure MosmlPrimTypes *)

structure MLtonPrimTypes : PRIMTYPES = struct
    type ty = SMLType.ty
    fun mkArrowTy(pars,ret) =
	SMLType.ArrowTy([SMLType.TupTy pars], ret)
    val stringTy = fn neg =>
		      if neg then SMLType.TyApp([],["CString", "t"])
		      else SMLType.TyApp([],["CString", "cstring"])
    val unwrap = NONE
end (* structure MosmlPrimTypes *)

functor TypeInfo(structure Prim : PRIMTYPES) :> TypeInfo = struct
    
    type name = Name.name
    type 'a ty = (name,'a) Type.ty

    (* build a table mapping type names to
     *    SML types and SML primitive types
     *  & functions for converting to and from appropriate C values
     *  & a possible super type
     *)

    datatype kind = Base | Object | Boxed | Enum | Flag

    type info = {kind : kind,
		 stype: (unit -> string) -> SMLType.ty, ptype: SMLType.ty,
		 toc: string (* which function to call *),
		 fromc: string (* which function to call *),
		 ctype: TinyC.ctype,
		 super: name option,
		 toprim: string option (* which function to call *),
		 fromprim: TinySML.exp -> TinySML.exp,
		 wrapped: bool,
		 default: TinySML.exp
		}
    type typeinfo = (Name.name, info) Splaymap.dict
    exception NotFound = Splaymap.NotFound
    exception Unbound of Name.name
    val add = Splaymap.insert
    fun lookup table name = Splaymap.find(table,name)
			    handle NotFound => raise Unbound name

    fun id x = x
    fun make e = TinySML.App(TinySML.Var("make"), [e])
    fun ccall name = fn e => TinyC.Call(name,NONE,[e])

    fun addObject (table,name) =
	let 
	    val _  = MsgUtil.debug("Binding " ^ Name.toString' name)
	    val info = {kind = Object,
			toc="GtkObj_val",fromc="Val_GtkObj",
			ptype=SMLType.TyApp([],["cptr"]),
			fromprim = make, toprim = Prim.unwrap, wrapped = true,
			stype=fn fresh => SMLType.TyApp([SMLType.TyVar(fresh())],["t"]),
			super=NONE, 
			ctype = TinyC.TStar(TinyC.TTyName (Name.asCType name)),
			default = TinySML.Var"GObject.null"
		        }
	in  add(table,name,info)  end

    open TinySML
    type binfo
      = ((unit -> string) -> SMLType.ty) * SMLType.ty * string * string * TinyC.ctype * Name.name option * TinySML.exp
    val basic: (string * binfo) list
    =   [("int",       (fn _ => SMLType.IntTy,SMLType.IntTy,
		        "Int_val", "Val_int", TinyC.TInt, NONE, Const "0"))
        ,("uint",      (fn _ => SMLType.IntTy,SMLType.IntTy,
		        "Int_val", "Val_int", TinyC.TInt, NONE, Const "0"))
        ,("guint",     (fn _ => SMLType.IntTy,SMLType.IntTy,    (* FIXME *)
		        "Int_val", "Val_int", TinyC.TInt, NONE, Const "0"))
        ,("char",      (fn _ => SMLType.CharTy,SMLType.CharTy,
		        "Char_val", "Val_char", TinyC.TChar, NONE, Const "#\"\000\""))
        ,("gunichar",  (fn _ => SMLType.CharTy,SMLType.CharTy,
		        "Long_val", "Val_long", TinyC.TChar, NONE, Const "#\"\000\"")) (* FIXME *)
        ,("float",     (fn _ => SMLType.RealTy,SMLType.RealTy,
		        "Double_val", "copy_double", TinyC.TFloat, NONE, Const "0.0"))
        ,("double",    (fn _ => SMLType.RealTy,SMLType.RealTy,
		        "Double_val", "copy_double", TinyC.TDouble, NONE, Const "0.0"))
        ,("gdouble",   (fn _ => SMLType.RealTy,SMLType.RealTy,
		        "Double_val", "copy_double", TinyC.TDouble, NONE, Const "0.0"))
        ,("ptr",       (fn _ => SMLType.TyApp([],["cptr"]),SMLType.TyApp([],["cptr"]),         (* FIXME *)
		        "(gpointer)", "(value)", TinyC.TStar TinyC.TVoid, NONE, Var"GObject.null"))
        ,("bool",      (fn _ => SMLType.BoolTy,SMLType.BoolTy,
		        "Bool_val", "Val_bool", TinyC.TInt, NONE, Const "true"))
        ,("GType",     (fn _ => SMLType.IntTy,SMLType.IntTy,           (* FIXME *)
		        "Int_val", "Val_int", TinyC.TInt, NONE, Const"0"))
        ,("GtkType",   (fn _ => SMLType.IntTy,SMLType.IntTy,           (* FIXME *)
		        "Int_val", "Val_int", TinyC.TInt, NONE, Const"0"))
        ]

    fun init () = 
	let fun a ((n,i:binfo),t)=
		add(t,Name.fromPaths([],[],[n]),
		    {kind = Base, stype= #1 i,ptype= #2 i,toc= #3 i, 
		     wrapped = false, fromprim = id, toprim = NONE,
		     fromc= #4 i, ctype= #5 i, super= #6 i, default = #7 i})
	    val table = List.foldl a (Splaymap.mkDict Name.compare) basic
	in  addObject(table, Name.fromString "GObject")
	end

    fun build module =
	let fun bmod (AST.Module{name,members,info=SOME(n,parent,impl)},table) = 
		let val table' = List.foldl bmem table members
(*
		    val nb = Name.getBase name
		    val name' = Name.fromPaths(Name.getFullPath name@nb,
					       Name.getPath name,
					       nb)
*)
		in  addObject(table',name) end
	      | bmod (AST.Module{name,members,info=NONE},table) = 
		List.foldl bmem table members
	    and bmem (AST.Sub module,table) = bmod(module,table)
	      | bmem (AST.Member{name,info},table) =
		case info of
		    AST.Enum(false (* not a flag *), _) => 
		    (MsgUtil.debug("Binding " ^ Name.toString' name);
		        add(table,name,
			    {kind = Enum, toc="Int_val", fromc="Val_int",
			     ctype = TinyC.TTyName(Name.asCType name),
			     fromprim=id, toprim = NONE, wrapped = false,
			     ptype=SMLType.IntTy, super=NONE, default=Const"0",
			     stype=fn _ => SMLType.TyApp([],[Name.asEnum name])})
                    )
		  | AST.Enum(true (* a flag *), _) => 
		    (MsgUtil.debug("Binding " ^ Name.toString' name);
		        add(table,name,
			    {kind = Flag, toc="Int_val", fromc="Val_int",
			     ctype = TinyC.TTyName(Name.asCType name),
			     toprim= SOME"Flags.set",
			     fromprim=fn e => TinySML.App(TinySML.Var"Flags.get",[e]),
			     wrapped = false, default = Const "[]",
			     ptype=SMLType.IntTy, super=NONE,
			     stype=fn _ => 
				SMLType.TyApp([SMLType.TyApp([],[Name.asEnum name])],["list"])})
                    )
		  | AST.Boxed _ =>
		    (MsgUtil.debug("Binding " ^ Name.toString' name);
		        add(table,name,
			    {kind = Boxed, toc=(Name.asCBoxed name^"_val"), 
			     ctype = TinyC.TStar(TinyC.TTyName(Name.asCType name)),
			     fromc=("Val_"^Name.asCBoxed name), default = Var"GObject.null",
			     fromprim=id, toprim=NONE, wrapped = false,
			     ptype=SMLType.TyApp([],["cptr"]),super=NONE,
			     stype=fn _ => SMLType.TyApp([],[Name.asBoxed name])})
                    )
		  | _ => table
	in  bmod (module,init())
	end

    fun nextgen () =
	let val no = ref 0
	    val orda = Char.ord #"a"
	    fun next () = if(!no<26) then ("'"^Char.toString(Char.chr(!no+orda))
				           before
					   no := !no + 1)
			  else raise Fail("Not implemented: fromTypeSeq.next()")
	in  next
	end

(*
    fun pp table =
	let open Pretty
	    fun ppinfo {stype,ptype,toc,fromc,super,fromprim,wrapped} = 
		bracket "{#}" (
		   ppBinary(SMLType.pp ptype, "x", 
			    SMLType.pp (stype (nextgen())))
                )
	in  ppSplayMap Name.pp' ppinfo table end

    (* Debugging: *)
    val build = fn module =>
		   let val table = build module
		       val os = TextIO.openOut("typetable.txt")
		       val device = Pretty.plainOutput ("/*","*/")
		   in  Pretty.ppPrint (pp table) device os
                     ; TextIO.closeOut os
		     ; table
		   end
*)

    fun prependPath (path,base) ty =
	case ty of
	    SMLType.TyApp(alphas, tyname) =>
		SMLType.TyApp(alphas, path @ tyname)
	  | _ => ty

    fun toSMLType tinfo fresh ty =
	case ty of
	    (* recognize standard "patterns" *)
	    Type.Ptr(ty as Type.Base n) => 
	       if Name.asType n = "char" then SMLType.StringTy
	       else toSMLType tinfo fresh ty

            (* then the general stuff *)
	  | Type.Void => SMLType.UnitTy
	  | Type.WithDefault(ty, default) => 
               SMLType.TyApp([toSMLType tinfo fresh ty], ["option"])
	  | Type.Base n => 
	       let val info: info = lookup tinfo n
	       in  #stype info fresh end
	  | Type.Tname n => 
	       let val info: info = lookup tinfo n
	       in  prependPath (Name.getPath n, Name.getBase n) 
			       (#stype info fresh)
	       end
	  | Type.Const ty => toSMLType tinfo fresh ty
	  | Type.Ptr ty => toSMLType tinfo fresh ty
	  | Type.Func(pars,ret) => 
	       SMLType.ArrowTy(List.map (toSMLType tinfo fresh o #2) pars,
			       toSMLType tinfo fresh ret)
	  | Type.Output(pass,ty) => toSMLType tinfo fresh ty
	  | Type.Arr(len,ty) => 
	       raise Fail("Not implemented: toSMLType(arr)")
	       (* FIXME 
	       SMLType.TyVar(SMLType.toString(toSMLType tinfo fresh ty)^"["^
		     (case len of NONE => "" | SOME l => Int.toString l)^"]")
               *)

    fun toSMLTypeSeq tinfo = 
	let val no = ref 0
	    val orda = Char.ord #"a"
	    fun next () = if(!no<26) then ("'"^Char.toString(Char.chr(!no+orda))
				           before
					   no := !no + 1)
			  else raise Fail("Not implemented: fromTypeSeq.next()")
	in  toSMLType tinfo next
	end

    fun toPrimType negative tinfo ty = (* FIXME negative: ugly, but it works *)
	case ty of 
	    Type.Ptr(Type.Base n) => 
	       if Name.asType n = "char" then Prim.stringTy negative
	       else SMLType.TyApp([],["cptr"])
	  | Type.Void => SMLType.UnitTy
	  | Type.WithDefault(ty,default) => toPrimType negative tinfo ty
	  | Type.Base n => 
	       let val info: info = lookup tinfo n
	       in  #ptype info end
	  | Type.Tname n => 
	       let val info: info = lookup tinfo n
	       in  #ptype info end
	  | Type.Const ty => toPrimType negative tinfo ty
	  | Type.Output(pass, ty) => SMLType.RefTy(toPrimType negative tinfo ty)
	  | Type.Ptr ty => SMLType.TyApp([],["cptr"])
	  | Type.Func(pars,ret) => 
	       Prim.mkArrowTy(List.map (toPrimType negative tinfo o #2) pars,
			      toPrimType (not negative) tinfo ret)
	  | Type.Arr(len,ty) => SMLType.TyApp([],["..."]) (* FIXME *)
    val toPrimType = fn tinfo => fn ty => toPrimType false tinfo ty

    local open TinySML in
    fun prependPath n exp = (* FIXME *)
	case exp of
	    App(Var "make", [arg]) =>
	       let val p = Name.getPath n
	       in  if List.length p > 0
		   then App(Long(Name.fromPaths(p,p,["inherit"])), 
			    [Unit,Fn("()", arg)])
		   else exp
	       end
	  | exp => exp
    end (* local *)

    fun toprimvalue tinfo ty =
	case ty of
	    Type.Base n =>
	       let val info: info = lookup tinfo n
	       in  #toprim info end
	  | Type.Tname n =>
	       let val info: info = lookup tinfo n
	       in  #toprim info end
	  | Type.WithDefault(ty,default) => toprimvalue tinfo ty
	  | Type.Ptr ty => toprimvalue tinfo ty (* FIXME: ? *)
	  | Type.Const ty => toprimvalue tinfo ty
	  | Type.Output(pass,ty) => toprimvalue tinfo ty
	  | _ => NONE
    local open TinySML in
    fun toPrimValue tinfo ty =
	case ty of
	    Type.WithDefault(ty,default) =>
	       (case toprimvalue tinfo ty of
		    NONE => id
		  | SOME f => fn e => App(Var"Option.map", [Var f, e])
               )
	  | Type.Output(pass,ty) => id
	  | ty =>
	       (case toprimvalue tinfo ty of
		    NONE => id
		  | SOME f => fn e => App(Var f, [e])
               )
    end (* local *)
    fun fromPrimValue tinfo ty =
	case ty of
	    Type.Base n =>
	       let val info: info = lookup tinfo n
	       in  #fromprim info end
	  | Type.Tname n =>
	       let val info: info = lookup tinfo n
	       in  (prependPath n) o #fromprim info end
	  | Type.WithDefault(ty,default) => fromPrimValue tinfo ty
	  | Type.Ptr ty => fromPrimValue tinfo ty (* FIXME: ? *)
	  | Type.Const ty => fromPrimValue tinfo ty
	  | Type.Output(pass, ty) => fromPrimValue tinfo ty
	  | _ => id

    fun isWrapped tinfo ty =
	case ty of
	    Type.Base n =>
	       let val info: info = lookup tinfo n
	       in  #wrapped info end
	  | Type.Tname n =>
	       let val info: info = lookup tinfo n
	       in  #wrapped info end
	  | Type.WithDefault(ty,default) => isWrapped tinfo ty
	  | Type.Ptr ty => isWrapped tinfo ty
	  | Type.Output(pass,ty) => isWrapped tinfo ty
	  | _ => false

    fun isDefault tinfo ty =
	case ty of
	    Type.Ptr ty => isDefault tinfo ty
	  | Type.Const ty => isDefault tinfo ty
	  | Type.Output(pass,ty) => isDefault tinfo ty
	  | Type.WithDefault(ty,default) => true
	  | _ => false

    fun isOutput isPass tinfo ty =
	case ty of
	    Type.Ptr ty => isOutput isPass tinfo ty
	  | Type.Const ty => isOutput isPass tinfo ty
	  | Type.Output(p,ty) => isPass p
	  | Type.WithDefault(ty,default) => isOutput isPass tinfo ty
	  | _ => false

    fun isString tinfo ty =
	case ty of
	    Type.Ptr(ty as Type.Base n) =>
               Name.asType n = "char"
	  | Type.Const ty => isString tinfo ty
	  | Type.Output(pass,ty) => isString tinfo ty
	  | Type.WithDefault(ty,default) => isString tinfo ty
	  | _ => false

    fun toCType tinfo ty = 
	case ty of
	    Type.Ptr ty => TinyC.TStar(toCType tinfo ty)
	  | Type.Base n =>
	      let val info:info = lookup tinfo n
	      in  #ctype info end
	  | Type.Tname n =>
	      let val info:info = lookup tinfo n
	      in  #ctype info end
	  | Type.Output(_, ty) => TinyC.TStar(toCType tinfo ty)
	  | Type.WithDefault(ty,default) => toCType tinfo ty
	  | Type.Const ty => toCType tinfo ty
	  | _ => raise Fail("toCType: not implemented")

    fun tocvalue tinfo ty =
	case ty of
	    Type.Ptr(ty as Type.Base n) =>
               if Name.asType n = "char" then "String_val"
	       else tocvalue tinfo ty
	  | Type.Const ty => tocvalue tinfo ty
	  | Type.WithDefault(ty,default) => tocvalue tinfo ty
	  | Type.Output(pass,ty) =>  (* FIXME *)
               (case ty of
		    Type.Tname n => let val info:info = lookup tinfo n
				    in  if #kind info = Boxed then  ""
					 else "&" (* YUCK *)
				    end
		  | _ => "&"
               )
	  | Type.Base n => 
               let val info:info = lookup tinfo n
	       in  #toc info end
	  | Type.Tname n => 
               let val info:info = lookup tinfo n
	       in  #toc info end
	  | Type.Ptr ty => tocvalue tinfo ty (* FIXME: ? *)
	  | Type.Void => raise Fail "toCValue: shouldn't happen (Void)"
	  | Type.Arr _ => raise Fail "toCValue: not implemented (Arr)"
	  | Type.Func _ => raise Fail "toCValue: shouldn't happen (Func)"
    fun toCValue tinfo ty exp = ccall (tocvalue tinfo ty) exp
    fun fromCValue tinfo ty =
	case ty of 
	    Type.Ptr(ty as Type.Base n) => 
	       if Name.asType n = "char" then ccall "my_copy_string"
	       else fromCValue tinfo ty
	  | Type.WithDefault(ty,default) => fromCValue tinfo ty
	  | Type.Output(pass,ty) => fromCValue tinfo ty
	  | Type.Base n => 
               let val info:info = lookup tinfo n
	       in  ccall(#fromc info) end
	  | Type.Tname n => 
               let val info:info = lookup tinfo n
	       in  ccall(#fromc info) end
          | Type.Const ty => fromCValue tinfo ty
	  | Type.Ptr ty => fromCValue tinfo ty
	  | Type.Void => (fn e => e)
	  | Type.Arr _ => raise Fail "fromCValue: not implemented (Arr)"
	  | Type.Func _ => raise Fail "fromCValue: shouldn't happen (Func)"

    fun toSignalType tinfo ty =
	let fun loop ty =
		case ty of
		    Type.Void => SMLType.TyApp([],["void"]) (* FIXME: HACK *)
		  | Type.Ptr ty => loop ty
		  | Type.Const ty => loop ty
		  | Type.Func(args,ret) =>
		    SMLType.ArrowTy(List.map (loop o #2) args, loop ret)
		  | Type.WithDefault(ty,_) => loop ty
		  | Type.Output(pass,ty) => loop ty
		  | Type.Tname tn => SMLType.UnitTy (* FIXME: true? *)
		  | Type.Base tn =>
		    let val info: info = lookup tinfo tn
		    in  #ptype info end
		  | Type.Arr(i,ty) => 
		    raise Fail("toSignalType(arr): not implemented")
	    val ret = SMLType.TyApp([SMLType.TyApp([SMLType.TyVar "'a"], ["t"])], 
				    ["Signal","signal"])
	in  SMLType.ArrowTy([loop ty], ret) end

    fun defaultValue tinfo ty = (* FIXME *)
	case ty of
	    Type.Base tn => 
	    let val info: info = lookup tinfo tn
	    in  #default info end
	  | Type.Tname tn => 
	    let val info: info = lookup tinfo tn
	    in  #default info end
	  | Type.Const ty => defaultValue tinfo ty
	  | Type.Ptr ty => defaultValue tinfo ty
	  | Type.WithDefault(ty,v) => defaultValue tinfo ty
	  | Type.Output(pass,ty) => defaultValue tinfo ty
	  | Type.Void => Const "()"
	  | Type.Func _ => Util.abort 98342
	  | Type.Arr _ => Util.abort 98343
end (* structure TypeInfo *)