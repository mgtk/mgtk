(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

signature PRIMTYPES = sig
    type ty = SMLType.ty
    val mkArrowTy : ty list * ty -> ty
    val stringTy : ty
    val toPrimString : TinySML.exp -> TinySML.exp
end (* signature PRIMTYPES *)

structure MosmlPrimTypes : PRIMTYPES = struct
    type ty = SMLType.ty
    fun mkArrowTy(pars,ret) =
	SMLType.ArrowTy(
	   if List.length pars > TinySML.max_curried
	   then [SMLType.TupTy pars]
	   else pars
        ,  ret)
    val stringTy = SMLType.StringTy
    val toPrimString = fn e => e
end (* structure MosmlPrimTypes *)

structure MLtonPrimTypes : PRIMTYPES = struct
    type ty = SMLType.ty
    fun mkArrowTy(pars,ret) =
	SMLType.ArrowTy([SMLType.TupTy pars], ret)
    val stringTy = SMLType.TyApp([],["CString", "cstring"])
    local open TinySML in  
        fun toPrimString e = App(Var("Cstring.cstring"), [e])
    end
end (* structure MosmlPrimTypes *)

functor TypeInfo(structure Prim : PRIMTYPES) :> TypeInfo = struct
    
    type name = Name.name
    type ty = name Type.ty

    (* build a table mapping type names to
     *    SML types and SML primitive types
     *  & functions for converting to and from appropriate C values
     *  & a possible super type
     *)

    type info = {stype: (unit -> string) -> SMLType.ty, ptype: SMLType.ty,
		 toc: TinyC.expr -> TinyC.expr,
		 fromc: TinyC.expr -> TinyC.expr,
		 super: name option,
		 fromprim: TinySML.exp -> TinySML.exp,
		 toprim: TinySML.exp -> TinySML.exp
		}
    type typeinfo = (Name.name, info) Splaymap.dict
    exception NotFound = Splaymap.NotFound
    exception Unbound of Name.name
    val add = Splaymap.insert
    fun lookup table name = Splaymap.find(table,name)

    fun id x = x
    fun make e = TinySML.App(TinySML.Var("make"), [e])
    fun repr e = TinySML.App(TinySML.Var("repr"), [e])
    fun ccall name = fn e => TinyC.Call(name,NONE,[e])
    val basic =
	[("int",       (fn _ => SMLType.IntTy,SMLType.IntTy,
		        ccall"Int_val", ccall"Val_int", NONE))
        ,("uint",      (fn _ => SMLType.IntTy,SMLType.IntTy,
		        ccall"Int_val", ccall"Val_int", NONE))
        ,("guint",     (fn _ => SMLType.IntTy,SMLType.IntTy,           (* FIXME *)
		        ccall"Int_val", ccall"Val_int", NONE))
        ,("char",      (fn _ => SMLType.CharTy,SMLType.CharTy,
		        ccall"Int_val", ccall"Val_int", NONE)) (* FIXME *)
        ,("float",     (fn _ => SMLType.RealTy,SMLType.RealTy,
		        ccall"Double_val", ccall"copy_double", NONE))
        ,("double",    (fn _ => SMLType.RealTy,SMLType.RealTy,
		        ccall"Double_val", ccall"copy_double", NONE))
        ,("gdouble",   (fn _ => SMLType.RealTy,SMLType.RealTy,
		        ccall"Double_val", ccall"copy_double", NONE))
        ,("ptr",       (fn _ => SMLType.TyApp([],["cptr"]),SMLType.TyApp([],["cptr"]),         (* FIXME *)
		        fn e => TinyC.Cast(TinyC.TTyName "gpointer",e), 
			fn e => TinyC.Cast(TinyC.TValue,e), NONE))
        ,("bool",      (fn _ => SMLType.BoolTy,SMLType.BoolTy,
		        ccall"Bool_val", ccall"Val_bool", NONE))
        ,("GType",     (fn _ => SMLType.IntTy,SMLType.IntTy,           (* FIXME *)
		        ccall"Int_val", ccall"Val_int", NONE))
        ,("GtkType",   (fn _ => SMLType.IntTy,SMLType.IntTy,           (* FIXME *)
		        ccall"Int_val", ccall"Val_int", NONE))
        ]
    fun init () = 
	let fun a ((n,i),t)=
		add(t,Name.fromPaths([],[],[n]),
		    {stype= #1 i,ptype= #2 i,toc= #3 i, 
		     toprim = id, fromprim = id,
		     fromc= #4 i,super= #5 i})
	in  List.foldl a (Splaymap.mkDict Name.compare) basic
	end

    fun build module =
	let fun bmod (AST.Module{name,members,info=SOME(n,parent)},table) = 
		let val table' = List.foldl bmem table members
		    val nb = Name.getBase name
(*
		    val name' = Name.fromPaths(Name.getFullPath name@nb,
					       Name.getPath name,
					       nb)
*)
		    val _  = MsgUtil.debug("Binding " ^ Name.toString' name)
		    val info = {toc=ccall"GtkObj_val",fromc=ccall"Val_GtkObj",
				ptype=SMLType.TyApp([],["cptr"]),
				fromprim = make, toprim = repr,
				stype=fn fresh => SMLType.TyApp([SMLType.TyVar(fresh())],["t"]),
				super=NONE}
		in  add(table',name,info)  end
	      | bmod (AST.Module{name,members,info=NONE},table) = 
		List.foldl bmem table members
	    and bmem (AST.Sub module,table) = bmod(module,table)
	      | bmem (AST.Member{name,info},table) =
		case info of
		    AST.Enum _ => 
		    (MsgUtil.debug("Binding " ^ Name.toString' name);
		        add(table,name,
			    {toc=ccall"Int_val", fromc=ccall"Val_int",
			     fromprim=id, toprim=id,
			     ptype=SMLType.IntTy, super=NONE,
			     stype=fn _ => SMLType.TyApp([],[Name.asEnum name])})
                    )
		  | AST.Boxed _ =>
		    (MsgUtil.debug("Binding " ^ Name.toString' name);
		        add(table,name,
			    {toc=ccall(Name.asCBoxed name^"_val"), 
			     fromc=ccall("Val_"^Name.asCBoxed name),
			     fromprim=id, toprim=id,
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

    fun show os table =
	let fun sinfo {stype,ptype,toc,fromc,super,fromprim,toprim} = 
		(SMLType.show ptype) ^ " x "  ^
		(SMLType.show (stype (nextgen())))
	in  List.app (fn (n,i) => 
			 TextIO.output(os,Name.toString' n^" -> "^sinfo i^"\n"))
		     (Splaymap.listItems table)
	end

    (* Debugging: *)
    val build = fn module =>
		   let val table = build module
		       val os = TextIO.openOut("typetable.txt")
		   in  show os table
                     ; TextIO.closeOut os
		     ; table
		   end

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
	  | Type.Base n => 
	       (let val info: info = lookup tinfo n
		in  #stype info fresh
		end
		    handle NotFound => raise Unbound n
               )
	  | Type.Tname n => 
	       (let val info: info = lookup tinfo n
		in  prependPath (Name.getPath n, Name.getBase n) 
				(#stype info fresh)
		end
		    handle NotFound => raise Unbound n
               )
	  | Type.Const ty => toSMLType tinfo fresh ty
	  | Type.Ptr ty => toSMLType tinfo fresh ty
	  | Type.Func(pars,ret) => 
	       SMLType.ArrowTy(List.map (toSMLType tinfo fresh o #2) pars,
			       toSMLType tinfo fresh ret)
	  | Type.Arr(len,ty) => 
	       (* FIXME *)
	       SMLType.TyVar(SMLType.toString(toSMLType tinfo fresh ty)^"["^
		     (case len of NONE => "" | SOME l => Int.toString l)^"]")

    fun toSMLTypeSeq tinfo = 
	let val no = ref 0
	    val orda = Char.ord #"a"
	    fun next () = if(!no<26) then ("'"^Char.toString(Char.chr(!no+orda))
				           before
					   no := !no + 1)
			  else raise Fail("Not implemented: fromTypeSeq.next()")
	in  toSMLType tinfo next
	end

    fun toPrimType tinfo ty =
	case ty of 
	    Type.Ptr(Type.Base n) => 
	       if Name.asType n = "char" then Prim.stringTy
	       else SMLType.TyApp([],["cptr"])
	  | Type.Void => SMLType.UnitTy
	  | Type.Base n => 
	       (let val info: info = lookup tinfo n
		in  #ptype info
		end
		    handle NotFound => raise Unbound n
               )
	  | Type.Tname n => 
	       (let val info: info = lookup tinfo n
		in  #ptype info
		end
		    handle NotFound => raise Unbound n
               )
	  | Type.Const ty => toPrimType tinfo ty
	  | Type.Ptr ty => SMLType.TyApp([],["cptr"])
	  | Type.Func(pars,ret) => 
	       Prim.mkArrowTy(List.map (toPrimType tinfo o #2) pars,
			      toPrimType tinfo ret)
	  | Type.Arr(len,ty) => SMLType.TyApp([],["..."]) (* FIXME *)


    fun call func arg = TinyC.Call(func, NONE, [arg])

    fun fromPrimValue tinfo ty =
	case ty of
	    Type.Base n =>
	       (let val info: info = lookup tinfo n
		in  #fromprim info
		end
		    handle NotFound => raise Unbound n
	       )
	  | Type.Tname n =>
	       (let val info: info = lookup tinfo n
		in  #fromprim info
		end
		    handle NotFound => raise Unbound n
	       )
	  | Type.Ptr ty => fromPrimValue tinfo ty (* FIXME: ? *)
	  | _ => id
    fun toPrimValue tinfo ty =
	case ty of
	    Type.Ptr(ty as Type.Base n) =>
	       if Name.asType n = "char" then Prim.toPrimString
	       else toPrimValue tinfo ty
	  | Type.Base n =>
	       (let val info: info = lookup tinfo n
		in  #toprim info
		end
		    handle NotFound => raise Unbound n
	       )
	  | Type.Tname n =>
	       (let val info: info = lookup tinfo n
		in  #toprim info
		end
		    handle NotFound => raise Unbound n
	       )
	  | Type.Ptr ty => toPrimValue tinfo ty (* FIXME: ? *)
	  | _ => id

    fun toCValue tinfo ty =
	case ty of
	    Type.Ptr(ty as Type.Base n) =>
               if Name.asType n = "char" then call "String_val"
	       else toCValue tinfo ty
	  | Type.Const ty => toCValue tinfo ty
	  | Type.Base n => 
               (let val info:info = lookup tinfo n
		in  #toc info end
		    handle NotFound => raise Unbound n
               )
	  | Type.Tname n => 
               (let val info:info = lookup tinfo n
		in  #toc info end
		    handle NotFound => raise Unbound n
               )
	  | Type.Ptr ty => toCValue tinfo ty (* FIXME: ? *)
	  | Type.Void => raise Fail "toCValue: shouldn't happen (Void)"
	  | Type.Arr _ => raise Fail "toCValue: not implemented (Arr)"
	  | Type.Func _ => raise Fail "toCValue: shouldn't happen (Func)"

    fun fromCValue tinfo ty =
	case ty of 
	    Type.Ptr(ty as Type.Base n) => 
	       if Name.asType n = "char" then call "my_copy_string"
	       else fromCValue tinfo ty
	  | Type.Base n => 
               (let val info:info = lookup tinfo n
		in  #fromc info end
		    handle NotFound => 
			   (TextIO.output(TextIO.stdErr, "Unbound type name ("^Name.toString n^")\n");
			    call "Val_int")
               )
	  | Type.Tname n => 
               (let val info:info = lookup tinfo n
		in  #fromc info end
		    handle NotFound => 
			   (TextIO.output(TextIO.stdErr, "Unbound type name ("^Name.toString n^")\n");
			    call "Val_GtkObj")
               )
          | Type.Const ty => fromCValue tinfo ty
	  | Type.Ptr ty => fromCValue tinfo ty
	  | Type.Void => (fn e => e)
	  | Type.Arr _ => raise Fail "fromCValue: not implemented (Arr)"
	  | Type.Func _ => raise Fail "fromCValue: shouldn't happen (Func)"

end (* structure TypeInfo *)