(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

functor GenCMLton(structure TypeInfo : TypeInfo) 
	:> GEN_C where type typeinfo = TypeInfo.typeinfo
= struct

    open TinyC

    type ty = (Name.name,Name.name) Type.ty
    type 'a module  = (Name.name, 'a, (Name.name,ty) AST.api_info) AST.module
    type 'a module' = (Name.name, 'a, (topdecl*SMLType.ty option) list) AST.module

    type typeinfo = TypeInfo.typeinfo

    fun print os module =
	let 
	    fun dump s = TextIO.output(os, s)
	    fun spaces n = String.implode(List.tabulate(n,fn _ => #" "))

	    fun print1 indent (decl, SOME ty) =
              ( dump ("/* ML type: "^ (SMLType.toString ty) ^ " */\n")
              ; dump (toString (spaces indent) decl)
              )
	      | print1 indent (decl, NONE) = dump(toString(spaces indent) decl)
	    fun print_module indent module =
		case module of
		    AST.Module{name,members,info} =>
	            ( dump("\n\n/* *** " ^ Name.toString name ^ " *** */\n")
                    ; List.app (print_member (indent+2)) members
	            )
	    and print_member indent member =
		case member of 
		    AST.Sub(module) => print_module indent module
		  | AST.Member{name,info} => List.app (print1 indent) info
	in  print_module 0 module
	end

    exception Skip of string

    fun trans typeinfo (name, member) = 
	case member of
	    AST.Enum(flag,enums) =>
	        let val fresh = List.tabulate(List.length enums, fn i => "x"^Int.toString i)
		    val construct = List.map(fn (v,c) => Ass(Var("*"^v),TStar TInt,Var(Name.asCEnumConst c))) (ListPair.zip(fresh,enums))
		    val body = 
			Block(NONE,[], rev construct)
		    val ty =  
			SMLType.ArrowTy([SMLType.TupTy(List.map (fn _ => SMLType.RefTy(SMLType.IntTy)) enums)], SMLType.UnitTy)
		in  [(Fun(Proto(SOME"EXTERNML",Name.asCGetEnum name, List.map (fn v => (v,TStar TInt)) fresh, TVoid),
			  body),
		      SOME ty)]
		end
	  | AST.Boxed(SOME{copy,...}) =>
                let val ty = TTyName(Name.asCBoxed name)
		    val body = Block(NONE,[VDecl("res",ty,NONE)], 
				     [Return(Call(copy,NONE,[Call("&",NONE,[Var"res"])]))])
		in  [(Fun(Proto(SOME"EXTERNML","alloc_"^Name.asCBoxed name,[],TStar ty),
			  body),
		      NONE)]
		end

(*
val construct = List.foldl (fn (e,(c,i)) => (Ass(Call("Field", NONE, [Var "res", Int i]),TInt,Call("Val_int",NONE,[Var (Name.asCEnumConst e)]))::c,i+1)) ([],0) enums

static void ml_finalize_gdk_font (value val) {
  gdk_font_unref (gdk_font_val(val)); 
}

value Val_gdk_font (void* obj) {
  value res;
  gdk_font_ref(obj);
  res = alloc_final (2, ml_finalize_gdk_font, 0, 1);
  gdk_font_val(res) = obj;
  return res;
}
*)
	  | _ => []

    val trans = fn tinfo => fn (name, member) => trans tinfo (name, member)
		   handle Skip msg => ( TextIO.output(TextIO.stdErr,
		       "Error translating " ^ Name.toString name ^ ": " ^msg^"\n")
                     ; [])
    fun generate typeinfo module = 
	AST.mapi (fn (module,info) => info, trans typeinfo) module

end
