(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

structure GenSML :>
    sig
	type name = Name.name
	type typeexp = name Type.ty
	type sml_type = SMLType.ty

	type sml_info
	type 'a incl
	type module_info

	val max_curried : int

	val generate: TypeInfo.typeinfo -> (name,(name*name option)option,typeexp AST.api_info) AST.module -> 
		      (name,module_info,sml_info incl) AST.module
	val print: string option -> TextIO.outstream -> (name,module_info,sml_info incl) AST.module 
                   -> unit
    end =
struct

    val max_curried = 5

    datatype exp =
	Unit 
      | Var of string
      | Long of Name.name
      | Str of string
      | Fn of string * exp
      | App of exp * exp list
      | Tup of exp list
    infix ==>
    fun x ==> e = Fn(x,e)

    datatype pat =
        VarPat of string
      | TupPat of pat list

    type sml_type = SMLType.ty
    type sml_tyvar = SMLType.tyvar
    type sml_tyname = SMLType.tyname

    datatype 'a incl =
        None
      | StrOnly of 'a
      | SigOnly of 'a
      | Some of 'a
	     
    datatype sml_info =
	ValDecl of pat * sml_type incl * exp
      | FunDecl of string * pat list * sml_type incl * exp
      | TypeDecl of (sml_tyvar list * sml_tyname) * sml_type incl
      | SeqDecl of sml_info incl list
      | EmptyDecl
      | Comment of string option (* an empty comment prints as newline *)
      | Open of string list

    (* convenience *)
    infix ++
    fun d1 ++ d2 = Some(SeqDecl[d1,d2])

    open SMLType
    infix --> ==>
    fun ty1 --> ty2 = ArrowTy([ty1], ty2)
    fun tys ==> ty2 = ArrowTy(tys, ty2)

    (* "primitives" *)
    fun ccall name args =
	let val app = if args > max_curried then "app1" 
		      else "app" ^ Int.toString args
	in  App(Var app, [App(Var "symb", [Str name])])
	end

    (* print *)
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

    fun toString mode indent info =
	let fun parens safe level s = if level > safe then "(" ^ s ^ ")"
				      else s
	    fun show_exp level exp =
		case exp of
		    Unit => "()"
		  | Var x => x
		  | Long n => Name.toString n
		  | Str s => "\"" ^ s ^ "\""
		  | Fn(x,e) => parens 1 level ("fn " ^ x ^ " => " ^ show_exp 1 e)
		  | App(Var "symb",[Str s]) => "(symb\""^s^"\")"
		  | App(e,es) => parens 2 level 
				    (show_exp 3 e ^ 
				     Util.stringSep " " "" " " (show_exp 3) es)
		  | Tup [] => "()"
		  | Tup [e] => show_exp level e
		  | Tup es => Util.stringSep "(" ")" "," (show_exp 1) es
	    val show_exp = fn exp => show_exp 1 exp
	    fun show_pat pat =
		case pat of
		    VarPat x => x
		  | TupPat ps => Util.stringSep "(" ")" "," show_pat ps
	    fun show_type_incl sep None = "" 
	      | show_type_incl sep (Some ty) = sep ^ SMLType.toString ty
	      | show_type_incl sep (StrOnly ty) = 
		if isStrMode mode then sep ^ SMLType.toString ty else ""
	      | show_type_incl sep (SigOnly ty) = 
		if isSigMode mode then sep ^ SMLType.toString ty else ""

	    fun printing s =
		List.exists (not o Char.isSpace) (String.explode s)
	    local
		fun str _ nil _ _ = ""
		  | str p (h::t) sep needSep =
		    let val ph = p h
			val ns = printing ph
			val s = p h ^ (str p t sep ns)
		    in  if needSep then sep ^ s else s
		    end
	    in
	    fun stringSep start finish sep p l = 
		start ^ (str p l sep false) ^ finish
	    end (* local *)

	    fun show decl =
		case decl of
		    ValDecl(pat,ty,exp) =>
		       if isStrMode mode then
			   indent ^ "val " ^ show_pat pat ^ show_type_incl " : " ty
			   ^ (if printing (show_type_incl " : " ty) then "\n" ^ indent ^ "    = " else " = ") ^ show_exp exp
		       else
			   indent ^ "val " ^ show_pat pat ^ show_type_incl " : " ty
		  | FunDecl(name,pars,ty,exp) =>
		       if isStrMode mode then
			   indent ^ "fun " ^ name ^ Util.stringSep " " "" " " show_pat pars
			   ^ " = " ^ show_exp exp
		       else
			   indent ^ "val " ^ name ^ show_type_incl " : " ty
		  | TypeDecl((tvs,tname),ty) =>
		       indent ^ "type " 
		     ^ SMLType.toString(TyApp(map TyVar tvs,tname))
		     ^ show_type_incl " = " ty
		  | SeqDecl decs =>
		       stringSep "" "" "\n" show' decs
		  | EmptyDecl => ""
		  | Comment NONE => ""
		  | Comment(SOME c) => "(*" ^ c ^ "*)"
		  | Open strs => indent ^ Util.stringSep "open " "" " " (fn s=>s) strs
	    and show' (None) = ""
	      | show' (Some decl) = show decl
	      | show' (StrOnly decl) = if isStrMode mode then show decl else ""
	      | show' (SigOnly decl) = if isSigMode mode then show decl else ""

	in  show' info ^ "\n"
	end
	    
    fun print preamble os module =
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
		( dump(spaces 0 ^ showModInfo info ^ " = " ^ showModBegin info ^ "\n")
                ; dump_preamble 4 preamble
                ; List.app (print_member info 4) members
                ; dump("end\n")
                )
	in  print_toplevel module
	end

    (* code generation *)
    type name = Name.name
    type typeexp = name Type.ty

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
		let val cname = Name.asCStub name
		    val name = Name.asMethod name
		    val parsty = Type.getParams ty
		    val (pars,tys) = ListPair.unzip parsty
		    fun isWidgetType (Type.Tname n) = (* FIXME *)
			(case Name.getFullPath n of "Gtk"::_ => true | _ => false)
		      | isWidgetType (Type.Ptr t) = isWidgetType t
		      | isWidgetType _ = false
		    fun wrap (par,ty) = if isWidgetType ty 
					then App (Var"repr",[Var par])
					else Var par
		    val pars' = if List.length pars > max_curried
				then [Tup(List.map wrap parsty)]
				else List.map wrap parsty
		    fun ubnd n =
			raise Skip("Unbound type name: "^Name.toString' n)
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
				  
		    fun make e = 
			(* FIXME *)
			if isWidgetType ret then App(Var("make"), [e])
			else e
		in  StrOnly(
                       ValDecl(VarPat(name^"_"), Some(primtypeFromType ty), 
			       ccall cname (List.length pars)))
                 ++ Some(
                       ValDecl(VarPat name, Some(SMLType.ArrowTy(params',ret')),
			       List.foldr Fn (make(App(Var(name^"_"), pars')))
					  pars))
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
		    val tup = List.tabulate(List.length consts, fn _ => IntTy)
		in  Some(TypeDecl(([],[name]), StrOnly IntTy))
                 ++ StrOnly(ValDecl(VarPat("get_" ^ name ^ "_"), Some(UnitTy --> TupTy tup),
			    ccall ("mgtk_get_"^cname) 1))
		 ++ StrOnly(ValDecl(TupPat(List.map VarPat consts), None,
			    App(Var("get_" ^ name ^ "_"), [Unit])))
		end

	  | AST.Boxed funcs =>
	        let val name = Name.asBoxed name
		in  Some(TypeDecl(([],[name]), StrOnly(TyApp([],["GObject.cptr"]))))
		end
          | AST.Field ty => 
		let val name = Name.asField name
		in  StrOnly(ValDecl(VarPat ("get_"^name), None, 
			    ccall ("mgtk_get_"^name) 1))
		end

	  | AST.Signal ty =>
	        let val name = Name.asSignal name
		in  Some(ValDecl(VarPat name, None, Var name))
		end

    fun header tinfo (name, info) =
	let val (typ,parent) = 
		case info of
		    SOME (n,parent) => 
		       (Name.asType n,
			case parent of SOME(p) => p
		                     | _ => raise Skip("No parent")
                       )
		  | _ => raise Skip("No type information")

	    val base_t = "base"
	    val witness_t = typ^"_t"
	    val parent_t = Name.asModule parent ^ ".t"

	    val type_t = "t"
	    fun pRef id = Name.asModule parent ^ "." ^ id (* FIXME: Using names instead *)

	in  Some(TypeDecl(([],[base_t]),StrOnly UnitTy))
         ++ Some(TypeDecl((["'a"],[witness_t]),StrOnly UnitTy))
         ++ Some(TypeDecl((["'a"],[type_t]), 
		    Some(TyApp([TyApp([TyVar "'a"],[witness_t])],[parent_t]))))
         ++ StrOnly(Open["Dynlib"])
         ++ StrOnly(TypeDecl(([],["cptr"]), Some(TyApp([],["GObject.cptr"]))))
         ++ StrOnly(ValDecl(VarPat"repr", None, Var("GObject.repr")))
         ++ StrOnly(ValDecl(VarPat"symb", None, Var("GtkBasis.symb")))
         ++ Some(Comment NONE)
         ++ Some(FunDecl("inherit",[VarPat "w",VarPat "con"],
		    (* 'a -> GObject.constructor -> 'a t *)
		    SigOnly([TyVar "'a", TyApp([],["GObject","constructor"])] ==> TyApp([TyVar"'a"],["t"])),
		    App(Var(pRef "inherit"),[Unit,Var "con"])))
	 ++ StrOnly(FunDecl("make"(*^Name.asModule name*),[VarPat"ptr"],None,
		    App(Var(pRef "inherit"),[Unit,Fn("()",Var"ptr")])))
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
	    Module{name=name,info=STRUCTURE(Name.asModule name, NONE),
		   members=Member{name=Name.fromString "structureheader",
				  info = StrOnly(Open["Dynlib"])}  
                           :: Member{name=Name.fromString "structureheader",
				  info = StrOnly(ValDecl(VarPat"symb", None, Var("GtkBasis.symb")))}
			   :: List.concat(List.map (generate_member tinfo) members)}
    end (* local *)

end (* structure GenSML *)
