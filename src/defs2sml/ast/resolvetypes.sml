(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

structure ResolveTypes :> ResolveTypes = struct

    open Type

    structure Parse = struct
	open Parsercomb 
	infix 6 $-- --$ #-- --#
	infix 5 -- unless    
	infix 3 >> >>*
	infix 2 >>=
	infix 0 ||

	val num = getChars1 Char.isDigit >>* Int.fromString

	val void = ($"void" || $"none") >> (fn _ => Void)

	val bool = $"gboolean" >> (fn _ => Base "bool")
	val ptr = $"gpointer" >> (fn _ => Base "ptr")
	val char = ($"gchar" || $"char") >> (fn _ => Base "char")
	val int = $"gint" >> (fn _ => Base "int")
	val uint = $"guint" >> (fn _ => Base "uint")
	val float = $"gfloat" >> (fn _ => Base "float")
	val double = $"gdouble" >> (fn _ => Base "double")
	val base = bool || int || uint || ptr || char || float || double

	val dash_char = fn #"-" => true | _ => false
	fun name_char ch = Char.isAlphaNum ch orelse dash_char ch
	val tname = getChars1 name_char >> (fn n => Tname n)

	val simple = void || base || tname

	val array = simple -- "[" $-- num --$ "]" 
			>> (fn (ty,len) => Arr(SOME len, ty))
	val pointer = simple --$ "*" >> Ptr

	val unqualtyp = pointer || array || simple

	val const = "const-" $-- unqualtyp >> Const

	val typ = const || unqualtyp

	fun toType str = 
	    case scanString typ str of
		NONE => raise Fail("Unrecognized type: " ^ str)
	      | SOME ty => ty

    end (* structure Parse *)

    type 'a ty = 'a Type.ty
    type 'a module_info = ('a * 'a option) option
    type 'a member_info = (string,'a) AST.api_info

(* old stuff

    fun deModularize name (Tname _, tyname) = (* FIXME *)
	let val context = Name.getFullPath name @ Name.getBase name
	    val (path,base) = ResolveNames.toName Name.separateWords context tyname
(*
	    val _ = 
		( TextIO.print  ("deModularizing: " ^ tyname ^ 
			       " (in " ^ Util.stringSep "" "" "." (fn s=>s) context ^")" ^
			       " -> " ^
			       Util.stringSep "" "!" "." (fn s=>s) path ^
			       Util.stringSep "" "" "-" (fn s=>s) base ^ "\n")
                )
*)
	    val base = 
		if length base = 0 then [hd(rev context)]
		else base
        in  Name.fromPaths (context,path,base)
	end
      | deModularize name (Base _ , tyname) = Name.fromPaths([],[],[tyname])
      | deModularize name _ = raise Fail("deModularize: shouldn't happen")

    fun deModularize name (Tname _, tyname) = (* FIXME *)
	let val context = Name.getFullPath name @ Name.getBase name
	    val splits = Name.separateWords tyname
	    fun return full [] = (rev full,[],Name.getBase name)
	      | return full [p] = (rev full,[p],[p])
	      | return full path =
		let val reversed = rev path
		in (rev full, rev(tl reversed), [hd reversed]) end
	    fun loop (e::p, e'::p') full = 
		if Name.toLower e = Name.toLower e' then loop (p,p') (e::full)
		else return full (e'::p')
	      | loop ([], p') full = return full p'
	      | loop (p, []) full = return (p@full) []

	    val (full,path,base) = loop (context, splits) []
	    val name = Name.fromPaths (full,path,base)
(*
	    val _ = 
		( TextIO.print  ("deModularizing: " ^ tyname ^ 
			       " [" ^ Util.stringSep "" "" "-" (fn s=>s) splits ^ "]" ^
			       " (in " ^ Util.stringSep "" "" "." (fn s=>s) context ^")" ^
			       " -> " ^
			       Name.toString' name^"\n")
                )
*)
        in  name
	end
      | deModularize name (Base _ , tyname) = Name.fromPaths([],[],[tyname])
      | deModularize name _ = raise Fail("deModularize: shouldn't happen")
old stuff *)

    fun resTy ty = 
	case ty of
	    AST.ApiTy str => Parse.toType str
          | AST.ArrowTy(pars, ret) =>
		Func(List.map (fn (n,t) => (n,resTy t)) pars,  resTy ret)
    fun resTy' ty = 
	case ty of 
	    NONE => NONE
	  | SOME(ty,parent) => 
	    SOME(resTy ty, Option.map resTy parent)

    fun resMember member =
	case member of
	    AST.Method ty => AST.Method(resTy ty)
          | AST.Field ty => AST.Field(resTy ty)
          | AST.Enum mems => AST.Enum(mems)
          | AST.Signal ty => AST.Signal(resTy ty)
	  | AST.Boxed funcs => AST.Boxed funcs

    fun resolve (AST.Module{name=n,members=m,info=i}) =
	AST.Module{name=n,members=List.map resolve' m,info=resTy' i}
    and resolve' mem =
	case mem of
	    AST.Sub(module) => AST.Sub(resolve module)
	  | AST.Member{name=n,info=i} => AST.Member{name=n,info=resMember i}

    (* For debugging: *)
    val resolve = fn module => 
        let fun pptype ty = Type.show (fn s => s) ty
	    fun ppmodi (SOME(ty, parent)) = 
		": " ^ pptype ty ^
		   (case parent of NONE => "" | SOME ty => " extends " ^ pptype ty)
	      | ppmodi NONE = ""
	    fun ppmemi (AST.Method ty) = ": method " ^ pptype ty
	      | ppmemi (AST.Field ty) = ": field " ^ pptype ty
	      | ppmemi (AST.Enum ss) = ": enum" ^ Util.stringSep "{" "}" ", " (fn s=>s) ss
	      | ppmemi (AST.Signal ty) = ": signal " ^ pptype ty
	      | ppmemi (AST.Boxed func) = ": boxed"
	    val print = TextIO.print

	    val module' = resolve module
	in  if Debug.included "ResolveTypes.debug_resolve_types" then
		( print("After resolving types:\n")
		; AST.ppString (ppmodi, ppmemi) print module' )
	    else ()
          ; module'
        end

end (* structure ResolveTypes *)

val _ = Debug.add {name="ResolveTypes.debug_resolve_types",
		   short_option="drt",long_option=SOME("debug-resolve-types"),
		   included=false}