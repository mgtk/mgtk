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
	val char = $"gchar" >> (fn _ => Base "char")
	val int = $"gint" >> (fn _ => Base "int")
	val uint = $"guint" >> (fn _ => Base "uint")
	val float = $"gfloat" >> (fn _ => Base "float")
	val double = $"gdouble" >> (fn _ => Base "double")
	val dash_char = fn #"-" => true | _ => false
	fun name_char ch = Char.isAlphaNum ch orelse dash_char ch
	val tname = getChars1 name_char >> (fn n => Tname n)

	val base = bool || int || uint || ptr || char || tname
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

    type module_info = (AST.api_type * AST.api_type option) option
    type member_info = AST.api_type AST.api_info

    type ty = Name.name Type.ty
    type module_info' = (ty * ty option) option
    type member_info' = ty AST.api_info

    fun deModularize name tyname = (* FIXME *)
	let val (path,base) = ResolveNames.toName Name.separateWords (Name.getFullPath name) tyname
(*
	    val _ = 
		( TextIO.print  ("deModularizing: " ^ tyname ^ 
			       "(in " ^ Util.stringSep "" "" "." (fn s=>s) (Name.getFullPath name)^")" ^
			       " -> " ^
			       Util.stringSep "" "!" "." (fn s=>s) path ^
			       Util.stringSep "" "" "-" (fn s=>s) base ^ "\n")
                )
*)
        in  Name.fromPaths (Name.getFullPath name,path,base)
	end

    fun resTy (module, ty) = 
	case ty of
	    AST.ApiTy str => 
	    let val typ = (Parse.toType str)
			  handle exn => (TextIO.print("Error for " ^ Name.toString module); raise exn)
	    in  map (deModularize module) typ
	    end
          | AST.ArrowTy(pars, ret) =>
		Func(List.map (fn (n,t) => (n,resTy(module, t))) pars, 
		     resTy(module, ret))
    fun resTy' (module, ty) = 
	let val module' = let val p = Name.getFullPath module  (* YIKES *)
			 in  Name.fromPaths(p,p,Name.getBase module) end
	in  case ty of 
		NONE => NONE
	      | SOME(ty,parent) => SOME(resTy(module,ty),
					Option.map(fn ty => (resTy(module,ty)))parent)
	end

    fun resMember inm member =
	case member of
	    AST.Method ty => AST.Method(resTy(inm, ty))
          | AST.Field ty => AST.Field(resTy(inm, ty))
          | AST.Enum mems => AST.Enum(mems)
          | AST.Signal ty => AST.Signal(resTy(inm, ty))

    fun resolve inm module =
	case module of
	    AST.Module{name=n,members=m,info=i} =>
	        AST.Module{name=n,members=List.map (resolve' n) m,info=resTy'(n,i)}
    and resolve' inm mem =
	case mem of
	    AST.Sub(module) => AST.Sub(resolve inm module)
	  | AST.Member{name=n,info=i} => AST.Member{name=n,info=resMember inm i}
    val resolve = fn module => resolve (Name.fromPaths ([""],[""],[])) module

(*
    (* For debugging: *)
    val resolve = fn module => 
        let fun pptype ty = Type.show(Name.toString) ty
	    fun ppmodi (SOME(ty, parent)) = 
		": " ^ pptype ty ^
		   (case parent of NONE => "" | SOME ty => " extends " ^ pptype ty)
	      | ppmodi NONE = ""
	    fun ppmemi (AST.Method ty) = ": method " ^ pptype ty
	      | ppmemi (AST.Field ty) = ": field " ^ pptype ty
	      | ppmemi (AST.Enum ss) = ": enum" ^ Util.stringSep "{" "}" ", " (fn s=>s) ss
	      | ppmemi (AST.Signal ty) = ": signal " ^ pptype ty
	    val print = TextIO.print

	    val module' = resolve module
	in  print("After resolving types:\n")
          ; AST.ppName (ppmodi, ppmemi) print module'
          ; module'
        end
*)
end (* structure ResolveTypes *)
