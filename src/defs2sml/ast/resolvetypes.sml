(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

structure ResolveTypes :> ResolveTypes = struct

    open Type

    type 'a ty = ('a,'a) Type.ty

    structure Parse :> sig val toType: string -> string ty end = struct
	open Parsercomb 
	infix 6 $-- --$ #-- --#
	infix 5 -- unless    
	infix 3 >> >>*
	infix 2 >>=
	infix 0 ||

	val num = getChars1 Char.isDigit >>* Int.fromString

	val void = ($"void" || $"none") >> (fn _ => Void: string ty)

	val bool = $"gboolean" >> (fn _ => Base "bool": string ty)
	val ptr = $"gpointer" >> (fn _ => Base "ptr": string ty)
	val char = ($"gchar" || $"char") >> (fn _ => Base "char": string ty)
	val int = $"gint" >> (fn _ => Base "int": string ty)
	val uint = $"guint" >> (fn _ => Base "uint": string ty)
	val float = $"gfloat" >> (fn _ => Base "float": string ty)
	val double = $"gdouble" >> (fn _ => Base "double": string ty)
	val base = bool || int || uint || ptr || char || float || double

	val dash_char = fn #"-" => true | _ => false
	fun name_char ch = Char.isAlphaNum ch orelse dash_char ch
	val tname = getChars1 name_char >> (fn n => Tname n: string ty)

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

    fun resTy ty = 
	case ty of
	    AST.ApiTy str => Parse.toType str
	  | AST.Defaulted(ty, v) => WithDefault(resTy ty, v)
	  | AST.Output(pass,ty) => Output(case pass of AST.OUT => OUT
						     | AST.INOUT => INOUT,
					  resTy ty)
          | AST.ArrowTy(pars, ret) =>
		Func(List.map (fn (n,t) => (n,resTy t)) pars,  resTy ret)
    fun resTy' ty = 
	case ty of 
	    NONE => NONE
	  | SOME(ty,parent,impl) => SOME(ty, parent, impl)

    fun resMember member =
	case member of
	    AST.Method ty => AST.Method(resTy ty)
          | AST.Field ty => AST.Field(resTy ty)
          | AST.Enum(flag, mems) => AST.Enum(flag,mems)
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
        let val pps = Pretty.ppString
	    val pp = AST.ppAst pps (Type.pp pps pps)
	    val module' = resolve module
	in  if Debug.included "ResolveTypes.debug_resolve_types" then
		( print("After resolving types:\n")
		; Pretty.ppPlain (pp module') TextIO.stdOut)
	    else ()
          ; module'
        end

end (* structure ResolveTypes *)

val _ = Debug.add {name="ResolveTypes.debug_resolve_types",
		   short_option="drt",long_option=SOME("debug-resolve-types"),
		   included=false}