(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

structure GenC :> sig
        type topdecl
	type typeexp = Name.name Type.ty
        type 'a module  = (Name.name, 'a, typeexp AST.api_info) AST.module
        type 'a module' = (Name.name, 'a, (topdecl*typeexp) option) AST.module
        val generate: TypeInfo.typeinfo -> 'a module -> 'a module'
        val print: TypeInfo.typeinfo -> TextIO.outstream -> 'a module' -> unit
    end =
struct

    open TinyC

    type typeexp = Name.name Type.ty
    type 'a module  = (Name.name, 'a, typeexp AST.api_info) AST.module
    type 'a module' = (Name.name, 'a, (topdecl*typeexp) option) AST.module


    fun print tinfo os module =
	let 
	    fun dump s = TextIO.output(os, s)
	    fun spaces n = String.implode(List.tabulate(n,fn _ => #" "))

	    fun print_module indent module =
		case module of
		    AST.Module{name,members,info} =>
	            ( dump("\n\n/* *** " ^ Name.toString name ^ " *** */\n")
                    ; List.app (print_member (indent+2)) members
	            )
	    and print_member indent member =
		case member of 
		    AST.Sub(module) => print_module indent module
		  | AST.Member{name,info=SOME(decl,ty)} => 
		    ( dump ("/* ML type: "^SMLType.show (TypeInfo.toPrimType tinfo ty) ^" */\n")
                    ; dump (toString (spaces indent) decl)
                    )
		  | AST.Member{name,info=NONE} => ()
	in  print_module 0 module
	end

    exception Skip of string

    fun trans typeinfo (name, member) = 
	case member of
	    AST.Method ty => 
	        let fun isVoid (Type.Void) = true
		      | isVoid _ = false
		    val parsty = map (fn(p,t)=>if p="value" then ("valu",t)
					       else (p,t)) 
				     (Type.getParams ty)
		    val args = if List.length parsty > 5
			       then [("mgtk_params", Type.Tname(Name.fromPaths([],[],["int"])))]
			       else parsty
		    fun f (par,Type.Void) = NONE
		      | f (par,ty) = SOME(TypeInfo.toCValue typeinfo ty (Var par))
				     handle Fail m => raise Skip m
		    val parsty' = List.mapPartial f parsty
		    val ret = Type.getRetType ty

		    fun f ((par,ty),i) = VDecl(par,TValue,SOME(Call("Field", NONE, [Var "mgtk_params", Int(Int.toString i)])))
		    val extract = if List.length parsty > 5 
				  then List.map f (ListPair.zip(parsty,List.tabulate(List.length parsty, fn i=>i)))
				  else []
		    val call = TypeInfo.fromCValue typeinfo ret (Call(Name.asCFunc name, NONE, parsty'))
			       handle Fail m => raise Skip m
		    val body = 
			Block(NONE,extract, 
			  Comment "ML" ::
                          (if isVoid ret then [Exp(call),Return(Var("Val_unit"))]
			   else [Return(call)])
                        )
		    fun f (par,ty) = (par,TValue)
		in  SOME(Fun(Proto(SOME"EXTERNML",Name.asCStub name, map f args, TValue), 
			     body),
			 ty)
		end
	  | AST.Enum enums =>
	        let val construct = List.foldl (fn (e,(c,i)) => (Ass(Call("Field", NONE, [Var "res", Int(Int.toString i)]),TInt,Call("Val_int",NONE,[Var e]))::c,i+1)) ([],0) enums
		    val body = 
			Block(NONE,[VDecl("res",TValue,SOME(Call("alloc_tuple",NONE,[Int(Int.toString(List.length enums))])))],
			  Comment "ML" ::
			  rev(#1 construct) @
			  [Return(Var "res")]
                        )
		in  SOME(Fun(Proto(SOME"EXTERNML",Name.asCStub name, [("dummy",TValue)], TValue),
			     body),
			 Type.Void)
		end
	  | _ => NONE

    val trans = fn tinfo => fn (name, member) => trans tinfo (name, member)
		   handle Skip msg => ( TextIO.output(TextIO.stdErr,
		       "Error translating " ^ Name.toString name ^ ": " ^msg^"\n")
                     ; NONE)
    fun generate typeinfo module = 
	AST.mapi (fn (module,info) => info, trans typeinfo) module

end
