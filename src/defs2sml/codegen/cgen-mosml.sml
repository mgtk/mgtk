(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

functor GenCMosml(structure TypeInfo : TypeInfo) 
	:> GEN_C where type typeinfo = TypeInfo.typeinfo
= struct

    open TinyC
    structure N = Name

    type ty = (N.name,N.name) Type.ty
    type 'a module  = (N.name, 'a, (N.name,ty) AST.api_info) AST.module
    type 'a module' = (N.name, 'a, (topdecl*ty option) list) AST.module

    type typeinfo = TypeInfo.typeinfo

    fun print tinfo os module =
	let 
	    fun dump s = TextIO.output(os, s)
	    fun spaces n = String.implode(List.tabulate(n,fn _ => #" "))

	    fun print1 indent (decl, SOME ty) =
              ( dump ("/* ML type: "^
		           (SMLType.toString (TypeInfo.toPrimType tinfo ty)
			    handle TypeInfo.Unbound _ => "") ^" */\n")
              ; dump (toString (spaces indent) decl)
              )
	      | print1 indent (decl, NONE) = dump(toString(spaces indent) decl)
	    fun print_module indent module =
		case module of
		    AST.Module{name,members,info} =>
	            ( dump("\n\n/* *** " ^ N.toString name ^ " *** */\n")
                    ; List.app (print_member (indent+2)) members
	            )
	    and print_member indent member =
		case member of 
		    AST.Sub(module) => print_module indent module
		  | AST.Member{name,info} => List.app (print1 indent) info
	in  print_module 0 module
	end

    exception Skip of string

    val extern = SOME "EXTERNML"

    fun trans typeinfo (name, member) = 
	case member of
	    AST.Method ty => 
	        let fun isVoid (Type.Void) = true
		      | isVoid _ = false
		    val parsty = map (fn(p,t)=>if p="value" then ("valu",t)
					       else (p,t)) 
				     (Type.getParams ty)
		    val args = if List.length parsty > 5
			       then [("mgtk_params", Type.Tname(N.fromPaths([],[],["int"])))]
			       else parsty
		    fun ubnd whre n =
			raise Skip("Unbound type name ("^whre^"): "^N.toString n)
		    fun f (par,Type.Void) = NONE
		      | f (par,ty) = 
			(SOME(TypeInfo.toCValue typeinfo ty (Var par))
			 handle TypeInfo.Unbound n => ubnd "parameters" n)
		    val parsty' = List.mapPartial f parsty
		    val ret = Type.getRetType ty

		    val isOut = TypeInfo.isOutput (fn _ => true) typeinfo
		    fun f ((par,ty),i) = 
			VDecl(if isOut ty then par^"_ref" else par,TValue,
			      SOME(Call("Field",NONE,[Var"mgtk_params",Int i])))
		    val extract = if List.length parsty > 5 
				  then List.map f (ListPair.zip(parsty,List.tabulate(List.length parsty, fn i=>i)))
				  else []
		    val (outputs,build) = 
			let fun f (p,t as Type.Output(_,ty)) = 
				SOME(VDecl(p,TypeInfo.toCType typeinfo ty,SOME(TypeInfo.toCValue typeinfo ty (Call("GetRefVal",NONE, [Var (p^"_ref")])))),
				     Exp(Call("SetRefVal",NONE,[Var(p^"_ref"),TypeInfo.fromCValue typeinfo ty (Var p)])))
			      | f (p,t) = NONE
			in  ListPair.unzip(List.mapPartial f parsty) end
			handle TypeInfo.Unbound n => ubnd "outputs" n
		    val (arrays_decl,arrays_setup) = 
			let fun f (p,t as Type.Array ty) =
				let val ety = TypeInfo.toCType typeinfo ty 
				    val cty = TypeInfo.toCType typeinfo t
				in SOME(VDecl(p,cty,NONE),
				     Exp(Call("list_to_array",NONE,
					  [VerbExp(showTy ety),Var p,Var(TypeInfo.tocvalue typeinfo ty),Var (p^"_arr")])))
				end
			      | f (p,t) = NONE
			in  ListPair.unzip(List.mapPartial f parsty) end
		    val call = TypeInfo.fromCValue typeinfo ret (Call(N.asCFunc name, NONE, parsty'))
			       handle TypeInfo.Unbound n => ubnd "call" n
		    val body = 
			Block(NONE,extract@outputs@arrays_decl,
			  Comment "ML" ::
                          (if isVoid ret then arrays_setup @ [Exp(call)] @ build @ [Return(Var("Val_unit"))]
			   else arrays_setup @ [Return(call)])
                        )
		    fun f (par,ty) = 
			(case ty of
			     Type.Output _ => par^"_ref"
			   | Type.Array _  => par^"_arr"
			   | _             => par,
			 TValue)
		in  [(Fun(Proto(extern,N.asCStub name, map f args, TValue), 
			  body),
		      SOME ty)]
		end

	  | AST.Enum(flag,enums) =>
	        let fun f (enum, (code, no)) =
			let val enum = N.asCEnumConst enum
			    val stmt = 
				Ass(Call("Field", NONE, [Var "res", Int no]),
				    TInt, Call("Val_int",NONE,[Var enum]))
			in  (stmt::code, no+1) end
		    val construct = List.foldl f ([],0) enums

		    val decls =
                        [ VDecl("res",TValue,
				SOME(Call("alloc_tuple",NONE,[Int(List.length enums)])))
                        ]
		    val body = 
			Block(NONE, decls,
			  Comment "ML" ::
			  rev(#1 construct) @
			  [Return(Var "res")]
                        )
		in  [(Fun(Proto(extern,N.asCGetEnum name,[("dummy",TValue)],TValue),
			  body),
		      SOME Type.Void)]
		end
	  | AST.Boxed(wrappers) =>
	        let val name = N.asCBoxed name
		in  [(Define(name^"_val(x)", "(((void*) Field(x, 1)))"),NONE),
		     (Define(name^"_val_nocast(x)", "(Field(x, 1))"),NONE),
		     (Fun(Proto(SOME"static","ml_finalize_"^name,[("val",TValue)],TVoid),
			  Block(NONE,[],
				[case wrappers of
				     SOME{release,...} =>
				        Exp(Call(release,NONE,[Call(name^"_val",NONE,[Var"val"])]))
				   | NONE => Comment"Empty"
                                ])),
		      NONE),
                     (Fun(Proto(NONE,"Val_"^name,[("obj",TStar TVoid)],TValue),
			  Block(NONE,[VDecl("res",TValue,NONE)],
			     Ass(Var"res", TValue, Call("alloc_final",NONE,[Int 2,Var("ml_finalize_"^name),Int 0, Int 1]))::
			     Ass(Call(name^"_val_nocast",NONE,[Var"res"]),TValue,
				 case wrappers of
				     SOME{copy,...} =>
				        Cast(TValue,Call(copy,NONE,[Var"obj"]))
				   | NONE =>
				        Cast(TValue,Var("obj"))
                                 )::
			     [Return(Var"res")]
                          )),
		      NONE)]
		end

	  | _ => []

    val trans = fn tinfo => fn (name, member) => trans tinfo (name, member)
		   handle Skip msg => ( TextIO.output(TextIO.stdErr,
		       "Error translating " ^ N.toString name ^ ": " ^msg^"\n")
                     ; [])
    fun generate typeinfo module = 
	AST.mapi (fn (module,info) => info, trans typeinfo) module

end
