(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

functor GenCMosml(structure TypeInfo : TypeInfo) 
	:> GEN_C where type typeinfo = TypeInfo.typeinfo
= struct

    open TinyC
    structure N = Name
    structure TI = TypeInfo

    type ty = (N.name,N.name) Type.ty
    type 'a module  = (N.name, 'a, (N.name,ty) AST.api_info) AST.module
    type 'a module' = (N.name, 'a, (topdecl*SMLType.ty option) list) AST.module

    type typeinfo = TypeInfo.typeinfo

    fun print os module =
	let 
	    fun dump s = TextIO.output(os, s)
	    fun spaces n = String.implode(List.tabulate(n,fn _ => #" "))

	    fun print1 indent (decl, SOME ty) =
              ( dump ("/* ML type: "^ (SMLType.toString ty) ^" */\n")
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

    fun ubnd whre n = raise Skip("Unbound type name ("^whre^"): "^N.toString n)
    fun trans tinfo (name, member) = 
	case member of
	    AST.Method ty => 
	       (let fun isVoid (Type.Void) = true
		      | isVoid _ = false
		    val isOut = TI.isOutput (fn _ => true) tinfo
		    val isInOut = TI.isOutput (fn Type.INOUT => true
						| Type.OUT => false) tinfo
		    val isOnlyOut = TI.isOutput (fn Type.INOUT => false
						  | Type.OUT => true) tinfo

		    val parsty = map (fn(p,t)=>if p="value" then ("valu",t)
					       else (p,t)) 
				     (Type.getParams ty)
		    val (extract,args) = 
			let val parsty = List.filter (not o isOnlyOut o #2) parsty
			    fun f (i,(p,ty)) =
				VDecl(p,TValue,
				      SOME(Call("Field",NONE,
						[Var"mgtk_params",Int i])))
			in  if List.length parsty > 5 
			    then (List.map f (Util.number parsty), 
				  [("mgtk_params", Type.Tname(N.fromPaths([],[],["int"])))])
			    else ([],parsty)
			end
		    fun f (par,Type.Void) = NONE
		      | f (par,ty) = SOME(TI.toCValue tinfo ty (Var par))
		    val parsty' = List.mapPartial f parsty
		    val ret = Type.getRetType ty

(*
		    val (outputs,build) = 
			let fun f (p,t as Type.Output(_,ty)) = 
				SOME(VDecl(p,TI.toCType tinfo ty,SOME(TI.toCValue tinfo ty (Call("GetRefVal",NONE, [Var (p^"_ref")])))),
				     Exp(Call("SetRefVal",NONE,[Var(p^"_ref"),TI.fromCValue tinfo ty (Var p)])))
			      | f (p,t) = NONE
			in  ListPair.unzip(List.mapPartial f parsty) end
			handle TI.Unbound n => ubnd "outputs" n
*)
		    fun ccall name = fn e => TinyC.Call(name,NONE,[e])
		    val outputs_decl =
			let fun f (p,t as Type.Output(pass,ty)) =
				let val cty = TI.toCType tinfo ty
				    val init = case pass of
				        Type.INOUT => SOME(TI.toCValue tinfo ty (Var(p^"_in")))
				      | Type.OUT => NONE
				    fun deref e =
					ccall "*" (TinyC.Cast(TinyC.TStar cty,e))
				    val init = case ty of
				        Type.Tname _ => Option.map deref init
				      | _ => init
				in  SOME(VDecl(p,cty,init)) end
			      | f (p,t) = NONE
			in  List.mapPartial f parsty end
		    val (outputs_build,ret) =
			let val outputs = List.filter (TI.isOutput(fn _ => true) tinfo o #2) parsty
			    val len = List.length outputs

			    fun f (p,t) = (TI.fromCValue tinfo t (Var p),
					   TI.toCType tinfo t)
			    val outputs' = 
				(if isVoid ret then [] 
				 else [(Var"res",TI.toCType tinfo ret)])
                                @ List.map f outputs
			    val len' = List.length outputs'
			    fun g (i,(e,t)) =
				Ass(Call("Field", NONE, [Var "result", Int i]),
				    t, e)
			in  if len > 0 andalso len' > 1 then
				(Ass(Var "result", TValue,
				      Call("alloc_tuple",NONE,[Int(len')]))
				 :: List.map g (Util.number outputs'),
				 ret)
			    else if len = 1 andalso len' = 1 then
                                ([Ass(Var "result", TValue, #1(f(hd outputs)))], ret)
			    else
				([],ret)
			end
		    val (arrays_decl,arrays_setup) = 
			let fun f (p,t as Type.Array ty) =
				let val ety = TI.toCType tinfo ty 
				    val cty = TI.toCType tinfo t
				in SOME(VDecl(p,cty,NONE),
				     Exp(Call("list_to_array",NONE,
					  [VerbExp(showTy ety),Var p,Var(TI.tocvalue tinfo ty),Var (p^"_arr")])))
				end
			      | f (p,t) = NONE
			in  ListPair.unzip(List.mapPartial f parsty) end
		    val call = TI.fromCValue tinfo ret (Call(N.asCFunc name, NONE, parsty'))
		    val body = 
			Block(NONE,VDecl("result",TValue,NONE)::VDecl("res",TValue,NONE)::extract@outputs_decl@arrays_decl,
			  Comment "ML" ::
                          arrays_setup @
                          (* this silly code
			          result = Val_unit; 
				  return result;
                             is recognized by the PP and prettified *)
                          (if isVoid ret 
			   then if length outputs_build > 0 then [Exp(call)]
				else [Exp(call),Ass(Var"result",TValue,Var"Val_unit")]
			   else [Ass(Var(if length outputs_build > 0 then "res" else "result"), TValue, call)]
                          ) @ outputs_build @
			  [Return(Var"result")]
                        )
		    fun f (par,ty) = 
			case ty of
			    Type.Output(Type.INOUT,_) => SOME(par^"_in",TValue)
			  | Type.Output(Type.OUT,  _) => NONE
			  | Type.Array _  => SOME(par^"_arr",TValue)
			  | _             => SOME(par,TValue)
		in  [(Fun(Proto(extern,N.asCStub name, List.mapPartial f args, TValue), 
			  body),
		      SOME(TI.toPrimType tinfo (Type.Func(args,ret))))]
		end handle TI.Unbound n => ubnd "method" n)

(*

  /* ML type: unit */
  EXTERNML value mgtk_get_gtk_window_type(value dummy) { /* ML */
      value res = alloc_tuple(2);
      Field(res, 0) = Val_int(GTK_WINDOW_TOPLEVEL);
      Field(res, 1) = Val_int(GTK_WINDOW_POPUP);
      return res;
  }

*)
             
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
		    val ty =  
			SMLType.ArrowTy([SMLType.UnitTy], 
					SMLType.TupTy(List.map (fn _ => SMLType.IntTy) enums))
		in  [(Fun(Proto(extern,N.asCGetEnum name,[("dummy",TValue)],TValue),
			  body),
		      SOME(ty))]
		end

(*

  #define GtkTreeIter_val(x) (( ( void* ) Field(x, 1)))

  #define GtkTreeIter_val_nocast(x) (Field(x, 1))

  static void ml_finalize_GtkTreeIter(value val) {
    gtk_tree_iter_free(GtkTreeIter_val(val));
  }

  value Val_GtkTreeIter(void* obj) {
    value res;
    res = alloc_final(2, ml_finalize_GtkTreeIter, 0, 1);
    GtkTreeIter_val_nocast(res) = (value) gtk_tree_iter_copy(obj);
    return res;
  }

*)

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
