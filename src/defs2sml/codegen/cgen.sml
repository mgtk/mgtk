(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

structure GenC :> sig
        type topdecl
	type typeexp = Name.name Type.ty
        type 'a module  = (Name.name, 'a, typeexp AST.api_info) AST.module
        type 'a module' = (Name.name, 'a, (topdecl*typeexp) option) AST.module
        val generate: 'a module -> 'a module'
        val print: TextIO.outstream -> 'a module' -> unit
    end =
struct

    open TinyC

    type typeexp = Name.name Type.ty
    type 'a module  = (Name.name, 'a, typeexp AST.api_info) AST.module
    type 'a module' = (Name.name, 'a, (topdecl*typeexp) option) AST.module


    fun print os module =
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
		    ( dump ("/* ML type: "^ SMLType.show (SMLType.primtypeFromType ty) ^" */\n")
                    ; dump (toString (spaces indent) decl)
                    )
		  | AST.Member{name,info=NONE} => ()
	in  print_module 0 module
	end


    fun call func arg = Call(func, NONE, [arg])

    fun toCValue ty =
	case ty of
	    Type.Base base =>
	       (case base of
		    "int" => call "Int_val"
		  |  "uint" => call "Int_val"
		  |  "float" => call "Double_val"
		  |  "double" => call "Double_val"
		  |  "bool" => call "Bool_val"
		  |  "char" => call "Int_val" (* FIXME: ? *)
		  |  _ => raise Fail("toCValue: unknown base type ("^base^")")
		)
	  | Type.Ptr(Type.Base "char") => call "String_val"
	  | Type.Ptr(Type.Tname n) => call "GtkObj_val"
	  | Type.Const ty => toCValue ty
	  | Type.Tname n => if Name.asType n = "gtype" then call "Val_int"
			    else call "GtkObj_val" (* FIXME: ? *)
	  | Type.Void => raise Fail "toCValue: shouldn't happen (Void)"
	  | Type.Ptr _ => raise Fail "toCValue: not implemented (Ptr)"
	  | Type.Arr _ => raise Fail "toCValue: not implemented (Arr)"
	  | Type.Func _ => raise Fail "toCValue: shouldn't happen (Func)"

    fun fromCValue ty =
	case ty of 
	    Type.Base base =>
	       (case base of
		    "int" => call "Val_int"
		  | "uint" => call "Val_int"
		  | "float" => call "copy_double"
		  | "double" => call "copy_double"
		  | "bool" => call "Val_bool"
		  | "char" => call "Val_int" (* FIXME: ? *)
		  | _ => raise Fail("fromCValue: unknown base type ("^base^")")
                )
	  | Type.Ptr(Type.Base "char") => call "copy_string"
	  | Type.Ptr(Type.Tname n) => call "Val_GtkObj"
	  | Type.Tname n => if Name.asType n = "gtype" then call "Val_int"
			    else call "Val_GtkObj" (* FIXME: ? *)
	  | Type.Const ty => fromCValue ty
	  | Type.Void => (fn e => e)
	  | Type.Ptr _ => raise Fail "fromCValue: not implemented (Ptr)"
	  | Type.Arr _ => raise Fail "fromCValue: not implemented (Arr)"
	  | Type.Func _ => raise Fail "fromCValue: shouldn't happen (Func)"

    exception Skip of string

    fun trans (name, member) = 
	case member of
	    AST.Method ty => 
	        let fun isVoid (Type.Void) = true
		      | isVoid _ = false
		    val parsty = map (fn(p,t)=>if p="value" then ("valu",t)
					       else (p,t)) 
				     (Type.getParams ty)
		    val args = if List.length parsty > 5
			       then [("mgtk_params", Type.Base "int")]
			       else parsty
		    fun f (par,Type.Void) = NONE
		      | f (par,ty) = SOME(toCValue ty (Var par))
				     handle Fail m => raise Skip m
		    val parsty' = List.mapPartial f parsty
		    val ret = Type.getRetType ty

		    fun f ((par,ty),i) = VDecl(par,TValue,SOME(Call("Field", NONE, [Var "mgtk_params", Int(Int.toString i)])))
		    val extract = if List.length parsty > 5 
				  then List.map f (ListPair.zip(parsty,List.tabulate(List.length parsty, fn i=>i)))
				  else []
		    val call = fromCValue ret (Call(Name.asCFunc name, NONE, parsty'))
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
	  | _ => NONE

    val trans = fn (name, member) => trans (name, member)
		   handle Skip msg => ( TextIO.output(TextIO.stdErr,
		       "Error translating " ^ Name.toString name ^ ": " ^msg^"\n")
                     ; NONE)
    fun generate module = 
	AST.mapi (fn (module,info) => info, trans) module

end
