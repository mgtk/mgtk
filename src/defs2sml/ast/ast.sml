structure AST :> AST =
struct

    datatype ('n,'i1,'i2) module = 
	Module of {name: 'n, members: ('n,'i1,'i2) member list, info: 'i1}
    and ('n,'i1,'i2) member =
        Sub of ('n,'i1,'i2) module
      | Member of {name: 'n, info: 'i2}


    fun mapi_module (f,g) current module =
	case module of 
	    Module {name,members,info} => 
		Module{name=name,members=map (mapi_member (f,g) name) members,
		       info=f(current,info)}
    and mapi_member (f,g) current member =
	case member of
	    Sub module => Sub(mapi_module (f,g) current module)
          | Member {name,info} => Member{name=name,info=g(name,info)}
    fun mapi (f,g) (m as Module{name,members,info}) = mapi_module (f,g) name m

    fun map (f,g) = mapi (fn (_,i1) => f i1, fn (_,i2) => g i2)

    fun filteri (f,g) (Module{name,members,info}) =
	if f(name,info) then 
	    SOME(Module{name=name, 
			members=List.mapPartial (filteri_member (f,g)) members,
			info=info})
	else NONE
    and filteri_member (f,g) (Sub module) = 
	Option.map Sub (filteri (f,g) module)
      | filteri_member (f,g) (m as Member{name,info}) = 
	if g(name,info) then SOME m else NONE
	
    exception Zip
(*
    fun exists p module = (* FIXME: what about the module info? *)
	case module of
	    Module {name,members,info} => List.exists (exists_member p) members
    and exists_member p member =
	case member of
	    Sub module => exists p module
          | Member {name,info} => p info

    fun zip (ma: (''n,'i1) module,mb: (''n,'i2) module) : (''n, 'i1*'i2) module =
	case (ma, mb) of
	    (Module{name=na,members=msa,info=ia},
	     Module{name=nb,members=msb,info=ib}) =>
		if na = nb
		then Module{name=na,
			    members=List.map zip_member
				         (ListPair.zip(msa,msb)),
			    info=(ia,ib)}
		else raise Zip
    and zip_member (ma: (''n,'i1) member,mb: (''n,'i2) member) : (''n, 'i1*'i2) member =
	case (ma, mb) of
	    (Sub ma, Sub mb) => Sub(zip (ma,mb))
          | (Member{name=na,info=ia},Member{name=nb,info=ib}) =>
		if na = nb then Member{name=na,info=(ia,ib)}
		else raise Zip
          | _ => raise Zip
*)

    fun fold (fmod, fmem) acc module = (* FIXME: what about module info? *)
	case module of Module{name,members,info} => 
	    List.foldl (fold_member (fmod,fmem)) (fmod ((name,members), acc)) members
    and fold_member (fmod, fmem) (member, acc) =
	case member of
	    Sub module => fold (fmod,fmem) acc module
	  | Member{name,info} => fmem ((name,info),acc)


    datatype ('n,'t) api_info =
	Method of 't
      | Field of 't
      | Boxed of {copy:string, release:string} option
      | Enum of bool (* flag? *) * 'n list
      | Signal of 't
    datatype pass = OUT | INOUT
    datatype api_type =
        ApiTy of string
      | ArrowTy of (string * api_type) list * api_type
      | Defaulted of api_type * string
      | Output of pass * api_type
      | Array of api_type

    type ('n,'t) ast_module = 
	 ('n, 
	  ('n*'n option*'n list) option,
	  ('n, 't) api_info
         ) module

    local 
	open Pretty
	fun ppI start ppn ppi (n,i) = ppString start ++ ppn n ++ ppi i
    in 

    fun pp (ppMod, ppMem) module =
	let fun loop (Module{name,members,info}) = 
		close(1,"end")
                  (always 4 ( ppMod(name,info) , ilist "#" loop' members ) )
	    and loop' (Sub module) = loop module
	      | loop' (Member{name,info}) = ppMem(name,info)
	in  loop module end

    fun ppAstType (ApiTy s) = Pretty.ppString s
      | ppAstType (Defaulted(ty,v)) = 
	  ppAstType ty ++ ("with "^+ ppString v)
      | ppAstType (Output(pass,ty)) =
	  ppAstType ty ++ ("[" ^+ ppString(case pass of INOUT => "in/out"
						      | OUT => "out") +^ "]")
      | ppAstType (ArrowTy (pars,ret)) =
	  let fun f (par,ty) = ppBinary(ppString par,":",ppAstType ty)
	  in  ppBinary(ilist " #* " f pars, "->", ppAstType ret) end
      | ppAstType (Array ty) =
	  ppAstType ty +^ " array"

    fun ppAst ppn ppt =
	let fun ppmodi NONE = empty
	      | ppmodi (SOME(ty, parent, impl)) =
		let val ty = ": " ^+ ppn ty
		    val parent = case parent of 
				     NONE => empty
				   | SOME ty => "extends " ^+ ppn ty
		    val impl = "implements " ^+ clist ",# " ppn impl
		in  ty ++ parent ++ impl end
	    fun ppmemi (Method ty) = ": method " ^+ ppt ty
	      | ppmemi (Field ty)  = ": field "  ^+ ppt ty
	      | ppmemi (Signal ty) = ": signal " ^+ ppt ty
	      | ppmemi (Boxed func) = ppString ": boxed"
	      | ppmemi (Enum(flag, ss)) =
		  (if flag then ": flag " else ": enum ")
                  ^+ bracket "{#}" (clist ",# " ppn ss)
	in  pp (ppI "module" ppn ppmodi, ppI "member" ppn ppmemi) end
    end (* local *)


end (* structure AST *)
