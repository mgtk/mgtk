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


    fun pp (ppMod, ppMem) outputter module =
	let fun spaces n = String.implode(List.tabulate(n, fn _ => #" "))
	    fun loop indent (Module{name,members,info}) =
		( outputter(spaces indent)
		; outputter (ppMod (name,info))
		; outputter "\n"
		; List.app (loop' (indent+4)) members
		)
	    and loop' indent (Sub(module)) = loop indent module
	      | loop' indent (Member{name,info}) =
		( outputter(spaces indent)
		; outputter (ppMem (name,info))
		; outputter "\n"
		)
	in  loop 0 module  end

    local 
	fun ppMod ppModI (n,i) = "module " ^ Name.toString n ^ ppModI i
	fun ppMem ppMemI (n,i) = "member " ^ Name.toString' n ^ ppMemI i
    in
	val ppName = fn (ppModI, ppMemI) => pp (ppMod ppModI, ppMem ppMemI)
    end

    local 
	fun ppMod ppModI (n,i) = "module " ^ n ^ ppModI i
	fun ppMem ppMemI (n,i) = "member " ^ n ^ ppMemI i
    in
        val ppString = fn (ppModI, ppMemI) => pp (ppMod ppModI, ppMem ppMemI)
    end

    datatype 't api_info =
	Method of 't
      | Field of 't
      | Boxed of {copy:string, release:string} option
      | Enum of string list
      | Signal of 't
    and api_type =
        ApiTy of string
      | ArrowTy of (string * api_type) list * api_type
       
end (* structure AST *)
