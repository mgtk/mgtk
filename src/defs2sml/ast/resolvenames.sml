structure ResolveNames =
struct

    val toLower = String.map Char.toLower

    fun toName split current name = 
	let val splits = split name
	    fun loop (e::p,e'::p') acc = 
		if toLower e = toLower e' then loop (p,p') acc
		else (rev acc, e'::p')
	      | loop ([], p') acc = (rev acc, p')
	      | loop (_, []) acc = ([],rev acc) (* FIXME *)
	    val (path,base) = loop (current, splits) []
        in  (path,base)
	end

    open AST
    fun resolve module =
	let 
	    val show_path = Util.stringSep "{" "}" "-" (fn s => s)
		    
		
	    fun splitfcn info = 
                case info of
                    Method _ => Name.separateUnderscores
	          | Field _ => (fn s => [s])
		  | i => Name.separateWords

	    fun resMod current m =
	        case m of
		    Module{name,members,info} =>
			let val (path,base) = toName Name.separateWords current name
(*
			    val _ = Util.debug("module: " ^ name ^ " (at " ^ show_path current ^ ") -> " ^ show_path path ^ ":" ^ show_path base ^ "\n")
*)
			    val current' = Name.separateWords name
			in  Module{name=Name.fromPaths(current,path,base),
				   members=List.map (resMem current') members,
				   info=info}
			end
	    and resMem current m =
		case m of
		    Sub m => Sub(resMod current m)
	          | Member{name,info} => 
			let val (path,base) = toName (splitfcn info) current name
(*
			    val _ = TextIO.print("member: " ^ name ^ " (at " ^ show_path current ^ ") -> " ^ show_path path ^ ":" ^ show_path base ^ "\n")
*)
			in  Member{name=Name.fromPaths(current,path,base),
				   info= resMemInfo current info}
			end
	    and resMemInfo current (Enum enums) =
		let fun res e = 
			let val (p,b) = toName (Name.separateUnderscores) current e
			in  Util.stringSep "" "" "_" (fn s=>s) b end
		in  Enum(List.map res enums) end
	      | resMemInfo current info = info
	in  resMod [] module
	end

    fun modularize module =
	let fun modMod m =
		case m of 
		    Module{name={path,base,fullpath},members,info} =>
		       Module{name=Name.fromPaths (fullpath,[],base),
			      members=List.map (modMem) members,
			      info=info}
	    and modMem m =
		case m of
		    Sub m => Sub(modMod m)
		  | Member{name={path,base,fullpath},info} =>
			Member{name=Name.fromPaths(fullpath,[],base),info=info}
	in  modMod module
	end

    (* For debugging: *)
    val resolve = fn module => 
        let val print = TextIO.print

	    val module' = resolve module
	in  if Debug.included "ResolveNames.debug_resolve_names" then
		( print("After resolving names:\n")
		; AST.ppName (fn _ =>"", fn _=>"") print module')
	    else ()
          ; module'
        end

end (* structure ResolveNames *)

val _ = Debug.add {name="ResolveNames.debug_resolve_names",
		   short_option="drn",long_option=SOME("debug-resolve-names"),
		   included=false}