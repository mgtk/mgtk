(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

structure FromDefs :> FromDefs = struct

    open Defs

    structure Map = Splaymap
    structure A = AST

    type module_info = (AST.api_type * AST.api_type option) option
    type member_info = AST.api_type AST.api_info

    datatype module = 
	Module of {name: string, members: member list ref, info: module_info}
    and member =
        Sub of module
      | Member of {name: string, info: member_info}
    type map = (string, member list) Map.dict

    val empty = Map.mkDict String.compare
    fun new map name r = Map.insert(map, name, r)
    fun insert map name mem = 
	case Map.peek (map, name) of
	    SOME r => (r := mem :: !r; map)
	  | NONE => new map name (ref [])

    fun trans top (def, map) =
	let val name = getName def handle AttribNotFound _ => #1 def
	in  case getTag def of
		Object => 
		   let val md = getModule def
		       val r = ref []
		       val parent = SOME(A.ApiTy(getParent def))
			            handle AttribNotFound _ => NONE
		       val mem = Sub(Module{name=name,members=r,
					    info=SOME(A.ApiTy name,parent)})
		   in  new (insert map md mem) name r end
	      | Function =>
		   let val md = 
		       (getConstructor def)
		       handle AttribNotFound _ => 
			  (* okay, so this isn't a constructor; special-case
                             some functions
                                XXX_get_type
                                XXX_new_YYY
			     that need to go in the XXX module as well.
                          *)
			  let val n = Substring.all name
			      val (gt_md,gt) = Substring.position "_get_type" n
			      val (nw_md,nw) = Substring.position "_new_" n
			      fun asM ss =
				  Name.asModule(Name.fromPaths([],[], 
                                    Name.separateUnderscores(
				      Substring.string ss)))
			  in  if not(Substring.isEmpty gt) then (* XXX_get_type *)
				  asM gt_md
			      else if not(Substring.isEmpty nw) then (* XXX_new_YYY *)
				  asM nw_md
			      else top
			  end
		       val mem = Member{name=name,
					info=A.Method(functype NONE def)}
		   in  insert map md mem end
	      | Method => 
		   let val md = getObject def
		       val mem = Member{name=name,
					info=A.Method(functype (SOME md) def)}
		   in  insert map md mem end
	      | Enum =>
		   let val md = getModule def
		       val mem = Member{name=name,info=A.Enum(getValues def handle AttribNotFound _ => [])}
		   in  insert map md mem end
	      | Signal =>
		   let val md = getObject def
		       val mem = Member{name=name,
					info=A.Signal(functype NONE def)}
		   in  insert map md mem end
	      | Boxed => 
		   let val md = getModule def
		       val copy = SOME(getCopyFunc def)
			          handle AttribNotFound _ => NONE
		       val rel =  SOME(getReleaseFunc def)
			          handle AttribNotFound _ => NONE
		       val mem = Member{name=name,
					info=A.Boxed{copy=copy,release=rel}}
		   in  insert map md mem end
	end
    and functype self def =
	let fun addself ps = case self of NONE => ps
					| SOME s => ("self",A.ApiTy s) :: ps
	    fun nonempty [] = [("dummy",A.ApiTy "void")]
	      | nonempty ps = ps
	    val return = A.ApiTy(getReturnType def)
	    val params = map (fn(n,t)=>(n,A.ApiTy t)) 
			     (getParameters def handle AttribNotFound _ => [])
	in  A.ArrowTy(nonempty(addself params), return) end

    fun fromDefs top defs : (string,module_info,member_info) AST.module =
	let val map = List.foldl (trans top) (new empty top (ref[])) defs
	    fun convert () =
		let val ast = Map.find (map, top)
		    fun loop (Module{name,members,info}) = 
			A.Module{name=name,members=List.map loop' (rev (!members)),info=info}
		    and loop' (Sub module) = A.Sub(loop module)
		      | loop' (Member{name,info}) = A.Member{name=name,info=info}
		in  loop (Module{name=top,members=ast,info=NONE}) end
	in  convert () end

(*
    (* For debugging: *)
    val fromDefs = fn top => fn defs => 
        let fun pptype (AST.ApiTy s) = s
	      | pptype (AST.ArrowTy (pars,ret)) =
		Util.stringSep "[" ("]"^pptype ret) ", " (fn (s,ty)=>s^":"^pptype ty) pars
	    fun ppmodi (SOME(ty, parent)) = 
		": " ^ pptype ty ^
		   (case parent of NONE => "" | SOME ty => " extends " ^ pptype ty)
	      | ppmodi NONE = ""
	    fun ppmemi (AST.Method ty) = ": method " ^ pptype ty
	      | ppmemi (AST.Field ty) = ": field " ^ pptype ty
	      | ppmemi (AST.Enum ss) = ": enum" ^ Util.stringSep "{" "}" ", " (fn s=>s) ss
	      | ppmemi (AST.Signal ty) = ": signal " ^ pptype ty
	    val print = TextIO.print

	    val module' = fromDefs top defs
	in  print("After resolving types:\n")
          ; AST.ppString (ppmodi, ppmemi) print module'
          ; module'
        end
*)
end (* structure FromDefs *)