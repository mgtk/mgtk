(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

structure FromDefs :> FromDefs = struct

    open Defs

    structure Map = Splaymap
    structure A = AST

    type module_info = (AST.api_type * AST.api_type option) option
    type member_info = (string,AST.api_type) AST.api_info

    datatype module = 
	Module of {name: string, members: member list ref, info: module_info}
    and member =
        Sub of module
      | Member of {name: string, info: member_info}
    type map = (string, member list) Map.dict

    fun pairmap f (x,y) = (f x, f y)
    val empty = Map.mkDict (String.compare o pairmap Name.toLower) (* FIXME *)
    fun new map name r = Map.insert(map, name, r)
    fun insert map name mem = 
	case Map.peek (map, name) of
	    SOME r => (r := mem :: !r; map)
	  | NONE => new map name (ref [])

    fun trans top (def, map) =
	let val name = getName def handle AttribNotFound _ => #1 def
	    fun probableModule nothing split name = 
		(* look for modules "matching" name:
		   - functions beginning with xxx_yyy_zzz_... when
                     XxxYyyZzz is a known module matches that module
                   - enums beginning with XxxYyyZzz... when
                     XxxYyyZzz is a known module matches that module
                *)
		let val words = split name
		    fun loop [] acc = nothing
		      | loop (ith::rest) acc = 
			let val ith = Name.capitalize ith
			    val probable = String.concat(rev(ith::acc))
			in  if Map.peek(map,probable) = NONE then
				loop rest (ith::acc)
			    else probable
			end
		    val module = loop (tl words) [Name.capitalize(hd words)]
		in  if module = nothing then
			( MsgUtil.warning("Demoting "^name^" to top-level module " ^nothing)
			  ; nothing)
		    else module
		end

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
			  (* okay, so this isn't a constructor; look for
                             a probable module *)
			  probableModule top Name.separateUnderscores name
		       val isConst = 
			   (getConstructor def; true)
			   handle AttribNotFound _ =>
		              let open Substring in
				  not(isEmpty(#2(position "_new_" (all name))))
			      end
		       val rt = if not(md = top) andalso isConst 
				then SOME md
				else NONE
		       val mem = Member{name=name,
					info=A.Method(functype NONE rt def)}
		   in  insert map md mem end
	      | Method => 
		   let val md = getObject def
		       val mem = Member{name=name,
					info=A.Method(functype (SOME md) NONE def)}
		   in  insert map md mem end
	      | Enum =>
		   let val md = probableModule (getModule def) Name.separateWords name
		       val mem = Member{name=name,info=A.Enum(getValues def handle AttribNotFound _ => [])}
		   in  insert map md mem end
	      | Signal =>
		   let val md = getObject def
		       val mem = Member{name=name,
					info=A.Signal(functype NONE NONE def)}
		   in  insert map md mem end
	      | Boxed => 
		   let val md = getModule def
		       val copyrel =
			   SOME({copy=getCopyFunc def,release=getReleaseFunc def})
			   handle AttribNotFound _ => NONE
		       val mem = Member{name=name,info=A.Boxed copyrel}
		   in  insert map md mem end
	end
    and functype self rettype def =
	let fun addself ps = case self of NONE => ps
					| SOME s => ("self",A.ApiTy s) :: ps
	    fun nonempty [] = [("dummy",A.ApiTy "void")]
	      | nonempty ps = ps
	    val return = A.ApiTy(case rettype of NONE => getReturnType def
					       | SOME rt => rt)
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

    (* For debugging: *)
    val fromDefs = fn top => fn defs => 
        let fun pptype (AST.ApiTy s) = s
	      | pptype (AST.ArrowTy (pars,ret)) =
		Util.stringSep "[" ("] -> "^pptype ret) ", " (fn (s,ty)=>s^":"^pptype ty) pars
	    fun ppmodi (SOME(ty, parent)) = 
		": " ^ pptype ty ^
		   (case parent of NONE => "" | SOME ty => " extends " ^ pptype ty)
	      | ppmodi NONE = ""
	    fun ppmemi (A.Method ty) = ": method " ^ pptype ty
	      | ppmemi (A.Field ty) = ": field " ^ pptype ty
	      | ppmemi (A.Enum ss) = ": enum" ^ Util.stringSep "{" "}" ", " (fn s=>s) ss
	      | ppmemi (A.Boxed _) = ": boxed"
	      | ppmemi (A.Signal ty) = ": signal " ^ pptype ty
	    val print = TextIO.print

	    val module' = fromDefs top defs
	in  if Debug.included "FromDefs.debug_defs" then
		( print("After building defs:\n")
		; AST.ppString (ppmodi, ppmemi) print module')
	    else ()
          ; module'
        end

end (* structure FromDefs *)

val _ = Debug.add {name="FromDefs.debug_defs",
		   short_option="dfd",long_option=SOME("debug-from-defs"),
		   included=false}