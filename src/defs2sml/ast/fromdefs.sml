(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

structure FromDefs :> FromDefs = struct

    open Defs

    structure Map = Splaymap
    structure Set = Splayset
    structure A = AST

    type module_info = string option (* name *)
    type member_info = (string,AST.api_type) AST.api_info

    datatype module = 
	Module of {name: string, members: member list ref, info: module_info}
    and member =
        Sub of module
      | Member of {name: string, info: member_info}
    type map = (string, member list) Map.dict
    fun show (Sub(Module{name,...})) = "module " ^ name
      | show (Member{name,...}) = "member " ^ name

    fun pairmap f (x,y) = (f x, f y)
    val empty : (string, module) Map.dict
      = Map.mkDict (String.compare o pairmap Name.toLower) (* FIXME *)
    fun insert map name mem = 
	case Map.peek (map, name) of
	    SOME(Module{name,members,info}) => 
	      (members := mem :: !members; map)
	  | NONE => ( print("Unbound module "^name^" for "^show mem^"\n")
		    ; map) (* FIXME *)
    fun new map name parent info = 
	let val md = Module{name=name,info=info,members=ref[]}
	    val map = case parent of NONE => map
				   | SOME p => insert map p (Sub md)
	in  case Map.peek(map,name) of
		NONE => Map.insert(map, name, md) 
	      | SOME _ => ( TextIO.print("Already there - doing nothing\n")
                          ; map )
	end

    val demotedModuleName = "GtkDemoted"

    fun lookupParent map default name=
	case Map.peek(map,name) of
	    SOME parent => parent
	  | NONE => default

    fun transPhase1 top (metadata as (typemap,parentmap)) (def, map) = 
	let val name = getName def handle AttribNotFound _ => #1 def
	in  case getTag def of
		Object => 
		   let val md = lookupParent parentmap (getModule def) name
		       val parent = SOME(getParent def)
			            handle AttribNotFound _ => NONE
		       val implements = getImplements def
		       val info = (name,parent,implements)
		       val mem = Member{name=name,info=A.Object info}
		   in  insert (new map name (SOME md) (* parent *) NONE)
		              name mem

		   end
	      | Boxed => 
		   let val md = lookupParent parentmap (getModule def) name
		       val copyrel =
			   SOME({copy=getCopyFunc def,release=getReleaseFunc def})
			   handle AttribNotFound _ => NONE
		       val mem = Member{name=name,info=A.Boxed copyrel}
		   in  insert (new map name (SOME md) (* parent *) NONE)
			      name mem
		   end
	      | _ => map
	end

    fun transPhase2 top (typemap,parentmap) (def, map) =
	let val name = getName def handle AttribNotFound _ => #1 def
	    fun probableModule demote nothing split name = 
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
		in  if module = nothing andalso demote then
			( MsgUtil.warning("Demoting "^name^" to top-level module " ^nothing)
			  ; demotedModuleName)
		    else module
		end
	    fun getMeta md = Map.peek(typemap, md^"."^name)

	in  case getTag def of
		Function =>
		   (
		   let val md = 
		       (getConstructor def)
		       handle AttribNotFound _ => 
			  (* okay, so this isn't a constructor; look for
                             a probable module *)
			  let val md = probableModule true top Name.separateUnderscores name
			  in  if md = top then (getObject def handle AttribNotFound _ => top)
			      else md
			  end
		       val isConst = 
			   (getConstructor def; true)
			   handle AttribNotFound _ =>
		              let open Substring in
				  not(isEmpty(#2(position "_new_" (all name))))
			      end
		       val rt = if not(md = top) andalso isConst 
				then SOME md
				else NONE
		       val ty = functype (getMeta md) NONE rt def
		       val mem = Member{name=name, info=A.Method ty}
		   in  if isDeprecated def then map
		       else insert map md mem
		   end
		       handle AttribNotFound msg => 
			      ( TextIO.print("Problems ("^msg^") with " ^ name)
			      ; raise AttribNotFound msg)
                   )
	      | Method => 
                   (
		   let val md = getObject def
		       val ty = functype (getMeta md) (SOME md) NONE def
		       val mem = Member{name=name,info=A.Method ty}
		   in  if isDeprecated def then map
		       else insert map md mem
		   end
		       handle AttribNotFound msg => 
			      ( TextIO.print("Problems ("^msg^") with " ^ name)
			      ; raise AttribNotFound msg)
                   )
	      | Enum flag =>
		   let val md = probableModule false (getModule def) Name.separateWords name
		       val mem = Member{name=name,info=A.Enum(flag,getValues def handle AttribNotFound _ => [])}
		   in  insert map md mem end
	      | Signal =>
		   let val md = getObject def
		       val mem = Member{name=name,
					info=A.Signal(functype (getMeta md) NONE NONE def)}
		   in  insert map md mem end
	      | _ => map
	end
    and functype metadata self rettype def =
	let fun addself ps = case self of NONE => ps
					| SOME s => ("self",A.ApiTy s) :: ps
	    fun nonempty [] = [("dummy",A.ApiTy "void")]
	      | nonempty ps = ps
	    val return = A.ApiTy(case rettype of NONE => getReturnType def
					       | SOME rt => rt)
	    fun applyMeta params meta =
		let fun lookup default name =
			let fun loop [] = default
			      | loop ((n,t,f)::rest) = 
				if n = name then (t,f) else loop rest
			in  loop meta end 
		    fun one (n,t,f) = let val (t,f) = lookup (t,f) n
				      in  (n,t,f) end
		in  map one params end
	    fun applyFlags fs ty =
		let 
		    fun mkPass (OUT) = A.OUT | mkPass (INOUT) = A.INOUT
		    fun removedef ty =
			case ty of
			    A.ApiTy _ => ty
			  | A.Defaulted(ty,v) => ty
			  | A.Output(p,ty) => removedef ty
			  | A.Array ty => removedef ty
			  | A.ArrowTy _ => Util.abort 87456
		    fun f (f, ty) = 
			case f of NullOk => A.Defaulted(removedef ty,"NULL")
				| Default v => A.Defaulted(removedef ty,v)
				| Output p => A.Output(mkPass p, ty)
				| Array => A.Array ty
		in  List.foldl f ty fs
		end
	    val params = (getParameters def handle AttribNotFound _ => [])
	    val params = case metadata of
			     NONE => params
			   | SOME{params=p,...} => applyMeta params p
	    val params = map (fn(n,t,f)=>(n,applyFlags f (A.ApiTy t))) params
	in  A.ArrowTy(nonempty(addself params), return) end

    fun interpretMetadata map defs =
	let fun one (MetaOverride(name,attribs), ((typemap,parentmap),map)) =
		let val obj = getMetaObject attribs
		    val params = getMetaParams attribs 
			         handle AttribNotFound _ => []
		    val rettype = SOME(getMetaReturnType attribs)
			          handle AttribNotFound _ => NONE
		in  ((Map.insert(typemap, obj^"."^name, 
				 {params=params,rettype=rettype}),
		      parentmap),
		     map)
		end
	      | one (MetaExclude _, maps) = maps
	      | one (MetaModule(name,parent,members),((typemap,parentmap),map))=
		let val parentmap = Map.insert(parentmap,name,parent)
		    val parentmap =
			List.foldl (fn (mem,map) => Map.insert(map,mem,name))
			           parentmap
				   members
		in  ((typemap, parentmap),new map name (SOME parent) NONE)
		end
	in  List.foldl one 
	               ((Map.mkDict String.compare,Map.mkDict String.compare),
			map)
		       defs
	end
		     
    fun fromDefs top defs metadefs
	: (string,module_info,member_info) AST.module =
	let val map = new empty top NONE NONE
	    val (md,map) = interpretMetadata map metadefs
	    val map = new map demotedModuleName (SOME top) NONE
		      (* first look for all module definers *)
	    val map = List.foldl (transPhase1 top md) map defs
		      (* then populate the modules *)
	    val map = List.foldl (transPhase2 top md) map defs
	    fun convert (Module{name,members=mbs,info}) =
		A.Module{name=name,info=info,
			 members=List.map convert' (rev(!mbs))}
	    and convert' (Sub module) = A.Sub (convert module)
	      | convert' (Member{name,info}) = A.Member{name=name,info=info}
	in  convert(Map.find(map,top)) end

    (* For debugging: *)
    local open Pretty in
    val fromDefs = fn top => fn defs => fn metadefs =>
        let val pp = AST.ppAst ppString AST.ppAstType
	    val module' = fromDefs top defs metadefs
	in  if Debug.included "FromDefs.debug_defs" then
		( print("After building defs:\n")
		; ppPlain (pp module') TextIO.stdOut)
	    else ()
          ; module'
        end
    end (* local *)

end (* structure FromDefs *)

val _ = Debug.add {name="FromDefs.debug_defs",
		   short_option="dfd",long_option=SOME("debug-from-defs"),
		   included=false}