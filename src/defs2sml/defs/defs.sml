(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

structure Defs = struct

    type type_exp = string
    datatype pass = OUT | INOUT
    datatype type_flags = NullOk | Default of string | Output of pass
    type type_name_list = (type_exp * string * type_flags list) list

    type value_list = (string * string) list

    datatype when = Unknown | First | Last
    datatype attrib =
	Module of string
      | Parent of string
      | CName of string
      | TypeID of string
      | Fields of type_name_list
      | CopyFunc of string
      | ReleaseFunc of string
      | Constructor of string
      | OfObject of string
      | ReturnType of type_exp
      | Params of type_name_list
      | Values of value_list
      | Deprecated
      | Varargs of bool
      | CallerOwnsReturn of bool
      | When of when
      | Implements of string

    exception AttribNotFound of string

    fun lookup attrib p =
	let fun loop [] = raise AttribNotFound attrib
	      | loop (a::ats) = case p a of SOME v => v | NONE => loop ats
	in  loop
	end
    fun lookupAll attrib p attribs = List.mapPartial p attribs

    datatype def_tag =
	Object
      | Boxed
      | Enum of bool (* flags? *)
      | Function
      | Method
      | Signal

    type definition = string * def_tag * attrib list

    fun getTag (def: definition) = #2 def

    fun getName (def: definition) = 
	let val atts = #3 def
	in  lookup "c-name" (fn (CName n) => SOME n | _ => NONE) atts
	end
    fun getModule (def: definition) =
	let val atts = #3 def
	in  lookup "in-module" (fn (Module n) => SOME n | _ => NONE) atts
	end
    fun getObject (def: definition) =
	let val atts = #3 def
	in  lookup "of-object" (fn (OfObject n) => SOME n | _ => NONE) atts
	end
    fun getMetaObject (atts: attrib list) = 
	lookup "of-object" (fn (OfObject n) => SOME n | _ => NONE) atts

    fun getParent (def: definition) =
	let val atts = #3 def
	in  lookup "parent" (fn (Parent n) => SOME n | _ => NONE) atts
	end

    fun getConstructor (def: definition) = 
	let val atts = #3 def
	in  lookup "constructor-of" 
		   (fn (Constructor n) => SOME n | _ => NONE) atts
	end
    fun getReturnType  (def: definition) = 
	let val atts = #3 def
	in  lookup "return-type" (fn (ReturnType n) => SOME n | _ => NONE) atts
	end
    fun getMetaReturnType (atts: attrib list) = 
	lookup "return-type" (fn (ReturnType n) => SOME n | _ => NONE) atts
    fun getParameters  (def: definition) = 
	let val atts = #3 def
	in  lookup "params" (fn (Params ps) => SOME(List.map(fn(t,n,f)=>(n,t,f))ps)
			      | _ => NONE) atts
	end
    fun getMetaParams (atts: attrib list) = 
	lookup "params" (fn (Params ps) => SOME(List.map(fn(t,n,f)=>(n,t,f))ps)
			  | _ => NONE) atts
    fun getValues  (def: definition) = 
	let val atts = #3 def
	in  lookup "value"
		   (fn (Values vs) => SOME (List.map(fn(a,v)=>v)vs)
		     | _ => NONE) atts
	end
    fun getCopyFunc (def: definition) = 
	let val atts = #3 def
	in  lookup "copy-func" 
		   (fn (CopyFunc n) => SOME n | _ => NONE) atts
	end
    fun getReleaseFunc (def: definition) = 
	let val atts = #3 def
	in  lookup "release-func" 
		   (fn (ReleaseFunc n) => SOME n | _ => NONE) atts
	end
    fun getImplements (def: definition) = 
	let val atts = #3 def
	in  lookupAll "implements"
	              (fn (Implements n) => SOME n | _ => NONE) atts
	end

    type override = attrib (* but only OfObject, ReturnType, and Params
                              should occur *)
    datatype metadata =
	MetaExclude of string list
      | MetaOverride of string * override list

end (* structure Defs *)