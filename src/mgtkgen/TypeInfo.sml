(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

(* TODO: Clean-up this code and document it! *)

structure TypeInfo :> TypeInfo =
struct

    (* Convenience: *)
    structure U  = Util
    structure TE = TypeExp
    structure NU = NameUtil

    open WSeq
    infix &&
	
    type texp = TE.texp
    type tname = TE.tname
    type name = NU.name


(* new
    fun MLNamePath path name = prsep ($".") $ path && $"." && $name
new *)

    fun combine path = $$ path
    fun underscore path = prsep ($"_") $ path

    fun MLNamePath (path, base) = combine (path @ base) 
    fun MLUnderscorePath (path, base) = underscore (path @ base)

    fun mapPath f (path,base) = (map f path, map f base)

    fun fstPath (path,base) = (path,[])
    fun sndPath (path,base) = ([],base)

    fun addPre  pre name  = $pre && name 
    fun addSuff suff name = name && $suff

    fun tlPath (path,base) = 
	case path of 
	    [] => Util.shouldntHappen "tlPath: Empty path"
	|   _ => let val (pre,rest) = (hd path, tl path)
		 in  if NU.toLower pre = "gdk" then (path, base)
		     else (rest, base)
		 end

    fun MLName (TE.PRIMTYPE tName) = 
	(case tName of
	     "none" => ([],["unit"])
	   |   "int" => ([],["int"])
	   |   "uint" => ([],["word"])
	   |   "float" => ([],["real"])
	   |   "bool" => ([],["bool"])
	   |   "string" => ([],["string"])
	   |   "static_string" => ([],["string"])
	   |   "GtkType" => (["Gtk"], ["type"])
           |   _ => U.shouldntHappen "MLName: unknown primtive type"
	)
      | MLName (TE.WIDGET ((path, wid),_)) = (path, wid)
      | MLName (TE.POINTER ((path, boxed),_)) = (path, boxed)
      | MLName (TE.FLAG ((path, fName),_)) = (path, fName)
      | MLName _ = U.notImplemented "MLName: not a C type"

    val MLWidgetName = MLNamePath o MLName
    val MLShortWidgetName = MLNamePath o sndPath o MLName
    val MLBoxedName  = MLUnderscorePath o (mapPath NU.toLower) o MLName
    val MLFlagName =   MLUnderscorePath o (mapPath NU.toLower) o MLName

    val removeUnderscores = 
	let val remove = String.translate (fn #"_" => "" | c => String.str c)
	in  $ o remove o flatten
	end
    val CWidgetName  = removeUnderscores o MLNamePath o MLName
    val CBoxedName   = removeUnderscores o MLNamePath o MLName
    val CFlagName    = removeUnderscores o MLNamePath o MLName

    val MLFunName = MLUnderscorePath o (mapPath NU.toLower) o tlPath
    val MLFunNameWithoutOpt = addSuff "'" o MLFunName

    val MLConstrName = MLUnderscorePath o (mapPath NU.toUpper) o tlPath

    val CFunName = MLUnderscorePath o (mapPath NU.toLower)
    val CFunNameWithoutOpt = addSuff "_short" o CFunName

    val CConstrName = MLUnderscorePath o (mapPath NU.toUpper)


    fun MLSignalName (path,base) = 
	let val signal = path @ base
	    fun cnv signal = $(String.map (fn #"-" => #"_" | ch => ch) signal)
	in  case signal of
	        [signal] => cnv signal
              | [prefix,signal] => cnv (prefix ^ "_" ^ signal)
              | _ => U.shouldntHappen "Not a signal"
        end


    (* 5 is this is the largest X, such that Dynlib.appX exists *)
    fun fitsDynApp params = List.length params <= 5

    (* predicates *)
    fun isString (TE.PRIMTYPE "string") = true
      | isString (TE.PRIMTYPE "static_string") = true
      | isString _ = false
    fun isString' (texp, name) = isString texp

    fun isNullType (TE.OPTION _) = true
      | isNullType _ = false
    fun isNullType' (texp, name) = isNullType texp

    fun isOutputType (TE.OUTPUT _) = true
      | isOutputType _ = false
    fun isOutputType' (texp, name) = isOutputType texp
    
    fun isVoidType (TE.PRIMTYPE "none") = true
      | isVoidType _ = false
    fun isVoidType' (texp, name) = isVoidType texp

    fun isCompoundType (TE.ARROW _) = true
      | isCompoundType (TE.TUPLE _) = true
      | isCompoundType _ = false

    (* Generate appropriate C and ML versions of the types specified *)
    val mkcpath = prmap $
    fun mkCType (TE.PRIMTYPE tName) = 
	(case tName of
             "none" => $"void"
	 |   "int" => $"int"
	 |   "uint" => $"unsigned int"
         |   "float" => $"float"
         |   "bool" => $"int"
         |   "string" => $"char*"
         |   "static_string" => $"char*"
         |   "GtkType" => $"int"
         |   _ => U.shouldntHappen "mkCType: unknown primitive type"
	)
      | mkCType (TE.OUTPUT t) = mkCType t
      | mkCType (tExp as TE.WIDGET ((path, wid),_)) = 
	mkcpath path && $"GtkObject"
      | mkCType (tExp as TE.POINTER ((path, boxed),_)) = CBoxedName tExp
      | mkCType (tExp as TE.FLAG ((path, fName),_)) = CFlagName tExp
      | mkCType _ = U.notImplemented "mkCTypeType: not a type name"

    local
	datatype toType = ML_TYPE | ML_PRIM_TYPE

	fun parens true wseq = $"(" && wseq && $")"
          | parens false wseq = wseq

	val mkPath = prsep Empty $

	fun mkType nest _ tArg (TE.PRIMTYPE tName) = 
	    (case tName of
		 "none" => $"unit"
             |   "int" => $"int"
             |   "uint" => $"word"
             |   "float" => $"real"
             |   "bool" => $"bool"
             |   "string" => $"string"
             |   "static_string" => $"string"
             |   "GtkType" => $"gtk_type"
	     |   _ => U.shouldntHappen "mkType: unknown primitive type"
            )
	  | mkType nest toType tArg (TE.TUPLE tArgs) = 
	    prsep ($" * ") (mkType true toType tArg) tArgs
	  | mkType nest toType tArg (TE.ARROW(tArgs, _, _, tRet)) =
	    parens nest (prsep ($" -> ") (mkType true toType tArg o #1) tArgs
			&& $" -> " && mkType true toType (fn _ => $"base") tRet
                        )
	  | mkType nest toType tArg (TE.OPTION t) =
	    (mkType nest toType tArg t) && $" option"
	  | mkType nest toType tArg (TE.OUTPUT t) =
	    mkType nest toType tArg t
          | mkType nest ML_TYPE tArg (tExp as TE.WIDGET _) =
	    (fn argTyp => argTyp && $" " && MLWidgetName tExp) (tArg ()) 
          | mkType nest ML_PRIM_TYPE tArg (TE.WIDGET _) =
	    (fn argTyp => $"gtkobj") (tArg ()) 
          | mkType nest toType tArg (TE.LIST t) = 
	    (mkType nest toType tArg t) && $" list"
          | mkType nest ML_TYPE tArg (tExp as TE.FLAG (_,false)) = 
	    MLFlagName tExp && $" list"
          | mkType nest ML_TYPE tArg (tExp as TE.FLAG (_,true)) = 
	    MLFlagName tExp
          | mkType nest ML_PRIM_TYPE tArg (TE.FLAG _) = 
	    $"int"
          | mkType nest ML_TYPE tArg (tExp as TE.POINTER (_,NONE)) = 
	    MLBoxedName tExp
          | mkType nest ML_PRIM_TYPE tArg (tExp as TE.POINTER (_,NONE)) = 
	    MLBoxedName tExp
          | mkType nest ML_TYPE tArg (tExp as TE.POINTER (_,SOME _)) = 
	    (fn argTyp => argTyp && $" " && MLBoxedName tExp) (tArg())
          | mkType nest ML_PRIM_TYPE tArg (tExp as TE.POINTER (_,SOME _)) = 
	    (fn argTyp => argTyp && $" " && MLBoxedName tExp) (tArg())

	val index = ref 0
	fun reset () = index := 0
	fun fresh () = ($("'" ^ Char.toString (Char.chr (Char.ord #"a" + !index))) before
			index := !index + 1)
		       
    in (* local *)

	(* would we like versions with explicit type arguments? *)
	fun mkMLFreshType typExp = 
	    (reset (); mkType false ML_TYPE fresh typExp)
	fun mkMLType typExp = mkMLFreshType typExp

	fun mkMLPrimFreshType typExp = 
	    (reset (); mkType false ML_PRIM_TYPE fresh typExp)
	fun mkMLPrimType (TE.ARROW(tArgs, tOuts, tCmp, tRet)) =
	    if fitsDynApp tArgs (* check if it fits appX *)
	    then mkMLPrimFreshType (TE.ARROW(tArgs, tOuts, tCmp, tRet))
	    else mkMLPrimFreshType 
		      (TE.ARROW([(TE.TUPLE (map #1 tArgs),"")],tOuts,tCmp,tRet))
          | mkMLPrimType t = mkMLPrimFreshType t

    end (* local *)

    (* translate values to/from c types:

       toCValue: wraps an expression in appropriate conversion macros.

       toCValue': ditto, but handles options as well.

       fromCValue: wrap a c value in appropriate to-ML conversions.
    *)

    (* ugly hack: remove <ctype> *)
    fun toCValue (TE.PRIMTYPE "<ctype>", name) = name
      | toCValue (TE.PRIMTYPE tName, name) = 
	(case tName of
	     "none" => $"Unit_val"
	   |  "int" => $"Int_val(" && name && $")"
	   |  "uint" => $"Int_val(" && name && $")"
	   |  "float" => $"Double_val(" && name && $")"
	   |  "bool" => $"Bool_val(" && name && $")"
	   |  "string" => $"String_val(" && name && $")"
	   |  "static_string" => $"String_val(" && name && $")"
	   |   "GtkType" => $"Int_val(" && name && $")"
	   |  _ => U.shouldntHappen "toCValue: unknown primitive type"
	)
      | toCValue (TE.FLAG _, name) = $"Int_val(" && name && $")"
      | toCValue (TE.WIDGET _, name) = $"GtkObj_val(" && name && $")"
      | toCValue (tExp as TE.POINTER _, name) = 
	MLBoxedName tExp && $"_val(" && name && $")"
      | toCValue (typExp, name) = 
	U.notImplemented ("toCValue: not a type name: " ^ TE.toString typExp)

    (* ugly hack: options *)
    fun toCValue' (TE.OPTION (TE.WIDGET _), name) =
	$"GtkObjOption_nullok(" && name && $")"
      | toCValue' (TE.OPTION (TE.POINTER _), name) =
	$"GtkObjOption_nullok(" && name && $")"
      | toCValue' (TE.OPTION typExp', name) =
	if isString typExp' 
	then $"StringOption_nullok(" && name && $")"
	else raise Fail("Translate.toCValue': can only handle primitive values and strings (got " ^ TE.toString (TE.OPTION typExp') ^ ")")
      | toCValue' (TE.LIST (TE.WIDGET _), name) = 
	$"mgtk_smllist_to_glist_object(" && name && $")"
      | toCValue' (TE.LIST typExp, name) = 
	if isString typExp 
	then $"mgtk_smllist_to_glist_string(" && name && $")"
	else U.notImplemented "toCValue: can only handle lists of widgets or strings"
      | toCValue' (typExp, name) = toCValue (typExp, name)

    fun fromCValue (TE.PRIMTYPE tName, name) = 
	(case tName of
	     "none" => $"Val_unit"
	   |   "int" => $"Val_int(" && name && $")"
	   |   "uint" => $"Val_int(" && name && $")"
	   |   "float" => $"copy_double(" && name && $")"
	   |   "bool" => $"Val_bool(" && name && $")"
	   |   "string" => $"copy_string(" && name && $")"
	   |   "static_string" => $"copy_string(" && name && $")"
	   |   "GtkType" => $"Val_int(" && name && $")"
	   |   _ => U.shouldntHappen "fromCValue: unknown primitive type"
	)
      | fromCValue (TE.OUTPUT (widType as (TE.WIDGET _)), name) =
	fromCValue (widType, $"&" && name)
      | fromCValue (TE.OUTPUT (ptrType as (TE.POINTER _)), name) =
	fromCValue (ptrType, $"&" && name)
      | fromCValue (TE.OUTPUT (flagType as (TE.FLAG _)), name) = 
	fromCValue (flagType, name)
      | fromCValue (TE.OUTPUT (tName as (TE.PRIMTYPE _)), name) = 
	fromCValue (tName, name)
      | fromCValue (TE.WIDGET _, name) = 
	$"Val_GtkObj(" && name && $")"
      | fromCValue (tExp as TE.POINTER _, name) = 
	$"Val_" && MLBoxedName tExp && $"(" && name && $")"
      | fromCValue (TE.FLAG _, name) = 
	$"Val_int(" && name && $")"
      | fromCValue (typExp, name) = 
	U.notImplemented ("fromCValue: neither type name or output type (" ^ TE.toString typExp ^ ")")

end (* structure TypeInfo *)
