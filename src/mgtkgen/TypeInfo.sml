(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

(* TODO: Clean-up this code and document it! *)

structure TypeInfo =
struct

    (* Convenience: *)
    structure U  = Util
    structure TE = TypeExp
    structure NU = NameUtil

    open WSeq
    infix &&

    (* 5 is this is the largest X, such that Dynlib.appX exists *)
    fun fitsDynApp params = List.length params <= 5

    fun mlEnumTypeName name = NU.separateWords #"_" (NU.removePrefix name)
    fun mlFlagsTypeName name = NU.separateWords #"_" (NU.removePrefix name)
    fun mlBoxedTypeName name = NU.separateWords #"_" name

    fun get_texp (TE.LONG (_, typExp)) = typExp
    fun get_path (TE.LONG (path, _)) = path

    (* predicates *)
    local 
	fun isstring (TE.PRIMTYPE "string") = true
	  | isstring (TE.PRIMTYPE "static_string") = true
	  | isstring _ = false
    in  fun isString long = isstring (get_texp long)
	fun isString' (long, name) = isString long
    end

    local 
	fun isnull (TE.OPTION _) = true
	  | isnull _ = false
    in  fun isNullType long = isnull (get_texp long)
	fun isNullType' (long, name) = isNullType long
    end

    local 
	fun isoutput (TE.OUTPUT _) = true
	  | isoutput _ = false
    in  fun isOutputType long = isoutput (get_texp long)
	fun isOutputType' (long, name) = isOutputType long
    end

    local
	fun isvoid (TE.PRIMTYPE "none") = true
          | isvoid _ = false
    in  fun isVoidType long = isvoid (get_texp long)
	fun isVoidType' (long, name) = isVoidType long
    end

    fun compoundType (TE.ARROW _) = true
      | compoundType (TE.TUPLE _) = true
      | compoundType _ = false

    (* Generate appropriate C and ML versions of the types specified *)
    val mkcpath = prmap $
    fun mkc (TE.PRIMTYPE tName) = 
	(case tName of
             "none" => $"void"
	 |   "int" => $"int"
	 |   "uint" => $"unsigned int"
         |   "float" => $"float"
         |   "bool" => $"int"
         |   "string" => $"char*"
         |   "static_string" => $"char*"
         |   "GtkType" => $"int"
         |   _ => U.shouldntHappen "mkc: unknown primitive type"
	)
      | mkc (TE.OUTPUT t) = mkCType t
      | mkc (TE.WIDGET (wid,_)) = $"GtkObject"
      | mkc (TE.POINTER (boxed,_)) = $boxed
      | mkc (TE.FLAG (fName,_)) = $fName
      | mkc _ = U.notImplemented "mkCType: not a type name"
    and mkCType long = mkcpath (get_path long) && mkc (get_texp long)

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
	    prsep ($" * ") (mkLongType true toType tArg) tArgs
	  | mkType nest toType tArg (TE.ARROW(tArgs, _, _, tRet)) =
	    parens nest (prsep ($" -> ") (mkLongType true toType tArg o #1) tArgs
			&& $" -> " && mkLongType true toType (fn _ => $"base") tRet
                        )
	  | mkType nest toType tArg (TE.OPTION t) =
	    (mkLongType nest toType tArg t) && $" option"
	  | mkType nest toType tArg (TE.OUTPUT t) =
	    mkLongType nest toType tArg t
          | mkType nest ML_TYPE tArg (TE.WIDGET (wid,_)) =
	    (fn argTyp => argTyp && $" " && $wid) (tArg ()) 
          | mkType nest ML_PRIM_TYPE tArg (TE.WIDGET (wid,_)) =
	    (fn argTyp => $"gtkobj") (tArg ()) 
          | mkType nest toType tArg (TE.LIST t) = 
	    (mkLongType nest toType tArg t) && $" list"
          | mkType nest ML_TYPE tArg (TE.FLAG (fName,false)) = 
	    $$[mlFlagsTypeName fName, " list"]
          | mkType nest ML_TYPE tArg (TE.FLAG (fName,true)) = 
	    $(mlFlagsTypeName fName)
          | mkType nest ML_PRIM_TYPE tArg (TE.FLAG(fName,_)) = 
	    $"int"
          | mkType nest ML_TYPE tArg (TE.POINTER (boxed,NONE)) = 
	    $(mlBoxedTypeName boxed)
          | mkType nest ML_PRIM_TYPE tArg (TE.POINTER (boxed,NONE)) = 
	    $(mlBoxedTypeName boxed)
          | mkType nest ML_TYPE tArg (TE.POINTER (boxed,SOME _)) = 
	    (fn argTyp => argTyp && $" " && $(mlBoxedTypeName boxed)) (tArg())
          | mkType nest ML_PRIM_TYPE tArg (TE.POINTER (boxed,SOME _)) = 
	    (fn argTyp => argTyp && $" " && $(mlBoxedTypeName boxed)) (tArg())
	and mkLongType nest toType tArg long = 
	       (if compoundType (get_texp long) then Empty
	        else mkPath (get_path long))
	    && mkType nest toType tArg (get_texp long)

	val index = ref 0
	fun reset () = index := 0
	fun fresh () = ($("'" ^ Char.toString (Char.chr (Char.ord #"a" + !index))) before
			index := !index + 1)
		       
    in (* local *)

	(* would we like versions with explicit type arguments? *)
	fun mkMLFreshType typExp = 
	    (reset (); mkLongType false ML_TYPE fresh typExp)
	fun mkMLType typExp = mkMLFreshType typExp

	fun mkMLPrimFreshType typExp = 
	    (reset (); mkLongType false ML_PRIM_TYPE fresh typExp)
	fun mkMLPrimType (TE.LONG(path, TE.ARROW(tArgs, tOuts, tCmp, tRet))) =
	    if fitsDynApp tArgs (* check if it fits appX *)
	    then mkMLPrimFreshType (TE.LONG(path, TE.ARROW(tArgs, tOuts, tCmp, tRet)))
	    else mkMLPrimFreshType 
		      (TE.LONG(path, 
				TE.ARROW([(TE.LONG([],TE.TUPLE (map #1 tArgs)),"")],tOuts,tCmp,tRet)))
          | mkMLPrimType t = mkMLPrimFreshType t

    end (* local *)

    (* translate values to/from c types:

       toCValue: wraps an expression in appropriate conversion macros.

       toCValue': ditto, but handles options as well.

       fromCValue: wrap a c value in appropriate to-ML conversions.
    *)
    local 
	(* ugly hack: remove <ctype> *)
	fun toc long (TE.PRIMTYPE "<ctype>", name) = name
	  | toc long (TE.PRIMTYPE tName, name) = 
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
	  | toc long (TE.FLAG (fName,_), name) = $"Int_val(" && name && $")"
	  | toc long (TE.WIDGET (wid,_), name) = $"GtkObj_val(" && name && $")"
	  | toc long (TE.POINTER (boxed,_), name) = $boxed && $"_val(" && name && $")"
	  | toc long (typExp, name) = 
	    U.notImplemented ("toCValue: not a type name: " ^ TE.toString long)

	(* ugly hack: options *)
	fun toc' long (TE.OPTION (TE.LONG(_,TE.WIDGET _)), name) =
	    $"GtkObjOption_nullok(" && name && $")"
          | toc' long (TE.OPTION (TE.LONG(_,TE.POINTER _)), name) =
	    $"GtkObjOption_nullok(" && name && $")"
	  | toc' long (TE.OPTION typExp', name) =
	    if isString typExp' 
	    then $"StringOption_nullok(" && name && $")"
	    else raise Fail("Translate.toCValue': can only handle primitive values and strings (got " ^ TE.toString long ^ ")")
          | toc' long (TE.LIST (TE.LONG(_,TE.WIDGET _)), name) = 
	    $"mgtk_smllist_to_glist_object(" && name && $")"
          | toc' long (TE.LIST typExp, name) = 
	    if isString typExp 
	    then $"mgtk_smllist_to_glist_string(" && name && $")"
	    else U.notImplemented "toCValue: can only handle lists of widgets or strings"
	  | toc' long (typExp, name) = toc long (typExp, name)

	fun fromc long (TE.PRIMTYPE tName, name) = 
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
	  | fromc long (TE.OUTPUT (widType as (TE.LONG(_, TE.WIDGET _))), name) =
	    fromCValue (widType, $"&" && name)
	  | fromc long (TE.OUTPUT (ptrType as (TE.LONG(_, TE.POINTER _))), name) =
	    fromCValue (ptrType, $"&" && name)
	  | fromc long (TE.OUTPUT (flagType as (TE.LONG(_,TE.FLAG _))), name) = 
	    fromCValue (flagType, name)
	  | fromc long' (TE.OUTPUT (tName as (TE.LONG(_, TE.PRIMTYPE _))), name) = 
	    fromCValue (tName, name)
          | fromc long (TE.WIDGET (wid,_), name) = $"Val_GtkObj(" && name && $")"
          | fromc long (TE.POINTER (boxed,_), name) = $"Val_" && $boxed && $"(" && name && $")"
	  | fromc long (TE.FLAG (fName,_), name) = $"Val_int(" && name && $")"
	  | fromc long (typExp, name) = 
	    U.notImplemented ("fromCValue: neither type name or output type (" ^ TE.toString long ^ ")")
	    
	and fromCValue (long, name) = fromc long (get_texp long, name)

    in  fun toCValue (long, name) = toc long (get_texp long, name)
	fun toCValue' (long, name) = toc' long (get_texp long, name)
	val fromCValue = fromCValue
    end (* local *)
 

end (* structure TypeInfo *)