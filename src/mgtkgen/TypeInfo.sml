(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

(* TODO: Clean-up this code and document it! *)

structure TypeInfo =
struct

    open WSeq
    infix &&

    (* 5 is this is the largest X, such that Dynlib.appX exists *)
    fun fitsDynApp params = List.length params <= 5

    fun mlEnumTypeName name = NameUtil.separateWords #"_" (NameUtil.removePrefix name)
    fun mlFlagsTypeName name = NameUtil.separateWords #"_" (NameUtil.removePrefix name)
    fun mlBoxedTypeName name = NameUtil.separateWords #"_" name

    fun get_texp (TypeExp.LONG (_, typExp)) = typExp

    (* predicates *)
    local 
	fun isstring (TypeExp.PRIMTYPE "string") = true
	  | isstring (TypeExp.PRIMTYPE "static_string") = true
	  | isstring _ = false
    in  fun isString long = isstring (get_texp long)
	fun isString' (long, name) = isString long
    end

    local 
	fun isnull (TypeExp.OPTION _) = true
	  | isnull _ = false
    in  fun isNullType long = isnull (get_texp long)
	fun isNullType' (long, name) = isNullType long
    end

    local 
	fun isoutput (TypeExp.OUTPUT _) = true
	  | isoutput _ = false
    in  fun isOutputType long = isoutput (get_texp long)
	fun isOutputType' (long, name) = isOutputType long
    end

    local
	fun isvoid (TypeExp.PRIMTYPE "none") = true
          | isvoid _ = false
    in  fun isVoidType long = isvoid (get_texp long)
	fun isVoidType' (long, name) = isVoidType long
    end


    (* Generate appropriate C and ML versions of the types specified *)
    fun mkc (TypeExp.PRIMTYPE tName) = 
	(case tName of
             "none" => $"void"
	 |   "int" => $"int"
	 |   "uint" => $"unsigned int"
         |   "float" => $"float"
         |   "bool" => $"int"
         |   "string" => $"char*"
         |   "static_string" => $"char*"
         |   "GtkType" => $"int"
         |   _ => Util.shouldntHappen "mkc: unknown primitive type"
	)
      | mkc (TypeExp.OUTPUT t) = mkCType t
      | mkc (TypeExp.WIDGET (wid,_)) = $"GtkObject"
      | mkc (TypeExp.POINTER (boxed,_)) = $boxed
      | mkc (TypeExp.FLAG (fName,_)) = $fName
      | mkc _ = Util.notImplemented "mkCType: not a type name"
    and mkCType long = mkc (get_texp long)

    local
	datatype toType = ML_TYPE | ML_PRIM_TYPE

	fun parens true wseq = $"(" && wseq && $")"
          | parens false wseq = wseq

	fun mkType nest _ tArg (TypeExp.PRIMTYPE tName) = 
	    (case tName of
		 "none" => $"unit"
             |   "int" => $"int"
             |   "uint" => $"word"
             |   "float" => $"real"
             |   "bool" => $"bool"
             |   "string" => $"string"
             |   "static_string" => $"string"
             |   "GtkType" => $"gtk_type"
	     |   _ => Util.shouldntHappen "mkType: unknown primitive type"
            )
	  | mkType nest toType tArg (TypeExp.TUPLE tArgs) = 
	    prsep ($" * ") (mkLongType true toType tArg) tArgs
	  | mkType nest toType tArg (TypeExp.ARROW(tArgs, tRet)) =
	    parens nest (prsep ($" -> ") (mkLongType true toType tArg) tArgs
			&& $" -> " && mkLongType true toType (fn _ => $"base") tRet
                        )
	  | mkType nest toType tArg (TypeExp.OPTION t) =
	    (mkLongType nest toType tArg t) && $" option"
	  | mkType nest toType tArg (TypeExp.OUTPUT t) =
	    mkLongType nest toType tArg t
          | mkType nest ML_TYPE tArg (TypeExp.WIDGET (wid,_)) =
	    (fn argTyp => argTyp && $" " && $wid) (tArg ()) 
          | mkType nest ML_PRIM_TYPE tArg (TypeExp.WIDGET (wid,_)) =
	    (fn argTyp => $"gtkobj") (tArg ()) 
          | mkType nest toType tArg (TypeExp.LIST t) = 
	    (mkLongType nest toType tArg t) && $" list"
          | mkType nest ML_TYPE tArg (TypeExp.FLAG (fName,false)) = 
	    $$[mlFlagsTypeName fName, " list"]
          | mkType nest ML_TYPE tArg (TypeExp.FLAG (fName,true)) = 
	    $(mlFlagsTypeName fName)
          | mkType nest ML_PRIM_TYPE tArg (TypeExp.FLAG(fName,_)) = 
	    $"int"
          | mkType nest ML_TYPE tArg (TypeExp.POINTER (boxed,NONE)) = 
	    $(mlBoxedTypeName boxed)
          | mkType nest ML_PRIM_TYPE tArg (TypeExp.POINTER (boxed,NONE)) = 
	    $(mlBoxedTypeName boxed)
          | mkType nest ML_TYPE tArg (TypeExp.POINTER (boxed,SOME _)) = 
	    (fn argTyp => argTyp && $" " && $(mlBoxedTypeName boxed)) (tArg())
          | mkType nest ML_PRIM_TYPE tArg (TypeExp.POINTER (boxed,SOME _)) = 
	    (fn argTyp => argTyp && $" " && $(mlBoxedTypeName boxed)) (tArg())
	and mkLongType nest toType tArg long = 
	    mkType nest toType tArg (get_texp long)

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
	fun mkMLPrimType (TypeExp.LONG(path, TypeExp.ARROW(tArgs, tRet))) =
	    if fitsDynApp tArgs (* check if it fits appX *)
	    then mkMLPrimFreshType (TypeExp.LONG(path, TypeExp.ARROW(tArgs, tRet)))
	    else mkMLPrimFreshType 
		      (TypeExp.LONG(path, 
				TypeExp.ARROW([TypeExp.LONG([],TypeExp.TUPLE tArgs)],tRet)))
          | mkMLPrimType t = mkMLPrimFreshType t

    end (* local *)

    (* translate values to/from c types:

       toCValue: wraps an expression in appropriate conversion macros.

       toCValue': ditto, but handles options as well.

       fromCValue: wrap a c value in appropriate to-ML conversions.
    *)
    local 
	(* ugly hack: remove <ctype> *)
	fun toc long (TypeExp.PRIMTYPE "<ctype>", name) = name
	  | toc long (TypeExp.PRIMTYPE tName, name) = 
	    (case tName of
		"none" => $"Unit_val"
	     |  "int" => $"Int_val(" && name && $")"
	     |  "uint" => $"Int_val(" && name && $")"
             |  "float" => $"Double_val(" && name && $")"
             |  "bool" => $"Bool_val(" && name && $")"
             |  "string" => $"String_val(" && name && $")"
             |  "static_string" => $"String_val(" && name && $")"
             |   "GtkType" => $"Int_val(" && name && $")"
	     |  _ => Util.shouldntHappen "toCValue: unknown primitive type"
	    )
	  | toc long (TypeExp.FLAG (fName,_), name) = $"Int_val(" && name && $")"
	  | toc long (TypeExp.WIDGET (wid,_), name) = $"GtkObj_val(" && name && $")"
	  | toc long (TypeExp.POINTER (boxed,_), name) = $boxed && $"_val(" && name && $")"
	  | toc long (typExp, name) = 
	    Util.notImplemented ("toCValue: not a type name: " ^ TypeExp.toString long)

	(* ugly hack: options *)
	fun toc' long (TypeExp.OPTION (TypeExp.LONG(_,TypeExp.WIDGET _)), name) =
	    $"GtkObjOption_nullok(" && name && $")"
          | toc' long (TypeExp.OPTION (TypeExp.LONG(_,TypeExp.POINTER _)), name) =
	    $"GtkObjOption_nullok(" && name && $")"
	  | toc' long (TypeExp.OPTION typExp', name) =
	    if isString typExp' 
	    then $"StringOption_nullok(" && name && $")"
	    else raise Fail("Translate.toCValue': can only handle primitive values and strings (got " ^ TypeExp.toString long ^ ")")
          | toc' long (TypeExp.LIST (TypeExp.LONG(_,TypeExp.WIDGET _)), name) = 
	    $"mgtk_smllist_to_glist_object(" && name && $")"
          | toc' long (TypeExp.LIST typExp, name) = 
	    if isString typExp 
	    then $"mgtk_smllist_to_glist_string(" && name && $")"
	    else Util.notImplemented "toCValue: can only handle lists of widgets or strings"
	  | toc' long (typExp, name) = toc long (typExp, name)

	fun fromc long (TypeExp.PRIMTYPE tName, name) = 
	    (case tName of
		 "none" => $"Val_unit"
             |   "int" => $"Val_int(" && name && $")"
             |   "uint" => $"Val_int(" && name && $")"
             |   "float" => $"copy_double(" && name && $")"
             |   "bool" => $"Val_bool(" && name && $")"
	     |   "string" => $"copy_string(" && name && $")"
	     |   "static_string" => $"copy_string(" && name && $")"
             |   "GtkType" => $"Val_int(" && name && $")"
	     |   _ => Util.shouldntHappen "fromCValue: unknown primitive type"
            )
	  | fromc long (TypeExp.OUTPUT (widType as (TypeExp.LONG(_, TypeExp.WIDGET _))), name) =
	    fromCValue (widType, $"&" && name)
	  | fromc long (TypeExp.OUTPUT (ptrType as (TypeExp.LONG(_, TypeExp.POINTER _))), name) =
	    fromCValue (ptrType, $"&" && name)
	  | fromc long (TypeExp.OUTPUT (flagType as (TypeExp.LONG(_,TypeExp.FLAG _))), name) = 
	    fromCValue (flagType, name)
	  | fromc long' (TypeExp.OUTPUT (tName as (TypeExp.LONG(_, TypeExp.PRIMTYPE _))), name) = 
	    fromCValue (tName, name)
          | fromc long (TypeExp.WIDGET (wid,_), name) = $"Val_GtkObj(" && name && $")"
          | fromc long (TypeExp.POINTER (boxed,_), name) = $"Val_" && $boxed && $"(" && name && $")"
	  | fromc long (TypeExp.FLAG (fName,_), name) = $"Val_int(" && name && $")"
	  | fromc long (typExp, name) = 
	    Util.notImplemented ("fromCValue: neither type name or output type (" ^ TypeExp.toString long ^ ")")
	    
	and fromCValue (long, name) = fromc long (get_texp long, name)

    in  fun toCValue (long, name) = toc long (get_texp long, name)
	fun toCValue' (long, name) = toc' long (get_texp long, name)
	val fromCValue = fromCValue
    end (* local *)
 

end (* structure TypeInfo *)