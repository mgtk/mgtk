(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

structure Translate :> Translate =
struct

    open WSeq
    infix &&

    fun id x = x

    val fitsDynApp = TypeInfo.fitsDynApp

    (* mappings between defs strings representing (type) names,
       and the corresponding ML (type) names *)
    fun mlWidgetName (TypeExp.LONG(_,TypeExp.WIDGET(name,_))) = NameUtil.removePrefix name
      | mlWidgetName _ = Util.shouldntHappen "mlWidgetName: not a widget"
    fun mlFunName name = NameUtil.remove_prefix name
    fun mlSignalName signal = 
	let fun cnv signal = String.map (fn #"-" => #"_" | ch => ch) signal
	in  case signal of
	        [signal] => cnv signal
              | [prefix,signal] => cnv (prefix ^ "_" ^ signal)
              | _ => Util.shouldntHappen "Not a signal"
        end
    fun mlFlagName name = NameUtil.remove_PREFIX name
    fun mlEnumName name = NameUtil.remove_PREFIX name



    val get_texp = TypeInfo.get_texp
    val dummyPair = (TypeExp.LONG([], TypeExp.PRIMTYPE "none"), "dummy")



    type parlist = (TypeExp.long_texp * string) list

    fun splitList p l =
	let fun f (elem, (acc1,acc2)) =
	        if p elem then (elem::acc1,acc2)
		else (acc1, elem::acc2)
	in  List.foldr f ([],[]) l
	end

    fun separateParams (retType, params: parlist) =
	let fun mkTuple ([], retType) = retType
              | mkTuple (pars, retType) =
	        let val tOut = if TypeInfo.isVoidType retType then pars
			       else retType :: pars
		in  case tOut of
		       nil => raise Fail ("mkTuple: has to return *something*")
		     | [t] => t
                     | ts => TypeExp.LONG([], TypeExp.TUPLE ts)
		end
	    val (outPars, pars) = splitList (TypeInfo.isOutputType o #1) params
	in  (outPars, pars, mkTuple (map #1 outPars, retType))
	end

    fun mlFunType (retType, []: parlist) = 
	raise Fail ("mlFunType: no parameters")
      | mlFunType (retType, params: parlist) =
	let val (outPars, pars, retType) = separateParams (retType, params)
	in  TypeExp.LONG([], TypeExp.ARROW(map #1 pars, retType) )
	end


    (* Helper functions *)

    (* indentation in ML files *)
    val indent = "    "

    (* check if an expression, determined from the type of the expression,
       allocates in the MosML sense *)
    fun allocExpression (long as TypeExp.LONG(_, TypeExp.PRIMTYPE tName)) =
	let fun extractString (WSeq.$ s) = s
              | extractString (WSeq.Empty) = ""
              | extractString _ = Util.shouldntHappen "allocExpression.extractString"
	    val primType = extractString(TypeInfo.mkMLPrimType long)
	in   not (primType="int" orelse primType="word" orelse primType="bool")
	end
      | allocExpression (TypeExp.LONG(_, TypeExp.WIDGET _)) = true
      | allocExpression (TypeExp.LONG(_, TypeExp.POINTER _)) = true
      | allocExpression (TypeExp.LONG(_, TypeExp.FLAG _)) = false
      | allocExpression (TypeExp.LONG(_, TypeExp.OUTPUT tExp)) = allocExpression tExp
      | allocExpression _ = Util.shouldntHappen "allocExpression: not a type name"

    fun allocTuple name values =
	let val values' = ListPair.zip (List.tabulate(List.length values, fn n=>n), values)
	    val allocates = List.exists (allocExpression o #1) values
	    val tupleName = if allocates then "r[0]" else "res"
	    fun prValue (n, (t,v)) =
		$$["  Field(",tupleName,", ",Int.toString n,") = "] && TypeInfo.fromCValue (t,v) && $";"
	in  (if allocates then $"  Push_roots(r, 1);" && Nl else Empty)
         && $$["  ", tupleName, " = alloc_tuple(", Int.toString (List.length values), ");"] && Nl
         && prsep Nl prValue values'
         && Nl
         && (if allocates then $"  res = r[0];" && Nl && $"  Pop_roots();" && Nl 
	     else Empty)
	end

    (* ML comment *)
    fun mkComment cmt =	$indent && $"(* " && cmt && $" *)" && Nl

    (* ML primitive type specification *)
    fun mkPrimTypeDecl' tName = $indent && $"prim_type " && tName && Nl
    fun mkPrimTypeDecl tName = mkPrimTypeDecl' ($tName)

    (* ML type specification *)
    local 
	fun mktypedecl typ (tName, NONE) =
	    $indent && typ && tName && Nl
	  | mktypedecl typ (tName, SOME tExp) =
	    $indent && typ && tName && $" = " && tExp && Nl
    in  
	val mkTypeDecl' = mktypedecl ($"type ")
	fun mkTypeDecl (tName, tExpOpt) = mkTypeDecl' ($tName, tExpOpt)
	fun mkEqTypeDecl (tName, tExpOpt) = mktypedecl ($"eqtype ") ($tName, tExpOpt)
    end

    (* ML value specification *)
    fun mkValDecl' (vName, tExp, NONE) =
	$indent && $"val " && vName && $": " && tExp && Nl
      | mkValDecl' (vName, tExp, SOME vExp) =
	   $indent && $"val " && vName && $": " && tExp && Nl
        && $indent && $indent && $"= " && vExp && Nl 
    fun mkValDecl (vName, tExp, vExpOpt) = mkValDecl' ($vName, tExp, vExpOpt)

    (* Code to declare an mgtk C-function
       ------------------------------------------------------------

       There are two issues: extracting arguments, and translating
       ``output'' arguments to ordinary arguments returned via tuples.

       For the extraction of argument we have two (major) situations:
       when the number of arguments fits an Dynlib.appX call, and when
       there is too many arguments.

       (1) fits Dynlib.appX

           /* ML type: gtkobj -> word -> unit */
           value mgtk_text_backward_delete(value text, value nchars) { /* ML */
             ...
             return Val_unit;
           }

       (2) too many arguments

           /* ML type: gtkobj * gdk_font option * gdk_color option * 
                       gdk_color option * string * int -> unit */
           value mgtk_text_insert(value mgtk_params) { /* ML */
             value text = Field(mgtk_params, 0);
             value font = Field(mgtk_params, 1);
             value fore = Field(mgtk_params, 2);
             value back = Field(mgtk_params, 3);
             value chars = Field(mgtk_params, 4);
             value length = Field(mgtk_params, 5);
             ...
             return Val_unit;
           }

       For the handling of ``output'' arguments we need to declare
       variables to hold the returned values and construct a tuple
       (when necessary) in order to return:

       (1) Only one value is returned

           /* ML type: gtkobj -> string */
           value mgtk_label_get(value label) { /* ML */
             char* res;
             gtk_label_get(GtkObj_val(label), &res);
             return copy_string(res);
           }

       (2) Multiple values are returned

           <insert example>

    *)
    fun mkOutName n = $$["res", n]

    fun mkCall (name, args: parlist) =
	let val args' = List.filter (not o TypeInfo.isVoidType') args
	    fun coerce (TypeExp.LONG(_, TypeExp.OUTPUT t), n) = $"&" && mkOutName n
	      | coerce (t, n) = TypeInfo.toCValue' (t, $n)
	in  $name && $"(" && prsep ($", ") coerce args' && $")"
	end

    fun paramList (params: parlist) = 
	if fitsDynApp params
	then $"(" && prsep ($", ") ((fn n=> $$["value ",n]) o #2) params && $")"
	else $$["(value mgtk_params)"]

    fun extractParams (params: parlist) =
	let fun field ((t,n),i) =
	        $$["  value ", n, " = ", 
		   "Field(mgtk_params, ", Int.toString i, ");"]
	    val numbers = List.tabulate (List.length params, fn i=>i)
	in  prsep Nl field (ListPair.zip (params, numbers))
	end

    fun size (TypeExp.PRIMTYPE _) = 1
      | size (TypeExp.TUPLE ts) = List.length ts
      | size _ = 0 (* is this really true? *)

    fun declareOutParams (retType, []) = Empty
      | declareOutParams (retType, outPars) =
	let fun decl (tName, name) = 
	        $"  " && TypeInfo.mkCType tName && $" " && mkOutName name && $";"
	in     (if size(get_texp retType) > 1 then $"  value res;" && Nl else Empty)
            && prsep Nl decl outPars && Nl
	end

    (* for this function remember that the return type has already
       been combined with the output parameters. *)
    fun mkReturn' cExp (retType as (TypeExp.PRIMTYPE "none"), outPars) =
	   $"  " && cExp && $";" && Nl
	&& $"  return " && TypeInfo.fromCValue (TypeExp.LONG([], retType), $"dummy") && $";" && Nl
      | mkReturn' cExp (retType as (TypeExp.PRIMTYPE _), []) =
	   $"  return " && TypeInfo.fromCValue (TypeExp.LONG([], retType), cExp) && $";" && Nl
      | mkReturn' cExp (retType as (TypeExp.PRIMTYPE _), [(tOut,nOut)]) =
	   $"  " && cExp && $";" && Nl
	&& $"  return " && TypeInfo.fromCValue (tOut, mkOutName nOut) && $";" && Nl
      | mkReturn' cExp (retType as (TypeExp.WIDGET (wid,_)), []) =
	   $"  return " && TypeInfo.fromCValue (TypeExp.LONG([], retType), cExp) && $";" && Nl
      | mkReturn' cExp (retType as (TypeExp.WIDGET _), [(tOut,nOut)]) =
	   $"  " && cExp && $";" && Nl
	&& $"  return " && TypeInfo.fromCValue (tOut, mkOutName nOut) && $";" && Nl
      | mkReturn' cExp (retType as (TypeExp.POINTER _), []) =
	   $"  return " && TypeInfo.fromCValue (TypeExp.LONG([], retType), cExp) && $";" && Nl
      | mkReturn' cExp (retType as (TypeExp.POINTER _), [(tOut,nOut)]) =
	   $"  " && cExp && $";" && Nl
	&& $"  return " && TypeInfo.fromCValue (tOut, mkOutName nOut) && $";" && Nl
      | mkReturn' cExp (retType as (TypeExp.FLAG (fName,_)), []) =
	   $"  return " && TypeInfo.fromCValue (TypeExp.LONG([], retType), cExp) && $";" && Nl
      | mkReturn' cExp (retType as (TypeExp.FLAG (fName,_)), [(tOut,nOut)]) =
	   $"  " && cExp && $";" && Nl
	&& $"  return " && TypeInfo.fromCValue (tOut, mkOutName nOut) && $";" && Nl
      | mkReturn' cExp (TypeExp.PRIMTYPE _, _) = 
	  Util.shouldntHappen("mkReturn: multiple output parameters and return type is a type name")
      | mkReturn' cExp (retType as (TypeExp.TUPLE (tOuts as (t::_)), outPars)) =
	if List.length tOuts = List.length outPars (* original return type was none *)
	then    $"  " && cExp && $";" && Nl
	     && allocTuple "res" (map (fn (tOut,nOut) => (tOut, mkOutName nOut)) outPars)
             && $"  return res;" && Nl
	else    $"  value rescall = " && cExp && $";" && Nl
             && allocTuple "res" (map (fn (tOut,nOut) => (tOut, mkOutName nOut)) ((t,"call")::outPars))
             && $"  return res;" && Nl
      | mkReturn' cExp (TypeExp.TUPLE _, _) = 
	  Util.shouldntHappen("mkReturn: return tuple with only one output type")
      | mkReturn' cExp (TypeExp.OUTPUT typExp, outPars) = mkReturn cExp (typExp, outPars)
      | mkReturn' cExp (typExp, _) = 
	  Util.shouldntHappen("mkReturn: wrong return type")
    and mkReturn cExp (TypeExp.LONG(path, typExp), outPars) = mkReturn' cExp (typExp, outPars)

    fun mkFunDecl (name, retTyp, params, cExp) = 
	let val params = if null params then [dummyPair] else params
	    val (outPars, pars, retType) = separateParams (retTyp, params)
	    val typ = mlFunType (retTyp, params)
	in  $"/* ML type: " && TypeInfo.mkMLPrimType typ && $" */" && Nl
         && $$["EXTERNML ", "value m", name] && paramList pars && $" { /* ML */" && Nl
         && declareOutParams (retType, outPars)
         && (if fitsDynApp pars then Empty else extractParams pars && Nl)
         && mkReturn cExp (retType, outPars)
         && $"}" && Nl
         && Nl
	end

    (* Code to declare an ML function 
       ------------------------------------------------------------

       val text_insert_: gtkobj * gdk_font option * gdk_color option
                         * gdk_color option * string * int -> unit 
           = app1(symb"mgtk_text_insert") 

       val text_insert: 'a GtkText -> gdk_font option -> gdk_color option 
                        -> gdk_color option -> string -> int -> unit 
           = fn OBJ text => fn font => fn fore => fn back => fn chars => 
             fn length => text_insert_ (text, font, fore, back, chars, length)

       val text_insert'_: gtkobj -> string -> int -> unit
           = app3(symb"mgtk_text_insert_short")
       val text_insert': 'a GtkText -> string -> int -> unit
           = fn OBJ text => fn chars => fn length => 
             text_insert'_ text chars length
    *)

    fun mkMLFunDecl base (name, tExp) =
	let val name' = mlFunName name
	in  mkValDecl (name', TypeInfo.mkMLType tExp, NONE)
	end

    fun mkArg (TypeExp.LONG(_, TypeExp.WIDGET _), name) = $$["fn OBJ ", name, " => "]
      | mkArg (typExp, name) = $$["fn ", name, " => "]

    fun unwrapArg (TypeExp.LONG(_, TypeExp.OPTION (TypeExp.LONG(_, TypeExp.WIDGET _))), name) =
	$$["(unwrapObjOpt ", name, ")"]
      | unwrapArg (TypeExp.LONG(_, TypeExp.LIST (TypeExp.LONG(_, TypeExp.WIDGET _))), name) =
	$$["(map unwrap ", name, ")"]
      | unwrapArg (TypeExp.LONG(_, TypeExp.FLAG (fName,false)), name) = 
	$$["(setFlags ", name, ")"]
      | unwrapArg (typExp, name) = $name
    fun wrapResult (TypeExp.LONG(_, TypeExp.WIDGET _)) res = 
	$"OBJ(" && res && $")"
      | wrapResult (TypeExp.LONG(_, TypeExp.FLAG (fName,false))) res =
(* see comment on getSet below
	$"getSet" && $fName && $"(" && res && $")"
*)
	$"getFlags(" && res && $")"
      | wrapResult typExp res = res

    fun mkMLFunVal short (name, retTyp, params) =
	let val (outPars, pars', retTyp') = separateParams (retTyp, params)

	    val args = List.length pars'
	    val no_args = if fitsDynApp pars' then args else 1
	    val name' = mlFunName name
	    val (c_name,ml_name) = 
		    if short then (name ^ "_short", name' ^ "'")
		    else (name, name')
	    val primval = 
		   let val typ = mlFunType (retTyp, params)
		       val value = $$["app", Int.toString no_args,
				      "(symb\"m", c_name, "\")"]
		   in  mkValDecl (ml_name ^ "_", TypeInfo.mkMLPrimType typ, SOME value)
		   end
	    val funcval =  mkMLFunDecl (SOME ($"base")) (ml_name,mlFunType (retTyp,params))
                        && $$[indent, indent, "= "]
		        && prmap mkArg pars'
                        && wrapResult retTyp' 
                               ($$[ml_name, "_ "] && 
			         (if not(fitsDynApp pars')
				  then $"(" && prsep ($", ") unwrapArg pars' && $")"
				  else prsep ($" ") unwrapArg pars')
                               )
                       && Nl && Nl
	in  primval && funcval
	end

    (* Code to extract fields of a widget 
       ------------------------------------------------------------

       (1) for C

           /* ML type: gtkobj -> gtkobj */
           value mgtk_combo_get_entry(value wid) { /* ML */
             return Val_GtkObj((GTK_COMBO(GtkObj_val(wid))) -> entry);
           }

       (2) for SIG

           val combo_get_entry: 'a GtkCombo -> base GtkEntry

       (3) for SML

           val combo_get_entry_: gtkobj -> gtkobj
               = app1(symb"mgtk_combo_get_entry")
           val combo_get_entry: 'a GtkCombo -> base GtkEntry
               = fn OBJ wid => OBJ(combo_get_entry_ wid)
    *)

    fun getFieldName (name, field) =
	"gtk_" ^ NameUtil.separateWords #"_" (mlWidgetName name) ^ "_get_" ^ field

    fun mkGetField name (typExp, field) = 
	let val name' = NameUtil.separateWords #"_" (mlWidgetName name)
	    val macro = $$["GTK_", NameUtil.toUpper name',"("]
                        && TypeInfo.toCValue (name, $"wid") && $")"
	    val cExp = $"(" && macro && $")" && $" -> " && $field
	in  mkFunDecl (getFieldName (name, field), typExp, [(name,"wid")], cExp)
	end

    fun mkMLGetFieldDecl name (typExp, field) =
	mkMLFunDecl ($"base") (getFieldName(name,field), mlFunType (typExp,[(name,"wid")]))

    fun mkMLGetFieldVal name (typExp, field) =
	mkMLFunVal false (getFieldName(name,field), typExp, [(name,"wid")])

    (* Code to declare a new widget
       ------------------------------------------------------------
       (1) for C

           /* *** Text stuff *** */

       (2) for SML

           (* *** Text *** *)

           type 'a text_t = base
           type 'a GtkText = 'a text_t GtkEditable

       (3) for SIG

           (* *** Text *** *)

           type 'a text_t
           type 'a GtkText = 'a text_t GtkEditable
    *)

    fun mkWidgetDecl (wid, fields) =
	   $$["/* *** ", mlWidgetName wid, " stuff *** */"] && Nl && Nl
        && (prsep Nl (mkGetField wid) (case fields of SOME fields => fields | NONE => []))
        && Nl

    fun widgetName (TypeExp.LONG(_, TypeExp.WIDGET(name, _))) = name
      | widgetName _ = Util.shouldntHappen "widgetName: not a widget"
    fun inheritsFrom (TypeExp.LONG(_, TypeExp.WIDGET(_, SOME inherits))) = inherits
      | inheritsFrom (TypeExp.LONG(_, TypeExp.WIDGET(_, NONE))) = Util.shouldntHappen "inheritsFrom: got GtkObject"
      | inheritsFrom _ = Util.shouldntHappen "inheritsFrom: not a widget"
    fun mkMLWidgetDecl base wid = 
	let val name' = mlWidgetName wid
	    val witness = $$["'a ", NameUtil.toLower name', "_t"]
	in  mkComment ($$["*** ", name', " ***"]) && Nl

	 && mkTypeDecl' (witness, base)
         && mkTypeDecl' ($"'a " && $(widgetName wid), 
			 SOME(witness && $" " && $(inheritsFrom wid)))
         && Nl
	end

    (* Code to declare ``boxed'' types
       ------------------------------------------------------------

       (1) for C

           #define GdkFont_val(x) ( ( void* ) Field(x, 1))

           #define GdkFont_val_nocast(x) (Field(x, 1))

           static void ml_finalize_gdk_font (value val) {
             gdk_font_unref (GdkFont_val(val)); 
           }

           value Val_GdkFont (void* obj) {
             value res;
             gdk_font_ref(obj);
             res = alloc_final (2, ml_finalize_gdk_font, 0, 1);
             GdkFont_val_nocast(res) = (value) obj;
             return res;
           }

           I am guessing here: In the event that the ref function
           is a copy operation, we have to return the copied value
           not obj.

       (2) for SIG

           type gdk_font

       (3) for SML

           type gdk_font = gpointer

    *)

    fun boxedName (TypeExp.LONG(_, TypeExp.POINTER boxed)) = boxed
      | boxedName _ = Util.shouldntHappen "boxedName: not a pointer"
    fun mlBoxedType pointer = TypeInfo.mlBoxedTypeName (boxedName pointer)
    fun mkBoxedDecl (pointer, funcs) =
	let val name = boxedName pointer
	    val name' = TypeInfo.mlBoxedTypeName name
	    fun isCopy func =
		let fun last4 s = 
		        String.extract(s, Int.max(0,String.size s-4), NONE)
		in  "copy" = last4 func
		end
	    val (objExp, refExp, unRefExp) =
		case funcs of
                    [] => ("obj", Empty, Empty)
		  | [refFunc, unRefFunc] => 
			(if isCopy refFunc then "copy" else "obj",
			 $$[if isCopy refFunc then "void* copy = " else "",
                            refFunc, "(obj);"],
			 $$[unRefFunc, " (", name, "_val(val)); "])
		  | _ => raise Fail("wrong number of ref/unref functions (" ^ name ^ ")")
		   
	in  $$["#define ", name, "_val(x) ((void*) Field(x, 1))"] && Nl && Nl
         && $$["#define ", name, "_val_nocast(x) (Field(x, 1))"] && Nl && Nl

         && $$["static void ml_finalize_", name', " (value val) {"] && Nl
         &&  $"  " && unRefExp && Nl
         && $$["}"] && Nl && Nl

         && $$["value Val_", name, " (void* obj) {"] && Nl
         && $$["  value res;"] && Nl
         &&  $"  " && refExp && Nl
         && $$["  res = alloc_final (2, ml_finalize_", name', ", 0, 1);"] && Nl
         && $$["  ", name, "_val_nocast(res) = (value) ", objExp, ";"] && Nl
	 && $$["  return res;"] && Nl
	 && $$["}"] && Nl && Nl
	end

    fun mkMLBoxedDecl pointer = 
	mkTypeDecl (mlBoxedType pointer, NONE) && Nl

    fun mkMLBoxedVal pointer =
	mkTypeDecl (mlBoxedType pointer, SOME ($"gpointer")) && Nl


    (* code to declare enums
       ------------------------------------------------------------
       (1) for C
 
           /* ML type: unit -> int * int * int * int */
           value mgtk_get_font_type (value dummy) { /* ML */
             value res = alloc_tuple(4);
             Field(res,0) = Val_int(GTK_FONT_BITMAP);
             Field(res,1) = Val_int(GTK_FONT_SCALABLE);
             Field(res,2) = Val_int(GTK_FONT_SCALABLE_BITMAP);
             Field(res,3) = Val_int(GTK_FONT_ALL);
             return res;
           }

       (2) for SIG

           type font_type
           val FONT_BITMAP: font_type
           val FONT_SCALABLE: font_type
           val FONT_SCALABLE_BITMAP: font_type
           val FONT_ALL: font_type

       (3) for SML

           type font_type = int
           val get_font_type_: unit -> int * int * int * int
               = app1(symb"mgtk_get_font_type")
           val (FONT_BITMAP,FONT_SCALABLE,FONT_SCALABLE_BITMAP,FONT_ALL)
               = get_font_type_ ()

    *)

    fun flagName (TypeExp.LONG(_, TypeExp.FLAG(fName,_))) = 
	NameUtil.separateWords #"_" (NameUtil.removePrefix fName)
      | flagName _ =
	Util.shouldntHappen "flagName: Not a flag"
    fun flagRealName (TypeExp.LONG(_, TypeExp.FLAG(fName,_))) = fName
      | flagRealName _ = Util.shouldntHappen "flagReal Name: Not a flag"
    fun mlFlagTupleType constr = 
	TypeExp.LONG([], TypeExp.TUPLE(map (fn _=> TypeExp.LONG([], TypeExp.PRIMTYPE "int")) constr))
    fun mkFlagsDecl (flag, constr) =
	let val constr' = ListPair.zip (List.tabulate(List.length constr, fn n=>n), constr)
	    fun prCnstr (n,c) =
		$$["  Field(res,",Int.toString n,") = Val_int(",c,");"]
	    val fName = flagName flag
	    val tupleTyp = mlFlagTupleType constr
	in  (* why don't we use mkFunDecl above? *)
            $"/* ML type: unit -> " && TypeInfo.mkMLPrimType tupleTyp && $" */" && Nl
         && $$["EXTERNML value mgtk_get_", fName, " (value dummy) { /* ML */"] && Nl
         && $$["  value res = alloc_tuple(", Int.toString (List.length constr), ");"] && Nl
         && prsep Nl prCnstr constr' && Nl
         && $$["  return res;"] && Nl
         && $"}" && Nl
         && Nl
	end
    fun mkMLFlagsDecl (flag, constr) =
	let val fName = flagName flag
	    fun prCnstr const = mkValDecl (mlEnumName const, $fName, NONE)
	in  mkEqTypeDecl (fName, NONE)
         && prsep Empty prCnstr constr
         && Nl
	end
    fun mkMLFlagsVal (flag, constr) =
	let val fName = flagName flag
	    val fName' = flagRealName flag
	    val tupleType = mlFlagTupleType constr
	    fun cName const = $(mlEnumName const)
	in  mkTypeDecl (fName, SOME ($"int"))
         && $$[indent,"val get_", fName, "_: "]
	       && $"unit -> " && TypeInfo.mkMLPrimType tupleType && Nl
	       && $$[indent, indent, "= app1(symb\"mgtk_get_",fName,"\")"] && Nl
         && $$[indent,"val ("] && prsep ($",") cName constr && $")" && Nl
         && $$[indent,indent,"= get_", fName, "_ ()"] && Nl
(* we could do this, by the entire gtk.defs file only contains a
   single return value of type flag --- consequently, this seems
   excessive
	 && $$[indent,"val getSet", fName'," = ", "isSet ["] &&
	         prsep ($",") cName constr && $"]" && Nl
*)
         && Nl
	end

    (* signals 
       ------------------------------------------------------------
       (1) for C

           <nothing>

       (2) for SIG

           val connect_clicked: 'a GtkButton -> (unit -> unit) -> unit

       (3) for SML

           val connect_clicked: 'a GtkButton -> (unit -> unit) -> unit
                = fn wid => fn cb => unit_connect wid "clicked" cb
    *)

    fun longTName tName = TypeExp.LONG([], TypeExp.PRIMTYPE tName)

    fun mlConnectType (name, NONE) = 
	TypeExp.LONG([], TypeExp.ARROW([name,
				TypeExp.LONG([], TypeExp.ARROW([longTName "none"], 
						       longTName "none"))],
			       longTName "none"))
      | mlConnectType (name, SOME cb) = 
	TypeExp.LONG([], TypeExp.ARROW([name, cb], longTName "none"))

    fun mlConnectFunction NONE = "unit_connect"
      | mlConnectFunction (SOME(TypeExp.LONG(_,TypeExp.ARROW([TypeExp.LONG(_, TypeExp.PRIMTYPE "none")], TypeExp.LONG(_, TypeExp.PRIMTYPE "bool"))))) =
        "bool_connect"
      | mlConnectFunction (SOME _) = raise Fail("only know callbacks of type unit -> bool")

    (* Generation of C code
       ------------------------------------------------------------
    *)

    fun mkCFunction (name, retTyp, params) =
	   mkFunDecl (name, retTyp, params, mkCall (name, params))
        && let val params' = List.filter (not o TypeInfo.isNullType') params
	       val params'' = map (fn (TypeExp.LONG(path, TypeExp.OPTION t),n) => (TypeExp.LONG(path,TypeExp.PRIMTYPE "<ctype>"),"NULL")
                                    | (t,n) => (t,n))
                                  params
	   in  if List.length params' = List.length params
	       then Empty
	       else mkFunDecl (name ^ "_short", retTyp, params', mkCall (name,params''))
	   end

    fun mkCdecl (AST.OBJECT_DECL(pos, name, fields)) =
          mkWidgetDecl (name, fields)
      | mkCdecl (AST.FUNCTION_DECL(pos, name, retTyp, params)) =
	let val params' = if null params then [dummyPair] else params
	in  mkCFunction (name, retTyp, params)
	end
      | mkCdecl (AST.FLAGS_DECL(pos, name, constr)) =
           mkFlagsDecl (name, constr)
      | mkCdecl (AST.BOXED_DECL(pos, name, funcs, _)) =
	   mkBoxedDecl (name, funcs)
      | mkCdecl _ = Empty


    (* Generation of SML signature
       ------------------------------------------------------------
    *)
    fun mkMLSigdecl (AST.OBJECT_DECL(pos, name, fields)) =
	Nl && mkMLWidgetDecl NONE name
        && (prsep Empty (mkMLGetFieldDecl name) (case fields of SOME fields => fields | NONE => []))
        && Nl
      | mkMLSigdecl (AST.FUNCTION_DECL(pos, name, retTyp, params)) = 
	let val params' = List.filter (not o TypeInfo.isNullType') params
	    val params'' = if null params' then [dummyPair] else params'
	in  mkMLFunDecl NONE (name, mlFunType(retTyp, params))
         && (if List.length params' = List.length params
	     then Empty
             else mkMLFunDecl NONE (name^"'", mlFunType(retTyp, params'')))
	end
      | mkMLSigdecl (AST.FLAGS_DECL(pos, name,constr)) =
	   mkMLFlagsDecl (name, constr)
      | mkMLSigdecl (AST.SIGNAL_DECL(pos, name,signal,cbType)) =
	   mkValDecl ("connect_" ^ mlSignalName signal, 
		      TypeInfo.mkMLType(mlConnectType (name, cbType)), NONE)
      | mkMLSigdecl (AST.BOXED_DECL(pos, name, _, _)) =
	   mkMLBoxedDecl name

    (* Generation of SML structure
       ------------------------------------------------------------
    *)
    fun mkMLStrdecl (AST.OBJECT_DECL(pos, name, fields)) =
	   mkMLWidgetDecl (SOME ($"base")) name
        && (prsep Empty (mkMLGetFieldVal name) (case fields of SOME fields => fields | NONE => []))
        && Nl
      | mkMLStrdecl (AST.FUNCTION_DECL(pos, name, retTyp, params)) = 
	let val params' = List.filter (not o TypeInfo.isNullType') params
	    val params'' = if null params' then [dummyPair] else params'
	in  mkMLFunVal false (name, retTyp, params)
        &&  (if List.length params' = List.length params
	     then Empty
	     else mkMLFunVal true (name, retTyp, params''))
	end
      | mkMLStrdecl (AST.FLAGS_DECL(pos, name,constr)) =
	   mkMLFlagsVal (name, constr)
      | mkMLStrdecl (AST.SIGNAL_DECL(pos, name,signal,cbType)) =
	   let val cnc_func = mlConnectFunction cbType
	   in  mkValDecl ("connect_" ^ mlSignalName signal, 
			  TypeInfo.mkMLType(mlConnectType (name, cbType)),
			  SOME($$["fn wid => fn cb => ", cnc_func,
				  " wid \"", AST.signalOf signal, "\" cb"]))
	   end
      | mkMLStrdecl (AST.BOXED_DECL(pos, name, _, _)) =
	   mkMLBoxedVal name


    (* Main translate function - dispatches on target
       ------------------------------------------------------------
    *)
    fun translate os target decls =
	let val t = case target of
	               AST.C => mkCdecl
                     | AST.SIG => mkMLSigdecl
                     | AST.SML => mkMLStrdecl
	    fun trans d = (t d) 
		          handle exn => 
			      ( Util.explain (Util.extend exn (AST.nameOf d))
			      ; Empty)
	in  app (fn d => (printseq os (trans d))) decls
	end

end (* structure Translate *)
