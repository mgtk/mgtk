(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

structure Translate :> Translate =
struct

    (* Convenience: *)
    structure A  = AST
    structure U  = Util
    structure TE = TypeExp
    structure TI = TypeInfo
    structure NU = NameUtil

    open WSeq
    infix &&

    type parlist = (TE.texp * string) list

    fun mkArrowType (retType, pars) = TE.ARROW(pars, [], pars, retType)

    (* Helper functions *)

    (* indentation in ML files *)
    val indent = "    "

    (* check if an expression, determined from the type of the expression,
       allocates in the MosML sense *)
    fun allocExpression (texp as TE.PRIMTYPE tName) =
	let val primType = WSeq.flatten (TI.mkMLPrimType texp)
	in  (* choose the safe way out: only base types are few
	       base types are considered non-allocating:
            *)
	    not (primType="int" orelse primType="word" orelse primType="bool")
	end
      | allocExpression (TE.WIDGET _) = true
      | allocExpression (TE.POINTER _) = true
      | allocExpression (TE.FLAG _) = false
      | allocExpression (TE.OUTPUT tExp) = allocExpression tExp
      | allocExpression _ = U.shouldntHappen "allocExpression: not a type name"

    fun allocTuple name values =
	let val values' = ListPair.zip (List.tabulate(length values, fn n=>n), values)
	    val allocates = List.exists (allocExpression o #1) values
	    val tupleName = if allocates then "r[0]" else "res"
	    fun prValue (n, (t,v)) =
		$$["  Field(",tupleName,", ",Int.toString n,") = "] && TI.fromCValue (t,v) && $";"
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
	let val args' = List.filter (not o TI.isVoidType') args
	    fun coerce (TE.OUTPUT t, n) = $"&" && mkOutName n
	      | coerce (t, n) = TI.toCValue' (t, $n)
	in  $name && $"(" && prsep ($", ") coerce args' && $")"
	end

    fun paramList (params: parlist) = 
	if TI.fitsDynApp params
	then $"(" && prsep ($", ") ((fn n=> $$["value ",n]) o #2) params && $")"
	else $$["(value mgtk_params)"]

    fun extractParams (params: parlist) =
	let fun field ((t,n),i) =
	        $$["  value ", n, " = ", 
		   "Field(mgtk_params, ", Int.toString i, ");"]
	    val numbers = List.tabulate (List.length params, fn i=>i)
	in  prsep Nl field (ListPair.zip (params, numbers))
	end

    fun size (TE.PRIMTYPE _) = 1
      | size (TE.TUPLE ts) = List.length ts
      | size _ = 0 (* is this really true? *)

    fun declareOutParams (retType, []) = Empty
      | declareOutParams (retType, outPars) =
	let fun decl (tName, name) = 
	        $"  " && TI.mkCType tName && $" " && mkOutName name && $";"
	in     (if size retType > 1 then $"  value res;" && Nl else Empty)
            && prsep Nl decl outPars && Nl
	end

    (* for this function remember that the return type has already
       been combined with the output parameters. *)
    fun mkReturn cExp (retType as (TE.PRIMTYPE "none"), outPars) =
	   $"  " && cExp && $";" && Nl
	&& $"  return " && TI.fromCValue (retType, $"dummy") && $";" && Nl

      | mkReturn cExp (retType as (TE.PRIMTYPE _), []) =
	   $"  return " && TI.fromCValue (retType, cExp) && $";" && Nl
      | mkReturn cExp (retType as (TE.PRIMTYPE _), [(tOut,nOut)]) =
	   $"  " && cExp && $";" && Nl
	&& $"  return " && TI.fromCValue (tOut, mkOutName nOut) && $";" && Nl

      | mkReturn cExp (retType as (TE.WIDGET _), []) =
	   $"  return " && TI.fromCValue (retType, cExp) && $";" && Nl
      | mkReturn cExp (retType as (TE.WIDGET _), [(tOut,nOut)]) =
	   $"  " && cExp && $";" && Nl
	&& $"  return " && TI.fromCValue (tOut, mkOutName nOut) && $";" && Nl

      | mkReturn cExp (retType as (TE.POINTER _), []) =
	   $"  return " && TI.fromCValue (retType, cExp) && $";" && Nl
      | mkReturn cExp (retType as (TE.POINTER _), [(tOut,nOut)]) =
	   $"  " && cExp && $";" && Nl
	&& $"  return " && TI.fromCValue (tOut, mkOutName nOut) && $";" && Nl

      | mkReturn cExp (retType as (TE.FLAG _), []) =
	   $"  return " && TI.fromCValue (retType, cExp) && $";" && Nl
      | mkReturn cExp (retType as (TE.FLAG _), [(tOut,nOut)]) =
	   $"  " && cExp && $";" && Nl
	&& $"  return " && TI.fromCValue (tOut, mkOutName nOut) && $";" && Nl

      | mkReturn cExp (TE.PRIMTYPE _, _) = 
	  U.shouldntHappen("mkReturn: multiple output parameters and return type is a type name")
      | mkReturn cExp (retType as (TE.TUPLE (tOuts as (t::_)), outPars)) =
	if List.length tOuts = List.length outPars (* original return type was none *)
	then    $"  " && cExp && $";" && Nl
	     && allocTuple "res" (map (fn (tOut,nOut) => (tOut, mkOutName nOut)) outPars)
             && $"  return res;" && Nl
	else    $"  value rescall = " && cExp && $";" && Nl
             && allocTuple "res" (map (fn (tOut,nOut) => (tOut, mkOutName nOut)) ((t,"call")::outPars))
             && $"  return res;" && Nl
      | mkReturn cExp (TE.TUPLE _, _) = 
	  U.shouldntHappen("mkReturn: return tuple with only one output type")
      | mkReturn cExp (TE.OUTPUT typExp, outPars) = mkReturn cExp (typExp, outPars)
      | mkReturn cExp (typExp, _) = 
	  U.shouldntHappen("mkReturn: wrong return type")

    fun mkFunDecl (name, funType as TE.ARROW(pars,outPars,cmp,retType), cExp) = 
	let val dummyPair = (TE.PRIMTYPE "none", "dummy")
	in  $"/* ML type: " && TI.mkMLPrimType funType && $" */" && Nl
         && $$["EXTERNML ", "value mgtk_", name] && paramList pars && $" { /* ML */" && Nl
         && declareOutParams (retType, outPars)
         && (if TI.fitsDynApp pars then Empty else extractParams pars && Nl)
         && mkReturn cExp (retType, outPars)
         && $"}" && Nl
         && Nl
	end
      | mkFunDecl _ = raise Fail("mkFunDecl: not a function type")

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

    fun mkMLFunDecl short (path,base,tExp) =
	let val name = if short then TI.MLFunNameWithoutOpt (path,base)
		       else TI.MLFunName (path, base)
	in  mkValDecl (name, TI.mkMLType tExp, NONE)
	end

    fun mkArg (TE.WIDGET _, name) = $$["fn OBJ ", name, " => "]
      | mkArg (typExp, name) = $$["fn ", name, " => "]

    fun unwrapArg (TE.OPTION (TE.WIDGET _), name) =
	$$["(unwrapObjOpt ", name, ")"]
      | unwrapArg (TE.LIST (TE.WIDGET _), name) =
	$$["(map unwrap ", name, ")"]
      | unwrapArg (TE.FLAG (_,false), name) = 
	$$["(setFlags ", name, ")"]
      | unwrapArg (typExp, name) = $name
    fun wrapResult (TE.WIDGET _) res = 
	$"OBJ(" && res && $")"
      | wrapResult (TE.FLAG (_,false)) res =
(* see comment on getSet below
	$"getSet" && $fName && $"(" && res && $")"
*)
	$"getFlags(" && res && $")"
      | wrapResult typExp res = res

    fun mkMLFunVal short (path,base,typ as (TE.ARROW(pars',outPars,_,retTyp'))) =
	let val no_args = if TI.fitsDynApp pars' then List.length pars' else 1
	    val (c_name,ml_name) = 
		    if short then (TI.CFunNameWithoutOpt (path,base), 
				   TI.MLFunNameWithoutOpt (path, base))
		    else (TI.CFunName (path,base), TI.MLFunName (path,base))
	    val primval = 
		   let val value = $$["app", Int.toString no_args,
				      "(symb\"mgtk_", c_name, "\")"]
		   in  mkValDecl (ml_name ^ "_", TI.mkMLPrimType typ, SOME value)
		   end
	    val funcval =  mkMLFunDecl short (path,base,typ)
                        && $$[indent, indent, "= "]
		        && prmap mkArg pars'
                        && wrapResult retTyp' 
                               ($$[ml_name, "_ "] && 
			         (if not(TI.fitsDynApp pars')
				  then $"(" && prsep ($", ") unwrapArg pars' && $")"
				  else prsep ($" ") unwrapArg pars')
                               )
                       && Nl && Nl
	in  primval && funcval
	end
      | mkMLFunVal short _ =
	raise Fail("mkMLFunVal: not a function type")

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

    fun getFieldName (path,base) field = (path, rev (field :: "get" :: rev base))

    fun mkGetField wid (typExp, field) = 
	let val (wid_path, wid_base) = TI.MLName wid
	    val (fld_path, fld_base) = getFieldName (wid_path,wid_base) field
	    val name  = TI.CFunName (wid_path, wid_base)
	    val macro = $(NU.toUpper name) && $"("
                        && TI.toCValue (wid, $"wid") && $")"
	    val cExp = $"(" && macro && $")" && $" -> " && $field
	in  mkFunDecl (TI.CFunName (fld_path, fld_base), 
		       mkArrowType (typExp, [(wid,"wid")]), cExp)
	end

    fun mkMLGetFieldDecl wid (typExp, field) =
	let val (path, base) = getFieldName (TI.MLName wid) field
	in  mkMLFunDecl false (path, base, mkArrowType (typExp,[(wid,"wid")]))
	end

    fun mkMLGetFieldVal wid (typExp, field) =
	let val (path, base) = getFieldName (TI.MLName wid) field
	in  mkMLFunVal false (path, base, mkArrowType (typExp, [(wid,"wid")]))
	end

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
	   $$["/* *** ", TI.MLShortWidgetName wid, " stuff *** */"] && Nl && Nl
        && (prsep Nl (mkGetField wid) (case fields of SOME fields => fields | NONE => []))
        && Nl

    fun inheritsFrom (TE.WIDGET(_, TE.INH_FROM inherits)) = inherits
      | inheritsFrom (TE.WIDGET(_, TE.INH_ROOT)) = 
	U.shouldntHappen "inheritsFrom: got GtkObject"
      | inheritsFrom _ = U.shouldntHappen "inheritsFrom: not a widget"
    fun mkMLWidgetDecl base wid = 
	let val name = TI.MLWidgetName wid
	    val short = TI.MLShortWidgetName wid
	    val witness = $$["'a ", NU.toLower short, "_t"]
	in  mkComment ($$["*** ", short, " ***"]) && Nl

	 && mkTypeDecl' (witness, base)
         && mkTypeDecl' ($"'a " && $name, 
			 SOME(witness && $" " && $(TI.MLNamePath (inheritsFrom wid))))
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
    fun boxedInherits (TE.POINTER (_,inh)) = inh
      | boxedInherits _ = U.shouldntHappen "boxedInherits: not a pointer"

    fun mkBoxedDecl (pointer, funcs) =
	let val name = TI.MLBoxedName pointer
	    val name' = name
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

    fun mkMLBoxedDecl' base pointer = 
	case boxedInherits pointer of
	    NONE => 
		mkTypeDecl' (TI.mkMLType pointer, if base=NONE then NONE
						  else SOME ($"gpointer"))
	     && Nl
        |   SOME (TE.INH_ROOT) => 
		mkTypeDecl' (TI.mkMLType pointer, if base=NONE then NONE
						  else SOME ($"gpointer"))
	     && Nl
        |   SOME (TE.INH_FROM inh) =>
		let val inheritsName = TI.MLFlagName (TE.POINTER(inh,NONE))
		    (* hack to avoid complications between GdkWindow and
		       GtkWindow *)
		    val short = TI.MLWidgetName pointer
		    val witness = $$["'a ", NU.toLower short, "_t"]
		in  mkTypeDecl' (witness, base)
		 && mkTypeDecl' (TI.mkMLType pointer, 
				 SOME(witness && $" " && $inheritsName))
		 && Nl
                end
    fun mkMLBoxedDecl pointer = mkMLBoxedDecl' NONE pointer
    fun mkMLBoxedVal pointer = mkMLBoxedDecl' (SOME ($"base")) pointer


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

    fun mlFlagTupleType constr = 
	TE.TUPLE(map (fn _=> TE.PRIMTYPE "int") constr)

    fun mkFlagsDecl (flag, constr) =
	let val constr' = ListPair.zip (List.tabulate(List.length constr, fn n=>n), constr)
	    fun prCnstr (n,c) =
		$$["  Field(res,",Int.toString n,") = Val_int(",TI.CConstrName c,");"]
	    val fName = TI.MLFlagName flag
	    val tupleTyp = mlFlagTupleType constr
	in  (* why don't we use mkFunDecl above? *)
            $"/* ML type: unit -> " && TI.mkMLPrimType tupleTyp && $" */" && Nl
         && $$["EXTERNML value mgtk_get_", fName, " (value dummy) { /* ML */"] && Nl
         && $$["  value res = alloc_tuple(", Int.toString (List.length constr), ");"] && Nl
         && prsep Nl prCnstr constr' && Nl
         && $$["  return res;"] && Nl
         && $"}" && Nl
         && Nl
	end
    fun mkMLFlagsDecl (flag, constr) =
	let val fName = TI.MLFlagName flag
	    fun prCnstr const = mkValDecl (TI.MLConstrName const, $fName, NONE)
	in  mkEqTypeDecl (fName, NONE)
         && prsep Empty prCnstr constr
         && Nl
	end
    fun mkMLFlagsVal (flag, constr) =
	let val fName = TI.MLFlagName flag
	    val tupleType = mlFlagTupleType constr
	    fun cName const = $(TI.MLConstrName const)
	in  mkTypeDecl (fName, SOME ($"int"))
         && $$[indent,"val get_", fName, "_: "]
	       && $"unit -> " && TI.mkMLPrimType tupleType && Nl
	       && $$[indent, indent, "= app1(symb\"mgtk_get_",fName,"\")"] && Nl
         && $$[indent,"val ("] && prsep ($",") cName constr && $")" && Nl
         && $$[indent,indent,"= get_", fName, "_ ()"] && Nl
(* we could do this, but the entire gtk.defs file only contains a
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

    val unitType = TE.PRIMTYPE "none"
    fun mlConnectType (name, NONE) = 
	mkArrowType (unitType,
		   [(name,"wid"), (mkArrowType(unitType, [(unitType, "()")]), "cb")])
      | mlConnectType (name, SOME cb) = 
	mkArrowType (unitType, [(name,"wid"),(cb,"cb")])

    fun mlConnectFunction NONE = "unit_connect"
      | mlConnectFunction (SOME(TE.ARROW([(TE.PRIMTYPE "none",_)], _, _, TE.PRIMTYPE "bool"))) =
        "bool_connect"
      | mlConnectFunction (SOME _) = raise Fail("only know callbacks of type unit -> bool")

    (* Generation of C code
       ------------------------------------------------------------
    *)
    fun getParams (TE.ARROW(_,_,cmp,_)) = cmp
      | getParams _ = raise Fail("getParams: not a function type")
    fun mkCFunction (path, base, A.FUNTYPE(funType, NONE)) =
	   mkFunDecl (TI.CFunName (path, base), funType, mkCall (TI.CFunName (path,base), getParams funType))
      | mkCFunction (path, base, A.FUNTYPE(funType, SOME shortFunType)) =
	let fun mkNULL (TE.OPTION _,n) = (TE.PRIMTYPE "<ctype>","NULL")
              | mkNULL (t,n) = (t,n)
	in  mkFunDecl (TI.CFunName (path,base), funType, mkCall (TI.CFunName (path,base), getParams funType))
         && mkFunDecl (TI.CFunNameWithoutOpt (path,base), shortFunType, mkCall (TI.CFunName (path,base),map mkNULL (getParams funType)))
	end

    fun mkCdecl (A.MODULE_DECL(pos,exp,path)) = Empty
      | mkCdecl (A.OBJECT_DECL(pos, name, fields)) =
          mkWidgetDecl (name, fields)
      | mkCdecl (A.FUNCTION_DECL(pos, (path, base), funType)) =
	  mkCFunction (path, base, funType)
      | mkCdecl (A.FLAGS_DECL(pos, name, constr)) =
           mkFlagsDecl (name, constr)
      | mkCdecl (A.BOXED_DECL(pos, name, funcs)) =
	   mkBoxedDecl (name, funcs)
      | mkCdecl _ = Empty


    (* Generation of SML signature
       ------------------------------------------------------------
    *)
    fun mkMLSigdecl (A.MODULE_DECL(pos,exp,path)) = Empty
(*
	Nl && mkComment ($(U.stringSep "++++ " (if exp then " (explicit) ++++"
						else " (implicit) ++++")
			               "." (fn s=>s) path))
*)
      | mkMLSigdecl (A.OBJECT_DECL(pos, name, fields)) =
	   Nl
        && mkMLWidgetDecl NONE name
        && (prsep Empty (mkMLGetFieldDecl name) (case fields of SOME fields => fields | NONE => []))
        && Nl
      | mkMLSigdecl (A.FUNCTION_DECL(pos, (path,base), A.FUNTYPE(longType, NONE))) = 
  	   mkMLFunDecl false (path, base, longType)
      | mkMLSigdecl (A.FUNCTION_DECL(pos, (path,base), A.FUNTYPE(longType, SOME shortType))) =
	   mkMLFunDecl false (path,base, longType) 
	&& mkMLFunDecl true  (path,base, shortType)
      | mkMLSigdecl (A.FLAGS_DECL(pos, name,constr)) =
	   mkMLFlagsDecl (name, constr)
      | mkMLSigdecl (A.SIGNAL_DECL(pos, name,signal,cbType)) =
	   mkValDecl ("connect_" ^ TI.MLSignalName signal, 
		      TI.mkMLType(mlConnectType (name, cbType)), NONE)
      | mkMLSigdecl (A.BOXED_DECL(pos, name, _)) =
	   mkMLBoxedDecl name

    (* Generation of SML structure
       ------------------------------------------------------------
    *)
    fun mkMLStrdecl (A.MODULE_DECL(pos,exp,path)) = Empty
      | mkMLStrdecl (A.OBJECT_DECL(pos, name, fields)) =
	   mkMLWidgetDecl (SOME ($"base")) name
        && (prsep Empty (mkMLGetFieldVal name) (case fields of SOME fields => fields | NONE => []))
        && Nl
      | mkMLStrdecl (A.FUNCTION_DECL(pos, (path,base), A.FUNTYPE(longType,NONE))) = 
	   mkMLFunVal false (path,base, longType)
      | mkMLStrdecl (A.FUNCTION_DECL(pos, (path,base), A.FUNTYPE(longType, SOME shortType))) =
	   mkMLFunVal false (path, base, longType) 
        && mkMLFunVal true  (path, base, shortType)
      | mkMLStrdecl (A.FLAGS_DECL(pos, name,constr)) =
	   mkMLFlagsVal (name, constr)
      | mkMLStrdecl (A.SIGNAL_DECL(pos, name,signal,cbType)) =
	   let val cnc_func = mlConnectFunction cbType
	   in  mkValDecl ("connect_" ^ TI.MLSignalName signal, 
			  TI.mkMLType(mlConnectType (name, cbType)),
			  SOME($$["fn wid => fn cb => ", cnc_func,
				  " wid \"", A.signalOf signal, "\" cb"]))
	   end
      | mkMLStrdecl (A.BOXED_DECL(pos, name, _)) =
	   mkMLBoxedVal name


    (* Main translate function - dispatches on target
       ------------------------------------------------------------
    *)
    fun translate os target decls =
	let val t = case target of
	               A.C => mkCdecl
                     | A.SIG => mkMLSigdecl
                     | A.SML => mkMLStrdecl
	    fun trans d = (t d) 
		          handle exn => 
			      ( U.explain (U.extend exn (A.nameOf d))
			      ; Empty)
	in  app (fn d => (printseq os (trans d))) decls
	end

end (* structure Translate *)
