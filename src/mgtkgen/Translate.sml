(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

structure Translate :> Translate =
struct

    open WSeq NameUtil
    infix &&

    fun id x = x

    (* 5 is this is the largest X, such that Dynlib.appX exists *)
    fun fitsDynApp params = List.length params <= 5

    (* mappings between defs strings representing (type) names,
       and the corresponding ML (type) names *)
    fun mlWidgetName typExp = removePrefix typExp
    fun mlFunName name = remove_prefix name
    fun mlSignalName signal = String.map (fn #"-" => #"_" | ch => ch) signal
    fun mlFlagName name = remove_PREFIX name
    fun mlEnumName name = remove_PREFIX name


    (* insert declaration in type info table *)
    fun insertDecl (AST.OBJECT_DECL(pos, name,inherits,_)) =
	TypeInfo.insert (name, TypeInfo.objectInfo name)
      | insertDecl (AST.FLAGS_DECL(pos, name,constr)) =
	TypeInfo.insert (name, TypeInfo.enumInfo (name,constr))
      | insertDecl (AST.BOXED_DECL(pos, name, funcs, _)) =
	TypeInfo.insert (name, TypeInfo.boxedInfo (name, funcs))
      | insertDecl _ = ()



    fun toCValue (AST.TYPENAME "<ctype>", name) = name
      | toCValue (AST.TYPENAME typExp, name) = (#toCValue (TypeInfo.lookupTypeName typExp)) name
      | toCValue (typExp, name) = Util.notImplemented ("toCValue: not a type name: " ^ AST.typeClass typExp)

    fun toCValue' (typExp as (AST.OPTION typExp'), name) =
	if TypeInfo.isNullType typExp
	then if TypeInfo.isPrimVal typExp' then $"GtkObjOption_nullok(" && name && $")"
	     else if TypeInfo.isString typExp' then $"StringOption_nullok(" && name && $")"
             else raise Fail("Translate.toCValue': can only handle primitive values and strings: " ^ AST.typeClass typExp')
        else toCValue (typExp, name)
      | toCValue' (typExp, name) = toCValue (typExp, name)

    fun fromCValue (AST.TYPENAME typExp, name) = (#fromCValue (TypeInfo.lookupTypeName typExp)) name
      | fromCValue (AST.OUTPUT (typExp as (AST.TYPENAME _)), name) = 
	  if TypeInfo.isPrimVal typExp 
	  then fromCValue (typExp, $"&" && name)
	  else fromCValue (typExp, name)
      | fromCValue (typExp, name) = Util.notImplemented ("fromCValue: not a type name: " ^ WSeq.flatten name ^ " of " ^ AST.typeClass typExp)


    (* Types *)

    val mlPrimType = #mlPrimType o TypeInfo.lookupTypeName
    val cType = #cType o TypeInfo.lookupTypeName

    fun mlType arg tName = (#mlType (TypeInfo.lookupTypeName tName)) arg
    val mlPrimType = fn (_:wseq) => fn tName => mlPrimType tName


    fun mkCType (AST.TYPENAME name) = cType name
      | mkCType (AST.OUTPUT t) = mkCType t
      | mkCType _ = Util.notImplemented "mkCType: not a type name"

    local
	fun parens true wseq = $"(" && wseq && $")"
          | parens false wseq = wseq

	fun mkType nest mkTName tArg (AST.TYPENAME tName) = 
	    mkTName (tArg ()) tName
	  | mkType nest mkTName tArg (AST.TUPLE tArgs) = 
	    prsep ($" * ") (mkType true mkTName tArg) tArgs
	  | mkType nest mkTName tArg (AST.ARROW(tArgs, tRet)) =
	    parens nest (prsep ($" -> ") (mkType true mkTName tArg) tArgs
			&& $" -> " && mkType true mkTName (fn _ => $"base") tRet
                        )
	  | mkType nest mkTName tArg (AST.OPTION t) =
	    (mkType nest mkTName tArg t) && $" option"
	  | mkType nest mkTName tArg (AST.OUTPUT t) =
	    mkType nest mkTName tArg t

	val index = ref 0
	fun reset () = index := 0
	fun fresh () = ($("'" ^ Char.toString (Char.chr (Char.ord #"a" + !index))) before
			index := !index + 1)
		       
    in (* local *)

	(* would we like versions with explicit type arguments? *)
	fun mkMLFreshType typExp = 
	    (reset (); mkType false mlType fresh typExp)
	fun mkMLType typExp = mkMLFreshType typExp

	fun mkMLPrimFreshType typExp = 
	    (reset (); mkType false mlPrimType fresh typExp)
	fun mkMLPrimType (AST.ARROW(tArgs, tRet)) =
	    if fitsDynApp tArgs (* check if it fits appX *)
	    then mkMLPrimFreshType (AST.ARROW(tArgs, tRet))
	    else mkMLPrimFreshType (AST.ARROW([AST.TUPLE tArgs], tRet))
          | mkMLPrimType t = mkMLPrimFreshType t

    end (* local *)
	
    type parlist = (AST.type_expression * string) list

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
                     | ts => AST.TUPLE ts
		end
	    fun removeOutput (AST.OUTPUT t, n) = (t, n)
              | removeOutput (t,n) = (t,n)
	    val (outPars, pars) = splitList (TypeInfo.isOutputType o #1) params
	in  (outPars, pars, mkTuple (map #1 outPars, retType))
	end

    fun mlFunType (retType, []: parlist) = 
	raise Fail ("mlFunType: no parameters")
      | mlFunType (retType, params: parlist) =
	let val (outPars, pars, retType) = separateParams (retType, params)
	in  AST.ARROW(map #1 pars, retType) 
	end


    (* Helper functions *)

    (* indentation in ML files *)
    val indent = "    "

    fun allocTuple name values =
	let val values' = ListPair.zip (List.tabulate(List.length values, fn n=>n), values)
	    fun prValue (n, (t,v)) =
		$$["  Field(",name,", ",Int.toString n,") = "] && fromCValue (t,v) && $";"
	in  $$["  ", name, " = alloc_tuple(", Int.toString (List.length values), ");"] && Nl
         && prsep Nl prValue values'
         && Nl
	end

    (* ML comment *)
    fun mkComment cmt =	$indent && $"(* " && cmt && $" *)" && Nl

    (* ML primitive type specification *)
    fun mkPrimTypeDecl' tName = $indent && $"prim_type " && tName && Nl
    fun mkPrimTypeDecl tName = mkPrimTypeDecl' ($tName)

    (* ML type specification *)
    fun mkTypeDecl' (tName, NONE) =
	$indent && $"type " && tName && Nl
      | mkTypeDecl' (tName, SOME tExp) =
	$indent && $"type " && tName && $" = " && tExp && Nl

    fun mkTypeDecl (tName, tExpOpt) = mkTypeDecl' ($tName, tExpOpt)

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
	    fun coerce (AST.OUTPUT t, n) = $"&" && mkOutName n
	      | coerce (t, n) = toCValue' (t, $n)
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

    fun size (AST.TYPENAME _) = 1
      | size (AST.TUPLE ts) = List.length ts
      | size _ = 0 (* is this really true? *)

    fun declareOutParams (retType, []) = Empty
      | declareOutParams (retType, outPars) =
	let fun decl (tName, name) = 
	        $"  " && mkCType tName && $" " && mkOutName name && $";"
	in     (if size(retType) > 1 then $"  value res;" && Nl else Empty)
            && prsep Nl decl outPars && Nl
	end

    (* for this function remember that the return type has already
       been combined with the output parameters. *)
    fun mkReturn cExp (retType as (AST.TYPENAME "none"), outPars) =
	   $"  " && cExp && $";" && Nl
	&& $"  return " && fromCValue (retType, $"dummy") && $";" && Nl
      | mkReturn cExp (retType as (AST.TYPENAME _), []) =
	   $"  return " && fromCValue (retType, cExp) && $";" && Nl
      | mkReturn cExp (retType as (AST.TYPENAME _), [(tOut,nOut)]) =
	   $"  " && cExp && $";" && Nl
	&& $"  return " && fromCValue (tOut, mkOutName nOut) && $";" && Nl
      | mkReturn cExp (AST.TYPENAME _, _) = 
	  Util.shouldntHappen("mkReturn: multiple output parameters and return type is a type name")
      | mkReturn cExp (retType as (AST.TUPLE (tOuts as (t::_)), outPars)) =
	if List.length tOuts = List.length outPars (* original return type was none *)
	then    $"  " && cExp && $";" && Nl
	     && allocTuple "res" (map (fn (tOut,nOut) => (tOut, mkOutName nOut)) outPars)
             && $"  return res;" && Nl
	else    $"  value rescall = " && cExp && $";" && Nl
             && allocTuple "res" (map (fn (tOut,nOut) => (tOut, mkOutName nOut)) ((t,"call")::outPars))
             && $"  return res;" && Nl
      | mkReturn cExp (AST.TUPLE _, _) = 
	  Util.shouldntHappen("mkReturn: return tuple with only one output type")
      | mkReturn cExp (AST.OUTPUT typExp, outPars) = mkReturn cExp (typExp, outPars)
      | mkReturn cExp (typExp, _) = 
	  Util.shouldntHappen("mkReturn: wrong return type class " ^ AST.typeClass typExp)

    fun mkFunDecl (name, retTyp, params, cExp) = 
	let val params = if null params then [(AST.TYPENAME "none", "dummy")]
			 else params
	    val (outPars, pars, retType) = separateParams (retTyp, params)
	    val typ = mlFunType (retTyp, params)
	in  $"/* ML type: " && mkMLPrimType typ && $" */" && Nl
         && $$["value m", name] && paramList pars && $" { /* ML */" && Nl
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
	in  mkValDecl (name', mkMLType tExp, NONE)
	end

    fun mkArg (typExp, name) =
	if TypeInfo.isNullType typExp then $$["fn ", name, " => "]
	else if TypeInfo.isWidget typExp then $$["fn OBJ ", name, " => "]
        else $$["fn ", name, " => "]
    fun unwrapArg (AST.OPTION typExp, name) =
	if TypeInfo.isWidget typExp then $$["(unwrapObjOpt ", name, ")"]
	else $name
      | unwrapArg (typExp, name) = $name
    fun wrapResult typExp res =
	if TypeInfo.isWidget typExp then $"OBJ(" && res && $")"
        else res

    fun mkMLFunVal short (name, retTyp, params) =
	let val (outPars, pars', retTyp') = separateParams (retTyp, params)

	    val args = List.length pars'
	    val no_args = if fitsDynApp pars' then args else 1
	    val name' = remove_prefix name
	    val (c_name,ml_name) = 
		    if short then (name ^ "_short", name' ^ "'")
		    else (name, name')
	    val primval = 
		   let val typ = mlFunType (retTyp, params)
		       val value = $$["app", Int.toString no_args,
				      "(symb\"m", c_name, "\")"]
		   in  mkValDecl (ml_name ^ "_", mkMLPrimType typ, SOME value)
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
	"gtk_" ^ separateWords #"_" (NameUtil.removePrefix name) ^ "_get_" ^ field

    fun mkGetField name (typExp, field) = 
	let val params = [(AST.TYPENAME name, "wid")]
	    val name' = separateWords #"_" (NameUtil.removePrefix name)
	    val macro = $$["GTK_", toUpper name',"("]
                        && toCValue (AST.TYPENAME name, $"wid") && $")"
	    val cExp = $"(" && macro && $")" && $" -> " && $field
	in  mkFunDecl (getFieldName (name, field), typExp, params, cExp)
	end

    fun mkMLGetFieldDecl name (typExp, field) =
	let val params = [(AST.TYPENAME name, "wid")]
	in  mkMLFunDecl ($"base") (getFieldName(name,field), mlFunType (typExp,params))
	end

    fun mkMLGetFieldVal name (typExp, field) =
	let val params = [(AST.TYPENAME name, "wid")]
	in  mkMLFunVal false (getFieldName(name,field), typExp, params)
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

    fun mkWidgetDecl (name, fields) =
	   $$["/* *** ", removePrefix name, " stuff *** */"] && Nl && Nl
        && (prsep Nl (mkGetField name) (case fields of SOME fields => fields | NONE => []))
        && Nl

    fun mkMLWidgetDecl base (name, inherits) =
	let val name' = mlWidgetName name
	    val witness = $$["'a ", toLower name', "_t"]
	in  mkComment ($$["*** ", removePrefix name, " ***"]) && Nl

	 && mkTypeDecl' (witness, base)
         && mkTypeDecl' ($"'a " && $name, SOME(witness && $" " && $inherits))
         && Nl
	end


    (* Code to declare ``boxed'' types
       ------------------------------------------------------------

       (1) for C

           #define GdkFont_val(x) ( ( void* ) Field(x, 1))

           static void ml_finalize_gdk_font (value val) {
             gdk_font_unref (GdkFont_val(val)); 
           }

           value Val_GdkFont (void* obj) {
             value res;
             gdk_font_ref(obj);
             res = alloc_final (2, ml_finalize_gdk_font, 0, 1);
             GdkFont_val(res) = (value) obj;
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

    fun mkBoxedDecl (name, funcs) =
	let val name' = TypeInfo.mlBoxedTypeName name
	    val (refFunc, unRefFunc) =
		case funcs of
		    [refFunc, unRefFunc] => (refFunc, unRefFunc)
		  | _ => raise Fail("wrong number of ref/unref functions (" ^ name ^ ")")
	    fun isCopy func =
		let fun last4 s = 
		        String.extract(s, Int.max(0,String.size s-4), NONE)
		in  "copy" = last4 func
		end
		   
	in  $$["#define ", name, "_val(x) ((void*) Field(x, 1))"] && Nl && Nl

         && $$["static void ml_finalize_", name', " (value val) {"] && Nl
         && $$["  ", unRefFunc, " (", name, "_val(val)); "] && Nl
         && $$["}"] && Nl && Nl

         && $$["value Val_", name, " (void* obj) {"] && Nl
         && $$["  value res;"] && Nl
         && $$["  ", if isCopy refFunc then "void* copy = " else "", refFunc, "(obj);"] && Nl
         && $$["  res = alloc_final (2, ml_finalize_", name', ", 0, 1);"] && Nl
         && $$["  ", name, "_val(res) = (value) ", if isCopy refFunc then "copy" else "obj", " ;"] && Nl
	 && $$["  return res;"] && Nl
	 && $$["}"] && Nl && Nl
	end

    fun mkMLBoxedDecl name = mkTypeDecl (TypeInfo.mlBoxedTypeName name, NONE) && Nl

    fun mkMLBoxedVal name =
	mkTypeDecl (TypeInfo.mlBoxedTypeName name, SOME ($"gpointer")) && Nl


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

    fun mlPrimEnumType (name, constr) =	AST.TUPLE(map (fn _=> AST.TYPENAME "int") constr)
    fun mlEnumType (name, constr) = AST.TYPENAME(TypeInfo.mlEnumTypeName name)

    fun mkEnumDecl (name, constr) =
	let val type_name = TypeInfo.mlEnumTypeName name
	    val constr' = ListPair.zip (List.tabulate(List.length constr, fn n=>n), constr)
	    fun prCnstr (n, (_,c)) =
		$$["  Field(res,",Int.toString n,") = Val_int(",c,");"]
	    val typ = mlPrimEnumType (name, constr)
	in  $"/* ML type: unit -> " && mkMLPrimType typ && $" */" && Nl
         && $$["value mgtk_get_", type_name, " (value dummy) { /* ML */"] && Nl
         && $$["  value res = alloc_tuple(", Int.toString (List.length constr), ");"] && Nl
         && prsep Nl prCnstr constr' && Nl
         && $$["  return res;"] && Nl
         && $"}" && Nl
         && Nl
	end

    fun mkMLEnumDecl (name, constr) =
	let val type_name = TypeInfo.mlEnumTypeName name
	    val ml_type = mkMLType (AST.TYPENAME name)
	    fun prCnstr (n,const) = mkValDecl (mlFlagName const, ml_type, NONE)
	in  mkTypeDecl (type_name, NONE)
         && prsep Empty prCnstr constr
         && Nl
	end

    fun mkMLEnumVal (name, constr) =
	let val type_name = TypeInfo.mlEnumTypeName name
	    val typ = mlPrimEnumType (name, constr)
	    fun cName (name, const) = $(remove_PREFIX const)
	in  $$[indent,"type ", type_name, " = int"] && Nl
         && $$[indent,"val get_", type_name, "_: "]
	       && $"unit -> " && mkMLPrimType typ && Nl
	       && $$[indent, indent, "= app1(symb\"mgtk_get_",type_name,"\")"] && Nl
         && $$[indent,"val ("] && prsep ($",") cName constr && $")" && Nl
         && $$[indent,indent,"= get_", type_name, "_ ()"] && Nl
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

    fun mlConnectType (name, NONE) = 
	AST.ARROW([AST.TYPENAME name, AST.ARROW([AST.TYPENAME "none"], AST.TYPENAME "none")], AST.TYPENAME "none")
      | mlConnectType (name, SOME cb) = 
	AST.ARROW([AST.TYPENAME name, cb], AST.TYPENAME "none")

    fun mlConnectFunction NONE = "unit_connect"
      | mlConnectFunction (SOME(AST.ARROW([AST.TYPENAME "none"], AST.TYPENAME "bool"))) =
        "bool_connect"
      | mlConnectFunction (SOME _) = raise Fail("only know callbacks of type unit -> bool")


    (* Generation of C code
       ------------------------------------------------------------
    *)

    fun mkCFunction (name, retTyp, params) =
	   mkFunDecl (name, retTyp, params, mkCall (name, params))
        && let val params' = List.filter (not o TypeInfo.isNullType') params
	       val params'' = map (fn (AST.OPTION t,n) => (AST.TYPENAME "<ctype>","NULL")
                                    | (t,n) => (t,n))
                                  params
	   in  if List.length params' = List.length params
	       then Empty
	       else mkFunDecl (name ^ "_short", retTyp, params', mkCall (name,params''))
	   end

    fun mkCdecl (AST.OBJECT_DECL(pos, name, _, fields)) =
          mkWidgetDecl (name, fields)
      | mkCdecl (AST.FUNCTION_DECL(pos, name, retTyp, params)) =
	let val params' = if null params then [(AST.TYPENAME "none","dummy")]
			  else params
	in  mkCFunction (name, retTyp, params)
	end
      | mkCdecl (AST.FLAGS_DECL(pos, name, constr)) =
           mkEnumDecl (name, constr)
      | mkCdecl (AST.BOXED_DECL(pos, name, funcs, _)) =
	   mkBoxedDecl (name, funcs)
      | mkCdecl _ = Empty


    (* Generation of SML signature
       ------------------------------------------------------------
    *)
    fun mkMLSigdecl (AST.OBJECT_DECL(pos, name, inherits, fields)) =
	Nl && mkMLWidgetDecl NONE (name, inherits)
        && (prsep Empty (mkMLGetFieldDecl name) (case fields of SOME fields => fields | NONE => []))
        && Nl
      | mkMLSigdecl (AST.FUNCTION_DECL(pos, name, retTyp, params)) = 
	let val params' = List.filter (not o TypeInfo.isNullType') params
	    val params' = if null params' then [(AST.TYPENAME "none","dummy")]
			  else params'
	in  mkMLFunDecl NONE (name, mlFunType(retTyp, params))
         && (if List.length params' = List.length params
	     then Empty
             else mkMLFunDecl NONE (name^"'", mlFunType(retTyp, params')))
	end
      | mkMLSigdecl (AST.FLAGS_DECL(pos, name,constr)) =
	   mkMLEnumDecl (name, constr)
      | mkMLSigdecl (AST.SIGNAL_DECL(pos, name,signal,cbType)) =
	   mkValDecl ("connect_" ^ mlSignalName signal, 
		      mkMLType(mlConnectType (name, cbType)), NONE)
      | mkMLSigdecl (AST.BOXED_DECL(pos, name, _, _)) =
	   mkMLBoxedDecl name

    (* Generation of SML structure
       ------------------------------------------------------------
    *)
    fun mkMLStrdecl (AST.OBJECT_DECL(pos, name, inherits, fields)) =
	   mkMLWidgetDecl (SOME ($"base")) (name, inherits)
        && (prsep Empty (mkMLGetFieldVal name) (case fields of SOME fields => fields | NONE => []))
        && Nl
      | mkMLStrdecl (AST.FUNCTION_DECL(pos, name, retTyp, params)) = 
	let val params' = List.filter (not o TypeInfo.isNullType') params
	    val params'' = if null params' then [(AST.TYPENAME "none","dummy")]
			   else params'
	in  mkMLFunVal false (name, retTyp, params)
        &&  (if List.length params' = List.length params
	     then Empty
	     else mkMLFunVal true (name, retTyp, params''))
	end
      | mkMLStrdecl (AST.FLAGS_DECL(pos, name,constr)) =
	   mkMLEnumVal (name, constr)
      | mkMLStrdecl (AST.SIGNAL_DECL(pos, name,signal,cbType)) =
	   let val cnc_func = mlConnectFunction cbType
	   in  mkValDecl ("connect_" ^ mlSignalName signal, 
			  mkMLType(mlConnectType (name, cbType)),
			  SOME($$["fn wid => fn cb => ", cnc_func,
				  " wid \"", signal, "\" cb"]))
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
	in  app (fn d => (insertDecl d; printseq os (trans d))) decls
	end

end (* structure Translate *)