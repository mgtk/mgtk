(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

(* TODO: Clean-up this code and document it! *)

structure TypeInfo =
struct

    open WSeq
    infix &&

    fun mlEnumTypeName name = NameUtil.separateWords #"_" (NameUtil.removePrefix name)
    fun mlBoxedTypeName name = NameUtil.separateWords #"_" name

    type type_info = {mlType: wseq -> wseq, 
		      mlPrimType: wseq,
		      cType: wseq,
		      toCValue: wseq -> wseq,
		      fromCValue: wseq -> wseq,
		      widget: bool,
		      primitive: bool
		     }

    fun cstFnc value = fn _ => $value
    fun wrapFnc value = fn n => $value && $"(" && n && $")"
    fun info (primTyp, typ, cTyp, toC, fromC, prim): type_info =
	{mlType = cstFnc typ, mlPrimType = $primTyp, cType = $cTyp, 
	 widget=false, primitive = prim,
	 toCValue = toC, fromCValue = fromC}

    val unitInfo = info ("unit", "unit", "void", cstFnc "Unit_val", cstFnc "Val_unit", false)
    val intInfo = info ("int", "int", "int", wrapFnc "Int_val", wrapFnc "Val_int", false)
    val wordInfo = (* this is NOT correct *)
	  info ("word", "word", "unsigned int", wrapFnc "Int_val", wrapFnc "Val_int", false)
    val staticStringInfo = (* this is NOT correct *)
          info ("string", "string", "char*", wrapFnc "String_val", wrapFnc "copy_string", false)
    val stringInfo = staticStringInfo
    val boolInfo = info ("bool", "bool", "int", wrapFnc "Bool_val", wrapFnc "Val_bool", false)
    val realInfo = info ("real", "real", "int", wrapFnc "Double_val", wrapFnc "copy_double", false)
    fun enumInfo (typExp,constr) =
	info ("int", mlEnumTypeName typExp, typExp, wrapFnc "Int_val", wrapFnc "Val_int", false)
    fun boxedInfo (typExp, funcs) =
	info (mlBoxedTypeName typExp, mlBoxedTypeName typExp, typExp,
	      wrapFnc (typExp ^ "_val"), wrapFnc ("Val_" ^ typExp),
	      true)

    fun objectInfo objTyp =
	    {mlType=fn argTyp => argTyp && $" " && $objTyp, 
	     mlPrimType= $"gtkobj", 
	     cType= $"GtkObject",
	     widget=true,
	     primitive=true,
	     toCValue=fn n => $"GtkObj_val(" && n && $")",
	     fromCValue=fn n => $"Val_GtkObj(" && n && $")"
	    }

(*
    fun hash str = Char.ord(String.sub(str, 0))
    fun compare (str1, str2) = str1 = str2
*)
    exception Find
    val (table: (string, type_info) Polyhash.hash_table) = 
	   Polyhash.mkPolyTable (*(hash, compare)*) (99, Find)

    (* insert some ``base'' types *)
    val _ = app (Polyhash.insert table)
	       [
		("none",      unitInfo),
		("int",       intInfo),
		("uint",      wordInfo),
		("float",     realInfo),
		("bool",      boolInfo),
		("string",    stringInfo),
		("static_string",    staticStringInfo),
                ("GtkObject", objectInfo "GtkObject"),
                ("GtkWidget", objectInfo "GtkWidget")
               ]

    val insert = Polyhash.insert table
    fun lookupTypeName tName =
	((Polyhash.find table tName)
         handle Find => Util.notFound("unbound type name: " ^ tName))

    (* predicates *)
    fun isWidget (AST.TYPENAME tName) = #widget (lookupTypeName tName)
      | isWidget _ = false

    fun isPrimVal (AST.TYPENAME tName) = #primitive (lookupTypeName tName)
      | isPrimVal _ = false

    fun isString (AST.TYPENAME "string") = true
      | isString (AST.TYPENAME "static_string") = true
      | isString _ = false

    fun isNullType (AST.OPTION _) = true
      | isNullType _ = false
    fun isNullType' (typExp, name) = isNullType typExp

    fun isOutputType (AST.OUTPUT _) = true
      | isOutputType _ = false
    fun isOutputType' (typExp, name) = isOutputType typExp

    fun isVoidType (AST.TYPENAME "none") = true
      | isVoidType _ = false
    fun isVoidType' (typExp, name) = isVoidType typExp      
 
end (* structure TypeInfo *)