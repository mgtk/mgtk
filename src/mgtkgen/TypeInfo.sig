(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

signature TypeInfo =
sig

    type texp = TypeExp.texp
    type tname = TypeExp.tname
    type name = NameUtil.name
    type wseq  = WSeq.wseq

    val fitsDynApp: 'a list -> bool

    (* predicates *)
    val isString: texp -> bool
    val isNullType: texp -> bool
    val isVoidType: texp -> bool
    val isOutputType: texp -> bool
    val isString': texp * 'a -> bool
    val isNullType': texp * 'a -> bool
    val isVoidType': texp * 'a -> bool
    val isOutputType': texp * 'a -> bool

    (* type conversions *)
    val mkCType: texp -> wseq
    val mkMLPrimType: texp -> wseq
    val mkMLType: texp -> wseq

    (* value conversions *)
    val fromCValue: texp * wseq -> wseq
    val toCValue: texp * wseq -> wseq

    (* name conversions *)
    val MLName: texp -> tname
    val MLNamePath: tname -> wseq

    val MLWidgetName: texp -> wseq
    val MLShortWidgetName: texp -> wseq
    val MLBoxedName:  texp -> wseq
    val MLFlagName:   texp -> wseq

    val MLFunName: name -> wseq
    val MLFunNameWithoutOpt: name -> wseq
    val MLConstrName: name -> wseq

    val MLSignalName: name -> wseq

    val CWidgetName: texp -> wseq
    val CBoxedName:  texp -> wseq
    val CFlagName:   texp -> wseq

    val CSignalName: name -> wseq

    val CFunName: name -> wseq
    val CFunNameWithoutOpt: name -> wseq
    val CConstrName: name -> wseq

end 

(*

    [fitsDynApp lst] returns true if the list lst, supposedly
    representing parameters to a call to a dynamically loaded
    function, can be transfered in a curried fashion; false otherwise
    (in which case a tuple has to be constructed and deconstructed
    again in C).

    [isXXX texp] predicates indicating wether the supplied type
    expression texp is an XXX.

    [mkCType texp] returns the C type corresponding to the type
    expression texp.

    [mkMLPrimType texp] returns the SML "primitive" type (when
    constructors providing type safety have been removed)
    corresponding to the type expression texp.

    [mkMLType texp] returns the SML type corresponding to the type
    expression texp.

    [fromCValue (texp, wseq)] returns the code that coerces the value
    described by the code wseq to an SML type representing texp from a
    C type representing texp. Works for type expressions representing
    "type names" (i.e., PRIMTYPE, WIDGET, FLAG, POINTER) and output
    types.

    [toCValue (texp, wseq)] returns the code that coerces the value
    described by the code wseq to a C type representing texp from an
    SML type representing texp. Works for type expressions
    representing "type names" (i.e., PRIMTYPE, WIDGET, FLAG, POINTER)
    and optionals and lists.

    [MLName texp] returns the type name (if such exist, otherwise
    raises Util.ShouldntHappen) contained in texp.

    [MLNamePath tname] converts a type name tname to a wseq by simply
    concatenating the strings in tname.

    The [MLXXXName] functions convert names and type expressions to
    wseq to be used in SML code.

    The [CXXXName] functions convert names and type expressions to
    wseq to be used in C code.

*)