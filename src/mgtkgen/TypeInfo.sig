(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

signature TypeInfo =
sig

    type texp = TypeExp.texp
    type tname = TypeExp.tname
    type name = NameUtil.name
    type wseq  = WSeq.wseq

    val fitsDynApp: 'a list -> bool

    val isString: texp -> bool
    val isNullType: texp -> bool
    val isVoidType: texp -> bool
    val isOutputType: texp -> bool
    val isString': texp * 'a -> bool
    val isNullType': texp * 'a -> bool
    val isVoidType': texp * 'a -> bool
    val isOutputType': texp * 'a -> bool

    val mkCType: texp -> wseq
    val mkMLPrimType: texp -> wseq
    val mkMLType: texp -> wseq

    val fromCValue: texp * wseq -> wseq
    val toCValue: texp * wseq -> wseq
    val toCValue': texp * wseq -> wseq


    val MLName: texp -> tname
    val MLNamePath: tname -> string

    val MLWidgetName: texp -> string
    val MLShortWidgetName: texp -> string
    val MLBoxedName:  texp -> string
    val MLFlagName:   texp -> string

    val MLFunName: name -> string
    val MLFunNameWithoutOpt: name -> string
    val MLConstrName: name -> string

    val MLSignalName: name -> string

    val CWidgetName: texp -> string
    val CBoxedName:  texp -> string
    val CFlagName:   texp -> string

    val CFunName: name -> string
    val CFunNameWithoutOpt: name -> string
    val CConstrName: name -> string

end 