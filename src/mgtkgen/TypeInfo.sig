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