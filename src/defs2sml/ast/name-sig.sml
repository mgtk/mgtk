signature NAME = sig

    type name

    val separateWords: string -> string list
    val separateUnderscores: string -> string list

    val toLower: string -> string
    val toUpper: string -> string
    val capitalize: string -> string

    val equal: name * name -> bool
    val compare: name * name -> order

    val toString: name -> string
    val toString': name -> string
    val fromString: string -> name
    val fromPaths: string list * string list * string list -> name

    val getPath: name -> string list
    val getFullPath: name -> string list
    val getBase: name -> string list

    val asModule: name -> string
    val asEnum: name -> string
    val asEnumConst: name -> string
    val asBoxed: name -> string
    val asField: name -> string
    val asMethod: name -> string
    val asSignal: name -> string
    val asType: name -> string

    val asCEnum: name -> string
    val asCEnumConst: name -> string
    val asCBoxed: name -> string
    val asCFunc: name -> string
    val asCStub: name -> string
    val asCGetEnum: name -> string

end (* signature Name *)
