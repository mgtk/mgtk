signature AST =
sig

    datatype ('n,'i1,'i2) module = 
	Module of {name: 'n, members: ('n,'i1,'i2) member list, info: 'i1}
    and ('n,'i1,'i2) member =
        Sub of ('n,'i1,'i2) module
      | Member of {name: 'n, info: 'i2}


    exception Zip
    val map: ('i1 -> 'i1') * ('i2 -> 'i2') 
              -> ('n,'i1, 'i2) module -> ('n,'i1', 'i2') module
    val mapi: (('n*'i1 -> 'i1') * ('n*'i2 -> 'i2'))
               -> ('n,'i1,'i2) module -> ('n,'i1','i2') module

    val filteri: (('n*'i1 -> bool) * ('n*'i2 -> bool)) -> ('n,'i1,'i2) module
                 -> ('n,'i1,'i2) module option

(*
    val zip: (''n,'i1) module * (''n,'i2) module -> (''n, 'i1*'i2) module
    val exists: ('i -> bool) -> ('n,'i) module -> bool
*)
    val fold:   ( ('n * ('n,'i1,'i2) member list) * 'a -> 'a )
              * ( ('n * 'i2) * 'a -> 'a )
                -> 'a -> ('n,'i1,'i2) module -> 'a

    datatype ('n,'t) api_info =
	Method of 't
      | Field of 't
      | Object of 'n (* type name *) * 'n option (* parent *) * 'n list (* implements *)
      | Boxed of {copy: string, release: string} option
      | Enum of bool (* flag? *) * 'n list
      | Signal of 't
    datatype pass = OUT | INOUT
    datatype api_type =
        ApiTy of string
      | ArrowTy of (string * api_type) list * api_type
      | Defaulted of api_type * string
      | Output of pass * api_type
      | Array of api_type

    type ('n,'t) ast_module = 
	 ('n, 'n option, ('n, 't) api_info) module

    val pp: ('n * 'modi) Pretty.pp * ('n * 'memi) Pretty.pp
            -> ('n, 'modi, 'memi) module Pretty.pp

    val ppAstType : api_type Pretty.pp
    val ppAst: 'n Pretty.pp -> 't Pretty.pp -> ('n,'t) ast_module Pretty.pp

end (* signature AST *)