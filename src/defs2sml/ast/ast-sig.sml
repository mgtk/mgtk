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

    val filteri: ('n*'i2 -> bool) -> ('n,'i1,'i2) module -> ('n,'i1,'i2) module

(*
    val zip: (''n,'i1) module * (''n,'i2) module -> (''n, 'i1*'i2) module
    val exists: ('i -> bool) -> ('n,'i) module -> bool
*)
    val fold:   ( ('n * ('n,'i1,'i2) member list) * 'a -> 'a )
              * ( ('n * 'i2) * 'a -> 'a )
                -> 'a -> ('n,'i1,'i2) module -> 'a

    datatype 't api_info =
	Method of 't
      | Field of 't
      | Boxed of {copy: string, release: string} option
      | Enum of string list
      | Signal of 't
    and api_type =
        ApiTy of string
      | ArrowTy of (string * api_type) list * api_type

    val pp: ('n * 'modi -> string) * ('n * 'memi -> string)
            -> (string -> unit) -> ('n, 'modi, 'memi) module -> unit

    val ppName: ('modi -> string) * ('memi -> string) -> (string -> unit)
                 -> (Name.name, 'modi, 'memi) module -> unit

    val ppString: ('modi -> string) * ('memi -> string) -> (string -> unit)
                 -> (string, 'modi, 'memi) module -> unit

end (* signature AST *)