signature TinySML = sig

    val max_curried: int

    datatype 'a incl =
        None
      | StrOnly of 'a
      | SigOnly of 'a
      | Some of 'a
	     
    type typeexp = SMLType.ty
    type tyvar = SMLType.tyvar
    type tyname = SMLType.tyname

    datatype pat =
        VarPat of string
      | TupPat of pat list

    datatype fixity = Right | Left

    datatype exp =
	Unit 
      | Const of string
      | Var of string
      | Long of Name.name
      | Str of string
      | Fn of string * exp
      | App of exp * exp list
      | Tup of exp list
      | Import of string * typeexp
      | Let of decl * exp
      | SeqExp of exp list

    and decl =
	ValDecl of pat * typeexp incl * exp
      | FunDecl of string * pat list * typeexp incl * exp
      | TypeDecl of (tyvar list * tyname) * typeexp incl
      | SeqDecl of decl incl list
      | EmptyDecl
      | Comment of string option (* an empty comment prints as newline *)
      | Open of string list
      | Infix of fixity option * string list
      | Local of decl incl * decl

    datatype topdec =
	StrDec of string (* strid *) * string option (* signature constraint *)
		  * topdec list
      | SigDec of string (* sigid *) * topdec list (* really specs *)
      | CoreDec of decl incl

    val ppDec : bool * bool -> decl incl Pretty.pp
    val ppTopDec : topdec Pretty.pp

end (* signature TinySML *)