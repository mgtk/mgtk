signature TinySML = sig

    val max_curried: int

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
      | Let of dec * exp
      | SeqExp of exp list

    and dec =
	ValDec of pat * typeexp option * exp
      | FunDec of string * pat list * typeexp option * exp
      | TypeDec of (tyvar list * tyname) * typeexp option
      | SeqDec of dec list
      | EmptyDec
      | CommentDec of string option (* an empty comment prints as newline *)
      | OpenDec of string list
      | InfixDec of fixity option * string list
      | LocalDec of dec * dec

    datatype spec =
	ValSpec of pat * typeexp
      | FunSpec of string * typeexp
      | TypeSpec of (tyvar list * tyname) * typeexp option
      | SeqSpec of spec list
      | EmptySpec
      | CommentSpec of string option
      | StrSpec of string (* strid *) * sigexp
    and sigexp = 
	SigBasic of spec
      | SigId of string (* sigid *)

    datatype topdec =
	StrDec of string (* strid *) * sigexp option * topdec list
      | SigDec of string (* sigid *) * sigexp
      | CoreDec of dec

    val ppDec    :    dec Pretty.pp
    val ppSigExp : sigexp Pretty.pp
    val ppTopDec : topdec Pretty.pp

end (* signature TinySML *)