(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

structure TinyC =
struct

    type var = string
    type operator = string
    datatype ctype =
             TInt
	   | TChar
	   | TFloat
	   | TDouble
	   | TValue
	   | TVoid
	   | TStar of ctype
(*
	   | TWord
	   | TStruct of string
	   | TFunc of ctype list * ctype
*)
	   | TTyName of string

    datatype expr =
	     Var of var
	   | Int of int
	   | Float of real
	   | Cast of ctype * expr
(*
	   | Null 
	   | Op of operator * expr * expr
	   | DeRef of expr
	   | Addr of expr
	   | Cast of ctype * expr
	   | Field of expr * string
	   | Sub of expr * int
	   | Comma of expr * expr
*)
           | Call of var * ctype option (* need a cast? *)
                   * expr list
(*
	   | Print of expr
*)
           | VerbExp of string

    datatype vardecl = VDecl of var * ctype * expr option

    datatype stmt =
             Empty
	   | Exp of expr
           | Ass of expr * ctype * expr
(*
           | If of expr * stmt * stmt
	   | Switch of expr * (string option * stmt) list
	   | Goto of var
*)
           | Block of var option * vardecl list * stmt list
	   | Return of expr
	   | Comment of string
           | Verbatim of string

    type specifiers = string
    datatype prototype = 
	     Proto of specifiers option * var * (var * ctype) list * ctype
    datatype topdecl = 
	     Fun of prototype * stmt
	   | Define of string * string

    fun showTy ty =
	case ty of 
	    TInt => "int"
	  | TChar => "char"
	  | TFloat => "float"
	  | TDouble => "double"
	  | TValue => "value"
	  | TVoid => "void"
	  | TStar ty => showTy ty ^ "*"
	  | TTyName tname => tname
    fun toString indent topdecl =
	let 
	    fun showExp exp =
		case exp of
		    Var x => x
		  | Int i => Int.toString i
		  | Float f => Real.toString f
		  | Cast(t,e) => "(" ^ showTy t ^ ") " ^ showExp e
		  | VerbExp e => e
		  | Call(f,cast,args) => 
		       f ^ Util.stringSep "(" ")" ", " showExp args
	    fun showDecl (VDecl(x,ty,e)) =
		indent^showTy ty ^" "^ x ^
		(case e of NONE => "" | SOME e => " = " ^ showExp e)^";\n"
	    fun showStmt stmt =
		case stmt of
		    Empty => ""
		  | Exp e => indent^showExp e ^ ";\n"
		  | Ass(l,ty,r) => 
		       indent^showExp l ^ " = " ^ showExp r ^ ";\n"
		  | Block(label,decls, Comment c :: stmts) =>
		       ("{ /* "^c^" */\n") 
		       ^ Util.stringSep "" "" "" showDecl decls
		       ^ Util.stringSep "" "}\n" "" showStmt stmts
		  | Block(label,decls, stmts) =>
		       Util.stringSep "{\n" "" "" showDecl decls
		       ^ Util.stringSep "" "}\n" "" showStmt stmts
		  | Return e => indent^"return " ^ showExp e ^ ";\n"
		  | Comment c => indent^"/* " ^ c ^ " */\n"
		  | Verbatim s => s
	    fun showTop top =
		case top of
		    Fun(Proto(spec, f, pars, ret),stmt) =>
		       (case spec of NONE => "" | SOME s => s^" ") ^ 
                       showTy ret ^ " " ^ f ^ Util.stringSep "(" ")" ", " (fn (p,t) => showTy t ^ " " ^ p) pars ^ " "
                    ^  showStmt stmt ^ "\n"
		  | Define(s1,s2) => "#define " ^s1^ " " ^s2^"\n\n"
	in  showTop topdecl end

end