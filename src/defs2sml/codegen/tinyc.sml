(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003, 2004. *)

structure TinyC =
struct

    type var = string
    type operator = string
    datatype ctype =
             TInt
	   | TValue
(*
	   | TFloat
	   | TChar
	   | TWord
	   | TStar of ctype
	   | TStruct of string
	   | TFunc of ctype list * ctype
*)

    datatype expr =
	     Var of var
	   | Int of string
	   | Float of string
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

    fun toString indent topdecl =
	let fun showTy ty =
		case ty of 
		    TInt => "int"
		  | TValue => "value"
	    fun showExp exp =
		case exp of
		    Var x => x
		  | Int i => i
		  | VerbExp e => e
		  | Call(f,cast,args) => 
		       f ^ Util.stringSep "(" ")" ", " showExp args
	    fun showStmt stmt =
		case stmt of
		    Empty => ""
		  | Exp e => indent^showExp e ^ ";\n"
		  | Ass(l,_,r) => indent^showExp l ^ " = " ^ showExp r ^ ";\n"
		  | Block(label,decls, Comment c :: stmts) =>
		       ("{ /* "^c^" */\n") ^ Util.stringSep "" "}\n" "" showStmt stmts
		  | Block(label,decls, stmts) =>
		       Util.stringSep "{\n" "}\n" "" showStmt stmts
		  | Return e => indent^"return " ^ showExp e ^ ";\n"
		  | Comment c => indent^"/* " ^ c ^ " */\n"
		  | Verbatim s => s
	    fun showTop top =
		case top of
		    Fun(Proto(spec, f, pars, ret),stmt) =>
		       (case spec of NONE => "" | SOME s => s^" ") ^ 
                       showTy ret ^ " " ^ f ^ Util.stringSep "(" ")" ", " (fn (p,t) => showTy t ^ " " ^ p) pars ^ " "
                    ^  showStmt stmt ^ "\n"
	in  showTop topdecl end

end