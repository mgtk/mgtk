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

    structure Set = Splayset
    val empty = Set.empty(String.compare)
    val singleton = Set.singleton(String.compare)
    val union = Set.union
    val difference = Set.difference
    val member = Set.member
    fun usedStmt (stmt,acc) =
	case stmt of 
	    Empty => acc
	  | Exp e => usedExp (e,acc)
	  | Ass(e,_,e') => usedExp (e', usedExp (e, acc))
	  | Block(_,decls,stmts) =>
	    let val locally = List.foldl usedStmt empty stmts
		fun f (VDecl(_,_,SOME e),acc) = usedExp(e,acc)
		  | f (VDecl(_,_,NONE),acc) = acc
		val locally = List.foldl f locally decls
		fun g (VDecl(x,_,_),bound) = union(singleton x, bound)
		val bound = List.foldl g empty decls
	    in  union(difference(locally, bound), acc)
	    end
	  | Return e => usedExp (e,acc)
	  | Comment _ => acc
	  | Verbatim _ => acc
    and usedExp (exp, acc) =
	case exp of
	     Var x => union(acc,singleton x)
	   | Int _ => acc
	   | Float _ => acc
	   | Cast(_,e) => usedExp (e,acc)
	   | Call(f,_,exps) => List.foldl usedExp acc exps
	   | VerbExp _ => acc
    val used = fn stmts => List.foldl usedStmt empty stmts
	    
    fun coalesce ((a as Ass(Var x,_,e))::
		  (r as Return(Var y)) :: ss) =
	if x = y then Return e :: coalesce ss
	else a::r :: coalesce ss
      | coalesce (stmt::stmts) = stmt :: coalesce stmts
      | coalesce [] = []

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
		  | Call("",cast,[arg]) => showExp arg
		  | Call("&",cast,[Var x]) => "&" ^ x
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
		       let val stmts = coalesce stmts
			   val needed = used stmts
			   fun f (VDecl(x,_,_)) = member(needed,x)
			   val decls = List.filter f decls
		       in  
			   ("{ /* "^c^" */\n") 
			 ^ Util.stringSep "" "" "" showDecl decls
			 ^ Util.stringSep "" "" "" showStmt stmts
                         ^ "}\n"
		       end
		  | Block(label,decls, stmts) =>
		       Util.stringSep "{\n" "" "" showDecl decls
		       ^ Util.stringSep "" "" "" showStmt (coalesce stmts) ^ "}\n"
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