structure TinySML = struct

    val max_curried = 5

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
      | Local of decl incl * decl incl

    infix ==>
    fun x ==> e = Fn(x,e)

    structure H = Polyhash
    exception NotFound
    val trans_table = H.mkPolyTable (17, NotFound)
    val () =
	List.app (fn (str, trans) => H.insert trans_table (str, trans))
            [ ("and",  "an")
            , ("case", "cas")
	    , ("end",  "en")
	    , ("open", "opn")
	    , ("ref", "refe")
            , ("type", "typ")
            , ("val", "valu")
	    , ("where", "wher")
	    , ("with",  "wth")
	    , ("NONE",  "NON")
            ]
    fun mlify str = H.find trans_table str
		    handle NotFound => str

    fun toString (isStrMode,isSigMode) mode indent info =
	let fun parens safe level s = if level > safe then "(" ^ s ^ ")"
				      else s
	    fun printing s =
		List.exists (not o Char.isSpace) (String.explode s)

	    local
		fun str _ nil _ _ = ""
		  | str p (h::t) sep needSep =
		    let val ph = p h
			val ns = printing ph
			val s = p h ^ (str p t sep ns)
		    in  if needSep then sep ^ s else s
		    end
	    in
	    fun stringSep start finish sep p l = 
		start ^ (str p l sep false) ^ finish
	    end (* local *)

	    fun show_pat pat =
		case pat of
		    VarPat x => mlify x
		  | TupPat ps => Util.stringSep "(" ")" "," show_pat ps
	    fun show_type_incl sep None = "" 
	      | show_type_incl sep (Some ty) = sep ^ SMLType.toString ty
	      | show_type_incl sep (StrOnly ty) = 
		if isStrMode mode then sep ^ SMLType.toString ty else ""
	      | show_type_incl sep (SigOnly ty) = 
		if isSigMode mode then sep ^ SMLType.toString ty else ""

	    fun show_fixity (NONE) = "infix"
	      | show_fixity (SOME Right) = "infixr"
	      | show_fixity (SOME Left)  = "infixl"

	    fun show_exp level exp =
		case exp of
		    Unit => "()"
		  | Var x => mlify x
		  | Const c => c
		  | Long n => Name.toString n
		  | Str s => "\"" ^ s ^ "\""
		  | Fn(x,e) => parens 1 level ("fn " ^ mlify x ^ " => " ^ show_exp 1 e)
		  | App(Var "symb",[Str s]) => "(symb\""^s^"\")"
		  | App(e,es) => parens 2 level 
				    (show_exp 3 e ^ 
				     Util.stringSep " " "" " " (show_exp 3) es)
		  | Tup [] => "()"
		  | Tup [e] => show_exp level e
		  | Tup es => Util.stringSep "(" ")" "," (show_exp 1) es
		  | Import(cglobal,ty) =>
		      "_import \"" ^ cglobal ^ "\" : " ^ SMLType.toString ty ^ ";"
		  | SeqExp es => Util.stringSep "" "" "; " (show_exp 1) es
		  | Let(d,e) =>
		      let val dstr = show d
		      in  if printing dstr
			  then "let " ^ show d ^ " in " ^ show_exp 1 e ^ " end"
			  else show_exp 1 e
		      end

	    and show decl =
		case decl of
		    ValDecl(pat,ty,exp) =>
		       if isStrMode mode then
			   indent ^ "val " ^ show_pat pat ^ show_type_incl " : " ty
			   ^ (if printing (show_type_incl " : " ty) then "\n" ^ indent ^ "    = " else " = ") ^ show_exp 1 exp
		       else
			   indent ^ "val " ^ show_pat pat ^ show_type_incl " : " ty
		  | FunDecl(name,pars,ty,exp) =>
		       if isStrMode mode then
			   indent ^ "fun " ^ name ^ Util.stringSep " " "" " " show_pat pars
			   ^ " = " ^ show_exp 1 exp
		       else
			   indent ^ "val " ^ name ^ show_type_incl " : " ty
		  | TypeDecl((tvs,tname),ty) =>
		       indent ^ "type " 
		     ^ SMLType.toString(SMLType.TyApp(map SMLType.TyVar tvs,tname))
		     ^ show_type_incl " = " ty
		  | SeqDecl decs =>
		       stringSep "" "" "\n" show' decs
		  | EmptyDecl => ""
		  | Comment NONE => ""
		  | Comment(SOME c) => "(*" ^ c ^ "*)"
		  | Open strs => indent ^ Util.stringSep "open " "" " " (fn s=>s) strs
		  | Infix(fixity, strs) => 
		        indent ^ Util.stringSep (show_fixity fixity ^ " ")
						"" " " (fn s=>s) strs
		  | Local(dec1, dec2) =>
		       let val dec1 = show' dec1
		       in  if printing dec1 
			   then indent ^ "local\n" 
			      ^ dec1 ^ "\n" ^ indent ^ "in\n"
                              ^ show' dec2
                              ^ "\n" ^ indent ^ "end"
			   else show' dec2
		       end
	    and show' (None) = ""
	      | show' (Some decl) = show decl
	      | show' (StrOnly decl) = if isStrMode mode then show decl else ""
	      | show' (SigOnly decl) = if isSigMode mode then show decl else ""

	in  show' info ^ "\n"
	end

end (* structure TinySML *)