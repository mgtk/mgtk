structure TinySML :> TinySML = struct

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

    fun show_fixity (NONE) = "infix"
      | show_fixity (SOME Right) = "infixr"
      | show_fixity (SOME Left)  = "infixl"

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

    fun equalincl eq (i, i') =
	case (i, i') of
	    (None, None) => true
	  | (Some x, Some x') => eq (x,x')
	  | (StrOnly x, StrOnly x') => eq (x,x')
	  | (SigOnly x, SigOnly x') => eq (x,x')
	  | _ => false

    fun equalpat (p,p') =
	case (p,p') of
	    (VarPat v, VarPat v') => v = v'
	  | (TupPat ps, TupPat ps') => List.all equalpat (ListPair.zip(ps,ps'))
	  | _ => false
    fun equalexp (e, e') =
	case (e, e') of
	    (Unit, Unit) => true
	  | (Const c, Const c') => c = c'
	  | (Var v, Var v') => v = v'
	  | (Long n, Long n') => Name.equal (n, n')
	  | (Str s, Str s') => s = s'
	  | (Fn(a,e), Fn(a',e')) => a = a' andalso equalexp (e, e')
	  | (App(e,es), App(e',es')) =>
	      equalexp (e,e') andalso equalexplists (es,es')
	  | (Tup es, Tup es') => equalexplists (es,es')
	  | (Import(c,ty), Import(c',ty')) => 
	      c = c' andalso SMLType.equal(ty,ty')
	  | (Let(d,e), Let(d',e')) => 
	      equal (d,d') andalso equalexp (e,e')
	  | (SeqExp es, SeqExp es') => equalexplists (es,es')
	  | _ => false
    and equalexplists es = List.all equalexp (ListPair.zip es)
    and equal (d, d') =
	case (d, d') of
	    (EmptyDecl, EmptyDecl) => true
	  | (Comment s, Comment s') => 
	      Util.optionCmp String.compare (s,s') = EQUAL
	  | (Open strs, Open strs') =>
              List.all (op=) (ListPair.zip (strs, strs'))
	  | (Infix(f, ids), Infix(f', ids')) =>
	      List.all (op=) (ListPair.zip (ids, ids'))
	  | (ValDecl(p,t,e), ValDecl(p',t',e')) =>
              equalpat(p,p') andalso equalincl SMLType.equal (t,t')
	      andalso equalexp (e,e')
	  | (FunDecl(n,ps,t,e), FunDecl(n',ps',t',e')) =>
              n=n' andalso List.all equalpat (ListPair.zip(ps,ps'))
              andalso equalincl SMLType.equal (t,t') andalso equalexp (e,e')
	  | (TypeDecl((a,n),t), TypeDecl((a',n'),t')) =>
              List.all SMLType.eqTyVar (ListPair.zip(a,a')) 
	      andalso SMLType.eqTyName(n,n')
              andalso equalincl SMLType.equal (t,t')
	  | (Local(d1,d2), Local(d1',d2')) =>
	      equalincl equal (d1,d1') andalso equal(d2,d2')
	  | (SeqDecl ds, SeqDecl ds') =>
              List.all (equalincl equal) (ListPair.zip (ds,ds'))
	  | _ => false
(*
    fun rank d =
	case d of
	    EmptyDecl => 0
	  | Comment => 10
	  | Open _ => 20
	  | Infix _ => 30
	  | ValDecl _ => 40
	  | FunDecl _ => 50
	  | TypeDecl _ => 60
	  | LocalDecl _ => 70
	  | SeqDecl _ => 80
*)

    datatype mode = STR | SIG
    fun flatten mode (d: decl) : decl list =
	case d of
	    SeqDecl ds => List.concat (List.map (flatten' mode) ds)
	  | Local(d,d') => [Local(d,d')]
	  | d => [d]
    and flatten' mode (d: decl incl) : decl list =
	let val d = case d of
			Some d => d
		      | StrOnly d => if mode = STR then d else EmptyDecl
		      | SigOnly d => if mode = SIG then d else EmptyDecl
		      | None => EmptyDecl
	in  flatten mode d end
(*
    fun iCtns (None) = NONE
      | iCtns (SigOnly x) = SOME x
      | iCtns (StrOnly x) = SOME x
      | iCtns (Some x) = SOME x
    fun iRank (None) = 0
      | iRank (SigOnly _) = 5
      | iRank (StrOnly _) = 10
      | iRank (Some _) = 20
    fun inclcmp cmp (x,x') =
	case Int.cmp(iRank x, iRank x') of
	    EQUAL => Util.optionCmp cmp (iCtns x, iCtns)
	  | order => order
*)
    fun coalesce ds =
	case ds of
	    (d as Local(d1,d1'))::
	    (d' as Local(d2,d2'))::rest =>
	      if equalincl equal (d1,d2)
	      then coalesce (Local(d1,SeqDecl[Some d1',Some d2'])::rest)
	      else d::coalesce(d'::rest)
	  | d::rest => d::coalesce rest
	  | [] => []

    infix ==>
    fun x ==> e = Fn(x,e)

    structure H = Polyhash
    val hash = Polyhash.hash
    exception NotFound
    val trans_table: (string,string) H.hash_table  
      = H.mkTable (hash, op=) (17, NotFound)
    val () =
	List.app (fn (str, trans) => H.insert trans_table (str, trans))
            [ ("and",  "an")
            , ("case", "cas")
	    , ("end",  "en")
	    , ("open", "opn")
	    , ("raise", "rais")
	    , ("ref", "refe")
            , ("type", "typ")
            , ("val", "valu")
	    , ("where", "wher")
	    , ("with",  "wth")
	    , ("NONE",  "NON")
	    , ("2BUTTON_PRESS", "TWO_BUTTON_PRESS")
	    , ("3BUTTON_PRESS", "THREE_BUTTON_PRESS")
            ]
    fun mlify str = H.find trans_table str
		    handle NotFound => str

    (* The pretty printer is not always all that pretty.

       However, please not that it sometimes sacrifies prettyness
       for compactness.
    *)
    local open Pretty in
    fun ppDec (strIncl,sigIncl) dec =
	let fun parens safe level tree = 
		if level > safe then "(" ^+ tree +^ ")"
		else tree
	    fun ppincl pp incl =
		case incl of
		    None => empty
		  | Some x => pp x
		  | StrOnly x => if strIncl then pp x else empty
		  | SigOnly x => if sigIncl then pp x else empty
	    fun ppexp level e =
		case e of
                    (* special case some idioms *)
		    App(Var "symb",[Str s]) => ppString("(symb\""^s^"\")")
		  | App(Var "!", [e as Var v]) => "!" ^+ ppexp level e

                    (* then the general stuff *)
		  | Unit => ppString "()"
		  | Var x => ppString (mlify x)
		  | Long n => ppString(Name.toString n)
		  | Const c => ppString c
		  | Str s => ppString("\"" ^ s ^ "\"")
		  | Import(cglobal,ty) =>
                      ppString "_import" ++
			   break(1,0)(ppString ("\""^cglobal^"\""),
				      ": " ^+ (SMLType.pp ty) +^ ";")
		  | Fn(x,e) =>
		      let fun bd (Fn(x,e)) fns = bd e (x::fns)
			    | bd _ [] = Util.abort 53745
			    | bd e fns = (rev fns,e)
			  val (fns,e) = bd (Fn(x,e)) []
			  fun f x = "fn " ^+ ppString(mlify x)
		      in  parens 1 level 
			     ( break(1,3)( ilist " => #" f fns +^ " =>"
                                         , ppexp 1 e )
                             )
		      end
		  | App(e,es) =>
		      parens 2 level
                         ( ppexp 3 e ++ ilist "# " (ppexp 4) es)
		  | Tup [] => ppString "()"
		  | Tup [e] => ppexp level e
		  | Tup es => pperoundlist es
		  | SeqExp es => clist "#; " (forcemulti o ppexp 1) es
		  | Let(d,e) => pplet (Let(d,e))
	    and pperoundlist es = bracket "(#)" (ilist ", #" (ppexp 1) es)
	    and ppelist start finish sep es =
		compose(start ^+ clist (sep^" #") (ppexp 1) es,
			1,2,0,ppString finish)
	    and pplet e =
		let fun loop (Let(d,e)) acc = loop e (ppdec d::acc)
		      | loop _ [] = raise Fail"pplet: shouldn't happen"
		      | loop e acc = 
			let val body = ppexp 1 e
			in  case rev acc of
				[] => body
			      | d::ds =>
				break(1,0) (ppString "let" ++ makelist(true,"") (d :: map forcemulti ds),
					    compose(ppString "in" ++ body,1,2,0,ppString "end"))
			end
		in  loop e [] end
	    and pplocal d =
		let fun loop (Local(d,d')) acc = loop d' (ppdec' d::acc)
		      | loop _ [] = raise Fail"pplocal: shouldn't happen"
		      | loop d acc = 
                          let val body = ppdec d
			  in  case rev acc of
				  [] => body
				| d::ds =>
				    let val bds = makelist(true,"") (d :: map forcemulti ds)
				    in  if isPrinting bds then
					    break(1,0) (ppString "local" ++ bds
					               ,compose(ppString "in"++body,1,2,0,ppString "end"))
					else body
				    end
			  end
		in  loop d [] end
	    and pppat p =
		case p of
		    VarPat v => ppString (mlify v)
		  | TupPat ps => bracket "(#)" (ilist ", #" pppat ps)
	    and brkprt sep opt (t1, t2) =
		if isPrinting t2 then break opt (t1, sep ^+ t2) else t1
	    and ppsimple d = 
		case d of
		    ValDecl(pat,ty,exp) => 
		      let val bind = ppString "val" ++ pppat pat
			  val ty = ppincl SMLType.pp ty
		      in  if sigIncl then
			      brkprt ": " (1,2) (bind, ty)
			  else 
			      break(1,4)
				( brkprt ": " (1,2)(bind, ty)
				, "= " ^+ ppexp 1 exp )
		      end
		  | FunDecl(name,args,ty,exp) => 
		      if sigIncl then
			  break(1,2) 
                            ( ppString "val" ++ ppString name
                            , ": " ^+ ppincl SMLType.pp ty )
		      else
			  break(1,2)
			    ( ppString "fun" ++ ppString name 
                                ++ clist " #" pppat args
                            , "= " ^+ ppexp 1 exp )

		  | TypeDecl((args,name),ty) => 
		      let val bindty = 
			      case args of
				  [] => SMLType.ppTyName name
				| [a] => SMLType.ppTyVar a ++ SMLType.ppTyName name
				| args => bracket "(#)"
					    (clist ",#" SMLType.ppTyVar args )
                                          ++ SMLType.ppTyName name
			  val bind = ppString "type" ++ bindty
			  val ty = ppincl SMLType.pp ty
		      in  brkprt "= " (1,2) ( bind, ty )
		      end
		  | EmptyDecl => empty
		  | Comment NONE => empty
		  | Comment (SOME s) => bracket "(*#*)" (ppString s)
		  | Open strs => ppString "open" ++ clist "# " ppString strs
		  | Infix (fixity, ids) => 
		      ppString (show_fixity fixity) ++ clist "# " ppString ids
		  | Local(d,d') => pplocal (Local(d,d'))
		  | SeqDecl _ => raise Fail "Shouldn't happen (ppsimple(seq))"
	    and ppdec d =
		case coalesce (flatten (if strIncl then STR else SIG) d) of
		    [] => empty
		  | [d] => ppsimple d
		  | d::ds => 
		      let val ds = 
			      List.filter isPrinting
				 ((ppsimple d) ::
			          List.map (forcemulti o ppsimple) ds)
		      in  makelist (true,"") ds end

	    and ppdec' d =
		case d of
		    None => empty
		  | Some d => ppdec d
		  | StrOnly d => ppdec d
		  | SigOnly d => ppdec d
	in  ppdec' dec
	end
    fun ppTopDec dec =
	let 
	    fun coalesce tops =
		case tops of
		    CoreDec(d1)::CoreDec(d2)::tops =>
		        coalesce (CoreDec(Some(SeqDecl[d1,d2]))::tops)
		  | top::tops => top::coalesce tops
		  | [] => []
	    fun pptop mode dec =
		case dec of
		    StrDec(strid, sigopt, decs) =>
                       let val sigcons = 
			       case sigopt of 
				   NONE => empty
				 | SOME sigid => ":> " ^+ ppString sigid
		       in  close(1,"end")
			     (always 4 ( ("structure " ^+ ppString strid ++ sigcons) +^ " = struct"
				       , pptoplist (true,false) decs )
                             )
		       end
		  | SigDec(sigid, specs) =>
		       close(1,"end")
                             (always 4 ( ("signature " ^+ ppString sigid +^ " = sig")
                                       , pptoplist (false,true) specs )
                             )
		  | CoreDec decl => ppDec mode decl
	    and pptoplist mode tops =
		let val tops = 
			List.filter isPrinting (map (pptop mode) (coalesce tops))
		in  clist "#" forcemulti tops
		end
	in  pptop (true,false) dec end
    end (* local *)

end (* structure TinySML *)