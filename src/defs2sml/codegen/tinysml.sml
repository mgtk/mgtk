structure TinySML :> TinySML = struct

    val max_curried = 5

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

    fun equalopt eq (SOME x, SOME x') = eq (x,x')
      | equalopt eq (NONE,   NONE   ) = true
      | equalopt eq _                 = false

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
	    (EmptyDec, EmptyDec) => true
	  | (CommentDec s, CommentDec s') => 
	      Util.optionCmp String.compare (s,s') = EQUAL
	  | (OpenDec strs, OpenDec strs') =>
              List.all (op=) (ListPair.zip (strs, strs'))
	  | (InfixDec(f, ids), InfixDec(f', ids')) =>
	      List.all (op=) (ListPair.zip (ids, ids'))
	  | (ValDec(p,t,e), ValDec(p',t',e')) =>
              equalpat(p,p') andalso equalopt SMLType.equal (t,t')
	      andalso equalexp (e,e')
	  | (FunDec(n,ps,t,e), FunDec(n',ps',t',e')) =>
              n=n' andalso List.all equalpat (ListPair.zip(ps,ps'))
              andalso equalopt SMLType.equal (t,t') andalso equalexp (e,e')
	  | (TypeDec((a,n),t), TypeDec((a',n'),t')) =>
              List.all SMLType.eqTyVar (ListPair.zip(a,a')) 
	      andalso SMLType.eqTyName(n,n')
              andalso equalopt SMLType.equal (t,t')
	  | (LocalDec(d1,d2), LocalDec(d1',d2')) =>
	      equal (d1,d1') andalso equal(d2,d2')
	  | (SeqDec ds, SeqDec ds') =>
              List.all equal (ListPair.zip (ds,ds'))
	  | _ => false

    fun flatten (d: dec) : dec list =
	case d of
	    SeqDec ds => List.concat (List.map flatten ds)
	  | d => [d]

    fun coalesce ds =
	case ds of
	    (d as LocalDec(d1,d1'))::
	    (d' as LocalDec(d2,d2'))::rest =>
	      if equal (d1,d2)
	      then coalesce (LocalDec(d1,SeqDec[d1',d2'])::rest)
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
    local open Pretty 
        fun brkprt sep opt (t1, t2) =
	    if isPrinting t2 then break opt (t1, sep ^+ t2) else t1
	fun ppopt pp NONE = empty
	  | ppopt pp (SOME x) = pp x
	fun pppat p =
	    case p of
		VarPat v => ppString (mlify v)
	      | TupPat [] => ppString "()"
	      | TupPat [p] => pppat p
	      | TupPat ps => bracket "(#)" (ilist ", #" pppat ps)
    in

    fun ppDec dec = 
	let fun parens safe level tree = 
		if level > safe then "(" ^+ tree +^ ")"
		else tree

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
		let fun loop (LocalDec(d,d')) acc = loop d' (ppdec d::acc)
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
	    and ppsimple d = 
		case d of
		    ValDec(pat,ty,exp) => 
		      let val bind = ppString "val" ++ pppat pat
			  val ty = ppopt SMLType.pp ty
		      in  break(1,4)
				( brkprt ": " (1,2)(bind, ty)
				, "= " ^+ ppexp 1 exp )
		      end
		  | FunDec(name,args,ty,exp) => 
		      break(1,2)
			    ( ppString "fun" ++ ppString name 
                                ++ clist " #" pppat args
                            , "= " ^+ ppexp 1 exp )

		  | TypeDec((args,name),ty) => 
		      let val bindty = 
			      case args of
				  [] => SMLType.ppTyName name
				| [a] => SMLType.ppTyVar a ++ SMLType.ppTyName name
				| args => bracket "(#)"
					    (clist ",#" SMLType.ppTyVar args )
                                          ++ SMLType.ppTyName name
			  val bind = ppString "type" ++ bindty
			  val ty = ppopt SMLType.pp ty
		      in  brkprt "= " (1,2) ( bind, ty )
		      end
		  | EmptyDec => empty
		  | CommentDec NONE => empty
		  | CommentDec (SOME s) => bracket "(*#*)" (ppString s)
		  | OpenDec strs => ppString "open" ++ clist "# " ppString strs
		  | InfixDec (fixity, ids) => 
		      ppString (show_fixity fixity) ++ clist "# " ppString ids
		  | LocalDec(d,d') => pplocal (LocalDec(d,d'))
		  | SeqDec _ => raise Fail "Shouldn't happen (ppsimple(seq))"
	    and ppdec d =
		case coalesce (flatten d) of
		    [] => empty
		  | [d] => ppsimple d
		  | d::ds => 
		      let val ds = 
			      List.filter isPrinting
				 ((ppsimple d) ::
			          List.map (forcemulti o ppsimple) ds)
		      in  makelist (true,"") ds end

	in  ppdec dec
	end
    fun ppSpec spec =
	case spec of
	    ValSpec(pat,ty) => 
	      break (1,2) ( ppString "val" ++ pppat pat
			  , ": " ^+ SMLType.pp ty )
	  | FunSpec(name,ty) => 
	      break (1,2) ( ppString "val" ++ ppString name
                          , ": " ^+ SMLType.pp ty )
	  | TypeSpec((args,name),tyopt) =>
	      let val bindty = 
		      case args of
			  [] => SMLType.ppTyName name
			| [a] => SMLType.ppTyVar a ++ SMLType.ppTyName name
			| args => bracket "(#)"
			             (clist ",#" SMLType.ppTyVar args )
                                  ++ SMLType.ppTyName name
		  val bind = ppString "type" ++ bindty
		  val ty = ppopt SMLType.pp tyopt
	      in  brkprt "= " (1,2) ( bind, ty )
	      end
	  | EmptySpec => empty
	  | CommentSpec NONE => empty
	  | CommentSpec (SOME s) => bracket "(*#*)" (ppString s)
	  | StrSpec(strid,sigexp) =>
	       ("structure " ^+ ppString strid +^ " = ") ++ ppSigExp sigexp
	  | SeqSpec _ =>
	       let fun loop (SeqSpec ss, acc) =  List.foldr loop acc ss
		     | loop (spec, acc) = ppSpec spec :: acc
		   val specs = List.filter isPrinting (loop(spec,[]))
	       in  clist "#" forcemulti specs
	       end
    and ppSigExp (SigBasic spec) = 
	close(1,"end") (always(2)(ppString"sig", ppSpec spec))
      | ppSigExp (SigId id) = ppString id
    fun ppTopDec dec =
	let 
	    fun coalesce tops =
		case tops of
		    CoreDec(d1)::CoreDec(d2)::tops =>
		        coalesce (CoreDec(SeqDec[d1,d2])::tops)
		  | top::tops => top::coalesce tops
		  | [] => []
	    fun pptop dec =
		case dec of
		    StrDec(strid, SOME(se as SigBasic _), decs) =>
		       close(1,"end")
			    (always 4
			       ( always 2 ( "structure " ^+ ppString strid +^ " :>"
					  , ppSigExp se )
                                 +^ " = struct"
			       , pptoplist (true,false) decs )
                             )
		  | StrDec(strid, sigexpopt, decs) =>
                       let val sigcons = 
			       case sigexpopt of 
				   NONE => empty
				 | SOME sigexp => ppSigExp sigexp
		       in  close(1,"end")
			     (always 4 ( ("structure " ^+ (ppString strid +^ " :>") ++ sigcons) +^ " = struct"
				       , pptoplist (true,false) decs )
                             )
		       end
		  | SigDec(sigid, SigBasic spec) =>
                       close(1,"end")
			    (always 4 ( "signature " ^+ ppString sigid +^ " = sig"
                                      , ppSpec spec )
                            )                                 
		  | SigDec(sigid, SigId id) =>
		       "signature " ^+ ppString sigid +^ (" = " ^ id)

		  | CoreDec decl => ppDec decl
	    and pptoplist mode tops =
		let val tops = 
			List.filter isPrinting (map pptop (coalesce tops))
		in  clist "#" forcemulti tops
		end
	in  pptop dec end
    end (* local *)

end (* structure TinySML *)