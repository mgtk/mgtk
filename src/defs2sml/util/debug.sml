structure Debug :> Debug = struct

    structure Map = Splaymap

    type debug_info =
	 {short: string, long: string option, included: bool}
    fun set ({short,long,included}:debug_info) =
	{short=short,long=long,included=true}
    fun clr ({short,long,included}:debug_info) =
	{short=short,long=long,included=false}
    fun toggle ({short,long,included}:debug_info) =
	{short=short,long=long,included=not included}

    val phases : (string,debug_info) Map.dict ref = 
	ref(Map.mkDict String.compare)
    fun add {name:string,short_option:string,long_option:string option,included:bool} =
	(Map.find(!phases, name); raise Fail("Debug.addPhase: "^name^" already registered"))
	handle Map.NotFound => 
	   phases := Map.insert(!phases,name,{short=short_option,long=long_option,included=included})

    fun included name = #included (Map.find(!phases,name))
			handle Map.NotFound => raise Fail("Debug.included: " ^ name ^ " unknown")

    fun update upd name =
	let val info = Map.find(!phases,name)
	in  phases := Map.insert(!phases,name,upd info) end
	    handle Map.NotFound => raise Fail("Debug.update: " ^ name ^ " unknown")
    val doInclude = update set
    val dontInclude = update clr
    val toggle = update toggle

    fun argparse () =
	let val debugs = Map.listItems(!phases)
	    val shorts = List.map (fn (n,{short,long,included}) => ("-"^short,ArgParse.Unit(fn () => toggle n))) debugs
	    val longs = List.mapPartial (fn (n,{short,long,included}) => Option.map (fn l => ("--"^l,ArgParse.Unit(fn () => toggle n))) long) debugs
	in  shorts @ longs end

end (* structure Debug *)