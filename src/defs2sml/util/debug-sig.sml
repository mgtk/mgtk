signature Debug = sig

    val add: {name:string,
	      short_option:string,
	      long_option:string option,
	      included:bool} -> unit

    val included: string -> bool
    val doInclude: string -> unit
    val dontInclude: string -> unit

    val argparse: unit -> (string * ArgParse.spec) list

end (* signature Debug *)