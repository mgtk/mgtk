(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

structure State =
struct

    (* Options *)
    datatype origin = COMMAND_LINE | FROM_FILE

    local
	(* option_value represents the values stored with an option. *)
	datatype option_value = 
	    STRING of string
          | BOOL of bool

	fun getString (STRING s) = if s="" then NONE else SOME s
          | getString _ = Util.shouldntHappen ("Not a string option")
	fun getBool (BOOL b) = b
          | getBool _ = Util.shouldntHappen ("Not a string option")


	(* options_info is the stuff that is stored with an option.
	   In this case it is simply an option_value together with
	   an indication of where the value originated from.
        *)
	datatype options_info = 
	    DEF of option_value
	  | CMD of option_value
	  | FILE of option_value

	fun originToInfo COMMAND_LINE value = CMD value
          | originToInfo FROM_FILE value = FILE value

	fun getValueFromInfo (DEF value) = value
          | getValueFromInfo (CMD value) = value
          | getValueFromInfo (FILE value) = value

        (* [priority (origin, info)] checks whether a value of origin
           origin has priority over the info already stored in the
           options table. *)
        fun priority (COMMAND_LINE, DEF _) = true
	  | priority (FROM_FILE, DEF _) = true
	  | priority (COMMAND_LINE, FILE _) = true
	  | priority (COMMAND_LINE, CMD _) = true
          | priority (FROM_FILE, FILE _) = true
          | priority _ = false


        (* options_table is the table of options. *)
	exception Find
	val (options_table: (string, options_info) Polyhash.hash_table) = 
	    Polyhash.mkPolyTable (*(hash, compare)*) (19, Find)
	    
	val insert = Polyhash.insert options_table 
	fun lookup name =
	    let val info = Polyhash.find options_table name
	    in  info
	    end
	handle Find => Util.notFound("unbound option name: " ^ name)
	fun set name value origin =
	    insert (name, originToInfo origin value)

    in 
	fun addStringOption name default = 
	    let val def = if default = NONE then "" else (valOf default)
	    in  insert (name, DEF (STRING def))
	    end
	fun addBoolOption name default = insert (name, DEF (BOOL default))

	fun setStringOption origin name value =
	    let val info = lookup name
		val valu = if value = NONE then "" else (valOf value)
	    in  if priority (origin, info) then set name (STRING valu) origin
		else ()
	    end
	fun setBoolOption origin name value =
	    let val info = lookup name
	    in  if priority (origin, info) then set name (BOOL value) origin
		else ()
	    end

	fun getStringOption name =
	    let val info = lookup name
	    in  getString(getValueFromInfo info)
	    end
	fun getBoolOption name =
	    let val info = lookup name
	    in  getBool(getValueFromInfo info)
	    end

    end

end (* structure State *)

(*

    [addStringOption name default] declare a new string option name
    with default value default.

    [addBoolOption name default] declare a new string option name
    with default value default.

    [setStringOption origin name value] set the string option name to
    the value value provided values originating from origin takes
    precedence over what is already stored for name. Everything take
    precedence over default values and command-line parameters take
    precedence over options specified in the .defs file.

    [setBoolOption origin name value] set the string option name to
    the value value provided values originating from origin takes
    precedence over what is already stored for name. The precedence
    rules are as for string options.

    [getStringOption name] return the value stored for string option
    name.

    [getBoolOption name] return the value stored for bool option name.

*)
