(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

(* why is there a difference between string options and bool options? *)

structure State =
struct

    (* Options *)
    datatype origin = COMMAND_LINE | FROM_FILE

    datatype opt = STRING of string option | BOOL of bool
    local
	(* option_value represents the values stored with an option. *)
	datatype option_value = 
	    STR of string
	  | BL of bool

	fun mkString (SOME s) = s
	  | mkString NONE = ""
	fun fromString "" = NONE
          | fromString s = SOME s
	fun mkOption (STRING s) = STR (mkString s)
          | mkOption (BOOL b) = BL b
        
	fun getString (STR s) = if s="" then NONE else SOME s
          | getString _ = Util.shouldntHappen ("Not a string option")
	fun getBool (BL b) = b
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

	fun addOption name default =
	    insert (name, DEF (mkOption default))

	fun addStringOption name = (addOption name) o STRING
	fun addBoolOption name = (addOption name) o BOOL

	fun setOption origin name value =
	    let val info = lookup name
	    in  if priority (origin, info) then set name (mkOption value) origin
		else ()
	    end

	fun setStringOption origin name = (setOption origin name) o STRING
	fun setBoolOption origin name = (setOption origin name) o BOOL

	fun getOption name = getValueFromInfo (lookup name)

	val getStringOption = getString o getOption
	val getBoolOption   = getBool o getOption

    end

end (* structure State *)

(*

    [addOption name value] declare a new option (type specified
    by the value argument) with default value default.

    [addStringOption name default] declare a new string option name
    with default value default.

    [addBoolOption name default] declare a new string option name
    with default value default.

    [setOption origin name value] set the option name to the value
    value PROVIDED values originating from origin takes precedence
    over what is already stored for name. Everything take precedence
    over default values and command-line parameters take precedence
    over options specified in the .defs file.

    [setStringOption origin name value] set the string option name to
    the value (under the rules for setOption).

    [setBoolOption origin name value] set the bool option name to
    the value (under the rules for setOption).

    [getOption name] return the value stored for the option name.

    [getStringOption name] return the value stored for string option
    name.

    [getBoolOption name] return the value stored for bool option name.

*)
