structure Gtk :> Gtk =
struct

    prim_type gtkobj
    prim_type gpointer (* the type of all boxed types *)

    open Dynlib
    local 
	val path = case Process.getEnv "MGTKHOME" of
	               SOME p => Path.concat (p, "mgtk.so")
		     | NONE   => "mgtk.so"
	
	val hdl  = dlopen {lib = path, flag = RTLD_NOW, global = false}
    in
	val symb = dlsym hdl
    end

    (* some helper functions *)
    fun cur2 h (a,b) = app2 h a b
    fun cur3 h (a,b,c) = app3 h a b c
  

    (* Basic GTK stuff *)
    val init_ : string vector -> unit = app1(symb "mgtk_init")
    fun init args = 
	let val args =
	        case args of
		    [] => (print "mGTK Warning: Gtk.init called with empty list\n";
			   [CommandLine.name()])
		  | _  => args
	in
	    init_(vector args)
	end
    val main : unit -> unit = app1(symb "mgtk_main")
    val main_quit : unit -> unit = app1(symb "mgtk_main_quit")

    (* A litle type cleverness *)
    datatype 'a GtkObject = OBJ of gtkobj

    (* Typecast function *)
    fun super (OBJ obj) = OBJ obj

    fun unwrap (OBJ obj) = obj
    fun unwrapObjOpt NONE = NONE
      | unwrapObjOpt (SOME (OBJ obj)) = SOME obj

    val toWidget = super
    val toObject = super

    (* convert a list of flags to a word *)
    local
	infix orb andb
	val notb = Word.notb 
	val op orb = Word.orb 
	val op andb = Word.andb
	val W = Word.fromInt
	fun set(f,res) = W f orb res
	fun unset(f, res) = notb(W f) andb res
    in
	(* val setFlagsGeneral : flag -> flag list -> flag list -> flag *)
	fun setFlagsGeneral init pos neg =
	    let val res = List.foldl set (W init) pos
		val res = List.foldl unset res neg
	    in  Word.toInt res
	    end
	(* we only need to set flags from the no flag *)
        (* val setFlags : flag list -> flag *)
        fun setFlags pos =
	    let val res = List.foldl set 0w0 pos
	    in  Word.toInt res
	    end

	(* Checks which flags from possible is set in flag
           val isSet : flag list -> flag -> flag list *) 
	fun isSet possible flag =
	    let val f = W flag
		fun isIn pos = (f andb (W pos)) <> 0w0
	    in  List.filter isIn possible
	    end

	fun getFlags f =
	    let val flag = W f
		fun loop p acc = 
		    let val acc = if (flag andb p) <> 0w0 
				  then Word.toInt p :: acc
				  else acc
			val next = Word.<<(p, 0w1)
		    in  if next = 0w0 then acc
			else loop next acc
		    end
	    in  loop 0w1 []
	    end

	fun areTheseSet flags flag = ((W(setFlags flags)) andb (W flag)) <> 0w0
    end

    local
	prim_type GtkArgs

        type callback_data = gtkobj * GtkArgs * int
	type callback = callback_data -> unit
	type callback_id  = int

	val callbackTable : (int, callback) Polyhash.hash_table
                          = Polyhash.mkPolyTable(401, Domain)
                      
	val add  = Polyhash.insert callbackTable
	val peek = Polyhash.peek callbackTable

	local
	    val intern = ref 0;
	in
	    val localId = fn f => f (!intern before intern := !intern + 1)
	end

	fun dispatch id data =
	    case peek id of
		SOME f => f data    (* FIXME: we need a handle here, but what 
                                              should it do *)
	      | NONE   => raise Fail("mgtk: Unknown callback function (id: "^
				     Int.toString id^")")

        fun destroy id = Polyhash.remove callbackTable id

	val dummy = ( Callback.register "mgtk_callback_dispatch" dispatch
                    ; Callback.register "mgtk_callback_destroy" destroy
		    )

        (* UNSAFE: no error checking in the set and get functions! *)
        type 'a setter = GtkArgs -> int -> 'a -> unit
        val setBool : bool setter = app3(symb "mgtk_set_retpos_bool")
        val setInt  : int setter  = app3(symb "mgtk_set_retpos_int")


        type 'a getter = GtkArgs -> int -> 'a
        val getBool   : bool getter   = app2(symb "mgtk_get_pos_bool")
        val getInt    : int getter    = app2(symb "mgtk_get_pos_int")
        val getLong   : int getter    = app2(symb "mgtk_get_pos_long")
        val getChar   : char getter   = app2(symb "mgtk_get_pos_char")
        val getString : string getter = app2(symb "mgtk_get_pos_string")

	fun register f = localId(fn id => (add (id, f); id))
	val signal_connect : gtkobj -> string -> int -> bool -> int
	                   = app4(symb"mgtk_signal_connect")
    in
    datatype state = S of GtkArgs * int * int
    type ('a, 'b) trans   = 'a * state -> 'b * state
    type ('a, 'rest) read = ('a -> 'rest, 'rest) trans
    type 'a return        = ('a, unit) trans

    fun getter get (f, S(arg, max, next)) = 
        if next <= max  (* FIXME: it should be < but that gives problems with
                                  return_unit.  Currently unsafe *)
        then (f (get arg next), S(arg, max, next+1))
        else raise Subscript

    fun setter set (x, dummy as S(arg, max, next)) =
        if next = max then (set arg max x; ((),dummy))
        else raise Subscript

    fun int x        = getter getInt x
    fun return_int x = setter setInt x

    fun bool x        = getter getBool x
    fun return_bool x = setter setBool x

    
    fun unit x        = getter (fn _ => fn _ => ()) x
    fun return_unit x = x
    (*fun return_unit x = setter (fn _ => fn _ => fn _ => ()) x
    *)           

               
    infix --> 

    fun (x --> y) arg = y (x arg)                        

    fun signalConnect (OBJ wid) sign after conv f =
        let fun wrap (_, arg, max) = ignore(conv (f, S(arg, max, 0)))
            val id = register wrap
        in  signal_connect wid sign id after
        end


    (* new formulations of the two old connect functions *)
    (* connect a callback with type unit -> unit *)
    fun unit_connect wid sign cb =
        ignore(signalConnect wid sign false (unit --> return_unit) cb)

    (* connect a callback with type unit -> bool *)
    fun bool_connect wid sign cb =
        ignore(signalConnect wid sign false (unit --> return_bool) cb)

    (* old connect functions 
    fun unit_connect (OBJ wid) sign cb =
	let val id = reg_unit cb
	in  ignore(signal_connect wid sign id false)
	end
    fun bool_connect (OBJ wid) sign cb =
	let val id = reg_bool cb
	in  ignore(signal_connect wid sign id true)
	end
    *)


    end	

    type base = unit
    type 'a widget_t = base
    type 'a GtkWidget = 'a widget_t GtkObject

    type gtk_type = int
