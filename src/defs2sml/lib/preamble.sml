signature GtkBasis =
sig
    val init : string list -> string list
    val main : unit -> unit
    val main_quit : unit -> unit

    val symb : string -> Dynlib.symHandle
end

structure GtkBasis :> GtkBasis =
struct
    open Dynlib
    local 
	val path = case Process.getEnv "MGTKHOME" of
	               SOME p => Path.concat (p, "mgtk.so")
		     | NONE   => "./mgtk.so"
	
	val hdl  = dlopen {lib = path, flag = RTLD_LAZY, global = false}
    in
	val symb = dlsym hdl
    end

    (* Basic GTK stuff *)
    val init_ : string vector -> string list = app1(symb "mgtk_init")
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
end


signature GObject =
sig
    type cptr
    type base
    type 'a t
    type constructor = unit -> cptr

    val null : cptr

    val repr     : 'a t -> cptr
    val inherit  : 'a -> constructor -> 'a t
    val toObject : 'a t -> base t

end


structure GObject :> GObject =
struct
    prim_type cptr
    type base = unit

    (* A litle type cleverness *)
    datatype 'a t = OBJ of cptr
    type constructor = unit -> cptr

    val null = Dynlib.app1(GtkBasis.symb("mgtk_get_null")) ()

    fun repr (OBJ ptr) = ptr
    fun inherit _ con = OBJ(con())
    fun toObject (OBJ ptr) = OBJ ptr

end

signature Signal =
sig
    type state
    type 'a t = 'a GObject.t


    type ('a, 'b) trans   = 'a * state -> 'b * state
    type ('a, 'rest) read = ('a -> 'rest, 'rest) trans
    type 'a return        = ('a, unit) trans

    val bool   : (bool,   'rest) read
    val int    : (int,    'rest) read
    val char   : (char,   'rest) read
    val double : (real,   'rest) read
    val string : (string, 'rest) read
    val unit   : (unit,   'rest) read

    val void : (unit, 'rest) read

    val return_bool   : bool   return
    val return_int    : int    return
    val return_char   : char   return
    val return_double : real   return
    val return_string : string return
    val return_unit   : unit   return

    val return_void : unit return

    val --> : ('a, 'b) read * ('b, 'c) trans -> ('a -> 'b, 'c) trans 

    type 'a signal
    type signal_id
    val signal  : string -> bool -> ('b -> 'c) return -> ('b -> 'c) ->
                                                  'a t signal
    val connect : 'a t -> 'a t signal -> signal_id
end


structure Signal :> Signal =
struct
    type 'a t = 'a GObject.t
    local
        structure GO = GObject
        prim_type GValues
        prim_type GValue

        type callback_data = GValue * GValues * int
	type callback = callback_data -> unit
	type callback_id  = int

	val callbackTable : (int, callback) Polyhash.hash_table
                          = Polyhash.mkPolyTable(401, Domain)
                      
	val add     = Polyhash.insert callbackTable
	val peek    = Polyhash.peek callbackTable
        val destroy = Polyhash.remove callbackTable

	local
	    val intern = ref 0;
	in
	    val localId = fn f => f (!intern before intern := !intern + 1)
	end

	open Dynlib
	val symb = GtkBasis.symb

	fun dispatch id data =
	    case peek id of
		SOME f => f data    (* FIXME: we need a handle here, but what 
                                              should it do *)
	      | NONE   => raise Fail("mgtk: Unknown callback function (id: "^
				     Int.toString id^")")

	val dummy = ( Callback.register "mgtk_callback_dispatch" dispatch
                    ; Callback.register "mgtk_callback_destroy" destroy
		    )

	fun register f = localId(fn id => (add (id, f); id))
	val signal_connect : GO.cptr -> string -> int -> bool -> int
	                   = app4(symb"mgtk_signal_connect")

        (* UNSAFE: no error checking in the set and get functions! *)
        type 'a setter = GValue -> 'a -> unit
        val setBool   : bool setter   = app2(symb "mgtk_set_bool")
        val setInt    : int setter    = app2(symb "mgtk_set_int")
        val setChar   : char setter   = app2(symb "mgtk_set_char")
        val setDouble : real setter   = app2(symb "mgtk_set_double")
        val setString : string setter = app2(symb "mgtk_set_string")


        type 'a getter = GValues -> int -> 'a
        val getBool   : bool getter   = app2(symb "mgtk_get_pos_bool")
        val getInt    : int getter    = app2(symb "mgtk_get_pos_int")
        val getChar   : char getter   = app2(symb "mgtk_get_pos_char")
        val getDouble : real getter   = app2(symb "mgtk_get_pos_double")
        val getString : string getter = app2(symb "mgtk_get_pos_string")
(*
        val getLong   : int getter    = app2(symb "mgtk_get_pos_long")
        val getString : string getter = app2(symb "mgtk_get_pos_string")
*)
    in
    datatype state = S of GValue * GValues * int * int
    type ('a, 'b) trans   = 'a * state -> 'b * state
    type ('a, 'rest) read = ('a -> 'rest, 'rest) trans
    type 'a return        = ('a, unit) trans

    fun state f ret arg max = (f, S(ret, arg, max, 0+1))
    (* NOTE: the +1 is for the object connected to *)

    fun wrap conv f (ret, arg, max) = ignore(conv(state f ret arg max)) 

    fun getter get (f, S(ret, arg, max, next)) = 
        if next < max  (* FIXME: it should be < but that gives problems with
                                  return_unit.  Currently unsafe! *)
        then (f (get arg next), S(ret, arg, max, next+1))
        else raise Subscript

    fun drop (f, S(ret, arg, max, next)) = 
        if next < max then (f, S(ret, arg, max, next+1))
        else raise Subscript

    fun setter set (x, dummy as S(ret, arg, max, next)) =
        if next = max then (set ret x; ((),dummy))
        else raise Subscript

    fun int x        = getter getInt x
    fun return_int x = setter setInt x

    fun bool x        = getter getBool x
    fun return_bool x = setter setBool x

    fun char x        = getter getChar x
    fun return_char x = setter setChar x

    fun double x        = getter getDouble x
    fun return_double x = setter setDouble x

    fun string x        = getter getString x
    fun return_string x = setter setString x

    
    (* FIXME: convince Ken that this correct *)
    fun void (f, state) = (f(), state)
    fun return_void (f, S(ret, arg, max, next)) = (f, S(ret, arg,max+1,next))

    fun unit x        = getter (fn _ => fn _ => ()) x
    fun return_unit x =  x
               
    infix --> 

    fun (x --> y) arg = y (x arg)                        

    datatype 'a signal = Sig of string * bool * callback

    fun signal sign after conv f = Sig(sign, after, wrap conv f)

    type signal_id = int

    fun connect wid (Sig(sign, after, wrap)) =
        let val id = register wrap
        in  signal_connect (GO.repr wid) sign id after
        end

    end	
end
