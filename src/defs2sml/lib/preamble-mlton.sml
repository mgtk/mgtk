signature CString =
sig
    type cstring
    val fromString : string -> cstring

    type t
    val toString : t -> string

    val free : t -> unit
end

structure CString :> CString =
struct
    type cstring = string 
    fun fromString s = s ^ "\000"

    type t = MLton.Pointer.t
    val sub = _import "mgtk_stringsub" : t * int -> char;

    fun toVector t =
        let fun size i = if sub(t, i) = #"\000" then i
                         else size(i+1)
        in  CharVector.tabulate(size 0, fn i => sub(t, i))
        end

    val toString = toVector

    val free = _import "free" : t -> unit;
end


signature GtkBasis =
sig
    val init : string list -> unit
    val main : unit -> unit
    val main_quit : unit -> unit
end




structure GtkBasis :> GtkBasis =
struct
    structure AS = ArraySlice
    (* Basic GTK stuff *)
    val gtk_init_ = _import "mgtk_init" 
                  : CString.cstring array * int -> unit;
    fun init args = 
	let val args =
	        case args of
		    [] => (print "mGTK Warning: Gtk.init called with empty list\n";
			   [CommandLine.name()])
		  | _  => args
            val argv = Array.fromList(List.map CString.fromString args)
        in  gtk_init_(argv, Array.length argv)
	end

    val main      = _import "gtk_main" : unit -> unit;
    val main_quit = _import "gtk_main_quit" : unit -> unit; 
end


signature GObject =
sig
    type cptr
    type base
    type 'a t
    type constructor = unit -> cptr

    val null : cptr

    val withPtr  : 'a t * (cptr -> 'b) -> 'b
    val inherit  : 'a -> constructor -> 'a t
    val toObject : 'a t -> base t

end


structure GObject :> GObject =
struct
    structure F = MLton.Finalizable
(*    structure FXP = FinalizableXP
*)
    type cptr = MLton.Pointer.t
    type base = unit

    (* A litle type cleverness *)
    datatype 'a t = OBJ of cptr F.t
    type constructor = unit -> cptr

    val null = MLton.Pointer.null

    fun withPtr (OBJ ptr, f) = F.withValue(ptr, f)

    val object_ref = _import "g_object_ref" : cptr -> cptr;
    val object_unref = _import "g_object_unref" : cptr -> unit;
    
    fun inherit _ con = let val ptr = object_ref(con())
                            val obj = F.new ptr
                        in  F.addFinalizer(obj, object_unref)
                          ; OBJ obj
                        end

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
    val unit   : (unit,   'rest) read

    val void : (unit, 'rest) read

    val return_bool   : bool   return
    val return_int    : int    return
    val return_char   : char   return
    val return_double : real   return
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
        structure CT = Callbacktable

        type GValues = MLton.Pointer.t
        type GValue = MLton.Pointer.t

        type callback_data = GValue * GValues * int
	type callback = callback_data -> unit
	type callback_id  = int

	val callbackTable : callback CT.t = CT.new 401
                      
	val add     = CT.insert callbackTable
	val peek    = CT.peek callbackTable
        val destroy = CT.remove callbackTable

	local
	    val intern = ref 0;
	in
	    val localId = fn f => f (!intern before intern := !intern + 1)
	end

	val dispatch : callback_id * GValue * GValues * int -> unit =
            fn (id, value, values, size) =>
	    case peek id of
		SOME f => f (value, values, size)   
              (* FIXME: we need a handle here, but what 
                        should it do 
               *)
	      | NONE   => (print ("mgtk: Unknown callback function (id: "^
				     Int.toString id^")\n")
                          ; TextIO.flushOut TextIO.stdOut)

	val _ = _export "mgtk_callback_dispatch_smlside" 
                        : callback_id * GValue * GValues * int -> unit;
                dispatch
        val _ = _export "mgtk_callback_destroy_smlside"
                         : callback_id -> unit; 
                destroy
		

	fun register f = localId(fn id => (add (id, f); id))
	val signal_connect = _import "mgtk_signal_connect"
                           : GO.cptr * CString.cstring * int * bool -> int;



        fun curry f x y = f(x,y)

        (* UNSAFE: no error checking in the set and get functions! *)
        type 'a setter_ = GValue * 'a -> unit
        type 'a setter = GValue -> 'a -> unit
        val setBool = _import "g_value_set_boolean" : bool setter_;
        val setBool = curry setBool
        val setInt  = _import "g_value_set_long" : int setter_;  
        val setInt  = curry setInt
	val setChar = _import "g_value_set_char" : char setter_;
        val setChar = curry setChar
	val setDouble = _import "g_value_set_double" : real setter_;
	val setDouble = curry setDouble

        type 'a getter_ = GValues * int -> 'a
        type 'a getter = GValues -> int -> 'a
        val getBool = _import "mgtk_get_pos_bool" : bool getter_;
        val getBool = curry getBool
        val getInt  = _import "mgtk_get_pos_int"  : int getter_;
        val getInt  = curry getInt 
	val getChar = _import "mgtk_get_pos_char" : char getter_;
	val getChar = curry getChar
	val getDouble = _import "mgtk_get_pos_double" : real getter_;
        val getDouble = curry getDouble

(*
        val getLong   : int getter    = app2(symb "mgtk_get_pos_long")
        val getChar   : char getter   = app2(symb "mgtk_get_pos_char")
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
        let val id    = register wrap
            val csign = CString.fromString sign
        in  GO.withPtr(wid, fn wid => signal_connect (wid, csign, id, after))
        end

    end	
end
