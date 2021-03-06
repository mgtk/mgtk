(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)
(*
app load ["Dynlib", "Polyhash", "Callback"];
*)
structure Gtk :> Gtk =
struct
    prim_type cptr

    open Dynlib
    local 
	val path = case Process.getEnv "MGTKHOME" of
	               SOME p => Path.concat (p, "mgtk.so")
		     | NONE   => "./mgtk.so"
	
	val hdl  = dlopen {lib = path, flag = RTLD_NOW, global = false}
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

    (* A litle type cleverness *)
    datatype 'a GtkObject = OBJ of cptr

    (* Typecast function *)
    fun super (OBJ obj) = OBJ obj

    fun unwrap (OBJ obj) = obj
    fun unwrapObjOpt NONE = NONE
      | unwrapObjOpt (SOME (OBJ obj)) = SOME obj

    val toWidget = super
    val toObject = super

    local
	prim_type GValues
        prim_type GValue (* a pointer to a GValue *)

        type callback_data = GValue * GValues * int
	type callback = callback_data -> unit
	type callback_id  = int

	val callbackTable : (int, callback) Polyhash.hash_table
                          = Polyhash.mkPolyTable(401, Domain)
                      
	val add  = Polyhash.insert callbackTable
	val peek = Polyhash.peek callbackTable

        val destroy = Polyhash.remove callbackTable

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

	val dummy = ( Callback.register "mgtk_callback_dispatch" dispatch
                    ; Callback.register "mgtk_callback_destroy" destroy
		    )

	fun register f = localId(fn id => (add (id, f); id))
	val signal_connect : cptr -> string -> int -> bool -> int
	                   = app4(symb"mgtk_signal_connect")

        (* UNSAFE: no error checking in the set and get functions! *)
        type 'a setter = GValue -> 'a -> unit
        val setBool : bool setter = app2(symb "mgtk_set_bool")
        val setInt  : int setter  = app2(symb "mgtk_set_int")


        type 'a getter = GValues -> int -> 'a
        val getBool   : bool getter   = app2(symb "mgtk_get_pos_bool")
        val getInt    : int getter    = app2(symb "mgtk_get_pos_int")
(*        val getLong   : int getter    = app2(symb "mgtk_get_pos_long")
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

    fun signalConnect (OBJ wid) (Sig(sign, after, wrap)) =
        let val id = register wrap
        in  signal_connect wid sign id after
        end

    (* new formulations of the two old connect functions *)
    (* connect a callback with type unit -> unit *)
    fun unit_connect wid sign cb =
        ignore(signalConnect wid (signal sign false (void --> return_void) cb))

    (* connect a callback with type unit -> bool *)
    fun bool_connect wid sign cb =
        ignore(signalConnect wid (signal sign true (unit --> return_bool) cb))

    end	

    type base = unit
    type 'a widget_t = base
    type 'a GtkWidget = 'a widget_t GtkObject

    val widget_destroy_: cptr -> unit
        = app1(symb"mgtk_gtk_widget_destroy")
    val widget_destroy: 'a GtkWidget -> unit
        = fn OBJ widget => widget_destroy_ widget

    val widget_show_: cptr -> unit
        = app1(symb"mgtk_gtk_widget_show")
    val widget_show: 'a GtkWidget -> unit
        = fn OBJ widget => widget_show_ widget

    val widget_show_all_: cptr -> unit
        = app1(symb"mgtk_gtk_widget_show_all")
    val widget_show_all: 'a GtkWidget -> unit
        = fn OBJ widget => widget_show_all_ widget

    (* *** Container *** *)

    type 'a container_t = base
    type 'a GtkContainer = 'a container_t GtkWidget


    val container_set_border_width_: cptr -> int -> unit
        = app2(symb"mgtk_gtk_container_set_border_width")
    val container_set_border_width: 'a GtkContainer -> int -> unit
        = fn OBJ container => fn border_width => container_set_border_width_ container border_width

    val container_add_: cptr -> cptr -> unit
        = app2(symb"mgtk_gtk_container_add")
    val container_add: 'a GtkContainer -> 'b GtkWidget -> unit
        = fn OBJ container => fn OBJ widget => container_add_ container widget

    val container_remove_: cptr -> cptr -> unit
        = app2(symb"mgtk_gtk_container_remove")
    val container_remove: 'a GtkContainer -> 'b GtkWidget -> unit
        = fn OBJ container => fn OBJ widget => container_remove_ container widget

    (* *** Button *** *)

    type 'a button_t = base
    type 'a GtkButton = 'a button_t GtkContainer


    val button_new_: unit -> cptr
        = app1(symb"mgtk_gtk_button_new")
    val button_new: unit -> base GtkButton
        = fn dummy => OBJ(button_new_ dummy)

    val button_new_with_label_: string -> cptr
        = app1(symb"mgtk_gtk_button_new_with_label")
    val button_new_with_label: string -> base GtkButton
        = fn label => OBJ(button_new_with_label_ label)

    (* *** Window *** *)

    type 'a window_t = base
    type 'a GtkWindow = 'a window_t GtkContainer


    val window_new_: int -> cptr
        = app1(symb"mgtk_gtk_window_new")
    val window_new: unit -> base GtkWindow
        = fn typ => OBJ(window_new_ 0)

    (* *** Signals *** *)
    val connect_destroy: 'a GtkObject -> (unit -> unit) -> unit
        = fn wid => fn cb => unit_connect wid "destroy" cb
    val connect_delete_event: 'a GtkWidget -> (unit -> bool) -> unit
        = fn wid => fn cb => (*bool_connect wid "delete_event" cb*)
                       ignore(signalConnect wid 
                              (signal "delete_event" false 
                                      (unit --> return_bool) cb))
    val connect_clicked: 'a GtkButton -> (unit -> unit) -> unit
        = fn wid => fn cb => unit_connect wid "clicked" cb

end
