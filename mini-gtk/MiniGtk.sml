(*
app load ["Dynlib", "Polyhash", "Callback"];
*)
signature GtkBasis =
sig
    type cptr
    type base
    type 'a Object
    type constructor = unit -> cptr

    val repr     : 'a Object -> cptr
    val inherit  : 'a -> constructor -> 'a Object
    val toObject : 'a Object -> base Object

    val init : string list -> unit
    val main : unit -> unit
    val main_quit : unit -> unit

    val symb : string -> Dynlib.symHandle

end


structure GtkBasis :> GtkBasis =
struct
    prim_type cptr
    type base = unit

    (* A litle type cleverness *)
    datatype 'a Object = OBJ of cptr
    type constructor = unit -> cptr


    fun repr (OBJ ptr) = ptr
    fun inherit _ con = OBJ(con())
    fun toObject (OBJ ptr) = OBJ ptr

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
end

signature Signal =
sig
    type state
    type 'a Object = 'a GtkBasis.Object


    type ('a, 'b) trans   = 'a * state -> 'b * state
    type ('a, 'rest) read = ('a -> 'rest, 'rest) trans
    type 'a return        = ('a, unit) trans

    val bool : (bool, 'rest) read
    val int  : (int, 'rest)  read
    val unit : (unit, 'rest) read

    val return_bool : bool return
    val return_int  : int  return
    val return_unit : unit return

    val --> : ('a, 'b) read * ('b, 'c) trans -> ('a -> 'b, 'c) trans 

    type 'a signal
    type signal_id
    val signal  : string -> bool -> ('b -> 'c) return -> ('b -> 'c) ->
                                                  'a Object signal
    val connect : 'a Object -> 'a Object signal -> signal_id
end


structure Signal :> Signal =
struct
    type 'a Object = 'a GtkBasis.Object
    local
        structure GB = GtkBasis
        prim_type GtkArgs

        type callback_data = GB.cptr * GtkArgs * int
        type callback = callback_data -> unit
        type callback_id  = int

        val callbackTable : (callback_id, callback) Polyhash.hash_table
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
                SOME f => f data(* handle e => main_quit_with e)*)
              | NONE   => raise Fail("mgtk: Unknown callback function (id: "^
                                     Int.toString id^")")

        fun destroy id = Polyhash.remove callbackTable id
                     
        val dummy = ( Callback.register "mgtk_callback_dispatch" dispatch
                    ; Callback.register "mgtk_callback_destroy" destroy
                    )
        open Dynlib
        val symb = GB.symb
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
        fun reg_unit f = register(fn _ => f())
        fun reg_bool f = register(fn (_,args,pos) => 
                                     setBool args pos (f()))
                         
        val signal_connect : GB.cptr -> string -> int -> bool -> int
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

    datatype 'a signal = Sig of string * bool * callback

    fun signal sign after conv f = 
        let fun wrap (_, arg, max) = ignore(conv (f, S(arg, max, 0)))
        in  Sig(sign, after, wrap)
        end

    type signal_id = int

    fun connect wid (Sig(sign, after, wrap)) =
        let val id = register wrap
        in  signal_connect (GB.repr wid) sign id after
        end

    (* connect a callback with type unit -> unit *)
    fun unit_connect wid sign cb =
        ignore(connect wid (signal sign false (unit --> return_unit) cb))

    (* connect a callback with type unit -> bool *)
    fun bool_connect wid sign cb =
        ignore(connect wid (signal sign false (unit --> return_bool) cb))
    end	
end



signature Widget =
sig
    type base
    type 'a widget_t
    type 'a t = 'a widget_t GtkBasis.Object

    val destroy : 'a t -> unit
    val show    : 'a t -> unit
    val show_all: 'a t -> unit

    val delete_event_sig : (unit -> bool) -> 'a t Signal.signal
    val destroy_sig      : (unit -> unit) -> 'a t Signal.signal

    val inherit : 'a -> GtkBasis.constructor -> 'a t
    val toWidget: 'a t -> base t

end

structure Widget :> Widget =
struct
    type base = unit
    type 'a widget_t = unit
    type 'a t = 'a widget_t GtkBasis.Object

    open Dynlib
    local 
	val path = case Process.getEnv "MGTKHOME" of
	               SOME p => Path.concat (p, "mgtk.so")
		     | NONE   => "./mgtk.so"
	val hdl  = dlopen {lib = path, flag = RTLD_LAZY, global = false}
    in
    val symb = dlsym hdl
    end
    type cptr = GtkBasis.cptr

    fun inherit w con = GtkBasis.inherit () con
    fun toWidget obj = GtkBasis.inherit () (fn () => GtkBasis.repr obj)

    val destroy_: cptr -> unit
        = app1(symb"mgtk_gtk_widget_destroy")
    val destroy: 'a t -> unit
        = fn widget => destroy_ (GtkBasis.repr widget)

    val show_: cptr -> unit
        = app1(symb"mgtk_gtk_widget_show")
    val show: 'a t -> unit
        = fn widget => show_ (GtkBasis.repr widget)

    val show_all_: cptr -> unit
        = app1(symb"mgtk_gtk_widget_show_all")
    val show_all: 'a t -> unit
        = fn widget => show_all_ (GtkBasis.repr widget)

    local open Signal infix --> in
    fun delete_event_sig f = 
        signal "delete_event" true (unit --> return_bool) f
    fun destroy_sig f = 
        signal "destroy" false (unit --> return_unit) f
    end
end

(* *** Container *** *)
signature Container =
sig
    type 'a container_t
    type 'a t = 'a container_t Widget.t

    val set_border_width: 'a t -> int -> unit
    val add   : 'a t -> 'b Widget.t -> unit
    val remove: 'a t -> 'b Widget.t -> unit

    val inherit : 'a -> GtkBasis.constructor -> 'a t
end

structure Container :> Container =
struct
    type 'a container_t = unit
    type 'a t = 'a container_t Widget.t

    open Dynlib
    val symb = GtkBasis.symb
    type cptr = GtkBasis.cptr
    val repr = GtkBasis.repr

    fun inherit w con = Widget.inherit () con

    val set_border_width_: cptr -> int -> unit
        = app2(symb"mgtk_gtk_container_set_border_width")
    val set_border_width: 'a t -> int -> unit
        = fn container => fn border_width => set_border_width_ (repr container) border_width

    val add_: cptr -> cptr -> unit
        = app2(symb"mgtk_gtk_container_add")
    val add: 'a t -> 'b Widget.t -> unit
        = fn container => fn widget => add_ (repr container) (repr widget)

    val remove_: cptr -> cptr -> unit
        = app2(symb"mgtk_gtk_container_remove")
    val remove: 'a t -> 'b Widget.t -> unit
        = fn container => fn widget => remove_ (repr container) (repr widget)
end


signature Button =
sig
    type 'a button_t
    type 'a t = 'a button_t Container.t
    type base

    val new: unit -> base t
    val new_with_label: string -> base t

    val clicked_sig : (unit -> unit) -> 'a t Signal.signal

    val inherit : 'a -> GtkBasis.constructor -> 'a t
end

structure Button :> Button =
struct
    type 'a button_t = unit
    type 'a t = 'a button_t Container.t
    type base = unit

    open Dynlib
    val symb = GtkBasis.symb
    type cptr = GtkBasis.cptr
    val repr = GtkBasis.repr

    fun inherit w con = Container.inherit () con
    fun makeBut ptr = Container.inherit () (fn() => ptr)

    val new_: unit -> cptr
        = app1(symb"mgtk_gtk_button_new")
    val new: unit -> base t
        = fn dummy => makeBut(new_ dummy)

    val new_with_label_: string -> cptr
        = app1(symb"mgtk_gtk_button_new_with_label")
    val new_with_label: string -> base t
        = fn label => makeBut(new_with_label_ label)

    local open Signal infix --> in
    fun clicked_sig f = 
        signal "clicked" false (unit --> return_unit) f
    end
end


(* The window stuff is only for demo purposes *)
signature Window =
sig
    type 'a window_t
    type 'a t = 'a window_t Container.t
    type base

    val new: unit -> base t
end


structure Window :> Window =
struct
    type 'a window_t = unit
    type 'a t = 'a window_t Container.t
    type base = unit

    open Dynlib
    val symb = GtkBasis.symb
    type cptr = GtkBasis.cptr
    val repr = GtkBasis.repr

    fun inherit w con = Container.inherit () con
    fun makeWin ptr = Container.inherit () (fn() => ptr)

    val new_: int -> cptr
        = app1(symb"mgtk_gtk_window_new")
    val new: unit -> base t
        = fn dummy => makeWin(new_ 0) (* FIXME: HACK ALERT!!!!!*)

end

structure HelloWorld =
struct
fun hello _ = print "Hello World\n"

fun delete_event _ = ( print "delete event occurred\n"
		     ; false)

fun destroy _ = GtkBasis.main_quit()

fun main () =
    let val _      = GtkBasis.init(CommandLine.name()::CommandLine.arguments())
	val window = Window.new ()
	val button = Button.new_with_label "Hello World"
    in  Signal.connect window (Widget.delete_event_sig delete_event) 
      ; Signal.connect window (Widget.destroy_sig destroy) 
      ; Container.set_border_width window 10
      ; Signal.connect button (Button.clicked_sig hello)
      ; Container.add window button
      ; Widget.show_all window
      ; GtkBasis.main() 
    end
end

val _ = HelloWorld.main()