(*
app load ["Dynlib", "Polyhash", "Callback"];
*)

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

    val bool : (bool, 'rest) read
    val int  : (int, 'rest)  read
    val unit : (unit, 'rest) read

    val void : (unit, 'rest) read

    val return_bool : bool return
    val return_int  : int  return
    val return_unit : unit return

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
        val setBool : bool setter = app2(symb "mgtk_set_bool")
        val setInt  : int setter  = app2(symb "mgtk_set_int")


        type 'a getter = GValues -> int -> 'a
        val getBool   : bool getter   = app2(symb "mgtk_get_pos_bool")
        val getInt    : int getter    = app2(symb "mgtk_get_pos_int")
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

(*signature IOChannel =
sig
    type input
    type output
    type 'a stream
    type instream = input stream
    type outstrem = output stream
end


signature Spawn =
sig
    type flag
    type error
    datatype result = ERROR of error
                    | OK    of { input  : IOChannel.outstream option
                               , output : IOChannel.instream option
                               , error  : IOChannel.instream option
                               }

    val spawn : { working_directory : string
                , command : string
                , arguments : string list
                , env : (string * string) list
                , flags : flag list
                , create_pipes : bool 
                } -> result

end
*)




signature Widget =
sig
    type base
    type 'a widget_t
    type 'a t = 'a widget_t GObject.t

    val destroy : 'a t -> unit
    val show    : 'a t -> unit
    val show_all: 'a t -> unit

    val delete_event_sig : (unit -> bool) -> 'a t Signal.signal
    val destroy_sig      : (unit -> unit) -> 'a t Signal.signal

    val inherit : 'a -> GObject.constructor -> 'a t
    val toWidget: 'a t -> base t

end

structure Widget :> Widget =
struct
    type base = unit
    type 'a widget_t = unit
    type 'a t = 'a widget_t GObject.t

    open Dynlib
    val symb  = GtkBasis.symb
    type cptr = GObject.cptr
    val repr  = GObject.repr

    fun inherit w con = GObject.inherit () con
    fun toWidget obj = GObject.inherit () (fn () => repr obj)

    val destroy_: cptr -> unit
        = app1(symb"mgtk_gtk_widget_destroy")
    val destroy: 'a t -> unit
        = fn widget => destroy_ (GObject.repr widget)

    val show_: cptr -> unit
        = app1(symb"mgtk_gtk_widget_show")
    val show: 'a t -> unit
        = fn widget => show_ (GObject.repr widget)

    val show_all_: cptr -> unit
        = app1(symb"mgtk_gtk_widget_show_all")
    val show_all: 'a t -> unit
        = fn widget => show_all_ (GObject.repr widget)

    local open Signal infix --> in
    fun delete_event_sig f = 
        signal "delete_event" true (unit --> return_bool) f
    fun destroy_sig f = 
        signal "destroy" false (void --> return_void) f
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

    val inherit : 'a -> GObject.constructor -> 'a t
end

structure Container :> Container =
struct
    type 'a container_t = unit
    type 'a t = 'a container_t Widget.t

    open Dynlib
    type cptr = GObject.cptr
    val symb  = GtkBasis.symb
    val repr  = GObject.repr

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

    val inherit : 'a -> GObject.constructor -> 'a t

    val set_label: 'a t -> string -> unit
end

structure Button :> Button =
struct
    type 'a button_t = unit
    type 'a t = 'a button_t Container.t
    type base = unit

    open Dynlib
    type cptr = GObject.cptr
    val symb  = GtkBasis.symb
    val repr  = GObject.repr

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

    val set_label_: cptr -> string -> unit
        = app2(symb"mgtk_gtk_button_set_label")
    val set_label: 'a t -> string -> unit
        = fn button => fn label => set_label_(repr button) label

    local open Signal infix --> in
    fun clicked_sig f = 
        signal "clicked" false (void --> return_void) f
    fun move_cursor_sig f =
	signal "move-cursor" false (unit --> (int --> (bool --> return_void)))
    end
end


(* The window stuff is only for demo purposes *)
signature Window =
sig
    type 'a window_t
    type 'a t = 'a window_t Container.t
    type base

    type window_type 
    val WINDOW_TOPLEVEL : window_type
    val WINDOW_POPUP : window_type

    val new: window_type -> base t

    val get_size: 'a t -> int * int
end


structure Window :> Window =
struct
    type 'a window_t = unit
    type 'a t = 'a window_t Container.t
    type base = unit

    open Dynlib
    type cptr = GObject.cptr
    val symb  = GtkBasis.symb
    val repr  = GObject.repr

    fun inherit w con = Container.inherit () con
    fun makeWin ptr = Container.inherit () (fn() => ptr)

    type window_type = int
    val get_window_type_: unit -> int * int
        = app1(symb"mgtk_get_window_type")
    val (WINDOW_TOPLEVEL,WINDOW_POPUP)
        = get_window_type_ ()


    val new_: int -> cptr
        = app1(symb"mgtk_gtk_window_new")
    val new: window_type -> base t
        = fn wtype => makeWin(new_ wtype)

    val get_size_: cptr -> int ref -> int ref -> unit
        = app3(symb"mgtk_gtk_window_get_size")
    val get_size = fn self =>
		      let val (x1,x2) = (ref 0, ref 0)
		      in  get_size_ (repr self) x1 x2
                        ; (!x1, !x2)
		      end
end

signature Editable =
sig
    type 'a editable_t
    type 'a t = 'a editable_t Widget.t

    val inherit : 'a -> GObject.constructor -> 'a t
end

structure Editable :> Editable =
struct
    type 'a editable_t = unit
    type 'a t = 'a editable_t Widget.t

    open Dynlib
    type cptr = GObject.cptr
    val symb  = GtkBasis.symb
    val repr  = GObject.repr

    fun inherit w con = Widget.inherit () con
end

signature Entry =
sig

    type 'a entry_t
    type 'a t = 'a entry_t Editable.t
    type base

    val new: unit -> base t
    val get_text : 'a t -> string
    val activate_sig : (unit -> unit) -> 'a t Signal.signal
end


structure Entry :> Entry =
struct
    type 'a entry_t = unit
    type 'a t = 'a entry_t Editable.t
    type base = unit

    open Dynlib
    type cptr = GObject.cptr
    val symb  = GtkBasis.symb
    val repr  = GObject.repr

    fun inherit w con = Editable.inherit () con
    fun makeEnt ptr = Editable.inherit () (fn() => ptr)

    val new_: unit -> cptr
        = app1(symb"mgtk_gtk_entry_new")
    val new: unit -> base t
        = fn dummy => makeEnt(new_ ()) 

    val get_text_: cptr -> string
        = app1(symb"mgtk_gtk_entry_get_text")
    val get_text: 'a t -> string
        = fn entry => get_text_ (repr entry)

    local open Signal infix --> in
    fun activate_sig f = 
        signal "activate" false (void --> return_void) f
    end
end

signature Box =
sig
    type 'a box_t
    type 'a t = 'a box_t Container.t

    val inherit : 'a -> GObject.constructor -> 'a t
    val pack_start: 'a t -> 'b Widget.t -> unit
end

structure Box :> Box =
struct
    type 'a box_t = unit
    type 'a t = 'a box_t Container.t

    open Dynlib
    type cptr = GObject.cptr
    val symb  = GtkBasis.symb
    val repr  = GObject.repr

    fun inherit w con = Container.inherit () con
    val pack_start_: cptr -> cptr -> unit
        = app2(symb"mgtk_gtk_box_pack_start")
    val pack_start: 'a t -> 'b Widget.t -> unit
        = fn box => fn widget => pack_start_ (repr box) (repr widget)
end

signature VBox =
sig

    type 'a vbox_t
    type 'a t = 'a vbox_t Box.t
    type base

    val new: unit -> base t
end


structure VBox :> VBox =
struct
    type 'a vbox_t = unit
    type 'a t = 'a vbox_t Box.t
    type base = unit

    open Dynlib
    type cptr = GObject.cptr
    val symb  = GtkBasis.symb
    val repr  = GObject.repr

    fun inherit w con = Box.inherit () con
    fun make ptr = Box.inherit () (fn() => ptr)

    val new_: bool -> int -> cptr
        = app2(symb"mgtk_gtk_vbox_new")
    val new: unit -> base t
        = fn dummy => make(new_ false 10) 
end

signature TextIter =
sig
    type t
    val copy : t -> t
end

structure TextIter :> TextIter =
struct
    prim_type t
    open Dynlib
    val symb  = GtkBasis.symb

    val copy : t -> t 
        = app1(symb"mgtk_gtk_text_iter_copy")
end

(*signature TextTag =
sig

end

structure TextTag :> TextTag =
struct

end

signature TextTagTable =
sig

end

structure TextTagTable :> TextTagTable =
struct

end
*)

signature TextBuffer =
sig
    type base
    type 'a text_buffer_t
    type 'a t = 'a text_buffer_t GObject.t

    (* val new : TextTagTable.t -> base t *)
    val insert : 'a t -> TextIter.t -> string -> unit
    val get_start_iter : 'a t -> TextIter.t
    val get_end_iter   : 'a t -> TextIter.t

    val inherit        : 'a -> GObject.constructor -> 'a t
end

structure TextBuffer :> TextBuffer =
struct
    type base = unit
    type 'a text_buffer_t = unit
    type 'a t = 'a text_buffer_t GObject.t

    open Dynlib
    val symb  = GtkBasis.symb
    type cptr = GObject.cptr
    val repr  = GObject.repr

    val insert_: cptr -> TextIter.t -> string -> unit
        = app3(symb"mgtk_gtk_text_buffer_insert")
    val insert: 'a t -> TextIter.t -> string -> unit
        = fn buffer => fn iter => fn string => insert_ (repr buffer) iter string

    val get_start_iter_: cptr -> TextIter.t 
        = app1(symb"mgtk_gtk_text_buffer_get_start_iter")
    val get_start_iter: 'a t -> TextIter.t
        = fn buffer => get_start_iter_ (repr buffer)

    val get_end_iter_: cptr -> TextIter.t 
        = app1(symb"mgtk_gtk_text_buffer_get_end_iter")
    val get_end_iter: 'a t -> TextIter.t
        = fn buffer => get_end_iter_ (repr buffer)

    fun inherit w con = GObject.inherit () con
end


signature TextView =
sig
    type 'a text_view_t
    type 'a t = 'a text_view_t Container.t
    type base

    val new : unit -> base t

    val get_buffer : 'a t -> base TextBuffer.t
end

structure TextView :> TextView =
struct

    type 'a text_view_t = unit
    type 'a t = 'a text_view_t Container.t
    type base = unit

    open Dynlib
    type cptr = GObject.cptr
    val symb  = GtkBasis.symb
    val repr  = GObject.repr

    fun inherit w con = Container.inherit () con
    fun makeView ptr = Container.inherit () (fn() => ptr)

    val new_: unit -> cptr
        = app1(symb"mgtk_gtk_text_view_new")
    val new: unit -> base t
        = fn dummy => makeView(new_ dummy)

    val get_buffer_ : cptr -> cptr 
        = app1(symb"mgtk_gtk_text_view_get_buffer")
    val get_buffer : 'a t -> base TextBuffer.t
        = fn buffer => TextBuffer.inherit () 
                                          (fn () => get_buffer_ (repr buffer))

end
