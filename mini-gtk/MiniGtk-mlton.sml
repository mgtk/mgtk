(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

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


    fun withPtr (OBJ ptr, f) = F.withValue(ptr, f)

    val object_ref = _import "g_object_ref" : cptr -> cptr;
    val object_unref = _import "g_object_ref" : cptr -> unit;
    
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

        type 'a getter_ = GValues * int -> 'a
        type 'a getter = GValues -> int -> 'a
        val getBool = _import "mgtk_get_pos_bool" : bool getter_;
        val getBool = curry getBool
        val getInt  = _import "mgtk_get_pos_int"  : int getter_;
        val getInt  = curry getInt 
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
        let val id    = register wrap
            val csign = CString.fromString sign
        in  GO.withPtr(wid, fn wid => signal_connect (wid, csign, id, after))
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

    structure GO = GObject
    type cptr = GO.cptr

    fun inherit w con = GObject.inherit () con

    (* HACK ALERT:  This is an incorrect way of using withPtr, 
                    but it should be safe in this case
    *)
    fun toWidget obj = 
        GObject.inherit () (fn () => GO.withPtr(obj, fn obj => obj))

    val destroy_ = _import "gtk_widget_destroy" : cptr -> unit;
    val destroy: 'a t -> unit
        = fn widget => GO.withPtr(widget, destroy_)

    val show_ = _import "gtk_widget_show" : cptr -> unit;
    val show: 'a t -> unit
        = fn widget => GO.withPtr(widget, show_)

    val show_all_ = _import "gtk_widget_show_all" : cptr -> unit;
    val show_all: 'a t -> unit
        = fn widget => GO.withPtr(widget, show_all_)

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
    structure GO = GObject
    type cptr = GO.cptr

    fun inherit w con = Widget.inherit () con

    val set_border_width_ = _import "gtk_container_set_border_width" 
                          : cptr * int -> unit;
    val set_border_width: 'a t -> int -> unit
        = fn container => fn border_width => 
          GO.withPtr(container, fn container => 
                                   set_border_width_ (container, border_width))

    val add_ = _import "gtk_container_add" : cptr * cptr -> unit;
    val add: 'a t -> 'b Widget.t -> unit
        = fn container => fn widget => 
                         GO.withPtr(container, fn container =>
                         GO.withPtr(widget, fn widget =>    
                                    add_ (container, widget)))

    val remove_ = _import "gtk_container_remove" : cptr * cptr -> unit;
    val remove: 'a t -> 'b Widget.t -> unit
        = fn container => fn widget => 
                         GO.withPtr(container, fn container =>
                         GO.withPtr(widget, fn widget =>    
                                    remove_ (container, widget)))
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
end

structure Button :> Button =
struct
    type 'a button_t = unit
    type 'a t = 'a button_t Container.t
    type base = unit

    structure GO = GObject
    type cptr = GO.cptr

    fun inherit w con = Container.inherit () con
    fun makeBut ptr = Container.inherit () (fn() => ptr)

    val new_ = _import "gtk_button_new" : unit -> cptr;
    val new: unit -> base t
        = fn dummy => makeBut(new_ dummy)

    val new_with_label_ = _import "gtk_button_new_with_label" 
                        : CString.cstring -> cptr;
    val new_with_label: string -> base t
        = fn label => makeBut(new_with_label_ (CString.fromString label))

    local open Signal infix --> in
    fun clicked_sig f = 
        signal "clicked" false (void --> return_void) f
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

    structure GO = GObject
    type cptr = GO.cptr

    fun inherit w con = Container.inherit () con
    fun makeWin ptr = Container.inherit () (fn() => ptr)

    val new_ = _import "gtk_window_new": int -> cptr;
    val new: unit -> base t
        = fn dummy => makeWin(new_ 0) (* FIXME: HACK ALERT!!!!!*)

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

    structure GO = GObject
    type cptr = GO.cptr

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

    structure GO = GObject
    type cptr = GO.cptr

    fun inherit w con = Editable.inherit () con
    fun makeEnt ptr = Editable.inherit () (fn() => ptr)

    val new_ = _import "gtk_entry_new": unit -> cptr;
    val new: unit -> base t
        = fn dummy => makeEnt(new_ ()) 

    val get_text_ = _import "gtk_entry_get_text" : cptr -> CString.t;
    val get_text: 'a t -> string
        = fn entry => 
          GO.withPtr(entry, fn entry =>
                     let val t = get_text_ entry (* Must not be free'ed *)
                     in  CString.toString t 
                     end)

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

    structure GO = GObject
    type cptr = GO.cptr

    fun inherit w con = Container.inherit () con
    val pack_start_= _import "gtk_box_pack_start_defaults" : cptr * cptr -> unit;
    val pack_start: 'a t -> 'b Widget.t -> unit
        = fn box => fn widget => 
                    GO.withPtr(box, fn box =>
                    GO.withPtr(widget, fn widget =>    
                               pack_start_ (box, widget)))
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

    structure GO = GObject
    type cptr = GO.cptr

    fun inherit w con = Box.inherit () con
    fun make ptr = Box.inherit () (fn() => ptr)

    val new_ = _import "gtk_vbox_new": int * int -> cptr;
    val new: unit -> base t
        = fn dummy => make(new_(1, 10)) 
end
