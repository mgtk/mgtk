structure Gtk  = struct 
(*  24*)structure CString :> 
(*  24*)  sig
(*  24*)    type cstring
(*  24*)    val fromString : string -> cstring
(*  24*)
(*  24*)    type t
(*  24*)    val toString : t -> string
(*  24*)
(*  24*)    val free : t -> unit
(*  24*)  end = struct
(*  24*)    type cstring = string 
(*  24*)    fun fromString s = s ^ "\000"
(*  24*)
(*  24*)    type t = MLton.Pointer.t
(*  24*)    val sub = _import "mgtk_stringsub" : t * int -> char;
(*  24*)
(*  24*)    fun toVector t =
(*  24*)        let fun size i = if sub(t, i) = #"\000" then i
(*  24*)                         else size(i+1)
(*  24*)        in  CharVector.tabulate(size 0, fn i => sub(t, i))
(*  24*)        end
(*  24*)
(*  24*)    val toString = toVector
(*  24*)
(*  24*)    val free = _import "free" : t -> unit;
(*  24*)end
(*  24*)
(*  24*)structure GtkBasis :> 
(*  24*)  sig
(*  24*)    val init : string list -> unit
(*  24*)    val main : unit -> unit
(*  24*)    val main_quit : unit -> unit
(*  24*)  end = struct
(*  24*)    structure AS = ArraySlice
(*  24*)    (* Basic GTK stuff *)
(*  24*)    val gtk_init_ = _import "mgtk_init" 
(*  24*)                  : CString.cstring array * int -> unit;
(*  24*)    fun init args = 
(*  24*)	let val args =
(*  24*)	        case args of
(*  24*)		    [] => (print "mGTK Warning: Gtk.init called with empty list\n";
(*  24*)			   [CommandLine.name()])
(*  24*)		  | _  => args
(*  24*)            val argv = Array.fromList(List.map CString.fromString args)
(*  24*)        in  gtk_init_(argv, Array.length argv)
(*  24*)	end
(*  24*)
(*  24*)    val main      = _import "gtk_main" : unit -> unit;
(*  24*)    val main_quit = _import "gtk_main_quit" : unit -> unit; 
(*  24*)end
(*  24*)
(*  24*)structure GObject :> 
(*  24*)  sig
(*  24*)    type cptr
(*  24*)    type base
(*  24*)    type 'a t
(*  24*)    type constructor = unit -> cptr
(*  24*)
(*  24*)    val null : cptr
(*  24*)
(*  24*)    val withPtr  : 'a t * (cptr -> 'b) -> 'b
(*  24*)    val withOpt  : 'a t option * (cptr -> 'b) -> 'b
(*  24*)    val inherit  : 'a -> constructor -> 'a t
(*  24*)    val toObject : 'a t -> base t
(*  24*)  end = struct
(*  24*)    structure F = MLton.Finalizable
(*  24*)
(*  24*)    type cptr = MLton.Pointer.t
(*  24*)    type base = unit
(*  24*)
(*  24*)    (* A litle type cleverness *)
(*  24*)    datatype 'a t = OBJ of cptr F.t
(*  24*)    type constructor = unit -> cptr
(*  24*)
(*  24*)    val null = MLton.Pointer.null
(*  24*)
(*  24*)    fun withPtr (OBJ ptr, f) = F.withValue(ptr, f)
(*  24*)    fun withOpt (SOME(OBJ ptr), f) = F.withValue(ptr, f)
(*  24*)      | withOpt (NONE, f)          = F.withValue(F.new null, f)
(*  24*)
(*  24*)    val object_ref = _import "g_object_ref" : cptr -> cptr;
(*  24*)    val object_unref = _import "g_object_unref" : cptr -> unit;
(*  24*)    
(*  24*)    fun inherit _ con = let val ptr = object_ref(con())
(*  24*)                            val obj = F.new ptr
(*  24*)                        in  F.addFinalizer(obj, object_unref)
(*  24*)                          ; OBJ obj
(*  24*)                        end
(*  24*)
(*  24*)    fun toObject (OBJ ptr) = OBJ ptr
(*  24*)end
(*  24*)
(*  24*)structure Signal :> 
(*  24*)  sig
(*  24*)    type state
(*  24*)    type 'a t = 'a GObject.t
(*  24*)
(*  24*)
(*  24*)    type ('a, 'b) trans   = 'a * state -> 'b * state
(*  24*)    type ('a, 'rest) read = ('a -> 'rest, 'rest) trans
(*  24*)    type 'a return        = ('a, unit) trans
(*  24*)
(*  24*)    val bool   : (bool,   'rest) read
(*  24*)    val int    : (int,    'rest) read
(*  24*)    val char   : (char,   'rest) read
(*  24*)    val real   : (real,   'rest) read
(*  24*)    val unit   : (unit,   'rest) read
(*  24*)
(*  24*)    val void : (unit, 'rest) read
(*  24*)
(*  24*)    val return_bool   : bool   return
(*  24*)    val return_int    : int    return
(*  24*)    val return_char   : char   return
(*  24*)    val return_real   : real   return
(*  24*)    val return_unit   : unit   return
(*  24*)
(*  24*)    val return_void : unit return
(*  24*)
(*  24*)    val --> : ('a, 'b) read * ('b, 'c) trans -> ('a -> 'b, 'c) trans 
(*  24*)
(*  24*)    type 'a signal
(*  24*)    type signal_id
(*  24*)    val signal  : string -> bool -> ('b -> 'c) return -> ('b -> 'c) ->
(*  24*)                                                  'a t signal
(*  24*)    val connect : 'a t -> 'a t signal -> signal_id
(*  24*)  end = struct
(*  24*)
(*  24*)    type 'a t = 'a GObject.t
(*  24*)    local
(*  24*)        structure GO = GObject
(*  24*)        structure CT = Callbacktable
(*  24*)
(*  24*)        type GValues = MLton.Pointer.t
(*  24*)        type GValue = MLton.Pointer.t
(*  24*)
(*  24*)        type callback_data = GValue * GValues * int
(*  24*)	type callback = callback_data -> unit
(*  24*)	type callback_id  = int
(*  24*)
(*  24*)	val callbackTable : callback CT.t = CT.new 401
(*  24*)                      
(*  24*)	val add     = CT.insert callbackTable
(*  24*)	val peek    = CT.peek callbackTable
(*  24*)        val destroy = CT.remove callbackTable
(*  24*)
(*  24*)	local
(*  24*)	    val intern = ref 0;
(*  24*)	in
(*  24*)	    val localId = fn f => f (!intern before intern := !intern + 1)
(*  24*)	end
(*  24*)
(*  24*)	val dispatch : callback_id * GValue * GValues * int -> unit =
(*  24*)            fn (id, value, values, size) =>
(*  24*)	    case peek id of
(*  24*)		SOME f => f (value, values, size)   
(*  24*)              (* FIXME: we need a handle here, but what 
(*  24*)                        should it do 
(*  24*)               *)
(*  24*)	      | NONE   => (print ("mgtk: Unknown callback function (id: "^
(*  24*)				     Int.toString id^")\n")
(*  24*)                          ; TextIO.flushOut TextIO.stdOut)
(*  24*)
(*  24*)	val _ = _export "mgtk_callback_dispatch_smlside" 
(*  24*)                        : callback_id * GValue * GValues * int -> unit;
(*  24*)                dispatch
(*  24*)        val _ = _export "mgtk_callback_destroy_smlside"
(*  24*)                         : callback_id -> unit; 
(*  24*)                destroy
(*  24*)		
(*  24*)
(*  24*)	fun register f = localId(fn id => (add (id, f); id))
(*  24*)	val signal_connect = _import "mgtk_signal_connect"
(*  24*)                           : GO.cptr * CString.cstring * int * bool -> int;
(*  24*)
(*  24*)
(*  24*)
(*  24*)        fun curry f x y = f(x,y)
(*  24*)
(*  24*)        (* UNSAFE: no error checking in the set and get functions! *)
(*  24*)        type 'a setter_ = GValue * 'a -> unit
(*  24*)        type 'a setter = GValue -> 'a -> unit
(*  24*)        val setBool = _import "g_value_set_boolean" : bool setter_;
(*  24*)        val setBool = curry setBool
(*  24*)        val setInt  = _import "g_value_set_long" : int setter_;  
(*  24*)        val setInt  = curry setInt
(*  24*)	val setChar = _import "g_value_set_char" : char setter_;
(*  24*)        val setChar = curry setChar
(*  24*)	val setReal = _import "g_value_set_double" : real setter_;
(*  24*)	val setReal = curry setReal
(*  24*)
(*  24*)        type 'a getter_ = GValues * int -> 'a
(*  24*)        type 'a getter = GValues -> int -> 'a
(*  24*)        val getBool = _import "mgtk_get_pos_bool" : bool getter_;
(*  24*)        val getBool = curry getBool
(*  24*)        val getInt  = _import "mgtk_get_pos_int"  : int getter_;
(*  24*)        val getInt  = curry getInt 
(*  24*)	val getChar = _import "mgtk_get_pos_char" : char getter_;
(*  24*)	val getChar = curry getChar
(*  24*)	val getReal = _import "mgtk_get_pos_real" : real getter_;
(*  24*)        val getReal = curry getReal
(*  24*)
(*  24*)(*
(*  24*)        val getLong   : int getter    = app2(symb "mgtk_get_pos_long")
(*  24*)        val getChar   : char getter   = app2(symb "mgtk_get_pos_char")
(*  24*)        val getString : string getter = app2(symb "mgtk_get_pos_string")
(*  24*)*)
(*  24*)    in
(*  24*)    datatype state = S of GValue * GValues * int * int
(*  24*)    type ('a, 'b) trans   = 'a * state -> 'b * state
(*  24*)    type ('a, 'rest) read = ('a -> 'rest, 'rest) trans
(*  24*)    type 'a return        = ('a, unit) trans
(*  24*)
(*  24*)    fun state f ret arg max = (f, S(ret, arg, max, 0+1))
(*  24*)    (* NOTE: the +1 is for the object connected to *)
(*  24*)
(*  24*)    fun wrap conv f (ret, arg, max) = ignore(conv(state f ret arg max)) 
(*  24*)
(*  24*)    fun getter get (f, S(ret, arg, max, next)) = 
(*  24*)        if next < max  (* FIXME: it should be < but that gives problems with
(*  24*)                                  return_unit.  Currently unsafe! *)
(*  24*)        then (f (get arg next), S(ret, arg, max, next+1))
(*  24*)        else raise Subscript
(*  24*)
(*  24*)    fun drop (f, S(ret, arg, max, next)) = 
(*  24*)        if next < max then (f, S(ret, arg, max, next+1))
(*  24*)        else raise Subscript
(*  24*)
(*  24*)    fun setter set (x, dummy as S(ret, arg, max, next)) =
(*  24*)        if next = max then (set ret x; ((),dummy))
(*  24*)        else raise Subscript
(*  24*)
(*  24*)    fun int x        = getter getInt x
(*  24*)    fun return_int x = setter setInt x
(*  24*)
(*  24*)    fun bool x        = getter getBool x
(*  24*)    fun return_bool x = setter setBool x
(*  24*)
(*  24*)    fun char x        = getter getChar x
(*  24*)    fun return_char x = setter setChar x
(*  24*)
(*  24*)    fun real x        = getter getReal x
(*  24*)    fun return_real x = setter setReal x
(*  24*)
(*  24*)    
(*  24*)    (* FIXME: convince Ken that this correct *)
(*  24*)    fun void (f, state) = (f(), state)
(*  24*)    fun return_void (f, S(ret, arg, max, next)) = (f, S(ret, arg,max+1,next))
(*  24*)
(*  24*)    fun unit x        = getter (fn _ => fn _ => ()) x
(*  24*)    fun return_unit x =  x
(*  24*)               
(*  24*)    infix --> 
(*  24*)
(*  24*)    fun (x --> y) arg = y (x arg)                        
(*  24*)
(*  24*)    datatype 'a signal = Sig of string * bool * callback
(*  24*)
(*  24*)    fun signal sign after conv f = Sig(sign, after, wrap conv f)
(*  24*)
(*  24*)    type signal_id = int
(*  24*)
(*  24*)    fun connect wid (Sig(sign, after, wrap)) =
(*  24*)        let val id    = register wrap
(*  24*)            val csign = CString.fromString sign
(*  24*)        in  GO.withPtr(wid, fn wid => signal_connect (wid, csign, id, after))
(*  24*)        end
(*  24*)
(*  24*)    end	
(*  24*)end
(*  24*)
(*  24*)structure Flags :> 
(*  24*)  sig
(*  24*)    val setGeneral : int -> int list -> int list -> int 
(*  24*)    val set : int list -> int
(*  24*)    val get : int -> int list
(*  24*)    val isSet : int list -> int -> int list
(*  24*)    val areTheseSet : int list -> int -> bool
(*  24*)  end = struct
(*  24*)    (* convert a list of flags to a word *)
(*  24*)    infix orb andb
(*  24*)    val notb = Word.notb 
(*  24*)    val op orb = Word.orb 
(*  24*)    val op andb = Word.andb
(*  24*)    val W = Word.fromInt
(*  24*)    fun set(f,res) = W f orb res
(*  24*)    fun unset(f, res) = notb(W f) andb res
(*  24*)
(*  24*)    fun setGeneral init pos neg =
(*  24*)	let val res = List.foldl set (W init) pos
(*  24*)	    val res = List.foldl unset res neg
(*  24*)	in  Word.toInt res
(*  24*)	end
(*  24*)
(*  24*)    (* we only need to set flags from the no flag *)
(*  24*)    val set = fn pos =>
(*  24*)	let val res = List.foldl set 0w0 pos
(*  24*)	in  Word.toInt res
(*  24*)	end
(*  24*)
(*  24*)    (* Checks which flags from possible is set in flag *)
(*  24*)    fun isSet possible flag =
(*  24*)	let val f = W flag
(*  24*)	    fun isIn pos = (f andb (W pos)) <> 0w0
(*  24*)	in  List.filter isIn possible
(*  24*)	end
(*  24*)
(*  24*)    fun get f =
(*  24*)	let val flag = W f
(*  24*)	    fun loop p acc = 
(*  24*)		let val acc = if (flag andb p) <> 0w0 
(*  24*)			      then Word.toInt p :: acc
(*  24*)			      else acc
(*  24*)		    val next = Word.<<(p, 0w1)
(*  24*)		in  if next = 0w0 then acc
(*  24*)		    else loop next acc
(*  24*)		end
(*  24*)	in  loop 0w1 []
(*  24*)	end
(*  24*)
(*  24*)    fun areTheseSet flags flag = ((W(set flags)) andb (W flag)) <> 0w0
(*  24*)end
(*  24*)type cptr = GObject.cptr
(*  24*)type requisition = GObject.cptr
(*  24*)type ctree_node = GObject.cptr
(*  24*)type icon_set = GObject.cptr
(*  24*)type icon_source = GObject.cptr
(*  24*)type selection_data = GObject.cptr
(*  24*)type text_attributes = GObject.cptr
(*  24*)type textiter = GObject.cptr
(*  24*)type treeiter = GObject.cptr
(*  24*)type accel_flags = int
(*  24*)val get_accel_flags_ : int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_accel_flags"
(*  24*)	      : int ref * int ref * int ref -> unit;
(*  24*)val (ACCEL_VISIBLE, ACCEL_LOCKED, ACCEL_MASK)
(*  24*)    = let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
(*  24*)      in get_accel_flags_ (x0, x1, x2)
(*  24*)       ; (!x0, !x1, !x2)
(*  24*)      end
(*  24*)type celltype = int
(*  24*)val get_celltype_
(*  24*)  : int ref * int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_celltype"
(*  24*)	      : int ref * int ref * int ref * int ref * int ref
(*  24*)		-> unit;
(*  24*)val (CELL_EMPTY, CELL_TEXT, CELL_PIXMAP, CELL_PIXTEXT, CELL_WIDGET)
(*  24*)    = let val (x0, x1, x2, x3, x4)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_celltype_ (x0, x1, x2, x3, x4)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4)
(*  24*)      end
(*  24*)type debug_flag = int
(*  24*)val get_debug_flag_
(*  24*)  : int ref * int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_debug_flag"
(*  24*)	      : int ref * int ref * int ref * int ref * int ref
(*  24*)		-> unit;
(*  24*)val (DEBUG_MISC, DEBUG_PLUGSOCKET, DEBUG_TEXT, DEBUG_TREE, 
(*  24*)     DEBUG_UPDATES)
(*  24*)    = let val (x0, x1, x2, x3, x4)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_debug_flag_ (x0, x1, x2, x3, x4)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4)
(*  24*)      end
(*  24*)type responsetype = int
(*  24*)val get_responsetype_
(*  24*)  : int ref * int ref * int ref * int ref * int ref * int ref 
(*  24*)  * int ref * int ref * int ref * int ref * int ref
(*  24*)    -> unit
(*  24*)    = _import "mgtk_get_gtk_responsetype"
(*  24*)	      : int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref
(*  24*)		-> unit;
(*  24*)val (RESPONSE_NONE, RESPONSE_REJECT, RESPONSE_ACCEPT, 
(*  24*)     RESPONSE_DELETE_EVENT, RESPONSE_OK, RESPONSE_CANCEL, 
(*  24*)     RESPONSE_CLOSE, RESPONSE_YES, RESPONSE_NO, RESPONSE_APPLY, 
(*  24*)     RESPONSE_HELP)
(*  24*)    = let val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
(*  24*)		 ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_responsetype_
(*  24*)	   (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6, !x7, !x8, !x9, !x10)
(*  24*)      end
(*  24*)type dest_defaults = int
(*  24*)val get_dest_defaults_ : int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_dest_defaults"
(*  24*)	      : int ref * int ref * int ref * int ref -> unit;
(*  24*)val (DEST_DEFAULT_MOTION, DEST_DEFAULT_HIGHLIGHT, DEST_DEFAULT_DROP, 
(*  24*)     DEST_DEFAULT_ALL)
(*  24*)    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_dest_defaults_ (x0, x1, x2, x3)
(*  24*)       ; (!x0, !x1, !x2, !x3)
(*  24*)      end
(*  24*)type target_flags = int
(*  24*)val get_target_flags_ : int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_target_flags" : int ref * int ref -> unit;
(*  24*)val (TARGET_SAME_APP, TARGET_SAME_WIDGET)
(*  24*)    = let val (x0, x1) = (ref 0, ref 0) in get_target_flags_ (x0, x1)
(*  24*)					 ; (!x0, !x1)
(*  24*)					end
(*  24*)type anchortype = int
(*  24*)val get_anchortype_
(*  24*)  : int ref * int ref * int ref * int ref * int ref * int ref 
(*  24*)  * int ref * int ref * int ref * int ref * int ref * int ref 
(*  24*)  * int ref * int ref * int ref * int ref * int ref
(*  24*)    -> unit
(*  24*)    = _import "mgtk_get_gtk_anchortype"
(*  24*)	      : int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref * int ref
(*  24*)		-> unit;
(*  24*)val (ANCHOR_CENTER, ANCHOR_NORTH, ANCHOR_NORTH_WEST, 
(*  24*)     ANCHOR_NORTH_EAST, ANCHOR_SOUTH, ANCHOR_SOUTH_WEST, 
(*  24*)     ANCHOR_SOUTH_EAST, ANCHOR_WEST, ANCHOR_EAST, ANCHOR_N, ANCHOR_NW, 
(*  24*)     ANCHOR_NE, ANCHOR_S, ANCHOR_SW, ANCHOR_SE, ANCHOR_W, ANCHOR_E)
(*  24*)    = let val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, 
(*  24*)	       x13, x14, x15, x16)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
(*  24*)		 ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
(*  24*)		 ref 0, ref 0, ref 0)
(*  24*)      in get_anchortype_ (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, 
(*  24*)			  x11, x12, x13, x14, x15, x16)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6, !x7, !x8, !x9, !x10, 
(*  24*)	  !x11, !x12, !x13, !x14, !x15, !x16)
(*  24*)      end
(*  24*)type arrowtype = int
(*  24*)val get_arrowtype_ : int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_arrowtype"
(*  24*)	      : int ref * int ref * int ref * int ref -> unit;
(*  24*)val (ARROW_UP, ARROW_DOWN, ARROW_LEFT, ARROW_RIGHT)
(*  24*)    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_arrowtype_ (x0, x1, x2, x3)
(*  24*)       ; (!x0, !x1, !x2, !x3)
(*  24*)      end
(*  24*)type attach_options = int
(*  24*)val get_attach_options_ : int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_attach_options"
(*  24*)	      : int ref * int ref * int ref -> unit;
(*  24*)val (EXPAND, SHRINK, FILL)
(*  24*)    = let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
(*  24*)      in get_attach_options_ (x0, x1, x2)
(*  24*)       ; (!x0, !x1, !x2)
(*  24*)      end
(*  24*)type curvetype = int
(*  24*)val get_curvetype_ : int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_curvetype"
(*  24*)	      : int ref * int ref * int ref -> unit;
(*  24*)val (CURVE_TYPE_LINEAR, CURVE_TYPE_SPLINE, CURVE_TYPE_FREE)
(*  24*)    = let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
(*  24*)      in get_curvetype_ (x0, x1, x2)
(*  24*)       ; (!x0, !x1, !x2)
(*  24*)      end
(*  24*)type deletetype = int
(*  24*)val get_deletetype_ : int ref * int ref * int ref * int ref * int ref 
(*  24*)		    * int ref * int ref * int ref
(*  24*)		      -> unit
(*  24*)    = _import "mgtk_get_gtk_deletetype" : int ref * int ref * int ref 
(*  24*)					* int ref * int ref * int ref 
(*  24*)					* int ref * int ref
(*  24*)					  -> unit;
(*  24*)val (DELETE_CHARS, DELETE_WORD_ENDS, DELETE_WORDS, 
(*  24*)     DELETE_DISPLAY_LINES, DELETE_DISPLAY_LINE_ENDS, 
(*  24*)     DELETE_PARAGRAPH_ENDS, DELETE_PARAGRAPHS, DELETE_WHITESPACE)
(*  24*)    = let val (x0, x1, x2, x3, x4, x5, x6, x7)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
(*  24*)		 ref 0)
(*  24*)      in get_deletetype_ (x0, x1, x2, x3, x4, x5, x6, x7)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6, !x7)
(*  24*)      end
(*  24*)type directiontype = int
(*  24*)val get_directiontype_
(*  24*)  : int ref * int ref * int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_directiontype" : int ref * int ref 
(*  24*)					   * int ref * int ref 
(*  24*)					   * int ref * int ref
(*  24*)					     -> unit;
(*  24*)val (DIR_TAB_FORWARD, DIR_TAB_BACKWARD, DIR_UP, DIR_DOWN, DIR_LEFT, 
(*  24*)     DIR_RIGHT)
(*  24*)    = let val (x0, x1, x2, x3, x4, x5)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_directiontype_ (x0, x1, x2, x3, x4, x5)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4, !x5)
(*  24*)      end
(*  24*)type expander_style = int
(*  24*)val get_expander_style_ : int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_expander_style"
(*  24*)	      : int ref * int ref * int ref * int ref -> unit;
(*  24*)val (EXPANDER_COLLAPSED, EXPANDER_SEMI_COLLAPSED, 
(*  24*)     EXPANDER_SEMI_EXPANDED, EXPANDER_EXPANDED)
(*  24*)    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_expander_style_ (x0, x1, x2, x3)
(*  24*)       ; (!x0, !x1, !x2, !x3)
(*  24*)      end
(*  24*)type icon_size = int
(*  24*)val get_icon_size_ : int ref * int ref * int ref * int ref * int ref 
(*  24*)		   * int ref * int ref
(*  24*)		     -> unit
(*  24*)    = _import "mgtk_get_gtk_icon_size" : int ref * int ref * int ref 
(*  24*)				       * int ref * int ref * int ref 
(*  24*)				       * int ref
(*  24*)					 -> unit;
(*  24*)val (ICON_SIZE_INVALID, ICON_SIZE_MENU, ICON_SIZE_SMALL_TOOLBAR, 
(*  24*)     ICON_SIZE_LARGE_TOOLBAR, ICON_SIZE_BUTTON, ICON_SIZE_DND, 
(*  24*)     ICON_SIZE_DIALOG)
(*  24*)    = let val (x0, x1, x2, x3, x4, x5, x6)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_icon_size_ (x0, x1, x2, x3, x4, x5, x6)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6)
(*  24*)      end
(*  24*)type sidetype = int
(*  24*)val get_sidetype_ : int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_sidetype"
(*  24*)	      : int ref * int ref * int ref * int ref -> unit;
(*  24*)val (SIDE_TOP, SIDE_BOTTOM, SIDE_LEFT, SIDE_RIGHT)
(*  24*)    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_sidetype_ (x0, x1, x2, x3)
(*  24*)       ; (!x0, !x1, !x2, !x3)
(*  24*)      end
(*  24*)type text_direction = int
(*  24*)val get_text_direction_ : int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_text_direction"
(*  24*)	      : int ref * int ref * int ref -> unit;
(*  24*)val (TEXT_DIR_NONE, TEXT_DIR_LTR, TEXT_DIR_RTL)
(*  24*)    = let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
(*  24*)      in get_text_direction_ (x0, x1, x2)
(*  24*)       ; (!x0, !x1, !x2)
(*  24*)      end
(*  24*)type justification = int
(*  24*)val get_justification_ : int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_justification"
(*  24*)	      : int ref * int ref * int ref * int ref -> unit;
(*  24*)val (JUSTIFY_LEFT, JUSTIFY_RIGHT, JUSTIFY_CENTER, JUSTIFY_FILL)
(*  24*)    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_justification_ (x0, x1, x2, x3)
(*  24*)       ; (!x0, !x1, !x2, !x3)
(*  24*)      end
(*  24*)type matchtype = int
(*  24*)val get_matchtype_
(*  24*)  : int ref * int ref * int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_matchtype" : int ref * int ref * int ref 
(*  24*)				       * int ref * int ref * int ref
(*  24*)					 -> unit;
(*  24*)val (MATCH_ALL, MATCH_ALL_TAIL, MATCH_HEAD, MATCH_TAIL, MATCH_EXACT, 
(*  24*)     MATCH_LAST)
(*  24*)    = let val (x0, x1, x2, x3, x4, x5)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_matchtype_ (x0, x1, x2, x3, x4, x5)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4, !x5)
(*  24*)      end
(*  24*)type metrictype = int
(*  24*)val get_metrictype_ : int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_metrictype"
(*  24*)	      : int ref * int ref * int ref -> unit;
(*  24*)val (PIXELS, INCHES, CENTIMETERS)
(*  24*)    = let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
(*  24*)      in get_metrictype_ (x0, x1, x2)
(*  24*)       ; (!x0, !x1, !x2)
(*  24*)      end
(*  24*)type movement_step = int
(*  24*)val get_movement_step_
(*  24*)  : int ref * int ref * int ref * int ref * int ref * int ref 
(*  24*)  * int ref * int ref * int ref
(*  24*)    -> unit
(*  24*)    = _import "mgtk_get_gtk_movement_step"
(*  24*)	      : int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref * int ref * int ref * int ref
(*  24*)		-> unit;
(*  24*)val (MOVEMENT_LOGICAL_POSITIONS, MOVEMENT_VISUAL_POSITIONS, 
(*  24*)     MOVEMENT_WORDS, MOVEMENT_DISPLAY_LINES, 
(*  24*)     MOVEMENT_DISPLAY_LINE_ENDS, MOVEMENT_PARAGRAPHS, 
(*  24*)     MOVEMENT_PARAGRAPH_ENDS, MOVEMENT_PAGES, MOVEMENT_BUFFER_ENDS)
(*  24*)    = let val (x0, x1, x2, x3, x4, x5, x6, x7, x8)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
(*  24*)		 ref 0, ref 0)
(*  24*)      in get_movement_step_ (x0, x1, x2, x3, x4, x5, x6, x7, x8)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6, !x7, !x8)
(*  24*)      end
(*  24*)type orientation = int
(*  24*)val get_orientation_ : int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_orientation" : int ref * int ref -> unit;
(*  24*)val (ORIENTATION_HORIZONTAL, ORIENTATION_VERTICAL)
(*  24*)    = let val (x0, x1) = (ref 0, ref 0) in get_orientation_ (x0, x1)
(*  24*)					 ; (!x0, !x1)
(*  24*)					end
(*  24*)type cornertype = int
(*  24*)val get_cornertype_ : int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_cornertype"
(*  24*)	      : int ref * int ref * int ref * int ref -> unit;
(*  24*)val (CORNER_TOP_LEFT, CORNER_BOTTOM_LEFT, CORNER_TOP_RIGHT, 
(*  24*)     CORNER_BOTTOM_RIGHT)
(*  24*)    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_cornertype_ (x0, x1, x2, x3)
(*  24*)       ; (!x0, !x1, !x2, !x3)
(*  24*)      end
(*  24*)type packtype = int
(*  24*)val get_packtype_ : int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_packtype" : int ref * int ref -> unit;
(*  24*)val (PACK_START, PACK_END) = let val (x0, x1) = (ref 0, ref 0)
(*  24*)			     in get_packtype_ (x0, x1)
(*  24*)			      ; (!x0, !x1)
(*  24*)			     end
(*  24*)type path_prioritytype = int
(*  24*)val get_path_prioritytype_
(*  24*)  : int ref * int ref * int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_path_prioritytype"
(*  24*)	      : int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref
(*  24*)		-> unit;
(*  24*)val (PATH_PRIO_LOWEST, PATH_PRIO_GTK, PATH_PRIO_APPLICATION, 
(*  24*)     PATH_PRIO_THEME, PATH_PRIO_RC, PATH_PRIO_HIGHEST)
(*  24*)    = let val (x0, x1, x2, x3, x4, x5)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_path_prioritytype_ (x0, x1, x2, x3, x4, x5)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4, !x5)
(*  24*)      end
(*  24*)type pathtype = int
(*  24*)val get_pathtype_ : int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_pathtype"
(*  24*)	      : int ref * int ref * int ref -> unit;
(*  24*)val (PATH_WIDGET, PATH_WIDGET_CLASS, PATH_CLASS)
(*  24*)    = let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
(*  24*)      in get_pathtype_ (x0, x1, x2)
(*  24*)       ; (!x0, !x1, !x2)
(*  24*)      end
(*  24*)type policytype = int
(*  24*)val get_policytype_ : int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_policytype"
(*  24*)	      : int ref * int ref * int ref -> unit;
(*  24*)val (POLICY_ALWAYS, POLICY_AUTOMATIC, POLICY_NEVER)
(*  24*)    = let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
(*  24*)      in get_policytype_ (x0, x1, x2)
(*  24*)       ; (!x0, !x1, !x2)
(*  24*)      end
(*  24*)type positiontype = int
(*  24*)val get_positiontype_ : int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_positiontype"
(*  24*)	      : int ref * int ref * int ref * int ref -> unit;
(*  24*)val (POS_LEFT, POS_RIGHT, POS_TOP, POS_BOTTOM)
(*  24*)    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_positiontype_ (x0, x1, x2, x3)
(*  24*)       ; (!x0, !x1, !x2, !x3)
(*  24*)      end
(*  24*)type previewtype = int
(*  24*)val get_previewtype_ : int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_previewtype" : int ref * int ref -> unit;
(*  24*)val (PREVIEW_COLOR, PREVIEW_GRAYSCALE)
(*  24*)    = let val (x0, x1) = (ref 0, ref 0) in get_previewtype_ (x0, x1)
(*  24*)					 ; (!x0, !x1)
(*  24*)					end
(*  24*)type relief_style = int
(*  24*)val get_relief_style_ : int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_relief_style"
(*  24*)	      : int ref * int ref * int ref -> unit;
(*  24*)val (RELIEF_NORMAL, RELIEF_HALF, RELIEF_NONE)
(*  24*)    = let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
(*  24*)      in get_relief_style_ (x0, x1, x2)
(*  24*)       ; (!x0, !x1, !x2)
(*  24*)      end
(*  24*)type resize_mode = int
(*  24*)val get_resize_mode_ : int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_resize_mode"
(*  24*)	      : int ref * int ref * int ref -> unit;
(*  24*)val (RESIZE_PARENT, RESIZE_QUEUE, RESIZE_IMMEDIATE)
(*  24*)    = let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
(*  24*)      in get_resize_mode_ (x0, x1, x2)
(*  24*)       ; (!x0, !x1, !x2)
(*  24*)      end
(*  24*)type scrolltype = int
(*  24*)val get_scrolltype_
(*  24*)  : int ref * int ref * int ref * int ref * int ref * int ref 
(*  24*)  * int ref * int ref * int ref * int ref * int ref * int ref 
(*  24*)  * int ref * int ref * int ref * int ref
(*  24*)    -> unit
(*  24*)    = _import "mgtk_get_gtk_scrolltype"
(*  24*)	      : int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref
(*  24*)		-> unit;
(*  24*)val (SCROLL_NONE, SCROLL_JUMP, SCROLL_STEP_BACKWARD, 
(*  24*)     SCROLL_STEP_FORWARD, SCROLL_PAGE_BACKWARD, SCROLL_PAGE_FORWARD, 
(*  24*)     SCROLL_STEP_UP, SCROLL_STEP_DOWN, SCROLL_PAGE_UP, 
(*  24*)     SCROLL_PAGE_DOWN, SCROLL_STEP_LEFT, SCROLL_STEP_RIGHT, 
(*  24*)     SCROLL_PAGE_LEFT, SCROLL_PAGE_RIGHT, SCROLL_START, SCROLL_END)
(*  24*)    = let val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, 
(*  24*)	       x13, x14, x15)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
(*  24*)		 ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
(*  24*)		 ref 0, ref 0)
(*  24*)      in get_scrolltype_ (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, 
(*  24*)			  x11, x12, x13, x14, x15)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6, !x7, !x8, !x9, !x10, 
(*  24*)	  !x11, !x12, !x13, !x14, !x15)
(*  24*)      end
(*  24*)type selection_mode = int
(*  24*)val get_selection_mode_ : int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_selection_mode"
(*  24*)	      : int ref * int ref * int ref * int ref -> unit;
(*  24*)val (SELECTION_NONE, SELECTION_SINGLE, SELECTION_BROWSE, 
(*  24*)     SELECTION_MULTIPLE)
(*  24*)    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_selection_mode_ (x0, x1, x2, x3)
(*  24*)       ; (!x0, !x1, !x2, !x3)
(*  24*)      end
(*  24*)type shadowtype = int
(*  24*)val get_shadowtype_
(*  24*)  : int ref * int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_shadowtype"
(*  24*)	      : int ref * int ref * int ref * int ref * int ref
(*  24*)		-> unit;
(*  24*)val (SHADOW_NONE, SHADOW_IN, SHADOW_OUT, SHADOW_ETCHED_IN, 
(*  24*)     SHADOW_ETCHED_OUT)
(*  24*)    = let val (x0, x1, x2, x3, x4)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_shadowtype_ (x0, x1, x2, x3, x4)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4)
(*  24*)      end
(*  24*)type statetype = int
(*  24*)val get_statetype_
(*  24*)  : int ref * int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_statetype"
(*  24*)	      : int ref * int ref * int ref * int ref * int ref
(*  24*)		-> unit;
(*  24*)val (STATE_NORMAL, STATE_ACTIVE, STATE_PRELIGHT, STATE_SELECTED, 
(*  24*)     STATE_INSENSITIVE)
(*  24*)    = let val (x0, x1, x2, x3, x4)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_statetype_ (x0, x1, x2, x3, x4)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4)
(*  24*)      end
(*  24*)type submenu_direction = int
(*  24*)val get_submenu_direction_ : int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_submenu_direction"
(*  24*)	      : int ref * int ref -> unit;
(*  24*)val (DIRECTION_LEFT, DIRECTION_RIGHT)
(*  24*)    = let val (x0, x1) = (ref 0, ref 0)
(*  24*)      in get_submenu_direction_ (x0, x1)
(*  24*)       ; (!x0, !x1)
(*  24*)      end
(*  24*)type submenu_placement = int
(*  24*)val get_submenu_placement_ : int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_submenu_placement"
(*  24*)	      : int ref * int ref -> unit;
(*  24*)val (TOP_BOTTOM, LEFT_RIGHT) = let val (x0, x1) = (ref 0, ref 0)
(*  24*)			       in get_submenu_placement_ (x0, x1)
(*  24*)				; (!x0, !x1)
(*  24*)			       end
(*  24*)type updatetype = int
(*  24*)val get_updatetype_ : int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_updatetype"
(*  24*)	      : int ref * int ref * int ref -> unit;
(*  24*)val (UPDATE_CONTINUOUS, UPDATE_DISCONTINUOUS, UPDATE_DELAYED)
(*  24*)    = let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
(*  24*)      in get_updatetype_ (x0, x1, x2)
(*  24*)       ; (!x0, !x1, !x2)
(*  24*)      end
(*  24*)type visibility = int
(*  24*)val get_visibility_ : int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_visibility"
(*  24*)	      : int ref * int ref * int ref -> unit;
(*  24*)val (VISIBILITY_NONE, VISIBILITY_PARTIAL, VISIBILITY_FULL)
(*  24*)    = let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
(*  24*)      in get_visibility_ (x0, x1, x2)
(*  24*)       ; (!x0, !x1, !x2)
(*  24*)      end
(*  24*)type wrap_mode = int
(*  24*)val get_wrap_mode_ : int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_wrap_mode"
(*  24*)	      : int ref * int ref * int ref -> unit;
(*  24*)val (WRAP_NONE, WRAP_CHAR, WRAP_WORD)
(*  24*)    = let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
(*  24*)      in get_wrap_mode_ (x0, x1, x2)
(*  24*)       ; (!x0, !x1, !x2)
(*  24*)      end
(*  24*)type sorttype = int
(*  24*)val get_sorttype_ : int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_sorttype" : int ref * int ref -> unit;
(*  24*)val (SORT_ASCENDING, SORT_DESCENDING)
(*  24*)    = let val (x0, x1) = (ref 0, ref 0) in get_sorttype_ (x0, x1)
(*  24*)					 ; (!x0, !x1)
(*  24*)					end
(*  24*)type imagetype = int
(*  24*)val get_imagetype_ : int ref * int ref * int ref * int ref * int ref 
(*  24*)		   * int ref * int ref
(*  24*)		     -> unit
(*  24*)    = _import "mgtk_get_gtk_imagetype" : int ref * int ref * int ref 
(*  24*)				       * int ref * int ref * int ref 
(*  24*)				       * int ref
(*  24*)					 -> unit;
(*  24*)val (IMAGE_EMPTY, IMAGE_PIXMAP, IMAGE_IMAGE, IMAGE_PIXBUF, 
(*  24*)     IMAGE_STOCK, IMAGE_ICON_SET, IMAGE_ANIMATION)
(*  24*)    = let val (x0, x1, x2, x3, x4, x5, x6)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_imagetype_ (x0, x1, x2, x3, x4, x5, x6)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6)
(*  24*)      end
(*  24*)type messagetype = int
(*  24*)val get_messagetype_ : int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_messagetype"
(*  24*)	      : int ref * int ref * int ref * int ref -> unit;
(*  24*)val (MESSAGE_INFO, MESSAGE_WARNING, MESSAGE_QUESTION, MESSAGE_ERROR)
(*  24*)    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_messagetype_ (x0, x1, x2, x3)
(*  24*)       ; (!x0, !x1, !x2, !x3)
(*  24*)      end
(*  24*)type buttonstype = int
(*  24*)val get_buttonstype_
(*  24*)  : int ref * int ref * int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_buttonstype" : int ref * int ref * int ref 
(*  24*)					 * int ref * int ref * int ref
(*  24*)					   -> unit;
(*  24*)val (BUTTONS_NONE, BUTTONS_OK, BUTTONS_CLOSE, BUTTONS_CANCEL, 
(*  24*)     BUTTONS_YES_NO, BUTTONS_OK_CANCEL)
(*  24*)    = let val (x0, x1, x2, x3, x4, x5)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_buttonstype_ (x0, x1, x2, x3, x4, x5)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4, !x5)
(*  24*)      end
(*  24*)type arg_flags = int
(*  24*)val get_arg_flags_
(*  24*)  : int ref * int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_arg_flags"
(*  24*)	      : int ref * int ref * int ref * int ref * int ref
(*  24*)		-> unit;
(*  24*)val (ARG_READABLE, ARG_WRITABLE, ARG_CONSTRUCT, ARG_CONSTRUCT_ONLY, 
(*  24*)     ARG_CHILD_ARG)
(*  24*)    = let val (x0, x1, x2, x3, x4)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_arg_flags_ (x0, x1, x2, x3, x4)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4)
(*  24*)      end
(*  24*)type rc_flags = int
(*  24*)val get_rc_flags_ : int ref * int ref * int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_rc_flags"
(*  24*)	      : int ref * int ref * int ref * int ref -> unit;
(*  24*)val (RC_FG, RC_BG, RC_TEXT, RC_BASE)
(*  24*)    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_rc_flags_ (x0, x1, x2, x3)
(*  24*)       ; (!x0, !x1, !x2, !x3)
(*  24*)      end
(*  24*)type rc_tokentype = int
(*  24*)val get_rc_tokentype_
(*  24*)  : int ref * int ref * int ref * int ref * int ref * int ref 
(*  24*)  * int ref * int ref * int ref * int ref * int ref * int ref 
(*  24*)  * int ref * int ref * int ref * int ref * int ref * int ref 
(*  24*)  * int ref * int ref * int ref * int ref * int ref * int ref 
(*  24*)  * int ref * int ref * int ref * int ref * int ref * int ref 
(*  24*)  * int ref * int ref * int ref * int ref * int ref * int ref 
(*  24*)  * int ref * int ref
(*  24*)    -> unit
(*  24*)    = _import "mgtk_get_gtk_rc_tokentype"
(*  24*)	      : int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref * int ref * int ref
(*  24*)		-> unit;
(*  24*)val (RC_TOKEN_INVALID, RC_TOKEN_INCLUDE, RC_TOKEN_NORMAL, 
(*  24*)     RC_TOKEN_ACTIVE, RC_TOKEN_PRELIGHT, RC_TOKEN_SELECTED, 
(*  24*)     RC_TOKEN_INSENSITIVE, RC_TOKEN_FG, RC_TOKEN_BG, RC_TOKEN_TEXT, 
(*  24*)     RC_TOKEN_BASE, RC_TOKEN_XTHICKNESS, RC_TOKEN_YTHICKNESS, 
(*  24*)     RC_TOKEN_FONT, RC_TOKEN_FONTSET, RC_TOKEN_FONT_NAME, 
(*  24*)     RC_TOKEN_BG_PIXMAP, RC_TOKEN_PIXMAP_PATH, RC_TOKEN_STYLE, 
(*  24*)     RC_TOKEN_BINDING, RC_TOKEN_BIND, RC_TOKEN_WIDGET, 
(*  24*)     RC_TOKEN_WIDGET_CLASS, RC_TOKEN_CLASS, RC_TOKEN_LOWEST, 
(*  24*)     RC_TOKEN_GTK, RC_TOKEN_APPLICATION, RC_TOKEN_THEME, RC_TOKEN_RC, 
(*  24*)     RC_TOKEN_HIGHEST, RC_TOKEN_ENGINE, RC_TOKEN_MODULE_PATH, 
(*  24*)     RC_TOKEN_IM_MODULE_PATH, RC_TOKEN_IM_MODULE_FILE, RC_TOKEN_STOCK, 
(*  24*)     RC_TOKEN_LTR, RC_TOKEN_RTL, RC_TOKEN_LAST)
(*  24*)    = let val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, 
(*  24*)	       x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, 
(*  24*)	       x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, 
(*  24*)	       x35, x36, x37)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
(*  24*)		 ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
(*  24*)		 ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
(*  24*)		 ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
(*  24*)		 ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
(*  24*)		 ref 0, ref 0, ref 0)
(*  24*)      in get_rc_tokentype_
(*  24*)	   (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, 
(*  24*)	    x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, 
(*  24*)	    x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, 
(*  24*)	    x35, x36, x37)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6, !x7, !x8, !x9, !x10, 
(*  24*)	  !x11, !x12, !x13, !x14, !x15, !x16, !x17, !x18, !x19, !x20, 
(*  24*)	  !x21, !x22, !x23, !x24, !x25, !x26, !x27, !x28, !x29, !x30, 
(*  24*)	  !x31, !x32, !x33, !x34, !x35, !x36, !x37)
(*  24*)      end
(*  24*)type spintype = int
(*  24*)val get_spintype_ : int ref * int ref * int ref * int ref * int ref 
(*  24*)		  * int ref * int ref
(*  24*)		    -> unit
(*  24*)    = _import "mgtk_get_gtk_spintype"
(*  24*)	      : int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref * int ref
(*  24*)		-> unit;
(*  24*)val (SPIN_STEP_FORWARD, SPIN_STEP_BACKWARD, SPIN_PAGE_FORWARD, 
(*  24*)     SPIN_PAGE_BACKWARD, SPIN_HOME, SPIN_END, SPIN_USER_DEFINED)
(*  24*)    = let val (x0, x1, x2, x3, x4, x5, x6)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_spintype_ (x0, x1, x2, x3, x4, x5, x6)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6)
(*  24*)      end
(*  24*)type text_search_flags = int
(*  24*)val get_text_search_flags_ : int ref * int ref -> unit
(*  24*)    = _import "mgtk_get_gtk_text_search_flags"
(*  24*)	      : int ref * int ref -> unit;
(*  24*)val (TEXT_SEARCH_VISIBLE_ONLY, TEXT_SEARCH_TEXT_ONLY)
(*  24*)    = let val (x0, x1) = (ref 0, ref 0)
(*  24*)      in get_text_search_flags_ (x0, x1)
(*  24*)       ; (!x0, !x1)
(*  24*)      end
(*  24*)type text_window_type_t = int
(*  24*)val get_text_window_type_t_ : int ref * int ref * int ref * int ref 
(*  24*)			    * int ref * int ref * int ref
(*  24*)			      -> unit
(*  24*)    = _import "mgtk_get_gtk_text_window_type"
(*  24*)	      : int ref * int ref * int ref * int ref * int ref 
(*  24*)	      * int ref * int ref
(*  24*)		-> unit;
(*  24*)val (TEXT_WINDOW_PRIVATE, TEXT_WINDOW_WIDGET, TEXT_WINDOW_TEXT, 
(*  24*)     TEXT_WINDOW_LEFT, TEXT_WINDOW_RIGHT, TEXT_WINDOW_TOP, 
(*  24*)     TEXT_WINDOW_BOTTOM)
(*  24*)    = let val (x0, x1, x2, x3, x4, x5, x6)
(*  24*)	      = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
(*  24*)      in get_text_window_type_t_ (x0, x1, x2, x3, x4, x5, x6)
(*  24*)       ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6)
(*  24*)      end
(*  24*)type tree_path = GObject.cptr
(*  24*)type function = GObject.cptr
(*  24*)
(*  24*)
(*  24*)val accelerator_get_default_mod_mask_ : unit -> int
(*  24*)    = _import "gtk_accelerator_get_default_mod_mask" : unit -> int;
(*  24*)val accelerator_get_default_mod_mask : unit -> int
(*  24*)    = fn dummy => accelerator_get_default_mod_mask_ dummy
(*  24*)
(*  24*)val accel_map_load_ : CString.cstring -> unit
(*  24*)    = _import "gtk_accel_map_load" : CString.cstring -> unit;
(*  24*)val accel_map_load : string -> unit
(*  24*)    = fn file_name => accel_map_load_ (CString.fromString file_name)
(*  24*)val accel_map_save_ : CString.cstring -> unit
(*  24*)    = _import "gtk_accel_map_save" : CString.cstring -> unit;
(*  24*)val accel_map_save : string -> unit
(*  24*)    = fn file_name => accel_map_save_ (CString.fromString file_name)
(*  24*)
(*  24*)val accel_map_load_fd_ : int -> unit
(*  24*)    = _import "gtk_accel_map_load_fd" : int -> unit;
(*  24*)val accel_map_load_fd : int -> unit = fn fd => accel_map_load_fd_ fd
(*  24*)val accel_map_save_fd_ : int -> unit
(*  24*)    = _import "gtk_accel_map_save_fd" : int -> unit;
(*  24*)val accel_map_save_fd : int -> unit = fn fd => accel_map_save_fd_ fd
(*  24*)val accel_map_add_filter_ : CString.cstring -> unit
(*  24*)    = _import "gtk_accel_map_add_filter" : CString.cstring -> unit;
(*  24*)val accel_map_add_filter : string -> unit
(*  24*)    = fn filter_pattern => accel_map_add_filter_
(*  24*)			     (CString.fromString filter_pattern)
(*  24*)
(*  24*)
(*  24*)
(*  24*)
(*  24*)
(*  24*)val icon_size_register_ : CString.cstring * int * int -> int
(*  24*)    = _import "gtk_icon_size_register"
(*  24*)	      : CString.cstring * int * int -> int;
(*  24*)val icon_size_register : string -> int -> int -> icon_size
(*  24*)    = fn name => fn width => fn height =>
(*  24*)	 icon_size_register_ (CString.fromString name, width, height)
(*  24*)val icon_size_register_alias_ : CString.cstring * int -> unit
(*  24*)    = _import "gtk_icon_size_register_alias"
(*  24*)	      : CString.cstring * int -> unit;
(*  24*)val icon_size_register_alias : string -> icon_size -> unit
(*  24*)    = fn alias => fn target => icon_size_register_alias_
(*  24*)				 (CString.fromString alias, target)
(*  24*)val icon_size_from_name_ : CString.cstring -> int
(*  24*)    = _import "gtk_icon_size_from_name" : CString.cstring -> int;
(*  24*)val icon_size_from_name : string -> icon_size
(*  24*)    = fn name => icon_size_from_name_ (CString.fromString name)
(*  24*)val icon_size_get_name_ : int -> CString.t
(*  24*)    = _import "gtk_icon_size_get_name" : int -> CString.t;
(*  24*)val icon_size_get_name : icon_size -> string
(*  24*)    = fn size => let val t = icon_size_get_name_ size
(*  24*)		 in CString.toString t end
(*  24*)val icon_set_new_ : unit -> cptr
(*  24*)    = _import "gtk_icon_set_new" : unit -> cptr;
(*  24*)val icon_set_new : unit -> icon_set = fn dummy => icon_set_new_ dummy
(*  24*)val icon_source_get_type_ : unit -> int
(*  24*)    = _import "gtk_icon_source_get_type" : unit -> int;
(*  24*)val icon_source_get_type : unit -> int
(*  24*)    = fn dummy => icon_source_get_type_ dummy
(*  24*)val check_version_ : int * int * int -> CString.t
(*  24*)    = _import "gtk_check_version" : int * int * int -> CString.t;
(*  24*)val check_version : int -> int -> int -> string
(*  24*)    = fn required_major => fn required_minor => fn required_micro =>
(*  24*)	 let val t = check_version_ (required_major, required_minor, 
(*  24*)				     required_micro)
(*  24*)	 in CString.toString t end
(*  24*)val exit_ : int -> unit = _import "gtk_exit" : int -> unit;
(*  24*)val exit : int -> unit = fn error_code => exit_ error_code
(*  24*)val disable_setlocale_ : unit -> unit
(*  24*)    = _import "gtk_disable_setlocale" : unit -> unit;
(*  24*)val disable_setlocale : unit -> unit
(*  24*)    = fn dummy => disable_setlocale_ dummy
(*  24*)val set_locale_ : unit -> CString.t
(*  24*)    = _import "gtk_set_locale" : unit -> CString.t;
(*  24*)val set_locale : unit -> string
(*  24*)    = fn dummy => let val t = set_locale_ dummy
(*  24*)		  in CString.toString t end
(*  24*)
(*  24*)val events_pending_ : unit -> int
(*  24*)    = _import "gtk_events_pending" : unit -> int;
(*  24*)val events_pending : unit -> int = fn dummy => events_pending_ dummy
(*  24*)val main_ : unit -> unit = _import "gtk_main" : unit -> unit;
(*  24*)val main : unit -> unit = fn dummy => main_ dummy
(*  24*)val main_level_ : unit -> int = _import "gtk_main_level" : unit -> int;
(*  24*)val main_level : unit -> int = fn dummy => main_level_ dummy
(*  24*)val main_quit_ : unit -> unit = _import "gtk_main_quit" : unit -> unit;
(*  24*)val main_quit : unit -> unit = fn dummy => main_quit_ dummy
(*  24*)val main_iteration_ : unit -> bool
(*  24*)    = _import "gtk_main_iteration" : unit -> bool;
(*  24*)val main_iteration : unit -> bool = fn dummy => main_iteration_ dummy
(*  24*)val main_iteration_do_ : bool -> bool
(*  24*)    = _import "gtk_main_iteration_do" : bool -> bool;
(*  24*)val main_iteration_do : bool option -> bool
(*  24*)    = fn blocking => main_iteration_do_ (getOpt (blocking, true))
(*  24*)val main_iteration_do' : unit -> bool
(*  24*)    = fn dummy => main_iteration_do_ true
(*  24*)val rc_add_default_file_ : CString.cstring -> unit
(*  24*)    = _import "gtk_rc_add_default_file" : CString.cstring -> unit;
(*  24*)val rc_add_default_file : string -> unit
(*  24*)    = fn filename => rc_add_default_file_ (CString.fromString filename)
(*  24*)
(*  24*)
(*  24*)
(*  24*)
(*  24*)
(*  24*)val border_get_type_ : unit -> int
(*  24*)    = _import "gtk_border_get_type" : unit -> int;
(*  24*)val border_get_type : unit -> int = fn dummy => border_get_type_ dummy
(*  24*)val tips_query_get_type_ : unit -> int
(*  24*)    = _import "gtk_tips_query_get_type" : unit -> int;
(*  24*)val tips_query_get_type : unit -> int
(*  24*)    = fn dummy => tips_query_get_type_ dummy
(*  24*)val tree_path_new_ : unit -> cptr
(*  24*)    = _import "gtk_tree_path_new" : unit -> cptr;
(*  24*)val tree_path_new : unit -> tree_path
(*  24*)    = fn dummy => tree_path_new_ dummy
(*  24*)val tree_path_new_from_string_ : CString.cstring -> cptr
(*  24*)    = _import "gtk_tree_path_new_from_string"
(*  24*)	      : CString.cstring -> cptr;
(*  24*)val tree_path_new_from_string : string -> tree_path
(*  24*)    = fn path => tree_path_new_from_string_ (CString.fromString path)
(*  24*)val requisition_get_type_ : unit -> int
(*  24*)    = _import "gtk_requisition_get_type" : unit -> int;
(*  24*)val requisition_get_type : unit -> int
(*  24*)    = fn dummy => requisition_get_type_ dummy
(*  24*)structure AccelGroup :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a accelgroup_t
(*  24*)    type 'a t = 'a accelgroup_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toAccelGroup : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val lock : 'a t -> unit
(*  24*)    val unlock : 'a t -> unit
(*  24*)    val accel_activate_sig : (unit -> int -> unit -> bool)
(*  24*)			     -> 'a t Signal.signal
(*  24*)    val accel_changed_sig : (int -> unit -> unit -> unit)
(*  24*)			    -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a accelgroup_t = unit
(*  24*)    type 'a t = 'a accelgroup_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toAccelGroup obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_accel_group_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_accel_group_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val lock_ : cptr -> unit
(*  24*)	= _import "gtk_accel_group_lock" : cptr -> unit;
(*  24*)    val lock : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => lock_ self)
(*  24*)    val unlock_ : cptr -> unit
(*  24*)	= _import "gtk_accel_group_unlock" : cptr -> unit;
(*  24*)    val unlock : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => unlock_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val accel_activate_sig : (unit -> int -> unit -> bool)
(*  24*)				-> 'a t Signal.signal
(*  24*)	   = fn f => signal "accel-activate" false
(*  24*)			    (unit --> int --> unit --> return_bool) f
(*  24*)       val accel_changed_sig : (int -> unit -> unit -> unit)
(*  24*)			       -> 'a t Signal.signal
(*  24*)	   = fn f => signal "accel-changed" false
(*  24*)			    (int --> unit --> unit --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure IconFactory :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a iconfactory_t
(*  24*)    type 'a t = 'a iconfactory_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toIconFactory : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val add : 'a t -> string -> icon_set -> unit
(*  24*)    val lookup : 'a t -> string -> icon_set
(*  24*)    val add_default : 'a t -> unit
(*  24*)    val remove_default : 'a t -> unit
(*  24*)    val lookup_default : string -> icon_set
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a iconfactory_t = unit
(*  24*)    type 'a t = 'a iconfactory_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toIconFactory obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_icon_factory_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_icon_factory_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val add_ : cptr * CString.cstring * cptr -> unit
(*  24*)	= _import "gtk_icon_factory_add"
(*  24*)		  : cptr * CString.cstring * cptr -> unit;
(*  24*)    val add : 'a t -> string -> icon_set -> unit
(*  24*)	= fn self => fn stock_id => fn icon_set =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => add_ (self, CString.fromString stock_id, 
(*  24*)				 icon_set))
(*  24*)    val lookup_ : cptr * CString.cstring -> cptr
(*  24*)	= _import "gtk_icon_factory_lookup"
(*  24*)		  : cptr * CString.cstring -> cptr;
(*  24*)    val lookup : 'a t -> string -> icon_set
(*  24*)	= fn self => fn stock_id =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => lookup_ (self, CString.fromString stock_id))
(*  24*)    val add_default_ : cptr -> unit
(*  24*)	= _import "gtk_icon_factory_add_default" : cptr -> unit;
(*  24*)    val add_default : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => add_default_ self)
(*  24*)    val remove_default_ : cptr -> unit
(*  24*)	= _import "gtk_icon_factory_remove_default" : cptr -> unit;
(*  24*)    val remove_default : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => remove_default_ self)
(*  24*)    val lookup_default_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_icon_factory_lookup_default"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val lookup_default : string -> icon_set
(*  24*)	= fn stock_id => lookup_default_ (CString.fromString stock_id)
(*  24*)end
(*  24*)structure Object :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a object_t
(*  24*)    type 'a t = 'a object_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toObject : 'a t -> base t
(*  24*)    type flags
(*  24*)    val IN_DESTRUCTION : flags
(*  24*)    val FLOATING : flags
(*  24*)    val RESERVED_1 : flags
(*  24*)    val RESERVED_2 : flags
(*  24*)    val get_type : unit -> int
(*  24*)    val new : int -> string -> base t
(*  24*)    val sink : 'a t -> unit
(*  24*)    val destroy : 'a t -> unit
(*  24*)    val destroy_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a object_t = unit
(*  24*)    type 'a t = 'a object_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toObject obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    type flags = int
(*  24*)    val get_flags_ : int ref * int ref * int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_object_flags"
(*  24*)		  : int ref * int ref * int ref * int ref -> unit;
(*  24*)    val (IN_DESTRUCTION, FLOATING, RESERVED_1, RESERVED_2)
(*  24*)	= let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)	  in get_flags_ (x0, x1, x2, x3)
(*  24*)	   ; (!x0, !x1, !x2, !x3)
(*  24*)	  end
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_object_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : int * CString.cstring -> cptr
(*  24*)	= _import "gtk_object_new" : int * CString.cstring -> cptr;
(*  24*)    val new : int -> string -> base t
(*  24*)	= fn typ => fn first_property_name =>
(*  24*)	     make (new_ (typ, CString.fromString first_property_name))
(*  24*)    val sink_ : cptr -> unit
(*  24*)	= _import "gtk_object_sink" : cptr -> unit;
(*  24*)    val sink : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => sink_ self)
(*  24*)    val destroy_ : cptr -> unit
(*  24*)	= _import "gtk_object_destroy" : cptr -> unit;
(*  24*)    val destroy : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => destroy_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val destroy_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "destroy" false (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Adjustment :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a adjustment_t
(*  24*)    type 'a t = 'a adjustment_t Object.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toAdjustment : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : real option -> real option -> real option -> real option 
(*  24*)	   -> real option -> real option
(*  24*)	      -> base t
(*  24*)    val new' : unit -> base t
(*  24*)    val changed : 'a t -> unit
(*  24*)    val value_changed : 'a t -> unit
(*  24*)    val clamp_page : 'a t -> real -> real -> unit
(*  24*)    val get_value : 'a t -> real
(*  24*)    val set_value : 'a t -> real -> unit
(*  24*)    val changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val value_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a adjustment_t = unit
(*  24*)    type 'a t = 'a adjustment_t Object.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Object.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toAdjustment obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_adjustment_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : real * real * real * real * real * real -> cptr
(*  24*)	= _import "gtk_adjustment_new"
(*  24*)		  : real * real * real * real * real * real -> cptr;
(*  24*)    val new : real option -> real option -> real option -> real option 
(*  24*)	   -> real option -> real option
(*  24*)	      -> base t
(*  24*)	= fn value => fn lower => fn upper => fn step_incr => 
(*  24*)	  fn page_incr => fn page_size =>
(*  24*)	     make (new_ (getOpt (value, 0.0), getOpt (lower, 0.0), 
(*  24*)			 getOpt (upper, 0.0), getOpt (step_incr, 0.0), 
(*  24*)			 getOpt (page_incr, 0.0), 
(*  24*)			 getOpt (page_size, 0.0)))
(*  24*)    val new' : unit -> base t
(*  24*)	= fn dummy => make (new_ (0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
(*  24*)    val changed_ : cptr -> unit
(*  24*)	= _import "gtk_adjustment_changed" : cptr -> unit;
(*  24*)    val changed : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => changed_ self)
(*  24*)    val value_changed_ : cptr -> unit
(*  24*)	= _import "gtk_adjustment_value_changed" : cptr -> unit;
(*  24*)    val value_changed : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => value_changed_ self)
(*  24*)    val clamp_page_ : cptr * real * real -> unit
(*  24*)	= _import "gtk_adjustment_clamp_page"
(*  24*)		  : cptr * real * real -> unit;
(*  24*)    val clamp_page : 'a t -> real -> real -> unit
(*  24*)	= fn self => fn lower => fn upper =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => clamp_page_ (self, lower, upper))
(*  24*)    val get_value_ : cptr -> real
(*  24*)	= _import "gtk_adjustment_get_value" : cptr -> real;
(*  24*)    val get_value : 'a t -> real
(*  24*)	= fn self => GObject.withPtr (self, fn self => get_value_ self)
(*  24*)    val set_value_ : cptr * real -> unit
(*  24*)	= _import "gtk_adjustment_set_value" : cptr * real -> unit;
(*  24*)    val set_value : 'a t -> real -> unit
(*  24*)	= fn self => fn value =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_value_ (self, value))
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "changed" false (void --> return_void) f
(*  24*)       val value_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "value-changed" false (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Widget :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a widget_t
(*  24*)    type 'a t = 'a widget_t Object.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toWidget : 'a t -> base t
(*  24*)    type flags
(*  24*)    val TOPLEVEL : flags
(*  24*)    val NO_WINDOW : flags
(*  24*)    val REALIZED : flags
(*  24*)    val MAPPED : flags
(*  24*)    val VISIBLE : flags
(*  24*)    val SENSITIVE : flags
(*  24*)    val PARENT_SENSITIVE : flags
(*  24*)    val CAN_FOCUS : flags
(*  24*)    val HAS_FOCUS : flags
(*  24*)    val CAN_DEFAULT : flags
(*  24*)    val HAS_DEFAULT : flags
(*  24*)    val HAS_GRAB : flags
(*  24*)    val RC_STYLE : flags
(*  24*)    val COMPOSITE_CHILD : flags
(*  24*)    val NO_REPARENT : flags
(*  24*)    val APP_PAINTABLE : flags
(*  24*)    val RECEIVES_DEFAULT : flags
(*  24*)    val DOUBLE_BUFFERED : flags
(*  24*)    type helptype
(*  24*)    val HELP_TOOLTIP : helptype
(*  24*)    val HELP_WHATS_THIS : helptype
(*  24*)    val drag_check_threshold : 'a t -> int -> int -> int -> int -> bool
(*  24*)    val drag_highlight : 'a t -> unit
(*  24*)    val drag_unhighlight : 'a t -> unit
(*  24*)    val drag_dest_unset : 'a t -> unit
(*  24*)    val drag_source_unset : 'a t -> unit
(*  24*)    val drag_source_set_icon_stock : 'a t -> string -> unit
(*  24*)    val grab_add : 'a t -> unit
(*  24*)    val grab_remove : 'a t -> unit
(*  24*)    val rc_get_style : 'a t -> base t
(*  24*)    val selection_remove_all : 'a t -> unit
(*  24*)    val get_type : unit -> int
(*  24*)    val new : int -> string -> base t
(*  24*)    val refe : 'a t -> base t
(*  24*)    val unref : 'a t -> unit
(*  24*)    val destroy : 'a t -> unit
(*  24*)    val destroyed : 'a t -> 'b t -> unit
(*  24*)    val set : 'a t -> string -> unit
(*  24*)    val unparent : 'a t -> unit
(*  24*)    val show : 'a t -> unit
(*  24*)    val show_now : 'a t -> unit
(*  24*)    val hide : 'a t -> unit
(*  24*)    val show_all : 'a t -> unit
(*  24*)    val hide_all : 'a t -> unit
(*  24*)    val map : 'a t -> unit
(*  24*)    val unmap : 'a t -> unit
(*  24*)    val realize : 'a t -> unit
(*  24*)    val unrealize : 'a t -> unit
(*  24*)    val queue_draw : 'a t -> unit
(*  24*)    val queue_draw_area : 'a t -> int -> int -> int -> int -> unit
(*  24*)    val queue_clear : 'a t -> unit
(*  24*)    val queue_clear_area : 'a t -> int -> int -> int -> int -> unit
(*  24*)    val queue_resize : 'a t -> unit
(*  24*)    val size_request : 'a t -> requisition -> unit
(*  24*)    val get_child_requisition : 'a t -> requisition -> unit
(*  24*)    val set_accel_path : 'a t -> string -> 'b AccelGroup.t -> unit
(*  24*)    val mnemonic_activate : 'a t -> bool -> bool
(*  24*)    val activate : 'a t -> bool
(*  24*)    val set_scroll_adjustments
(*  24*)      : 'a t -> 'b Adjustment.t option -> 'c Adjustment.t option
(*  24*)	-> bool
(*  24*)    val set_scroll_adjustments' : 'a t -> bool
(*  24*)    val reparent : 'a t -> 'b t -> unit
(*  24*)    val freeze_child_notify : 'a t -> unit
(*  24*)    val child_notify : 'a t -> string -> unit
(*  24*)    val thaw_child_notify : 'a t -> unit
(*  24*)    val is_focus : 'a t -> bool
(*  24*)    val grab_focus : 'a t -> unit
(*  24*)    val grab_default : 'a t -> unit
(*  24*)    val set_name : 'a t -> string -> unit
(*  24*)    val get_name : 'a t -> string
(*  24*)    val set_state : 'a t -> statetype -> unit
(*  24*)    val set_sensitive : 'a t -> bool -> unit
(*  24*)    val set_app_paintable : 'a t -> bool -> unit
(*  24*)    val set_double_buffered : 'a t -> bool -> unit
(*  24*)    val set_redraw_on_allocate : 'a t -> bool -> unit
(*  24*)    val set_parent : 'a t -> 'b t -> unit
(*  24*)    val set_child_visible : 'a t -> bool -> unit
(*  24*)    val get_child_visible : 'a t -> bool
(*  24*)    val get_parent : 'a t -> base t
(*  24*)    val child_focus : 'a t -> directiontype -> bool
(*  24*)    val set_size_request : 'a t -> int -> int -> unit
(*  24*)    val set_uposition : 'a t -> int -> int -> unit
(*  24*)    val set_usize : 'a t -> int -> int -> unit
(*  24*)    val set_events : 'a t -> int -> unit
(*  24*)    val add_events : 'a t -> int -> unit
(*  24*)    val get_toplevel : 'a t -> base t
(*  24*)    val get_ancestor : 'a t -> int -> base t
(*  24*)    val get_settings : 'a t -> base t
(*  24*)    val get_events : 'a t -> int
(*  24*)    val is_ancestor : 'a t -> 'b t -> bool
(*  24*)    val hide_on_delete : 'a t -> bool
(*  24*)    val set_style : 'a t -> 'b t option -> unit
(*  24*)    val set_style' : 'a t -> unit
(*  24*)    val ensure_style : 'a t -> unit
(*  24*)    val get_style : 'a t -> base t
(*  24*)    val modify_style : 'a t -> 'b t -> unit
(*  24*)    val get_modifier_style : 'a t -> base t
(*  24*)    val set_composite_name : 'a t -> string -> unit
(*  24*)    val get_composite_name : 'a t -> string
(*  24*)    val reset_rc_styles : 'a t -> unit
(*  24*)    val push_composite_child : unit -> unit
(*  24*)    val pop_composite_child : unit -> unit
(*  24*)    val pop_colormap : unit -> unit
(*  24*)    val style_get : 'a t -> string -> unit
(*  24*)    val get_default_style : unit -> base t
(*  24*)    val set_direction : 'a t -> text_direction -> unit
(*  24*)    val get_direction : 'a t -> text_direction
(*  24*)    val set_default_direction : text_direction -> unit
(*  24*)    val get_default_direction : unit -> text_direction
(*  24*)    val reset_shapes : 'a t -> unit
(*  24*)    val show_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val hide_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val map_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val unmap_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val realize_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val unrealize_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val size_request_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val size_allocate_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val state_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val parent_set_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val hierarchy_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val style_set_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val direction_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val grab_notify_sig : (bool -> unit) -> 'a t Signal.signal
(*  24*)    val child_notify_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val mnemonic_activate_sig : (bool -> bool) -> 'a t Signal.signal
(*  24*)    val grab_focus_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val focus_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val event_after_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val button_press_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val button_release_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val scroll_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val motion_notify_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val delete_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val destroy_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val expose_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val key_press_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val key_release_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val enter_notify_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val leave_notify_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val configure_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val focus_in_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val focus_out_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val map_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val unmap_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val property_notify_event_sig
(*  24*)      : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val selection_clear_event_sig
(*  24*)      : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val selection_request_event_sig
(*  24*)      : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val selection_notify_event_sig
(*  24*)      : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val selection_received_sig
(*  24*)      : (unit -> int -> unit) -> 'a t Signal.signal
(*  24*)    val selection_get_sig : (unit -> int -> int -> unit)
(*  24*)			    -> 'a t Signal.signal
(*  24*)    val proximity_in_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val proximity_out_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val drag_leave_sig : (unit -> int -> unit) -> 'a t Signal.signal
(*  24*)    val drag_begin_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val drag_end_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val drag_data_delete_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val drag_motion_sig : (unit -> int -> int -> int -> bool)
(*  24*)			  -> 'a t Signal.signal
(*  24*)    val drag_drop_sig : (unit -> int -> int -> int -> bool)
(*  24*)			-> 'a t Signal.signal
(*  24*)    val drag_data_get_sig : (unit -> unit -> int -> int -> unit)
(*  24*)			    -> 'a t Signal.signal
(*  24*)    val drag_data_received_sig
(*  24*)      : (unit -> int -> int -> unit -> int -> int -> unit)
(*  24*)	-> 'a t Signal.signal
(*  24*)    val visibility_notify_event_sig
(*  24*)      : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val client_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val no_expose_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val window_state_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val popup_menu_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val show_help_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val accel_closures_changed_sig
(*  24*)      : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a widget_t = unit
(*  24*)    type 'a t = 'a widget_t Object.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Object.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toWidget obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    type flags = int
(*  24*)    val get_flags_
(*  24*)      : int ref * int ref * int ref * int ref * int ref * int ref 
(*  24*)      * int ref * int ref * int ref * int ref * int ref * int ref 
(*  24*)      * int ref * int ref * int ref * int ref * int ref * int ref
(*  24*)	-> unit
(*  24*)	= _import "mgtk_get_gtk_widget_flags"
(*  24*)		  : int ref * int ref * int ref * int ref * int ref 
(*  24*)		  * int ref * int ref * int ref * int ref * int ref 
(*  24*)		  * int ref * int ref * int ref * int ref * int ref 
(*  24*)		  * int ref * int ref * int ref
(*  24*)		    -> unit;
(*  24*)    val (TOPLEVEL, NO_WINDOW, REALIZED, MAPPED, VISIBLE, SENSITIVE, 
(*  24*)	 PARENT_SENSITIVE, CAN_FOCUS, HAS_FOCUS, CAN_DEFAULT, 
(*  24*)	 HAS_DEFAULT, HAS_GRAB, RC_STYLE, COMPOSITE_CHILD, 
(*  24*)	 NO_REPARENT, APP_PAINTABLE, RECEIVES_DEFAULT, DOUBLE_BUFFERED)
(*  24*)	= let val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, 
(*  24*)		   x12, x13, x14, x15, x16, x17)
(*  24*)		  = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
(*  24*)		     ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
(*  24*)		     ref 0, ref 0, ref 0, ref 0)
(*  24*)	  in get_flags_ (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, 
(*  24*)			 x11, x12, x13, x14, x15, x16, x17)
(*  24*)	   ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6, !x7, !x8, !x9, !x10, 
(*  24*)	      !x11, !x12, !x13, !x14, !x15, !x16, !x17)
(*  24*)	  end
(*  24*)    type helptype = int
(*  24*)    val get_helptype_ : int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_widget_helptype"
(*  24*)		  : int ref * int ref -> unit;
(*  24*)    val (HELP_TOOLTIP, HELP_WHATS_THIS)
(*  24*)	= let val (x0, x1) = (ref 0, ref 0) in get_helptype_ (x0, x1)
(*  24*)					     ; (!x0, !x1)
(*  24*)					    end
(*  24*)    val drag_check_threshold_ : cptr * int * int * int * int -> bool
(*  24*)	= _import "gtk_drag_check_threshold"
(*  24*)		  : cptr * int * int * int * int -> bool;
(*  24*)    val drag_check_threshold : 'a t -> int -> int -> int -> int -> bool
(*  24*)	= fn self => fn start_x => fn start_y => fn current_x => 
(*  24*)	  fn current_y =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => drag_check_threshold_
(*  24*)					   (self, start_x, start_y, 
(*  24*)					    current_x, current_y))
(*  24*)    val drag_highlight_ : cptr -> unit
(*  24*)	= _import "gtk_drag_highlight" : cptr -> unit;
(*  24*)    val drag_highlight : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => drag_highlight_ self)
(*  24*)    val drag_unhighlight_ : cptr -> unit
(*  24*)	= _import "gtk_drag_unhighlight" : cptr -> unit;
(*  24*)    val drag_unhighlight : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => drag_unhighlight_ self)
(*  24*)    val drag_dest_unset_ : cptr -> unit
(*  24*)	= _import "gtk_drag_dest_unset" : cptr -> unit;
(*  24*)    val drag_dest_unset : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => drag_dest_unset_ self)
(*  24*)    val drag_source_unset_ : cptr -> unit
(*  24*)	= _import "gtk_drag_source_unset" : cptr -> unit;
(*  24*)    val drag_source_unset : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => drag_source_unset_ self)
(*  24*)    val drag_source_set_icon_stock_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_drag_source_set_icon_stock"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val drag_source_set_icon_stock : 'a t -> string -> unit
(*  24*)	= fn self => fn stock_id =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => drag_source_set_icon_stock_
(*  24*)			     (self, CString.fromString stock_id))
(*  24*)    val grab_add_ : cptr -> unit
(*  24*)	= _import "gtk_grab_add" : cptr -> unit;
(*  24*)    val grab_add : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => grab_add_ self)
(*  24*)    val grab_remove_ : cptr -> unit
(*  24*)	= _import "gtk_grab_remove" : cptr -> unit;
(*  24*)    val grab_remove : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => grab_remove_ self)
(*  24*)    val rc_get_style_ : cptr -> cptr
(*  24*)	= _import "gtk_rc_get_style" : cptr -> cptr;
(*  24*)    val rc_get_style : 'a t -> base t
(*  24*)	= fn self => make (GObject.withPtr
(*  24*)			     (self, fn self => rc_get_style_ self))
(*  24*)    val selection_remove_all_ : cptr -> unit
(*  24*)	= _import "gtk_selection_remove_all" : cptr -> unit;
(*  24*)    val selection_remove_all : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => selection_remove_all_ self)
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_widget_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : int * CString.cstring -> cptr
(*  24*)	= _import "gtk_widget_new" : int * CString.cstring -> cptr;
(*  24*)    val new : int -> string -> base t
(*  24*)	= fn typ => fn first_property_name =>
(*  24*)	     make (new_ (typ, CString.fromString first_property_name))
(*  24*)    val ref_ : cptr -> cptr = _import "gtk_widget_ref" : cptr -> cptr;
(*  24*)    val refe : 'a t -> base t
(*  24*)	= fn self => make (GObject.withPtr
(*  24*)			     (self, fn self => ref_ self))
(*  24*)    val unref_ : cptr -> unit
(*  24*)	= _import "gtk_widget_unref" : cptr -> unit;
(*  24*)    val unref : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => unref_ self)
(*  24*)    val destroy_ : cptr -> unit
(*  24*)	= _import "gtk_widget_destroy" : cptr -> unit;
(*  24*)    val destroy : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => destroy_ self)
(*  24*)    val destroyed_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_widget_destroyed" : cptr * cptr -> unit;
(*  24*)    val destroyed : 'a t -> 'b t -> unit
(*  24*)	= fn self => fn widget_pointer =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (widget_pointer, 
(*  24*)			      fn widget_pointer =>
(*  24*)				 destroyed_ (self, widget_pointer)))
(*  24*)    val set_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_widget_set" : cptr * CString.cstring -> unit;
(*  24*)    val set : 'a t -> string -> unit
(*  24*)	= fn self => fn first_property_name =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_ (self, 
(*  24*)					       CString.fromString
(*  24*)						 first_property_name))
(*  24*)    val unparent_ : cptr -> unit
(*  24*)	= _import "gtk_widget_unparent" : cptr -> unit;
(*  24*)    val unparent : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => unparent_ self)
(*  24*)    val show_ : cptr -> unit
(*  24*)	= _import "gtk_widget_show" : cptr -> unit;
(*  24*)    val show : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => show_ self)
(*  24*)    val show_now_ : cptr -> unit
(*  24*)	= _import "gtk_widget_show_now" : cptr -> unit;
(*  24*)    val show_now : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => show_now_ self)
(*  24*)    val hide_ : cptr -> unit
(*  24*)	= _import "gtk_widget_hide" : cptr -> unit;
(*  24*)    val hide : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => hide_ self)
(*  24*)    val show_all_ : cptr -> unit
(*  24*)	= _import "gtk_widget_show_all" : cptr -> unit;
(*  24*)    val show_all : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => show_all_ self)
(*  24*)    val hide_all_ : cptr -> unit
(*  24*)	= _import "gtk_widget_hide_all" : cptr -> unit;
(*  24*)    val hide_all : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => hide_all_ self)
(*  24*)    val map_ : cptr -> unit = _import "gtk_widget_map" : cptr -> unit;
(*  24*)    val map : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => map_ self)
(*  24*)    val unmap_ : cptr -> unit
(*  24*)	= _import "gtk_widget_unmap" : cptr -> unit;
(*  24*)    val unmap : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => unmap_ self)
(*  24*)    val realize_ : cptr -> unit
(*  24*)	= _import "gtk_widget_realize" : cptr -> unit;
(*  24*)    val realize : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => realize_ self)
(*  24*)    val unrealize_ : cptr -> unit
(*  24*)	= _import "gtk_widget_unrealize" : cptr -> unit;
(*  24*)    val unrealize : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => unrealize_ self)
(*  24*)    val queue_draw_ : cptr -> unit
(*  24*)	= _import "gtk_widget_queue_draw" : cptr -> unit;
(*  24*)    val queue_draw : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => queue_draw_ self)
(*  24*)    val queue_draw_area_ : cptr * int * int * int * int -> unit
(*  24*)	= _import "gtk_widget_queue_draw_area"
(*  24*)		  : cptr * int * int * int * int -> unit;
(*  24*)    val queue_draw_area : 'a t -> int -> int -> int -> int -> unit
(*  24*)	= fn self => fn x => fn y => fn width => fn height =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => queue_draw_area_
(*  24*)					   (self, x, y, width, height))
(*  24*)    val queue_clear_ : cptr -> unit
(*  24*)	= _import "gtk_widget_queue_clear" : cptr -> unit;
(*  24*)    val queue_clear : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => queue_clear_ self)
(*  24*)    val queue_clear_area_ : cptr * int * int * int * int -> unit
(*  24*)	= _import "gtk_widget_queue_clear_area"
(*  24*)		  : cptr * int * int * int * int -> unit;
(*  24*)    val queue_clear_area : 'a t -> int -> int -> int -> int -> unit
(*  24*)	= fn self => fn x => fn y => fn width => fn height =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => queue_clear_area_
(*  24*)					   (self, x, y, width, height))
(*  24*)    val queue_resize_ : cptr -> unit
(*  24*)	= _import "gtk_widget_queue_resize" : cptr -> unit;
(*  24*)    val queue_resize : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => queue_resize_ self)
(*  24*)    val size_request_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_widget_size_request" : cptr * cptr -> unit;
(*  24*)    val size_request : 'a t -> requisition -> unit
(*  24*)	= fn self => fn requisition =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => size_request_ (self, requisition))
(*  24*)    val get_child_requisition_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_widget_get_child_requisition"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val get_child_requisition : 'a t -> requisition -> unit
(*  24*)	= fn self => fn requisition =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => get_child_requisition_
(*  24*)					   (self, requisition))
(*  24*)    val set_accel_path_ : cptr * CString.cstring * cptr -> unit
(*  24*)	= _import "gtk_widget_set_accel_path"
(*  24*)		  : cptr * CString.cstring * cptr -> unit;
(*  24*)    val set_accel_path : 'a t -> string -> 'b AccelGroup.t -> unit
(*  24*)	= fn self => fn accel_path => fn accel_group =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (accel_group, 
(*  24*)		      fn accel_group =>
(*  24*)			 set_accel_path_
(*  24*)			   (self, CString.fromString accel_path, 
(*  24*)			    accel_group)))
(*  24*)    val mnemonic_activate_ : cptr * bool -> bool
(*  24*)	= _import "gtk_widget_mnemonic_activate" : cptr * bool -> bool;
(*  24*)    val mnemonic_activate : 'a t -> bool -> bool
(*  24*)	= fn self => fn group_cycling =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => mnemonic_activate_
(*  24*)					   (self, group_cycling))
(*  24*)    val activate_ : cptr -> bool
(*  24*)	= _import "gtk_widget_activate" : cptr -> bool;
(*  24*)    val activate : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr (self, fn self => activate_ self)
(*  24*)    val set_scroll_adjustments_ : cptr * cptr * cptr -> bool
(*  24*)	= _import "gtk_widget_set_scroll_adjustments"
(*  24*)		  : cptr * cptr * cptr -> bool;
(*  24*)    val set_scroll_adjustments
(*  24*)      : 'a t -> 'b Adjustment.t option -> 'c Adjustment.t option
(*  24*)	-> bool
(*  24*)	= fn self => fn hadjustment => fn vadjustment =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withOpt
(*  24*)			     (hadjustment, 
(*  24*)			      fn hadjustment =>
(*  24*)				 GObject.withOpt
(*  24*)				   (vadjustment, 
(*  24*)				    fn vadjustment =>
(*  24*)				       set_scroll_adjustments_
(*  24*)					 (self, hadjustment, 
(*  24*)					  vadjustment))))
(*  24*)    val set_scroll_adjustments' : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => set_scroll_adjustments_
(*  24*)				     (self, GObject.null, 
(*  24*)				      GObject.null))
(*  24*)    val reparent_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_widget_reparent" : cptr * cptr -> unit;
(*  24*)    val reparent : 'a t -> 'b t -> unit
(*  24*)	= fn self => fn new_parent =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (new_parent, 
(*  24*)			      fn new_parent =>
(*  24*)				 reparent_ (self, new_parent)))
(*  24*)    val freeze_child_notify_ : cptr -> unit
(*  24*)	= _import "gtk_widget_freeze_child_notify" : cptr -> unit;
(*  24*)    val freeze_child_notify : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => freeze_child_notify_ self)
(*  24*)    val child_notify_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_widget_child_notify"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val child_notify : 'a t -> string -> unit
(*  24*)	= fn self => fn child_property =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => child_notify_
(*  24*)			     (self, CString.fromString child_property))
(*  24*)    val thaw_child_notify_ : cptr -> unit
(*  24*)	= _import "gtk_widget_thaw_child_notify" : cptr -> unit;
(*  24*)    val thaw_child_notify : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => thaw_child_notify_ self)
(*  24*)    val is_focus_ : cptr -> bool
(*  24*)	= _import "gtk_widget_is_focus" : cptr -> bool;
(*  24*)    val is_focus : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr (self, fn self => is_focus_ self)
(*  24*)    val grab_focus_ : cptr -> unit
(*  24*)	= _import "gtk_widget_grab_focus" : cptr -> unit;
(*  24*)    val grab_focus : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => grab_focus_ self)
(*  24*)    val grab_default_ : cptr -> unit
(*  24*)	= _import "gtk_widget_grab_default" : cptr -> unit;
(*  24*)    val grab_default : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => grab_default_ self)
(*  24*)    val set_name_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_widget_set_name"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_name : 'a t -> string -> unit
(*  24*)	= fn self => fn name =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_name_ (self, CString.fromString name))
(*  24*)    val get_name_ : cptr -> CString.t
(*  24*)	= _import "gtk_widget_get_name" : cptr -> CString.t;
(*  24*)    val get_name : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_name_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val set_state_ : cptr * int -> unit
(*  24*)	= _import "gtk_widget_set_state" : cptr * int -> unit;
(*  24*)    val set_state : 'a t -> statetype -> unit
(*  24*)	= fn self => fn state =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_state_ (self, state))
(*  24*)    val set_sensitive_ : cptr * bool -> unit
(*  24*)	= _import "gtk_widget_set_sensitive" : cptr * bool -> unit;
(*  24*)    val set_sensitive : 'a t -> bool -> unit
(*  24*)	= fn self => fn sensitive =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_sensitive_ (self, sensitive))
(*  24*)    val set_app_paintable_ : cptr * bool -> unit
(*  24*)	= _import "gtk_widget_set_app_paintable" : cptr * bool -> unit;
(*  24*)    val set_app_paintable : 'a t -> bool -> unit
(*  24*)	= fn self => fn app_paintable =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_app_paintable_
(*  24*)					   (self, app_paintable))
(*  24*)    val set_double_buffered_ : cptr * bool -> unit
(*  24*)	= _import "gtk_widget_set_double_buffered"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_double_buffered : 'a t -> bool -> unit
(*  24*)	= fn self => fn double_buffered =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_double_buffered_
(*  24*)					   (self, double_buffered))
(*  24*)    val set_redraw_on_allocate_ : cptr * bool -> unit
(*  24*)	= _import "gtk_widget_set_redraw_on_allocate"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_redraw_on_allocate : 'a t -> bool -> unit
(*  24*)	= fn self => fn redraw_on_allocate =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_redraw_on_allocate_
(*  24*)					   (self, redraw_on_allocate))
(*  24*)    val set_parent_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_widget_set_parent" : cptr * cptr -> unit;
(*  24*)    val set_parent : 'a t -> 'b t -> unit
(*  24*)	= fn self => fn parent =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (parent, fn parent => set_parent_ (self, parent)))
(*  24*)    val set_child_visible_ : cptr * bool -> unit
(*  24*)	= _import "gtk_widget_set_child_visible" : cptr * bool -> unit;
(*  24*)    val set_child_visible : 'a t -> bool -> unit
(*  24*)	= fn self => fn is_visible =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_child_visible_ (self, is_visible))
(*  24*)    val get_child_visible_ : cptr -> bool
(*  24*)	= _import "gtk_widget_get_child_visible" : cptr -> bool;
(*  24*)    val get_child_visible : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_child_visible_ self)
(*  24*)    val get_parent_ : cptr -> cptr
(*  24*)	= _import "gtk_widget_get_parent" : cptr -> cptr;
(*  24*)    val get_parent : 'a t -> base t
(*  24*)	= fn self => make (GObject.withPtr
(*  24*)			     (self, fn self => get_parent_ self))
(*  24*)    val child_focus_ : cptr * int -> bool
(*  24*)	= _import "gtk_widget_child_focus" : cptr * int -> bool;
(*  24*)    val child_focus : 'a t -> directiontype -> bool
(*  24*)	= fn self => fn direction =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => child_focus_ (self, direction))
(*  24*)    val set_size_request_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_widget_set_size_request"
(*  24*)		  : cptr * int * int -> unit;
(*  24*)    val set_size_request : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn width => fn height =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_size_request_
(*  24*)					   (self, width, height))
(*  24*)    val set_uposition_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_widget_set_uposition"
(*  24*)		  : cptr * int * int -> unit;
(*  24*)    val set_uposition : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn x => fn y =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_uposition_ (self, x, y))
(*  24*)    val set_usize_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_widget_set_usize" : cptr * int * int -> unit;
(*  24*)    val set_usize : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn width => fn height =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_usize_ (self, width, height))
(*  24*)    val set_events_ : cptr * int -> unit
(*  24*)	= _import "gtk_widget_set_events" : cptr * int -> unit;
(*  24*)    val set_events : 'a t -> int -> unit
(*  24*)	= fn self => fn events =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_events_ (self, events))
(*  24*)    val add_events_ : cptr * int -> unit
(*  24*)	= _import "gtk_widget_add_events" : cptr * int -> unit;
(*  24*)    val add_events : 'a t -> int -> unit
(*  24*)	= fn self => fn events =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => add_events_ (self, events))
(*  24*)    val get_toplevel_ : cptr -> cptr
(*  24*)	= _import "gtk_widget_get_toplevel" : cptr -> cptr;
(*  24*)    val get_toplevel : 'a t -> base t
(*  24*)	= fn self => make (GObject.withPtr
(*  24*)			     (self, fn self => get_toplevel_ self))
(*  24*)    val get_ancestor_ : cptr * int -> cptr
(*  24*)	= _import "gtk_widget_get_ancestor" : cptr * int -> cptr;
(*  24*)    val get_ancestor : 'a t -> int -> base t
(*  24*)	= fn self => fn widget_type =>
(*  24*)	     make (GObject.withPtr
(*  24*)		     (self, 
(*  24*)		      fn self => get_ancestor_ (self, widget_type)))
(*  24*)    val get_settings_ : cptr -> cptr
(*  24*)	= _import "gtk_widget_get_settings" : cptr -> cptr;
(*  24*)    val get_settings : 'a t -> base t
(*  24*)	= fn self => make (GObject.withPtr
(*  24*)			     (self, fn self => get_settings_ self))
(*  24*)    val get_events_ : cptr -> int
(*  24*)	= _import "gtk_widget_get_events" : cptr -> int;
(*  24*)    val get_events : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_events_ self)
(*  24*)    val is_ancestor_ : cptr * cptr -> bool
(*  24*)	= _import "gtk_widget_is_ancestor" : cptr * cptr -> bool;
(*  24*)    val is_ancestor : 'a t -> 'b t -> bool
(*  24*)	= fn self => fn ancestor =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (ancestor, 
(*  24*)			      fn ancestor =>
(*  24*)				 is_ancestor_ (self, ancestor)))
(*  24*)    val hide_on_delete_ : cptr -> bool
(*  24*)	= _import "gtk_widget_hide_on_delete" : cptr -> bool;
(*  24*)    val hide_on_delete : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => hide_on_delete_ self)
(*  24*)    val set_style_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_widget_set_style" : cptr * cptr -> unit;
(*  24*)    val set_style : 'a t -> 'b t option -> unit
(*  24*)	= fn self => fn style =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withOpt
(*  24*)		     (style, fn style => set_style_ (self, style)))
(*  24*)    val set_style' : 'a t -> unit
(*  24*)	= fn self =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_style_ (self, GObject.null))
(*  24*)    val ensure_style_ : cptr -> unit
(*  24*)	= _import "gtk_widget_ensure_style" : cptr -> unit;
(*  24*)    val ensure_style : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => ensure_style_ self)
(*  24*)    val get_style_ : cptr -> cptr
(*  24*)	= _import "gtk_widget_get_style" : cptr -> cptr;
(*  24*)    val get_style : 'a t -> base t
(*  24*)	= fn self => make (GObject.withPtr
(*  24*)			     (self, fn self => get_style_ self))
(*  24*)    val modify_style_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_widget_modify_style" : cptr * cptr -> unit;
(*  24*)    val modify_style : 'a t -> 'b t -> unit
(*  24*)	= fn self => fn style =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (style, fn style => modify_style_ (self, style)))
(*  24*)    val get_modifier_style_ : cptr -> cptr
(*  24*)	= _import "gtk_widget_get_modifier_style" : cptr -> cptr;
(*  24*)    val get_modifier_style : 'a t -> base t
(*  24*)	= fn self =>
(*  24*)	     make (GObject.withPtr
(*  24*)		     (self, fn self => get_modifier_style_ self))
(*  24*)    val set_composite_name_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_widget_set_composite_name"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_composite_name : 'a t -> string -> unit
(*  24*)	= fn self => fn name =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_composite_name_
(*  24*)			     (self, CString.fromString name))
(*  24*)    val get_composite_name_ : cptr -> CString.t
(*  24*)	= _import "gtk_widget_get_composite_name" : cptr -> CString.t;
(*  24*)    val get_composite_name : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_composite_name_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val reset_rc_styles_ : cptr -> unit
(*  24*)	= _import "gtk_widget_reset_rc_styles" : cptr -> unit;
(*  24*)    val reset_rc_styles : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => reset_rc_styles_ self)
(*  24*)    val push_composite_child_ : unit -> unit
(*  24*)	= _import "gtk_widget_push_composite_child" : unit -> unit;
(*  24*)    val push_composite_child : unit -> unit
(*  24*)	= fn dummy => push_composite_child_ dummy
(*  24*)    val pop_composite_child_ : unit -> unit
(*  24*)	= _import "gtk_widget_pop_composite_child" : unit -> unit;
(*  24*)    val pop_composite_child : unit -> unit
(*  24*)	= fn dummy => pop_composite_child_ dummy
(*  24*)    val pop_colormap_ : unit -> unit
(*  24*)	= _import "gtk_widget_pop_colormap" : unit -> unit;
(*  24*)    val pop_colormap : unit -> unit = fn dummy => pop_colormap_ dummy
(*  24*)    val style_get_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_widget_style_get"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val style_get : 'a t -> string -> unit
(*  24*)	= fn self => fn first_property_name =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => style_get_ (self, 
(*  24*)				       CString.fromString
(*  24*)					 first_property_name))
(*  24*)    val get_default_style_ : unit -> cptr
(*  24*)	= _import "gtk_widget_get_default_style" : unit -> cptr;
(*  24*)    val get_default_style : unit -> base t
(*  24*)	= fn dummy => make (get_default_style_ dummy)
(*  24*)    val set_direction_ : cptr * int -> unit
(*  24*)	= _import "gtk_widget_set_direction" : cptr * int -> unit;
(*  24*)    val set_direction : 'a t -> text_direction -> unit
(*  24*)	= fn self => fn dir =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_direction_ (self, dir))
(*  24*)    val get_direction_ : cptr -> int
(*  24*)	= _import "gtk_widget_get_direction" : cptr -> int;
(*  24*)    val get_direction : 'a t -> text_direction
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_direction_ self)
(*  24*)    val set_default_direction_ : int -> unit
(*  24*)	= _import "gtk_widget_set_default_direction" : int -> unit;
(*  24*)    val set_default_direction : text_direction -> unit
(*  24*)	= fn dir => set_default_direction_ dir
(*  24*)    val get_default_direction_ : unit -> int
(*  24*)	= _import "gtk_widget_get_default_direction" : unit -> int;
(*  24*)    val get_default_direction : unit -> text_direction
(*  24*)	= fn dummy => get_default_direction_ dummy
(*  24*)    val reset_shapes_ : cptr -> unit
(*  24*)	= _import "gtk_widget_reset_shapes" : cptr -> unit;
(*  24*)    val reset_shapes : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => reset_shapes_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in
(*  24*)      val show_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "show" false (void --> return_void) f
(*  24*)      val hide_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "hide" false (void --> return_void) f
(*  24*)      val map_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "map" false (void --> return_void) f
(*  24*)      val unmap_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "unmap" false (void --> return_void) f
(*  24*)      val realize_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "realize" false (void --> return_void) f
(*  24*)      val unrealize_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "unrealize" false (void --> return_void) f
(*  24*)      val size_request_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f =>
(*  24*)	       signal "size-request" false (unit --> return_void) f
(*  24*)      val size_allocate_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f =>
(*  24*)	       signal "size-allocate" false (unit --> return_void) f
(*  24*)      val state_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f =>
(*  24*)	       signal "state-changed" false (unit --> return_void) f
(*  24*)      val parent_set_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "parent-set" false (unit --> return_void) f
(*  24*)      val hierarchy_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "hierarchy-changed" false
(*  24*)			   (unit --> return_void) f
(*  24*)      val style_set_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "style-set" false (unit --> return_void) f
(*  24*)      val direction_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "direction-changed" false
(*  24*)			   (unit --> return_void) f
(*  24*)      val grab_notify_sig : (bool -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "grab-notify" false (bool --> return_void) f
(*  24*)      val child_notify_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f =>
(*  24*)	       signal "child-notify" false (unit --> return_void) f
(*  24*)      val mnemonic_activate_sig : (bool -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "mnemonic-activate" false
(*  24*)			   (bool --> return_bool) f
(*  24*)      val grab_focus_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "grab-focus" false (void --> return_void) f
(*  24*)      val focus_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "focus" false (unit --> return_bool) f
(*  24*)      val event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "event" false (unit --> return_bool) f
(*  24*)      val event_after_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "event-after" false (unit --> return_void) f
(*  24*)      val button_press_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "button-press-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val button_release_event_sig
(*  24*)	: (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "button-release-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val scroll_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f =>
(*  24*)	       signal "scroll-event" false (unit --> return_bool) f
(*  24*)      val motion_notify_event_sig
(*  24*)	: (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "motion-notify-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val delete_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f =>
(*  24*)	       signal "delete-event" false (unit --> return_bool) f
(*  24*)      val destroy_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f =>
(*  24*)	       signal "destroy-event" false (unit --> return_bool) f
(*  24*)      val expose_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f =>
(*  24*)	       signal "expose-event" false (unit --> return_bool) f
(*  24*)      val key_press_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "key-press-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val key_release_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "key-release-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val enter_notify_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "enter-notify-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val leave_notify_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "leave-notify-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val configure_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "configure-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val focus_in_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "focus-in-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val focus_out_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "focus-out-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val map_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "map-event" false (unit --> return_bool) f
(*  24*)      val unmap_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "unmap-event" false (unit --> return_bool) f
(*  24*)      val property_notify_event_sig
(*  24*)	: (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "property-notify-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val selection_clear_event_sig
(*  24*)	: (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "selection-clear-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val selection_request_event_sig
(*  24*)	: (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "selection-request-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val selection_notify_event_sig
(*  24*)	: (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "selection-notify-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val selection_received_sig
(*  24*)	: (unit -> int -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "selection-received" false
(*  24*)			   (unit --> int --> return_void) f
(*  24*)      val selection_get_sig : (unit -> int -> int -> unit)
(*  24*)			      -> 'a t Signal.signal
(*  24*)	  = fn f => signal "selection-get" false
(*  24*)			   (unit --> int --> int --> return_void) f
(*  24*)      val proximity_in_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "proximity-in-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val proximity_out_event_sig
(*  24*)	: (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "proximity-out-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val drag_leave_sig : (unit -> int -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "drag-leave" false
(*  24*)			   (unit --> int --> return_void) f
(*  24*)      val drag_begin_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "drag-begin" false (unit --> return_void) f
(*  24*)      val drag_end_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "drag-end" false (unit --> return_void) f
(*  24*)      val drag_data_delete_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "drag-data-delete" false
(*  24*)			   (unit --> return_void) f
(*  24*)      val drag_motion_sig : (unit -> int -> int -> int -> bool)
(*  24*)			    -> 'a t Signal.signal
(*  24*)	  = fn f =>
(*  24*)	       signal "drag-motion" false
(*  24*)		      (unit --> int --> int --> int --> return_bool) f
(*  24*)      val drag_drop_sig : (unit -> int -> int -> int -> bool)
(*  24*)			  -> 'a t Signal.signal
(*  24*)	  = fn f =>
(*  24*)	       signal "drag-drop" false
(*  24*)		      (unit --> int --> int --> int --> return_bool) f
(*  24*)      val drag_data_get_sig : (unit -> unit -> int -> int -> unit)
(*  24*)			      -> 'a t Signal.signal
(*  24*)	  = fn f =>
(*  24*)	       signal "drag-data-get" false
(*  24*)		      (unit --> unit --> int --> int --> return_void) f
(*  24*)      val drag_data_received_sig
(*  24*)	: (unit -> int -> int -> unit -> int -> int -> unit)
(*  24*)	  -> 'a t Signal.signal
(*  24*)	  = fn f => signal 
(*  51*)"drag-data-received" false
(*  51*) (unit --> int --> int --> unit --> int --> int --> return_void) f
(*  24*)      val visibility_notify_event_sig
(*  24*)	: (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "visibility-notify-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val client_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f =>
(*  24*)	       signal "client-event" false (unit --> return_bool) f
(*  24*)      val no_expose_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "no-expose-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val window_state_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "window-state-event" false
(*  24*)			   (unit --> return_bool) f
(*  24*)      val popup_menu_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "popup-menu" false (void --> return_bool) f
(*  24*)      val show_help_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "show-help" false (unit --> return_bool) f
(*  24*)      val accel_closures_changed_sig
(*  24*)	: (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "accel-closures-changed" false
(*  24*)			   (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Editable :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a editable_t
(*  24*)    type 'a t = 'a editable_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toEditable : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val select_region : 'a t -> int -> int -> unit
(*  24*)    val delete_text : 'a t -> int -> int -> unit
(*  24*)    val get_chars : 'a t -> int -> int -> string
(*  24*)    val cut_clipboard : 'a t -> unit
(*  24*)    val copy_clipboard : 'a t -> unit
(*  24*)    val paste_clipboard : 'a t -> unit
(*  24*)    val delete_selection : 'a t -> unit
(*  24*)    val set_position : 'a t -> int -> unit
(*  24*)    val get_position : 'a t -> int
(*  24*)    val set_editable : 'a t -> bool -> unit
(*  24*)    val get_editable : 'a t -> bool
(*  24*)    val changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a editable_t = unit
(*  24*)    type 'a t = 'a editable_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toEditable obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_editable_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val select_region_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_editable_select_region"
(*  24*)		  : cptr * int * int -> unit;
(*  24*)    val select_region : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn start => fn en =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => select_region_ (self, start, en))
(*  24*)    val delete_text_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_editable_delete_text"
(*  24*)		  : cptr * int * int -> unit;
(*  24*)    val delete_text : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn start_pos => fn end_pos =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => delete_text_ (self, start_pos, end_pos))
(*  24*)    val get_chars_ : cptr * int * int -> CString.t
(*  24*)	= _import "gtk_editable_get_chars"
(*  24*)		  : cptr * int * int -> CString.t;
(*  24*)    val get_chars : 'a t -> int -> int -> string
(*  24*)	= fn self => fn start_pos => fn end_pos =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => let val t = get_chars_
(*  24*)					 (self, start_pos, end_pos)
(*  24*)			   in CString.toString t end)
(*  24*)    val cut_clipboard_ : cptr -> unit
(*  24*)	= _import "gtk_editable_cut_clipboard" : cptr -> unit;
(*  24*)    val cut_clipboard : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => cut_clipboard_ self)
(*  24*)    val copy_clipboard_ : cptr -> unit
(*  24*)	= _import "gtk_editable_copy_clipboard" : cptr -> unit;
(*  24*)    val copy_clipboard : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => copy_clipboard_ self)
(*  24*)    val paste_clipboard_ : cptr -> unit
(*  24*)	= _import "gtk_editable_paste_clipboard" : cptr -> unit;
(*  24*)    val paste_clipboard : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => paste_clipboard_ self)
(*  24*)    val delete_selection_ : cptr -> unit
(*  24*)	= _import "gtk_editable_delete_selection" : cptr -> unit;
(*  24*)    val delete_selection : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => delete_selection_ self)
(*  24*)    val set_position_ : cptr * int -> unit
(*  24*)	= _import "gtk_editable_set_position" : cptr * int -> unit;
(*  24*)    val set_position : 'a t -> int -> unit
(*  24*)	= fn self => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_position_ (self, position))
(*  24*)    val get_position_ : cptr -> int
(*  24*)	= _import "gtk_editable_get_position" : cptr -> int;
(*  24*)    val get_position : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_position_ self)
(*  24*)    val set_editable_ : cptr * bool -> unit
(*  24*)	= _import "gtk_editable_set_editable" : cptr * bool -> unit;
(*  24*)    val set_editable : 'a t -> bool -> unit
(*  24*)	= fn self => fn is_editable =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_editable_ (self, is_editable))
(*  24*)    val get_editable_ : cptr -> bool
(*  24*)	= _import "gtk_editable_get_editable" : cptr -> bool;
(*  24*)    val get_editable : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_editable_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "changed" false (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure ItemFactory :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a itemfactory_t
(*  24*)    type 'a t = 'a itemfactory_t Object.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toItemFactory : 'a t -> base t
(*  24*)    val get_item : 'a t -> string -> base Widget.t
(*  24*)    val get_widget : 'a t -> string -> base Widget.t
(*  24*)    val get_widget_by_action : 'a t -> int -> base Widget.t
(*  24*)    val get_item_by_action : 'a t -> int -> base Widget.t
(*  24*)    val delete_item : 'a t -> string -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a itemfactory_t = unit
(*  24*)    type 'a t = 'a itemfactory_t Object.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Object.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toItemFactory obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_item_ : cptr * CString.cstring -> cptr
(*  24*)	= _import "gtk_item_factory_get_item"
(*  24*)		  : cptr * CString.cstring -> cptr;
(*  24*)    val get_item : 'a t -> string -> base Widget.t
(*  24*)	= fn self => fn path =>
(*  24*)	     Widget.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, 
(*  24*)			    fn self =>
(*  24*)			       get_item_
(*  24*)				 (self, CString.fromString path)))
(*  24*)    val get_widget_ : cptr * CString.cstring -> cptr
(*  24*)	= _import "gtk_item_factory_get_widget"
(*  24*)		  : cptr * CString.cstring -> cptr;
(*  24*)    val get_widget : 'a t -> string -> base Widget.t
(*  24*)	= fn self => fn path =>
(*  24*)	     Widget.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, 
(*  24*)			    fn self =>
(*  24*)			       get_widget_
(*  24*)				 (self, CString.fromString path)))
(*  24*)    val get_widget_by_action_ : cptr * int -> cptr
(*  24*)	= _import "gtk_item_factory_get_widget_by_action"
(*  24*)		  : cptr * int -> cptr;
(*  24*)    val get_widget_by_action : 'a t -> int -> base Widget.t
(*  24*)	= fn self => fn action =>
(*  24*)	     Widget.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, 
(*  24*)			    fn self => get_widget_by_action_
(*  24*)					 (self, action)))
(*  24*)    val get_item_by_action_ : cptr * int -> cptr
(*  24*)	= _import "gtk_item_factory_get_item_by_action"
(*  24*)		  : cptr * int -> cptr;
(*  24*)    val get_item_by_action : 'a t -> int -> base Widget.t
(*  24*)	= fn self => fn action =>
(*  24*)	     Widget.inherit ()
(*  24*)			    (fn () => GObject.withPtr
(*  24*)					(self, 
(*  24*)					 fn self => get_item_by_action_
(*  24*)						      (self, action)))
(*  24*)    val delete_item_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_item_factory_delete_item"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val delete_item : 'a t -> string -> unit
(*  24*)	= fn self => fn path =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => delete_item_
(*  24*)			     (self, CString.fromString path))
(*  24*)end
(*  24*)structure IMContext :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a imcontext_t
(*  24*)    type 'a t = 'a imcontext_t Object.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toIMContext : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val focus_in : 'a t -> unit
(*  24*)    val focus_out : 'a t -> unit
(*  24*)    val reset : 'a t -> unit
(*  24*)    val set_use_preedit : 'a t -> bool -> unit
(*  24*)    val set_surrounding : 'a t -> string -> int -> int -> unit
(*  24*)    val delete_surrounding : 'a t -> int -> int -> bool
(*  24*)    val simple_get_type : unit -> int
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a imcontext_t = unit
(*  24*)    type 'a t = 'a imcontext_t Object.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Object.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toIMContext obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_im_context_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val focus_in_ : cptr -> unit
(*  24*)	= _import "gtk_im_context_focus_in" : cptr -> unit;
(*  24*)    val focus_in : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => focus_in_ self)
(*  24*)    val focus_out_ : cptr -> unit
(*  24*)	= _import "gtk_im_context_focus_out" : cptr -> unit;
(*  24*)    val focus_out : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => focus_out_ self)
(*  24*)    val reset_ : cptr -> unit
(*  24*)	= _import "gtk_im_context_reset" : cptr -> unit;
(*  24*)    val reset : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => reset_ self)
(*  24*)    val set_use_preedit_ : cptr * bool -> unit
(*  24*)	= _import "gtk_im_context_set_use_preedit"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_use_preedit : 'a t -> bool -> unit
(*  24*)	= fn self => fn use_preedit =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_use_preedit_ (self, use_preedit))
(*  24*)    val set_surrounding_ : cptr * CString.cstring * int * int -> unit
(*  24*)	= _import "gtk_im_context_set_surrounding"
(*  24*)		  : cptr * CString.cstring * int * int -> unit;
(*  24*)    val set_surrounding : 'a t -> string -> int -> int -> unit
(*  24*)	= fn self => fn text => fn len => fn cursor_index =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_surrounding_
(*  24*)			     (self, CString.fromString text, len, 
(*  24*)			      cursor_index))
(*  24*)    val delete_surrounding_ : cptr * int * int -> bool
(*  24*)	= _import "gtk_im_context_delete_surrounding"
(*  24*)		  : cptr * int * int -> bool;
(*  24*)    val delete_surrounding : 'a t -> int -> int -> bool
(*  24*)	= fn self => fn offset => fn n_chars =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => delete_surrounding_
(*  24*)					   (self, offset, n_chars))
(*  24*)    val simple_get_type_ : unit -> int
(*  24*)	= _import "gtk_im_context_simple_get_type" : unit -> int;
(*  24*)    val simple_get_type : unit -> int
(*  24*)	= fn dummy => simple_get_type_ dummy
(*  24*)end
(*  24*)structure IMContextSimple :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a imcontextsimple_t
(*  24*)    type 'a t = 'a imcontextsimple_t IMContext.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toIMContextSimple : 'a t -> base t
(*  24*)    val new : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a imcontextsimple_t = unit
(*  24*)    type 'a t = 'a imcontextsimple_t IMContext.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in IMContext.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toIMContextSimple obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_im_context_simple_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)end
(*  24*)structure IMMulticontext :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a immulticontext_t
(*  24*)    type 'a t = 'a immulticontext_t IMContext.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toIMMulticontext : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val append_menuitems : 'a t -> 'b t -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a immulticontext_t = unit
(*  24*)    type 'a t = 'a immulticontext_t IMContext.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in IMContext.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toIMMulticontext obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_im_multicontext_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_im_multicontext_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val append_menuitems_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_im_multicontext_append_menuitems"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val append_menuitems : 'a t -> 'b t -> unit
(*  24*)	= fn self => fn menushell =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (menushell, 
(*  24*)			      fn menushell => append_menuitems_
(*  24*)						(self, menushell)))
(*  24*)end
(*  24*)structure CellRenderer :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a cellrenderer_t
(*  24*)    type 'a t = 'a cellrenderer_t Object.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toCellRenderer : 'a t -> base t
(*  24*)    type state
(*  24*)    val CELL_RENDERER_SELECTED : state
(*  24*)    val CELL_RENDERER_PRELIT : state
(*  24*)    val CELL_RENDERER_INSENSITIVE : state
(*  24*)    val CELL_RENDERER_SORTED : state
(*  24*)    type mode
(*  24*)    val CELL_RENDERER_MODE_INERT : mode
(*  24*)    val CELL_RENDERER_MODE_ACTIVATABLE : mode
(*  24*)    val CELL_RENDERER_MODE_EDITABLE : mode
(*  24*)    val get_type : unit -> int
(*  24*)    val set_fixed_size : 'a t -> int -> int -> unit
(*  24*)    val pixbuf_get_type : unit -> int
(*  24*)    val text_get_type : unit -> int
(*  24*)    val toggle_get_type : unit -> int
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a cellrenderer_t = unit
(*  24*)    type 'a t = 'a cellrenderer_t Object.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Object.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toCellRenderer obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    type state = int
(*  24*)    val get_state_ : int ref * int ref * int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_cellrenderer_state"
(*  24*)		  : int ref * int ref * int ref * int ref -> unit;
(*  24*)    val (CELL_RENDERER_SELECTED, CELL_RENDERER_PRELIT, 
(*  24*)	 CELL_RENDERER_INSENSITIVE, CELL_RENDERER_SORTED)
(*  24*)	= let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)	  in get_state_ (x0, x1, x2, x3)
(*  24*)	   ; (!x0, !x1, !x2, !x3)
(*  24*)	  end
(*  24*)    type mode = int
(*  24*)    val get_mode_ : int ref * int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_cellrenderer_mode"
(*  24*)		  : int ref * int ref * int ref -> unit;
(*  24*)    val (CELL_RENDERER_MODE_INERT, CELL_RENDERER_MODE_ACTIVATABLE, 
(*  24*)	 CELL_RENDERER_MODE_EDITABLE)
(*  24*)	= let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
(*  24*)	  in get_mode_ (x0, x1, x2)
(*  24*)	   ; (!x0, !x1, !x2)
(*  24*)	  end
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_cell_renderer_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val set_fixed_size_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_cell_renderer_set_fixed_size"
(*  24*)		  : cptr * int * int -> unit;
(*  24*)    val set_fixed_size : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn width => fn height =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_fixed_size_ (self, width, height))
(*  24*)    val pixbuf_get_type_ : unit -> int
(*  24*)	= _import "gtk_cell_renderer_pixbuf_get_type" : unit -> int;
(*  24*)    val pixbuf_get_type : unit -> int
(*  24*)	= fn dummy => pixbuf_get_type_ dummy
(*  24*)    val text_get_type_ : unit -> int
(*  24*)	= _import "gtk_cell_renderer_text_get_type" : unit -> int;
(*  24*)    val text_get_type : unit -> int = fn dummy => text_get_type_ dummy
(*  24*)    val toggle_get_type_ : unit -> int
(*  24*)	= _import "gtk_cell_renderer_toggle_get_type" : unit -> int;
(*  24*)    val toggle_get_type : unit -> int
(*  24*)	= fn dummy => toggle_get_type_ dummy
(*  24*)end
(*  24*)structure CellEditable :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a celleditable_t
(*  24*)    type 'a t = 'a celleditable_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toCellEditable : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val editing_done : 'a t -> unit
(*  24*)    val remove_widget : 'a t -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a celleditable_t = unit
(*  24*)    type 'a t = 'a celleditable_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toCellEditable obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_cell_editable_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val editing_done_ : cptr -> unit
(*  24*)	= _import "gtk_cell_editable_editing_done" : cptr -> unit;
(*  24*)    val editing_done : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => editing_done_ self)
(*  24*)    val remove_widget_ : cptr -> unit
(*  24*)	= _import "gtk_cell_editable_remove_widget" : cptr -> unit;
(*  24*)    val remove_widget : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => remove_widget_ self)
(*  24*)end
(*  24*)structure CellRendererToggle :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a cellrenderertoggle_t
(*  24*)    type 'a t = 'a cellrenderertoggle_t CellRenderer.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toCellRendererToggle : 'a t -> base t
(*  24*)    val new : unit -> base t
(*  24*)    val get_radio : 'a t -> bool
(*  24*)    val set_radio : 'a t -> bool -> unit
(*  24*)    val get_active : 'a t -> bool
(*  24*)    val set_active : 'a t -> bool -> unit
(*  24*)    val toggled_sig : (char -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a cellrenderertoggle_t = unit
(*  24*)    type 'a t = 'a cellrenderertoggle_t CellRenderer.t
(*  24*)    fun inherit w con
(*  24*)      = let val con = let val ptr = con () in fn () => ptr end
(*  24*)	    val witness = ()
(*  24*)	in CellRenderer.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toCellRendererToggle obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_cell_renderer_toggle_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val get_radio_ : cptr -> bool
(*  24*)	= _import "gtk_cell_renderer_toggle_get_radio" : cptr -> bool;
(*  24*)    val get_radio : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr (self, fn self => get_radio_ self)
(*  24*)    val set_radio_ : cptr * bool -> unit
(*  24*)	= _import "gtk_cell_renderer_toggle_set_radio"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_radio : 'a t -> bool -> unit
(*  24*)	= fn self => fn radio =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_radio_ (self, radio))
(*  24*)    val get_active_ : cptr -> bool
(*  24*)	= _import "gtk_cell_renderer_toggle_get_active" : cptr -> bool;
(*  24*)    val get_active : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_active_ self)
(*  24*)    val set_active_ : cptr * bool -> unit
(*  24*)	= _import "gtk_cell_renderer_toggle_set_active"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_active : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_active_ (self, setting))
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val toggled_sig : (char -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "toggled" false (char --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure CellRendererText :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a cellrenderertext_t
(*  24*)    type 'a t = 'a cellrenderertext_t CellRenderer.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toCellRendererText : 'a t -> base t
(*  24*)    val new : unit -> base t
(*  24*)    val set_fixed_height_from_font : 'a t -> int -> unit
(*  24*)    val edited_sig : (char -> char -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a cellrenderertext_t = unit
(*  24*)    type 'a t = 'a cellrenderertext_t CellRenderer.t
(*  24*)    fun inherit w con
(*  24*)      = let val con = let val ptr = con () in fn () => ptr end
(*  24*)	    val witness = ()
(*  24*)	in CellRenderer.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toCellRendererText obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_cell_renderer_text_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val set_fixed_height_from_font_ : cptr * int -> unit
(*  24*)	= _import "gtk_cell_renderer_text_set_fixed_height_from_font"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_fixed_height_from_font : 'a t -> int -> unit
(*  24*)	= fn self => fn number_of_rows =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_fixed_height_from_font_
(*  24*)					   (self, number_of_rows))
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val edited_sig : (char -> char -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "edited" false
(*  24*)			    (char --> char --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure CellRendererPixbuf :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a cellrendererpixbuf_t
(*  24*)    type 'a t = 'a cellrendererpixbuf_t CellRenderer.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toCellRendererPixbuf : 'a t -> base t
(*  24*)    val new : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a cellrendererpixbuf_t = unit
(*  24*)    type 'a t = 'a cellrendererpixbuf_t CellRenderer.t
(*  24*)    fun inherit w con
(*  24*)      = let val con = let val ptr = con () in fn () => ptr end
(*  24*)	    val witness = ()
(*  24*)	in CellRenderer.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toCellRendererPixbuf obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_cell_renderer_pixbuf_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)end
(*  24*)structure RcStyle :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a rcstyle_t
(*  24*)    type 'a t = 'a rcstyle_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toRcStyle : 'a t -> base t
(*  24*)    val rc_get_style_by_paths
(*  24*)      : 'a t -> string -> string -> int -> base t
(*  24*)    val rc_reparse_all_for_settings : 'a t -> bool -> bool
(*  24*)    val rc_parse : string -> unit
(*  24*)    val rc_parse_string : string -> unit
(*  24*)    val rc_reparse_all : unit -> bool
(*  24*)    val rc_add_widget_name_style : 'a t -> string -> unit
(*  24*)    val rc_add_widget_class_style : 'a t -> string -> unit
(*  24*)    val rc_add_class_style : 'a t -> string -> unit
(*  24*)    val rc_style_get_type : unit -> int
(*  24*)    val rc_style_copy : 'a t -> base t
(*  24*)    val rc_style_ref : 'a t -> unit
(*  24*)    val rc_style_unref : 'a t -> unit
(*  24*)    val rc_find_module_in_path : string -> string
(*  24*)    val rc_get_theme_dir : unit -> string
(*  24*)    val rc_get_module_dir : unit -> string
(*  24*)    val rc_get_im_module_path : unit -> string
(*  24*)    val rc_get_im_module_file : unit -> string
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a rcstyle_t = unit
(*  24*)    type 'a t = 'a rcstyle_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toRcStyle obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val rc_get_style_by_paths_
(*  24*)      : cptr * CString.cstring * CString.cstring * int -> cptr
(*  24*)	= _import "gtk_rc_get_style_by_paths"
(*  24*)		  : cptr * CString.cstring * CString.cstring * int
(*  24*)		    -> cptr;
(*  24*)    val rc_get_style_by_paths
(*  24*)      : 'a t -> string -> string -> int -> base t
(*  24*)	= fn settings => fn widget_path => fn class_path => fn typ =>
(*  24*)	     make (GObject.withPtr
(*  24*)		     (settings, 
(*  24*)		      fn settings =>
(*  24*)			 rc_get_style_by_paths_
(*  24*)			   (settings, CString.fromString widget_path, 
(*  24*)			    CString.fromString class_path, typ)))
(*  24*)    val rc_reparse_all_for_settings_ : cptr * bool -> bool
(*  24*)	= _import "gtk_rc_reparse_all_for_settings"
(*  24*)		  : cptr * bool -> bool;
(*  24*)    val rc_reparse_all_for_settings : 'a t -> bool -> bool
(*  24*)	= fn settings => fn force_load =>
(*  24*)	     GObject.withPtr
(*  24*)	       (settings, 
(*  24*)		fn settings => rc_reparse_all_for_settings_
(*  24*)				 (settings, force_load))
(*  24*)    val rc_parse_ : CString.cstring -> unit
(*  24*)	= _import "gtk_rc_parse" : CString.cstring -> unit;
(*  24*)    val rc_parse : string -> unit
(*  24*)	= fn filename => rc_parse_ (CString.fromString filename)
(*  24*)    val rc_parse_string_ : CString.cstring -> unit
(*  24*)	= _import "gtk_rc_parse_string" : CString.cstring -> unit;
(*  24*)    val rc_parse_string : string -> unit
(*  24*)	= fn rc_string => rc_parse_string_
(*  24*)			    (CString.fromString rc_string)
(*  24*)    val rc_reparse_all_ : unit -> bool
(*  24*)	= _import "gtk_rc_reparse_all" : unit -> bool;
(*  24*)    val rc_reparse_all : unit -> bool
(*  24*)	= fn dummy => rc_reparse_all_ dummy
(*  24*)    val rc_add_widget_name_style_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_rc_add_widget_name_style"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val rc_add_widget_name_style : 'a t -> string -> unit
(*  24*)	= fn self => fn pattern =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => rc_add_widget_name_style_
(*  24*)			     (self, CString.fromString pattern))
(*  24*)    val rc_add_widget_class_style_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_rc_add_widget_class_style"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val rc_add_widget_class_style : 'a t -> string -> unit
(*  24*)	= fn self => fn pattern =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => rc_add_widget_class_style_
(*  24*)			     (self, CString.fromString pattern))
(*  24*)    val rc_add_class_style_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_rc_add_class_style"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val rc_add_class_style : 'a t -> string -> unit
(*  24*)	= fn self => fn pattern =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => rc_add_class_style_
(*  24*)			     (self, CString.fromString pattern))
(*  24*)    val rc_style_get_type_ : unit -> int
(*  24*)	= _import "gtk_rc_style_get_type" : unit -> int;
(*  24*)    val rc_style_get_type : unit -> int
(*  24*)	= fn dummy => rc_style_get_type_ dummy
(*  24*)    val rc_style_copy_ : cptr -> cptr
(*  24*)	= _import "gtk_rc_style_copy" : cptr -> cptr;
(*  24*)    val rc_style_copy : 'a t -> base t
(*  24*)	= fn self => make (GObject.withPtr
(*  24*)			     (self, fn self => rc_style_copy_ self))
(*  24*)    val rc_style_ref_ : cptr -> unit
(*  24*)	= _import "gtk_rc_style_ref" : cptr -> unit;
(*  24*)    val rc_style_ref : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => rc_style_ref_ self)
(*  24*)    val rc_style_unref_ : cptr -> unit
(*  24*)	= _import "gtk_rc_style_unref" : cptr -> unit;
(*  24*)    val rc_style_unref : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => rc_style_unref_ self)
(*  24*)    val rc_find_module_in_path_ : CString.cstring -> CString.t
(*  24*)	= _import "gtk_rc_find_module_in_path"
(*  24*)		  : CString.cstring -> CString.t;
(*  24*)    val rc_find_module_in_path : string -> string
(*  24*)	= fn module_file =>
(*  24*)	     let val t = rc_find_module_in_path_
(*  24*)			   (CString.fromString module_file)
(*  24*)	     in CString.toString t end
(*  24*)    val rc_get_theme_dir_ : unit -> CString.t
(*  24*)	= _import "gtk_rc_get_theme_dir" : unit -> CString.t;
(*  24*)    val rc_get_theme_dir : unit -> string
(*  24*)	= fn dummy => let val t = rc_get_theme_dir_ dummy
(*  24*)		      in CString.toString t end
(*  24*)    val rc_get_module_dir_ : unit -> CString.t
(*  24*)	= _import "gtk_rc_get_module_dir" : unit -> CString.t;
(*  24*)    val rc_get_module_dir : unit -> string
(*  24*)	= fn dummy => let val t = rc_get_module_dir_ dummy
(*  24*)		      in CString.toString t end
(*  24*)    val rc_get_im_module_path_ : unit -> CString.t
(*  24*)	= _import "gtk_rc_get_im_module_path" : unit -> CString.t;
(*  24*)    val rc_get_im_module_path : unit -> string
(*  24*)	= fn dummy => let val t = rc_get_im_module_path_ dummy
(*  24*)		      in CString.toString t end
(*  24*)    val rc_get_im_module_file_ : unit -> CString.t
(*  24*)	= _import "gtk_rc_get_im_module_file" : unit -> CString.t;
(*  24*)    val rc_get_im_module_file : unit -> string
(*  24*)	= fn dummy => let val t = rc_get_im_module_file_ dummy
(*  24*)		      in CString.toString t end
(*  24*)end
(*  24*)structure Settings :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a settings_t
(*  24*)    type 'a t = 'a settings_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toSettings : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val get_default : unit -> base t
(*  24*)    val set_string_property
(*  24*)      : 'a t -> string -> string -> string -> unit
(*  24*)    val set_double_property : 'a t -> string -> real -> string -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a settings_t = unit
(*  24*)    type 'a t = 'a settings_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toSettings obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_settings_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val get_default_ : unit -> cptr
(*  24*)	= _import "gtk_settings_get_default" : unit -> cptr;
(*  24*)    val get_default : unit -> base t
(*  24*)	= fn dummy => make (get_default_ dummy)
(*  24*)    val set_string_property_ : cptr * CString.cstring 
(*  24*)			     * CString.cstring * CString.cstring
(*  24*)			       -> unit
(*  24*)	= _import "gtk_settings_set_string_property"
(*  24*)		  : cptr * CString.cstring * CString.cstring 
(*  24*)		  * CString.cstring
(*  24*)		    -> unit;
(*  24*)    val set_string_property
(*  24*)      : 'a t -> string -> string -> string -> unit
(*  24*)	= fn self => fn name => fn v_string => fn origin =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_string_property_
(*  24*)			     (self, CString.fromString name, 
(*  24*)			      CString.fromString v_string, 
(*  24*)			      CString.fromString origin))
(*  24*)    val set_double_property_
(*  24*)      : cptr * CString.cstring * real * CString.cstring -> unit
(*  24*)	= _import "gtk_settings_set_double_property"
(*  24*)		  : cptr * CString.cstring * real * CString.cstring
(*  24*)		    -> unit;
(*  24*)    val set_double_property : 'a t -> string -> real -> string -> unit
(*  24*)	= fn self => fn name => fn v_double => fn origin =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_double_property_
(*  24*)			     (self, CString.fromString name, v_double, 
(*  24*)			      CString.fromString origin))
(*  24*)end
(*  24*)structure SizeGroup :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a sizegroup_t
(*  24*)    type 'a t = 'a sizegroup_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toSizeGroup : 'a t -> base t
(*  24*)    type mode
(*  24*)    val NON : mode
(*  24*)    val HORIZONTAL : mode
(*  24*)    val VERTICAL : mode
(*  24*)    val BOTH : mode
(*  24*)    val get_type : unit -> int
(*  24*)    val new : mode -> base t
(*  24*)    val set_mode : 'a t -> mode -> unit
(*  24*)    val get_mode : 'a t -> mode
(*  24*)    val add_widget : 'a t -> 'b Widget.t -> unit
(*  24*)    val remove_widget : 'a t -> 'b Widget.t -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a sizegroup_t = unit
(*  24*)    type 'a t = 'a sizegroup_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toSizeGroup obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    type mode = int
(*  24*)    val get_mode_ : int ref * int ref * int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_size_group_mode"
(*  24*)		  : int ref * int ref * int ref * int ref -> unit;
(*  24*)    val (NON, HORIZONTAL, VERTICAL, BOTH)
(*  24*)	= let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)	  in get_mode_ (x0, x1, x2, x3)
(*  24*)	   ; (!x0, !x1, !x2, !x3)
(*  24*)	  end
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_size_group_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : int -> cptr
(*  24*)	= _import "gtk_size_group_new" : int -> cptr;
(*  24*)    val new : mode -> base t = fn mode => make (new_ mode)
(*  24*)    val set_mode_ : cptr * int -> unit
(*  24*)	= _import "gtk_size_group_set_mode" : cptr * int -> unit;
(*  24*)    val set_mode : 'a t -> mode -> unit
(*  24*)	= fn self => fn mode =>
(*  24*)	     GObject.withPtr (self, fn self => set_mode_ (self, mode))
(*  24*)    val get_mode_ : cptr -> int
(*  24*)	= _import "gtk_size_group_get_mode" : cptr -> int;
(*  24*)    val get_mode : 'a t -> mode
(*  24*)	= fn self => GObject.withPtr (self, fn self => get_mode_ self)
(*  24*)    val add_widget_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_size_group_add_widget" : cptr * cptr -> unit;
(*  24*)    val add_widget : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn widget =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (widget, fn widget => add_widget_ (self, widget)))
(*  24*)    val remove_widget_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_size_group_remove_widget" : cptr * cptr -> unit;
(*  24*)    val remove_widget : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn widget =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (widget, 
(*  24*)			      fn widget => remove_widget_
(*  24*)					     (self, widget)))
(*  24*)end
(*  24*)structure Style :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a style_t
(*  24*)    type 'a t = 'a style_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toStyle : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val copy : 'a t -> base t
(*  24*)    val detach : 'a t -> unit
(*  24*)    val refe : 'a t -> base t
(*  24*)    val unref : 'a t -> unit
(*  24*)    val lookup_icon_set : 'a t -> string -> icon_set
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a style_t = unit
(*  24*)    type 'a t = 'a style_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toStyle obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_style_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr = _import "gtk_style_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val copy_ : cptr -> cptr = _import "gtk_style_copy" : cptr -> cptr;
(*  24*)    val copy : 'a t -> base t
(*  24*)	= fn self => make (GObject.withPtr
(*  24*)			     (self, fn self => copy_ self))
(*  24*)    val detach_ : cptr -> unit
(*  24*)	= _import "gtk_style_detach" : cptr -> unit;
(*  24*)    val detach : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => detach_ self)
(*  24*)    val ref_ : cptr -> cptr = _import "gtk_style_ref" : cptr -> cptr;
(*  24*)    val refe : 'a t -> base t
(*  24*)	= fn self => make (GObject.withPtr
(*  24*)			     (self, fn self => ref_ self))
(*  24*)    val unref_ : cptr -> unit
(*  24*)	= _import "gtk_style_unref" : cptr -> unit;
(*  24*)    val unref : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => unref_ self)
(*  24*)    val lookup_icon_set_ : cptr * CString.cstring -> cptr
(*  24*)	= _import "gtk_style_lookup_icon_set"
(*  24*)		  : cptr * CString.cstring -> cptr;
(*  24*)    val lookup_icon_set : 'a t -> string -> icon_set
(*  24*)	= fn self => fn stock_id =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => lookup_icon_set_
(*  24*)			     (self, CString.fromString stock_id))
(*  24*)end
(*  24*)structure TextBuffer :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a textbuffer_t
(*  24*)    type 'a t = 'a textbuffer_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTextBuffer : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : 'a t option -> base t
(*  24*)    val new' : unit -> base t
(*  24*)    val get_line_count : 'a t -> int
(*  24*)    val get_char_count : 'a t -> int
(*  24*)    val get_tag_table : 'a t -> base t
(*  24*)    val set_text : 'a t -> string -> int -> unit
(*  24*)    val insert : 'a t -> textiter -> string -> int option -> unit
(*  24*)    val insert' : 'a t -> textiter -> string -> unit
(*  24*)    val insert_at_cursor : 'a t -> string -> int option -> unit
(*  24*)    val insert_at_cursor' : 'a t -> string -> unit
(*  24*)    val insert_interactive
(*  24*)      : 'a t -> textiter -> string -> int -> bool -> bool
(*  24*)    val insert_interactive_at_cursor
(*  24*)      : 'a t -> string -> int -> bool -> bool
(*  24*)    val insert_range : 'a t -> textiter -> textiter -> textiter -> unit
(*  24*)    val insert_range_interactive
(*  24*)      : 'a t -> textiter -> textiter -> textiter -> bool -> bool
(*  24*)    val insert_with_tags
(*  24*)      : 'a t -> textiter -> string -> int -> 'b t -> unit
(*  24*)    val insert_with_tags_by_name
(*  24*)      : 'a t -> textiter -> string -> int -> string -> unit
(*  24*)    val delete : 'a t -> textiter -> textiter -> unit
(*  24*)    val delete_interactive
(*  24*)      : 'a t -> textiter -> textiter -> bool -> bool
(*  24*)    val get_text
(*  24*)      : 'a t -> textiter -> textiter -> bool option -> string
(*  24*)    val get_text' : 'a t -> textiter -> textiter -> string
(*  24*)    val get_slice
(*  24*)      : 'a t -> textiter -> textiter -> bool option -> string
(*  24*)    val get_slice' : 'a t -> textiter -> textiter -> string
(*  24*)    val insert_child_anchor : 'a t -> textiter -> 'b t -> unit
(*  24*)    val create_child_anchor : 'a t -> textiter -> base t
(*  24*)    val create_mark
(*  24*)      : 'a t -> string option -> textiter -> bool option -> base t
(*  24*)    val create_mark' : 'a t -> textiter -> base t
(*  24*)    val move_mark : 'a t -> 'b t -> textiter -> unit
(*  24*)    val delete_mark : 'a t -> 'b t -> unit
(*  24*)    val get_mark : 'a t -> string -> base t
(*  24*)    val move_mark_by_name : 'a t -> string -> textiter -> unit
(*  24*)    val delete_mark_by_name : 'a t -> string -> unit
(*  24*)    val get_insert : 'a t -> base t
(*  24*)    val get_selection_bound : 'a t -> base t
(*  24*)    val place_cursor : 'a t -> textiter -> unit
(*  24*)    val apply_tag : 'a t -> 'b t -> textiter -> textiter -> unit
(*  24*)    val remove_tag : 'a t -> 'b t -> textiter -> textiter -> unit
(*  24*)    val apply_tag_by_name
(*  24*)      : 'a t -> string -> textiter -> textiter -> unit
(*  24*)    val remove_tag_by_name
(*  24*)      : 'a t -> string -> textiter -> textiter -> unit
(*  24*)    val remove_all_tags : 'a t -> textiter -> textiter -> unit
(*  24*)    val create_tag : 'a t -> string -> string -> base t
(*  24*)    val getiter_at_line_offset : 'a t -> textiter -> int -> int -> unit
(*  24*)    val getiter_at_line_index : 'a t -> textiter -> int -> int -> unit
(*  24*)    val getiter_at_offset : 'a t -> textiter -> int -> unit
(*  24*)    val getiter_at_line : 'a t -> textiter -> int -> unit
(*  24*)    val get_startiter : 'a t -> textiter -> unit
(*  24*)    val get_enditer : 'a t -> textiter -> unit
(*  24*)    val get_bounds : 'a t -> textiter -> textiter -> unit
(*  24*)    val getiter_at_mark : 'a t -> textiter -> 'b t -> unit
(*  24*)    val getiter_at_child_anchor : 'a t -> textiter -> 'b t -> unit
(*  24*)    val get_modified : 'a t -> bool
(*  24*)    val set_modified : 'a t -> bool -> unit
(*  24*)    val add_selection_clipboard : 'a t -> 'b t -> unit
(*  24*)    val remove_selection_clipboard : 'a t -> 'b t -> unit
(*  24*)    val cut_clipboard : 'a t -> 'b t -> bool -> unit
(*  24*)    val copy_clipboard : 'a t -> 'b t -> unit
(*  24*)    val paste_clipboard : 'a t -> 'b t -> textiter -> bool -> unit
(*  24*)    val get_selection_bounds : 'a t -> textiter -> textiter -> bool
(*  24*)    val delete_selection : 'a t -> bool -> bool -> bool
(*  24*)    val begin_user_action : 'a t -> unit
(*  24*)    val end_user_action : 'a t -> unit
(*  24*)    val changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val insert_text_sig : (unit -> char -> int -> unit)
(*  24*)			  -> 'a t Signal.signal
(*  24*)    val insert_pixbuf_sig : (unit -> unit -> unit)
(*  24*)			    -> 'a t Signal.signal
(*  24*)    val insert_child_anchor_sig
(*  24*)      : (unit -> unit -> unit) -> 'a t Signal.signal
(*  24*)    val delete_range_sig : (unit -> unit -> unit) -> 'a t Signal.signal
(*  24*)    val modified_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val mark_set_sig : (unit -> unit -> unit) -> 'a t Signal.signal
(*  24*)    val mark_deleted_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val apply_tag_sig : (unit -> unit -> unit -> unit)
(*  24*)			-> 'a t Signal.signal
(*  24*)    val remove_tag_sig : (unit -> unit -> unit -> unit)
(*  24*)			 -> 'a t Signal.signal
(*  24*)    val begin_user_action_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val end_user_action_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a textbuffer_t = unit
(*  24*)    type 'a t = 'a textbuffer_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTextBuffer obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_text_buffer_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : cptr -> cptr
(*  24*)	= _import "gtk_text_buffer_new" : cptr -> cptr;
(*  24*)    val new : 'a t option -> base t
(*  24*)	= fn table => make (GObject.withOpt
(*  24*)			      (table, fn table => new_ table))
(*  24*)    val new' : unit -> base t = fn dummy => make (new_ GObject.null)
(*  24*)    val get_line_count_ : cptr -> int
(*  24*)	= _import "gtk_text_buffer_get_line_count" : cptr -> int;
(*  24*)    val get_line_count : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_line_count_ self)
(*  24*)    val get_char_count_ : cptr -> int
(*  24*)	= _import "gtk_text_buffer_get_char_count" : cptr -> int;
(*  24*)    val get_char_count : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_char_count_ self)
(*  24*)    val get_tag_table_ : cptr -> cptr
(*  24*)	= _import "gtk_text_buffer_get_tag_table" : cptr -> cptr;
(*  24*)    val get_tag_table : 'a t -> base t
(*  24*)	= fn self => make (GObject.withPtr
(*  24*)			     (self, fn self => get_tag_table_ self))
(*  24*)    val set_text_ : cptr * CString.cstring * int -> unit
(*  24*)	= _import "gtk_text_buffer_set_text"
(*  24*)		  : cptr * CString.cstring * int -> unit;
(*  24*)    val set_text : 'a t -> string -> int -> unit
(*  24*)	= fn self => fn text => fn len =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   set_text_ (self, CString.fromString text, len))
(*  24*)    val insert_ : cptr * cptr * CString.cstring * int -> unit
(*  24*)	= _import "gtk_text_buffer_insert"
(*  24*)		  : cptr * cptr * CString.cstring * int -> unit;
(*  24*)    val insert : 'a t -> textiter -> string -> int option -> unit
(*  24*)	= fn self => fn iter => fn text => fn len =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => insert_ (self, iter, 
(*  24*)				    CString.fromString text, 
(*  24*)				    getOpt (len, ~1)))
(*  24*)    val insert' : 'a t -> textiter -> string -> unit
(*  24*)	= fn self => fn iter => fn text =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => insert_ (self, iter, 
(*  24*)				    CString.fromString text, ~1))
(*  24*)    val insert_at_cursor_ : cptr * CString.cstring * int -> unit
(*  24*)	= _import "gtk_text_buffer_insert_at_cursor"
(*  24*)		  : cptr * CString.cstring * int -> unit;
(*  24*)    val insert_at_cursor : 'a t -> string -> int option -> unit
(*  24*)	= fn self => fn text => fn len =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => insert_at_cursor_
(*  24*)			     (self, CString.fromString text, 
(*  24*)			      getOpt (len, ~1)))
(*  24*)    val insert_at_cursor' : 'a t -> string -> unit
(*  24*)	= fn self => fn text =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => insert_at_cursor_
(*  24*)			     (self, CString.fromString text, ~1))
(*  24*)    val insert_interactive_
(*  24*)      : cptr * cptr * CString.cstring * int * bool -> bool
(*  24*)	= _import "gtk_text_buffer_insert_interactive"
(*  24*)		  : cptr * cptr * CString.cstring * int * bool -> bool;
(*  24*)    val insert_interactive
(*  24*)      : 'a t -> textiter -> string -> int -> bool -> bool
(*  24*)	= fn self => fn iter => fn text => fn len => 
(*  24*)	  fn default_editable =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => insert_interactive_
(*  24*)			     (self, iter, CString.fromString text, 
(*  24*)			      len, default_editable))
(*  24*)    val insert_interactive_at_cursor_
(*  24*)      : cptr * CString.cstring * int * bool -> bool
(*  24*)	= _import "gtk_text_buffer_insert_interactive_at_cursor"
(*  24*)		  : cptr * CString.cstring * int * bool -> bool;
(*  24*)    val insert_interactive_at_cursor
(*  24*)      : 'a t -> string -> int -> bool -> bool
(*  24*)	= fn self => fn text => fn len => fn default_editable =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => insert_interactive_at_cursor_
(*  24*)			     (self, CString.fromString text, len, 
(*  24*)			      default_editable))
(*  24*)    val insert_range_ : cptr * cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_insert_range"
(*  24*)		  : cptr * cptr * cptr * cptr -> unit;
(*  24*)    val insert_range : 'a t -> textiter -> textiter -> textiter -> unit
(*  24*)	= fn self => fn iter => fn start => fn en =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => insert_range_ (self, iter, start, en))
(*  24*)    val insert_range_interactive_
(*  24*)      : cptr * cptr * cptr * cptr * bool -> bool
(*  24*)	= _import "gtk_text_buffer_insert_range_interactive"
(*  24*)		  : cptr * cptr * cptr * cptr * bool -> bool;
(*  24*)    val insert_range_interactive
(*  24*)      : 'a t -> textiter -> textiter -> textiter -> bool -> bool
(*  24*)	= fn self => fn iter => fn start => fn en => 
(*  24*)	  fn default_editable =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => insert_range_interactive_
(*  24*)					   (self, iter, start, en, 
(*  24*)					    default_editable))
(*  24*)    val insert_with_tags_
(*  24*)      : cptr * cptr * CString.cstring * int * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_insert_with_tags"
(*  24*)		  : cptr * cptr * CString.cstring * int * cptr -> unit;
(*  24*)    val insert_with_tags
(*  24*)      : 'a t -> textiter -> string -> int -> 'b t -> unit
(*  24*)	= fn self => fn iter => fn text => fn len => fn first_tag =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (first_tag, 
(*  24*)			      fn first_tag =>
(*  24*)				 insert_with_tags_
(*  24*)				   (self, iter, 
(*  24*)				    CString.fromString text, len, 
(*  24*)				    first_tag)))
(*  24*)    val insert_with_tags_by_name_
(*  24*)      : cptr * cptr * CString.cstring * int * CString.cstring -> unit
(*  24*)	= _import "gtk_text_buffer_insert_with_tags_by_name"
(*  24*)		  : cptr * cptr * CString.cstring * int 
(*  24*)		  * CString.cstring
(*  24*)		    -> unit;
(*  24*)    val insert_with_tags_by_name
(*  24*)      : 'a t -> textiter -> string -> int -> string -> unit
(*  24*)	= fn self => fn iter => fn text => fn len => 
(*  24*)	  fn first_tag_name =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => insert_with_tags_by_name_
(*  24*)			     (self, iter, CString.fromString text, 
(*  24*)			      len, CString.fromString first_tag_name))
(*  24*)    val delete_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_delete"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val delete : 'a t -> textiter -> textiter -> unit
(*  24*)	= fn self => fn start => fn en =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => delete_ (self, start, en))
(*  24*)    val delete_interactive_ : cptr * cptr * cptr * bool -> bool
(*  24*)	= _import "gtk_text_buffer_delete_interactive"
(*  24*)		  : cptr * cptr * cptr * bool -> bool;
(*  24*)    val delete_interactive
(*  24*)      : 'a t -> textiter -> textiter -> bool -> bool
(*  24*)	= fn self => fn start_iter => fn end_iter => 
(*  24*)	  fn default_editable =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => delete_interactive_
(*  24*)			     (self, start_iter, end_iter, 
(*  24*)			      default_editable))
(*  24*)    val get_text_ : cptr * cptr * cptr * bool -> CString.t
(*  24*)	= _import "gtk_text_buffer_get_text"
(*  24*)		  : cptr * cptr * cptr * bool -> CString.t;
(*  24*)    val get_text
(*  24*)      : 'a t -> textiter -> textiter -> bool option -> string
(*  24*)	= fn self => fn start => fn en => fn include_hidden_chars =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   let val t = get_text_
(*  24*)				 (self, start, en, 
(*  24*)				  getOpt (include_hidden_chars, true))
(*  24*)		   in CString.toString t end)
(*  24*)    val get_text' : 'a t -> textiter -> textiter -> string
(*  24*)	= fn self => fn start => fn en =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   let val t = get_text_ (self, start, en, true)
(*  24*)		   in CString.toString t end)
(*  24*)    val get_slice_ : cptr * cptr * cptr * bool -> CString.t
(*  24*)	= _import "gtk_text_buffer_get_slice"
(*  24*)		  : cptr * cptr * cptr * bool -> CString.t;
(*  24*)    val get_slice
(*  24*)      : 'a t -> textiter -> textiter -> bool option -> string
(*  24*)	= fn self => fn start => fn en => fn include_hidden_chars =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   let val t = get_slice_
(*  24*)				 (self, start, en, 
(*  24*)				  getOpt (include_hidden_chars, true))
(*  24*)		   in CString.toString t end)
(*  24*)    val get_slice' : 'a t -> textiter -> textiter -> string
(*  24*)	= fn self => fn start => fn en =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => let val t = get_slice_
(*  24*)					 (self, start, en, true)
(*  24*)			   in CString.toString t end)
(*  24*)    val insert_child_anchor_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_insert_child_anchor"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val insert_child_anchor : 'a t -> textiter -> 'b t -> unit
(*  24*)	= fn self => fn iter => fn anchor =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (anchor, 
(*  24*)			      fn anchor => insert_child_anchor_
(*  24*)					     (self, iter, anchor)))
(*  24*)    val create_child_anchor_ : cptr * cptr -> cptr
(*  24*)	= _import "gtk_text_buffer_create_child_anchor"
(*  24*)		  : cptr * cptr -> cptr;
(*  24*)    val create_child_anchor : 'a t -> textiter -> base t
(*  24*)	= fn self => fn iter =>
(*  24*)	     make (GObject.withPtr (self, 
(*  24*)				    fn self => create_child_anchor_
(*  24*)						 (self, iter)))
(*  24*)    val create_mark_ : cptr * CString.cstring * cptr * bool -> cptr
(*  24*)	= _import "gtk_text_buffer_create_mark"
(*  24*)		  : cptr * CString.cstring * cptr * bool -> cptr;
(*  24*)    val create_mark
(*  24*)      : 'a t -> string option -> textiter -> bool option -> base t
(*  24*)	= fn self => fn mark_name => fn wher => fn left_gravity =>
(*  24*)	     make (GObject.withPtr
(*  24*)		     (self, 
(*  24*)		      fn self =>
(*  24*)			 create_mark_
(*  24*)			   (self, 
(*  24*)			    CString.fromString
(*  24*)			      (getOpt (mark_name, "")), 
(*  24*)			    wher, getOpt (left_gravity, false))))
(*  24*)    val create_mark' : 'a t -> textiter -> base t
(*  24*)	= fn self => fn wher =>
(*  24*)	     make (GObject.withPtr
(*  24*)		     (self, 
(*  24*)		      fn self => create_mark_ (self, 
(*  24*)					       CString.fromString "", 
(*  24*)					       wher, false)))
(*  24*)    val move_mark_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_move_mark"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val move_mark : 'a t -> 'b t -> textiter -> unit
(*  24*)	= fn self => fn mark => fn wher =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (mark, fn mark => move_mark_ (self, mark, wher)))
(*  24*)    val delete_mark_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_delete_mark" : cptr * cptr -> unit;
(*  24*)    val delete_mark : 'a t -> 'b t -> unit
(*  24*)	= fn self => fn mark =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (mark, fn mark => delete_mark_ (self, mark)))
(*  24*)    val get_mark_ : cptr * CString.cstring -> cptr
(*  24*)	= _import "gtk_text_buffer_get_mark"
(*  24*)		  : cptr * CString.cstring -> cptr;
(*  24*)    val get_mark : 'a t -> string -> base t
(*  24*)	= fn self => fn name =>
(*  24*)	     make (GObject.withPtr
(*  24*)		     (self, 
(*  24*)		      fn self =>
(*  24*)			 get_mark_ (self, CString.fromString name)))
(*  24*)    val move_mark_by_name_ : cptr * CString.cstring * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_move_mark_by_name"
(*  24*)		  : cptr * CString.cstring * cptr -> unit;
(*  24*)    val move_mark_by_name : 'a t -> string -> textiter -> unit
(*  24*)	= fn self => fn name => fn wher =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => move_mark_by_name_
(*  24*)			     (self, CString.fromString name, wher))
(*  24*)    val delete_mark_by_name_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_text_buffer_delete_mark_by_name"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val delete_mark_by_name : 'a t -> string -> unit
(*  24*)	= fn self => fn name =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => delete_mark_by_name_
(*  24*)			     (self, CString.fromString name))
(*  24*)    val get_insert_ : cptr -> cptr
(*  24*)	= _import "gtk_text_buffer_get_insert" : cptr -> cptr;
(*  24*)    val get_insert : 'a t -> base t
(*  24*)	= fn self => make (GObject.withPtr
(*  24*)			     (self, fn self => get_insert_ self))
(*  24*)    val get_selection_bound_ : cptr -> cptr
(*  24*)	= _import "gtk_text_buffer_get_selection_bound" : cptr -> cptr;
(*  24*)    val get_selection_bound : 'a t -> base t
(*  24*)	= fn self =>
(*  24*)	     make (GObject.withPtr
(*  24*)		     (self, fn self => get_selection_bound_ self))
(*  24*)    val place_cursor_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_place_cursor" : cptr * cptr -> unit;
(*  24*)    val place_cursor : 'a t -> textiter -> unit
(*  24*)	= fn self => fn wher =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => place_cursor_ (self, wher))
(*  24*)    val apply_tag_ : cptr * cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_apply_tag"
(*  24*)		  : cptr * cptr * cptr * cptr -> unit;
(*  24*)    val apply_tag : 'a t -> 'b t -> textiter -> textiter -> unit
(*  24*)	= fn self => fn tag => fn start => fn en =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (tag, 
(*  24*)			      fn tag => apply_tag_
(*  24*)					  (self, tag, start, en)))
(*  24*)    val remove_tag_ : cptr * cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_remove_tag"
(*  24*)		  : cptr * cptr * cptr * cptr -> unit;
(*  24*)    val remove_tag : 'a t -> 'b t -> textiter -> textiter -> unit
(*  24*)	= fn self => fn tag => fn start => fn en =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (tag, 
(*  24*)			      fn tag => remove_tag_
(*  24*)					  (self, tag, start, en)))
(*  24*)    val apply_tag_by_name_
(*  24*)      : cptr * CString.cstring * cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_apply_tag_by_name"
(*  24*)		  : cptr * CString.cstring * cptr * cptr -> unit;
(*  24*)    val apply_tag_by_name
(*  24*)      : 'a t -> string -> textiter -> textiter -> unit
(*  24*)	= fn self => fn name => fn start => fn en =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   apply_tag_by_name_
(*  24*)		     (self, CString.fromString name, start, en))
(*  24*)    val remove_tag_by_name_
(*  24*)      : cptr * CString.cstring * cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_remove_tag_by_name"
(*  24*)		  : cptr * CString.cstring * cptr * cptr -> unit;
(*  24*)    val remove_tag_by_name
(*  24*)      : 'a t -> string -> textiter -> textiter -> unit
(*  24*)	= fn self => fn name => fn start => fn en =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   remove_tag_by_name_
(*  24*)		     (self, CString.fromString name, start, en))
(*  24*)    val remove_all_tags_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_remove_all_tags"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val remove_all_tags : 'a t -> textiter -> textiter -> unit
(*  24*)	= fn self => fn start => fn en =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => remove_all_tags_ (self, start, en))
(*  24*)    val create_tag_ : cptr * CString.cstring * CString.cstring -> cptr
(*  24*)	= _import "gtk_text_buffer_create_tag"
(*  24*)		  : cptr * CString.cstring * CString.cstring -> cptr;
(*  24*)    val create_tag : 'a t -> string -> string -> base t
(*  24*)	= fn self => fn tag_name => fn first_property_name =>
(*  24*)	     make (GObject.withPtr
(*  24*)		     (self, 
(*  24*)		      fn self =>
(*  24*)			 create_tag_
(*  24*)			   (self, CString.fromString tag_name, 
(*  24*)			    CString.fromString first_property_name)))
(*  24*)    val getiter_at_line_offset_ : cptr * cptr * int * int -> unit
(*  24*)	= _import "gtk_text_buffer_get_iter_at_line_offset"
(*  24*)		  : cptr * cptr * int * int -> unit;
(*  24*)    val getiter_at_line_offset : 'a t -> textiter -> int -> int -> unit
(*  24*)	= fn self => fn iter => fn line_number => fn char_offset =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => getiter_at_line_offset_
(*  24*)			     (self, iter, line_number, char_offset))
(*  24*)    val getiter_at_line_index_ : cptr * cptr * int * int -> unit
(*  24*)	= _import "gtk_text_buffer_get_iter_at_line_index"
(*  24*)		  : cptr * cptr * int * int -> unit;
(*  24*)    val getiter_at_line_index : 'a t -> textiter -> int -> int -> unit
(*  24*)	= fn self => fn iter => fn line_number => fn byte_index =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => getiter_at_line_index_
(*  24*)			     (self, iter, line_number, byte_index))
(*  24*)    val getiter_at_offset_ : cptr * cptr * int -> unit
(*  24*)	= _import "gtk_text_buffer_get_iter_at_offset"
(*  24*)		  : cptr * cptr * int -> unit;
(*  24*)    val getiter_at_offset : 'a t -> textiter -> int -> unit
(*  24*)	= fn self => fn iter => fn char_offset =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => getiter_at_offset_
(*  24*)					   (self, iter, char_offset))
(*  24*)    val getiter_at_line_ : cptr * cptr * int -> unit
(*  24*)	= _import "gtk_text_buffer_get_iter_at_line"
(*  24*)		  : cptr * cptr * int -> unit;
(*  24*)    val getiter_at_line : 'a t -> textiter -> int -> unit
(*  24*)	= fn self => fn iter => fn line_number =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => getiter_at_line_
(*  24*)					   (self, iter, line_number))
(*  24*)    val get_startiter_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_get_start_iter"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val get_startiter : 'a t -> textiter -> unit
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => get_startiter_ (self, iter))
(*  24*)    val get_enditer_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_get_end_iter" : cptr * cptr -> unit;
(*  24*)    val get_enditer : 'a t -> textiter -> unit
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => get_enditer_ (self, iter))
(*  24*)    val get_bounds_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_get_bounds"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val get_bounds : 'a t -> textiter -> textiter -> unit
(*  24*)	= fn self => fn start => fn en =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => get_bounds_ (self, start, en))
(*  24*)    val getiter_at_mark_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_get_iter_at_mark"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val getiter_at_mark : 'a t -> textiter -> 'b t -> unit
(*  24*)	= fn self => fn iter => fn mark =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (mark, 
(*  24*)			      fn mark => getiter_at_mark_
(*  24*)					   (self, iter, mark)))
(*  24*)    val getiter_at_child_anchor_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_get_iter_at_child_anchor"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val getiter_at_child_anchor : 'a t -> textiter -> 'b t -> unit
(*  24*)	= fn self => fn iter => fn anchor =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (anchor, 
(*  24*)			      fn anchor => getiter_at_child_anchor_
(*  24*)					     (self, iter, anchor)))
(*  24*)    val get_modified_ : cptr -> bool
(*  24*)	= _import "gtk_text_buffer_get_modified" : cptr -> bool;
(*  24*)    val get_modified : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_modified_ self)
(*  24*)    val set_modified_ : cptr * bool -> unit
(*  24*)	= _import "gtk_text_buffer_set_modified" : cptr * bool -> unit;
(*  24*)    val set_modified : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_modified_ (self, setting))
(*  24*)    val add_selection_clipboard_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_add_selection_clipboard"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val add_selection_clipboard : 'a t -> 'b t -> unit
(*  24*)	= fn self => fn clipboard =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (clipboard, 
(*  24*)			      fn clipboard => add_selection_clipboard_
(*  24*)						(self, clipboard)))
(*  24*)    val remove_selection_clipboard_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_remove_selection_clipboard"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val remove_selection_clipboard : 'a t -> 'b t -> unit
(*  24*)	= fn self => fn clipboard =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (clipboard, 
(*  24*)			      fn clipboard =>
(*  24*)				 remove_selection_clipboard_
(*  24*)				   (self, clipboard)))
(*  24*)    val cut_clipboard_ : cptr * cptr * bool -> unit
(*  24*)	= _import "gtk_text_buffer_cut_clipboard"
(*  24*)		  : cptr * cptr * bool -> unit;
(*  24*)    val cut_clipboard : 'a t -> 'b t -> bool -> unit
(*  24*)	= fn self => fn clipboard => fn default_editable =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (clipboard, 
(*  24*)			      fn clipboard =>
(*  24*)				 cut_clipboard_ (self, clipboard, 
(*  24*)						 default_editable)))
(*  24*)    val copy_clipboard_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_text_buffer_copy_clipboard"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val copy_clipboard : 'a t -> 'b t -> unit
(*  24*)	= fn self => fn clipboard =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (clipboard, 
(*  24*)			      fn clipboard => copy_clipboard_
(*  24*)						(self, clipboard)))
(*  24*)    val paste_clipboard_ : cptr * cptr * cptr * bool -> unit
(*  24*)	= _import "gtk_text_buffer_paste_clipboard"
(*  24*)		  : cptr * cptr * cptr * bool -> unit;
(*  24*)    val paste_clipboard : 'a t -> 'b t -> textiter -> bool -> unit
(*  24*)	= fn self => fn clipboard => fn override_location => 
(*  24*)	  fn default_editable =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (clipboard, 
(*  24*)			      fn clipboard => paste_clipboard_
(*  24*)						(self, clipboard, 
(*  24*)						 override_location, 
(*  24*)						 default_editable)))
(*  24*)    val get_selection_bounds_ : cptr * cptr * cptr -> bool
(*  24*)	= _import "gtk_text_buffer_get_selection_bounds"
(*  24*)		  : cptr * cptr * cptr -> bool;
(*  24*)    val get_selection_bounds : 'a t -> textiter -> textiter -> bool
(*  24*)	= fn self => fn start => fn en =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => get_selection_bounds_
(*  24*)					   (self, start, en))
(*  24*)    val delete_selection_ : cptr * bool * bool -> bool
(*  24*)	= _import "gtk_text_buffer_delete_selection"
(*  24*)		  : cptr * bool * bool -> bool;
(*  24*)    val delete_selection : 'a t -> bool -> bool -> bool
(*  24*)	= fn self => fn interactive => fn default_editable =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => delete_selection_
(*  24*)					   (self, interactive, 
(*  24*)					    default_editable))
(*  24*)    val begin_user_action_ : cptr -> unit
(*  24*)	= _import "gtk_text_buffer_begin_user_action" : cptr -> unit;
(*  24*)    val begin_user_action : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => begin_user_action_ self)
(*  24*)    val end_user_action_ : cptr -> unit
(*  24*)	= _import "gtk_text_buffer_end_user_action" : cptr -> unit;
(*  24*)    val end_user_action : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => end_user_action_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "changed" false (void --> return_void) f
(*  24*)       val insert_text_sig : (unit -> char -> int -> unit)
(*  24*)			     -> 'a t Signal.signal
(*  24*)	   = fn f => signal "insert-text" false
(*  24*)			    (unit --> char --> int --> return_void) f
(*  24*)       val insert_pixbuf_sig : (unit -> unit -> unit)
(*  24*)			       -> 'a t Signal.signal
(*  24*)	   = fn f => signal "insert-pixbuf" false
(*  24*)			    (unit --> unit --> return_void) f
(*  24*)       val insert_child_anchor_sig
(*  24*)	 : (unit -> unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "insert-child-anchor" false
(*  24*)			    (unit --> unit --> return_void) f
(*  24*)       val delete_range_sig : (unit -> unit -> unit)
(*  24*)			      -> 'a t Signal.signal
(*  24*)	   = fn f => signal "delete-range" false
(*  24*)			    (unit --> unit --> return_void) f
(*  24*)       val modified_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "modified-changed" false
(*  24*)			    (void --> return_void) f
(*  24*)       val mark_set_sig : (unit -> unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "mark-set" false
(*  24*)			    (unit --> unit --> return_void) f
(*  24*)       val mark_deleted_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "mark-deleted" false (unit --> return_void) f
(*  24*)       val apply_tag_sig : (unit -> unit -> unit -> unit)
(*  24*)			   -> 'a t Signal.signal
(*  24*)	   = fn f => signal "apply-tag" false
(*  24*)			    (unit --> unit --> unit --> return_void) f
(*  24*)       val remove_tag_sig : (unit -> unit -> unit -> unit)
(*  24*)			    -> 'a t Signal.signal
(*  24*)	   = fn f => signal "remove-tag" false
(*  24*)			    (unit --> unit --> unit --> return_void) f
(*  24*)       val begin_user_action_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "begin-user-action" false
(*  24*)			    (void --> return_void) f
(*  24*)       val end_user_action_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "end-user-action" false
(*  24*)			    (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure TextChildAnchor :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a textchildanchor_t
(*  24*)    type 'a t = 'a textchildanchor_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTextChildAnchor : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val get_deleted : 'a t -> bool
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a textchildanchor_t = unit
(*  24*)    type 'a t = 'a textchildanchor_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTextChildAnchor obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_text_child_anchor_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_text_child_anchor_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val get_deleted_ : cptr -> bool
(*  24*)	= _import "gtk_text_child_anchor_get_deleted" : cptr -> bool;
(*  24*)    val get_deleted : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_deleted_ self)
(*  24*)end
(*  24*)structure TextMark :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a textmark_t
(*  24*)    type 'a t = 'a textmark_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTextMark : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val set_visible : 'a t -> bool -> unit
(*  24*)    val get_visible : 'a t -> bool
(*  24*)    val get_name : 'a t -> string
(*  24*)    val get_deleted : 'a t -> bool
(*  24*)    val get_buffer : 'a t -> base TextBuffer.t
(*  24*)    val get_left_gravity : 'a t -> bool
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a textmark_t = unit
(*  24*)    type 'a t = 'a textmark_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTextMark obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_text_mark_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val set_visible_ : cptr * bool -> unit
(*  24*)	= _import "gtk_text_mark_set_visible" : cptr * bool -> unit;
(*  24*)    val set_visible : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_visible_ (self, setting))
(*  24*)    val get_visible_ : cptr -> bool
(*  24*)	= _import "gtk_text_mark_get_visible" : cptr -> bool;
(*  24*)    val get_visible : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_visible_ self)
(*  24*)    val get_name_ : cptr -> CString.t
(*  24*)	= _import "gtk_text_mark_get_name" : cptr -> CString.t;
(*  24*)    val get_name : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_name_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val get_deleted_ : cptr -> bool
(*  24*)	= _import "gtk_text_mark_get_deleted" : cptr -> bool;
(*  24*)    val get_deleted : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_deleted_ self)
(*  24*)    val get_buffer_ : cptr -> cptr
(*  24*)	= _import "gtk_text_mark_get_buffer" : cptr -> cptr;
(*  24*)    val get_buffer : 'a t -> base TextBuffer.t
(*  24*)	= fn self => TextBuffer.inherit
(*  24*)		       ()
(*  24*)		       (fn () => GObject.withPtr
(*  24*)				   (self, fn self => get_buffer_ self))
(*  24*)    val get_left_gravity_ : cptr -> bool
(*  24*)	= _import "gtk_text_mark_get_left_gravity" : cptr -> bool;
(*  24*)    val get_left_gravity : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_left_gravity_ self)
(*  24*)end
(*  24*)structure TextTag :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a texttag_t
(*  24*)    type 'a t = 'a texttag_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTextTag : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : string option -> base t
(*  24*)    val new' : unit -> base t
(*  24*)    val get_priority : 'a t -> int
(*  24*)    val set_priority : 'a t -> int -> unit
(*  24*)    val table_get_type : unit -> int
(*  24*)    val event_sig : (unit -> unit -> unit -> bool)
(*  24*)		    -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a texttag_t = unit
(*  24*)    type 'a t = 'a texttag_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTextTag obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_text_tag_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_text_tag_new" : CString.cstring -> cptr;
(*  24*)    val new : string option -> base t
(*  24*)	= fn name => make (new_ (CString.fromString
(*  24*)				   (getOpt (name, ""))))
(*  24*)    val new' : unit -> base t
(*  24*)	= fn dummy => make (new_ (CString.fromString ""))
(*  24*)    val get_priority_ : cptr -> int
(*  24*)	= _import "gtk_text_tag_get_priority" : cptr -> int;
(*  24*)    val get_priority : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_priority_ self)
(*  24*)    val set_priority_ : cptr * int -> unit
(*  24*)	= _import "gtk_text_tag_set_priority" : cptr * int -> unit;
(*  24*)    val set_priority : 'a t -> int -> unit
(*  24*)	= fn self => fn priority =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_priority_ (self, priority))
(*  24*)    val table_get_type_ : unit -> int
(*  24*)	= _import "gtk_text_tag_table_get_type" : unit -> int;
(*  24*)    val table_get_type : unit -> int
(*  24*)	= fn dummy => table_get_type_ dummy
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val event_sig : (unit -> unit -> unit -> bool)
(*  24*)		       -> 'a t Signal.signal
(*  24*)	   = fn f => signal "event" false
(*  24*)			    (unit --> unit --> unit --> return_bool) f
(*  24*)    end
(*  24*)end
(*  24*)structure TextTagTable :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a texttagtable_t
(*  24*)    type 'a t = 'a texttagtable_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTextTagTable : 'a t -> base t
(*  24*)    val new : unit -> base t
(*  24*)    val add : 'a t -> 'b t -> unit
(*  24*)    val remove : 'a t -> 'b t -> unit
(*  24*)    val lookup : 'a t -> string -> base t
(*  24*)    val get_size : 'a t -> int
(*  24*)    val tag_changed_sig : (unit -> bool -> unit) -> 'a t Signal.signal
(*  24*)    val tag_added_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val tag_removed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a texttagtable_t = unit
(*  24*)    type 'a t = 'a texttagtable_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTextTagTable obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_text_tag_table_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val add_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_text_tag_table_add" : cptr * cptr -> unit;
(*  24*)    val add : 'a t -> 'b t -> unit
(*  24*)	= fn self => fn tag =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (tag, fn tag => add_ (self, tag)))
(*  24*)    val remove_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_text_tag_table_remove" : cptr * cptr -> unit;
(*  24*)    val remove : 'a t -> 'b t -> unit
(*  24*)	= fn self => fn tag =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (tag, fn tag => remove_ (self, tag)))
(*  24*)    val lookup_ : cptr * CString.cstring -> cptr
(*  24*)	= _import "gtk_text_tag_table_lookup"
(*  24*)		  : cptr * CString.cstring -> cptr;
(*  24*)    val lookup : 'a t -> string -> base t
(*  24*)	= fn self => fn name =>
(*  24*)	     make (GObject.withPtr
(*  24*)		     (self, 
(*  24*)		      fn self =>
(*  24*)			 lookup_ (self, CString.fromString name)))
(*  24*)    val get_size_ : cptr -> int
(*  24*)	= _import "gtk_text_tag_table_get_size" : cptr -> int;
(*  24*)    val get_size : 'a t -> int
(*  24*)	= fn self => GObject.withPtr (self, fn self => get_size_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val tag_changed_sig : (unit -> bool -> unit)
(*  24*)			     -> 'a t Signal.signal
(*  24*)	   = fn f => signal "tag-changed" false
(*  24*)			    (unit --> bool --> return_void) f
(*  24*)       val tag_added_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "tag-added" false (unit --> return_void) f
(*  24*)       val tag_removed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "tag-removed" false (unit --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Tooltips :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a tooltips_t
(*  24*)    type 'a t = 'a tooltips_t Object.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTooltips : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val enable : 'a t -> unit
(*  24*)    val disable : 'a t -> unit
(*  24*)    val set_delay : 'a t -> int -> unit
(*  24*)    val set_tip
(*  24*)      : 'a t -> 'b Widget.t -> string -> string option -> unit
(*  24*)    val set_tip' : 'a t -> 'b Widget.t -> string -> unit
(*  24*)    val force_window : 'a t -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a tooltips_t = unit
(*  24*)    type 'a t = 'a tooltips_t Object.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Object.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTooltips obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_tooltips_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_tooltips_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val enable_ : cptr -> unit
(*  24*)	= _import "gtk_tooltips_enable" : cptr -> unit;
(*  24*)    val enable : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => enable_ self)
(*  24*)    val disable_ : cptr -> unit
(*  24*)	= _import "gtk_tooltips_disable" : cptr -> unit;
(*  24*)    val disable : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => disable_ self)
(*  24*)    val set_delay_ : cptr * int -> unit
(*  24*)	= _import "gtk_tooltips_set_delay" : cptr * int -> unit;
(*  24*)    val set_delay : 'a t -> int -> unit
(*  24*)	= fn self => fn delay =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_delay_ (self, delay))
(*  24*)    val set_tip_
(*  24*)      : cptr * cptr * CString.cstring * CString.cstring -> unit
(*  24*)	= _import "gtk_tooltips_set_tip"
(*  24*)		  : cptr * cptr * CString.cstring * CString.cstring
(*  24*)		    -> unit;
(*  24*)    val set_tip
(*  24*)      : 'a t -> 'b Widget.t -> string -> string option -> unit
(*  24*)	= fn self => fn widget => fn tip_text => fn tip_private =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (widget, 
(*  24*)		      fn widget =>
(*  24*)			 set_tip_ (self, widget, 
(*  24*)				   CString.fromString tip_text, 
(*  24*)				   CString.fromString
(*  24*)				     (getOpt (tip_private, "")))))
(*  24*)    val set_tip' : 'a t -> 'b Widget.t -> string -> unit
(*  24*)	= fn self => fn widget => fn tip_text =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (widget, 
(*  24*)		      fn widget =>
(*  24*)			 set_tip_ (self, widget, 
(*  24*)				   CString.fromString tip_text, 
(*  24*)				   CString.fromString "")))
(*  24*)    val force_window_ : cptr -> unit
(*  24*)	= _import "gtk_tooltips_force_window" : cptr -> unit;
(*  24*)    val force_window : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => force_window_ self)
(*  24*)end
(*  24*)structure TreeModel :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a treemodel_t
(*  24*)    type 'a t = 'a treemodel_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTreeModel : 'a t -> base t
(*  24*)    type flags
(*  24*)    val TREE_MODEL_ITERS_PERSIST : flags
(*  24*)    val TREE_MODEL_LIST_ONLY : flags
(*  24*)    val get_type : unit -> int
(*  24*)    val get_flags : 'a t -> flags list
(*  24*)    val get_n_columns : 'a t -> int
(*  24*)    val get_columntype : 'a t -> int -> int
(*  24*)    val getiter : 'a t -> treeiter -> tree_path -> bool
(*  24*)    val get_path : 'a t -> treeiter -> tree_path
(*  24*)    val iter_has_child : 'a t -> treeiter -> bool
(*  24*)    val iter_n_children : 'a t -> treeiter option -> int
(*  24*)    val iter_n_children' : 'a t -> int
(*  24*)    val iter_nth_child
(*  24*)      : 'a t -> treeiter -> treeiter option -> int -> bool
(*  24*)    val iter_nth_child' : 'a t -> treeiter -> int -> bool
(*  24*)    val ref_node : 'a t -> treeiter -> unit
(*  24*)    val unref_node : 'a t -> treeiter -> unit
(*  24*)    val get : 'a t -> treeiter -> unit
(*  24*)    val row_changed : 'a t -> tree_path -> treeiter -> unit
(*  24*)    val row_inserted : 'a t -> tree_path -> treeiter -> unit
(*  24*)    val row_has_child_toggled : 'a t -> tree_path -> treeiter -> unit
(*  24*)    val row_deleted : 'a t -> tree_path -> unit
(*  24*)    val sort_get_type : unit -> int
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a treemodel_t = unit
(*  24*)    type 'a t = 'a treemodel_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTreeModel obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    type flags = int
(*  24*)    val get_flags_ : int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_treemodel_flags"
(*  24*)		  : int ref * int ref -> unit;
(*  24*)    val (TREE_MODEL_ITERS_PERSIST, TREE_MODEL_LIST_ONLY)
(*  24*)	= let val (x0, x1) = (ref 0, ref 0) in get_flags_ (x0, x1)
(*  24*)					     ; (!x0, !x1)
(*  24*)					    end
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_tree_model_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val get_flags_ : cptr -> int
(*  24*)	= _import "gtk_tree_model_get_flags" : cptr -> int;
(*  24*)    val get_flags : 'a t -> flags list
(*  24*)	= fn self => Flags.get (GObject.withPtr
(*  24*)				  (self, fn self => get_flags_ self))
(*  24*)    val get_n_columns_ : cptr -> int
(*  24*)	= _import "gtk_tree_model_get_n_columns" : cptr -> int;
(*  24*)    val get_n_columns : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_n_columns_ self)
(*  24*)    val get_columntype_ : cptr * int -> int
(*  24*)	= _import "gtk_tree_model_get_column_type" : cptr * int -> int;
(*  24*)    val get_columntype : 'a t -> int -> int
(*  24*)	= fn self => fn index =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => get_columntype_ (self, index))
(*  24*)    val getiter_ : cptr * cptr * cptr -> bool
(*  24*)	= _import "gtk_tree_model_get_iter"
(*  24*)		  : cptr * cptr * cptr -> bool;
(*  24*)    val getiter : 'a t -> treeiter -> tree_path -> bool
(*  24*)	= fn self => fn iter => fn path =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => getiter_ (self, iter, path))
(*  24*)    val get_path_ : cptr * cptr -> cptr
(*  24*)	= _import "gtk_tree_model_get_path" : cptr * cptr -> cptr;
(*  24*)    val get_path : 'a t -> treeiter -> tree_path
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr (self, fn self => get_path_ (self, iter))
(*  24*)    val iter_has_child_ : cptr * cptr -> bool
(*  24*)	= _import "gtk_tree_model_iter_has_child"
(*  24*)		  : cptr * cptr -> bool;
(*  24*)    val iter_has_child : 'a t -> treeiter -> bool
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => iter_has_child_ (self, iter))
(*  24*)    val iter_n_children_ : cptr * cptr -> int
(*  24*)	= _import "gtk_tree_model_iter_n_children"
(*  24*)		  : cptr * cptr -> int;
(*  24*)    val iter_n_children : 'a t -> treeiter option -> int
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => iter_n_children_
(*  24*)			     (self, getOpt (iter, GObject.null)))
(*  24*)    val iter_n_children' : 'a t -> int
(*  24*)	= fn self =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => iter_n_children_ (self, GObject.null))
(*  24*)    val iter_nth_child_ : cptr * cptr * cptr * int -> bool
(*  24*)	= _import "gtk_tree_model_iter_nth_child"
(*  24*)		  : cptr * cptr * cptr * int -> bool;
(*  24*)    val iter_nth_child
(*  24*)      : 'a t -> treeiter -> treeiter option -> int -> bool
(*  24*)	= fn self => fn iter => fn parent => fn n =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => iter_nth_child_
(*  24*)			     (self, iter, 
(*  24*)			      getOpt (parent, GObject.null), n))
(*  24*)    val iter_nth_child' : 'a t -> treeiter -> int -> bool
(*  24*)	= fn self => fn iter => fn n =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => iter_nth_child_
(*  24*)			     (self, iter, GObject.null, n))
(*  24*)    val ref_node_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_model_ref_node" : cptr * cptr -> unit;
(*  24*)    val ref_node : 'a t -> treeiter -> unit
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr (self, fn self => ref_node_ (self, iter))
(*  24*)    val unref_node_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_model_unref_node" : cptr * cptr -> unit;
(*  24*)    val unref_node : 'a t -> treeiter -> unit
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => unref_node_ (self, iter))
(*  24*)    val get_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_model_get" : cptr * cptr -> unit;
(*  24*)    val get : 'a t -> treeiter -> unit
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr (self, fn self => get_ (self, iter))
(*  24*)    val row_changed_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_model_row_changed"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val row_changed : 'a t -> tree_path -> treeiter -> unit
(*  24*)	= fn self => fn path => fn iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => row_changed_ (self, path, iter))
(*  24*)    val row_inserted_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_model_row_inserted"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val row_inserted : 'a t -> tree_path -> treeiter -> unit
(*  24*)	= fn self => fn path => fn iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => row_inserted_ (self, path, iter))
(*  24*)    val row_has_child_toggled_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_model_row_has_child_toggled"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val row_has_child_toggled : 'a t -> tree_path -> treeiter -> unit
(*  24*)	= fn self => fn path => fn iter =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => row_has_child_toggled_
(*  24*)					   (self, path, iter))
(*  24*)    val row_deleted_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_model_row_deleted" : cptr * cptr -> unit;
(*  24*)    val row_deleted : 'a t -> tree_path -> unit
(*  24*)	= fn self => fn path =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => row_deleted_ (self, path))
(*  24*)    val sort_get_type_ : unit -> int
(*  24*)	= _import "gtk_tree_model_sort_get_type" : unit -> int;
(*  24*)    val sort_get_type : unit -> int = fn dummy => sort_get_type_ dummy
(*  24*)end
(*  24*)structure TreeDragSource :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a treedragsource_t
(*  24*)    type 'a t = 'a treedragsource_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTreeDragSource : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val row_draggable : 'a t -> tree_path -> bool
(*  24*)    val drag_data_delete : 'a t -> tree_path -> bool
(*  24*)    val drag_data_get : 'a t -> tree_path -> selection_data -> bool
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a treedragsource_t = unit
(*  24*)    type 'a t = 'a treedragsource_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTreeDragSource obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_tree_drag_source_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val row_draggable_ : cptr * cptr -> bool
(*  24*)	= _import "gtk_tree_drag_source_row_draggable"
(*  24*)		  : cptr * cptr -> bool;
(*  24*)    val row_draggable : 'a t -> tree_path -> bool
(*  24*)	= fn self => fn path =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => row_draggable_ (self, path))
(*  24*)    val drag_data_delete_ : cptr * cptr -> bool
(*  24*)	= _import "gtk_tree_drag_source_drag_data_delete"
(*  24*)		  : cptr * cptr -> bool;
(*  24*)    val drag_data_delete : 'a t -> tree_path -> bool
(*  24*)	= fn self => fn path =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => drag_data_delete_ (self, path))
(*  24*)    val drag_data_get_ : cptr * cptr * cptr -> bool
(*  24*)	= _import "gtk_tree_drag_source_drag_data_get"
(*  24*)		  : cptr * cptr * cptr -> bool;
(*  24*)    val drag_data_get : 'a t -> tree_path -> selection_data -> bool
(*  24*)	= fn self => fn path => fn selection_data =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => drag_data_get_ (self, path, selection_data))
(*  24*)end
(*  24*)structure TreeDragDest :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a treedragdest_t
(*  24*)    type 'a t = 'a treedragdest_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTreeDragDest : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val drag_data_received
(*  24*)      : 'a t -> tree_path -> selection_data -> bool
(*  24*)    val row_drop_possible : 'a t -> tree_path -> selection_data -> bool
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a treedragdest_t = unit
(*  24*)    type 'a t = 'a treedragdest_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTreeDragDest obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_tree_drag_dest_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val drag_data_received_ : cptr * cptr * cptr -> bool
(*  24*)	= _import "gtk_tree_drag_dest_drag_data_received"
(*  24*)		  : cptr * cptr * cptr -> bool;
(*  24*)    val drag_data_received
(*  24*)      : 'a t -> tree_path -> selection_data -> bool
(*  24*)	= fn self => fn dest => fn selection_data =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => drag_data_received_
(*  24*)			     (self, dest, selection_data))
(*  24*)    val row_drop_possible_ : cptr * cptr * cptr -> bool
(*  24*)	= _import "gtk_tree_drag_dest_row_drop_possible"
(*  24*)		  : cptr * cptr * cptr -> bool;
(*  24*)    val row_drop_possible : 'a t -> tree_path -> selection_data -> bool
(*  24*)	= fn self => fn dest_path => fn selection_data =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => row_drop_possible_
(*  24*)					   (self, dest_path, 
(*  24*)					    selection_data))
(*  24*)end
(*  24*)structure TreeSortable :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a treesortable_t
(*  24*)    type 'a t = 'a treesortable_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTreeSortable : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val sort_column_changed : 'a t -> unit
(*  24*)    val set_sort_column_id : 'a t -> int -> sorttype -> unit
(*  24*)    val has_default_sort_func : 'a t -> bool
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a treesortable_t = unit
(*  24*)    type 'a t = 'a treesortable_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTreeSortable obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_tree_sortable_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val sort_column_changed_ : cptr -> unit
(*  24*)	= _import "gtk_tree_sortable_sort_column_changed"
(*  24*)		  : cptr -> unit;
(*  24*)    val sort_column_changed : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => sort_column_changed_ self)
(*  24*)    val set_sort_column_id_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_tree_sortable_set_sort_column_id"
(*  24*)		  : cptr * int * int -> unit;
(*  24*)    val set_sort_column_id : 'a t -> int -> sorttype -> unit
(*  24*)	= fn self => fn sort_column_id => fn order =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_sort_column_id_
(*  24*)			     (self, sort_column_id, order))
(*  24*)    val has_default_sort_func_ : cptr -> bool
(*  24*)	= _import "gtk_tree_sortable_has_default_sort_func"
(*  24*)		  : cptr -> bool;
(*  24*)    val has_default_sort_func : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => has_default_sort_func_ self)
(*  24*)end
(*  24*)structure ListStore :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a liststore_t
(*  24*)    type 'a t = 'a liststore_t TreeModel.t TreeDragSource.t
(*  24*)		  TreeDragDest.t
(*  24*)		  TreeSortable.t
(*  24*)		  GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toListStore : 'a t -> base t
(*  24*)    val new : int -> base t
(*  24*)    val set : 'a t -> treeiter -> unit
(*  24*)    val remove : 'a t -> treeiter -> unit
(*  24*)    val insert : 'a t -> treeiter -> int -> unit
(*  24*)    val insert_before : 'a t -> treeiter -> treeiter -> unit
(*  24*)    val insert_after : 'a t -> treeiter -> treeiter -> unit
(*  24*)    val prepend : 'a t -> treeiter -> unit
(*  24*)    val append : 'a t -> treeiter -> unit
(*  24*)    val clear : 'a t -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a liststore_t = unit
(*  24*)    type 'a t = 'a liststore_t TreeModel.t TreeDragSource.t
(*  24*)		  TreeDragDest.t
(*  24*)		  TreeSortable.t
(*  24*)		  GObject.t
(*  24*)    fun inherit w con
(*  24*)      = let val con = let val ptr = con () in fn () => ptr end
(*  24*)	    val witness = ()
(*  24*)	    val witness = TreeModel.inherit witness con
(*  24*)	    val witness = TreeDragSource.inherit witness con
(*  24*)	    val witness = TreeDragDest.inherit witness con
(*  24*)	    val witness = TreeSortable.inherit witness con
(*  24*)	in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toListStore obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val new_ : int -> cptr
(*  24*)	= _import "gtk_list_store_new" : int -> cptr;
(*  24*)    val new : int -> base t = fn n_columns => make (new_ n_columns)
(*  24*)    val set_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_list_store_set" : cptr * cptr -> unit;
(*  24*)    val set : 'a t -> treeiter -> unit
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr (self, fn self => set_ (self, iter))
(*  24*)    val remove_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_list_store_remove" : cptr * cptr -> unit;
(*  24*)    val remove : 'a t -> treeiter -> unit
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr (self, fn self => remove_ (self, iter))
(*  24*)    val insert_ : cptr * cptr * int -> unit
(*  24*)	= _import "gtk_list_store_insert" : cptr * cptr * int -> unit;
(*  24*)    val insert : 'a t -> treeiter -> int -> unit
(*  24*)	= fn self => fn iter => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => insert_ (self, iter, position))
(*  24*)    val insert_before_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_list_store_insert_before"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val insert_before : 'a t -> treeiter -> treeiter -> unit
(*  24*)	= fn self => fn iter => fn sibling =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => insert_before_ (self, iter, sibling))
(*  24*)    val insert_after_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_list_store_insert_after"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val insert_after : 'a t -> treeiter -> treeiter -> unit
(*  24*)	= fn self => fn iter => fn sibling =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => insert_after_ (self, iter, sibling))
(*  24*)    val prepend_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_list_store_prepend" : cptr * cptr -> unit;
(*  24*)    val prepend : 'a t -> treeiter -> unit
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr (self, fn self => prepend_ (self, iter))
(*  24*)    val append_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_list_store_append" : cptr * cptr -> unit;
(*  24*)    val append : 'a t -> treeiter -> unit
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr (self, fn self => append_ (self, iter))
(*  24*)    val clear_ : cptr -> unit
(*  24*)	= _import "gtk_list_store_clear" : cptr -> unit;
(*  24*)    val clear : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => clear_ self)
(*  24*)end
(*  24*)structure TreeModelSort :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a treemodelsort_t
(*  24*)    type 'a t = 'a treemodelsort_t TreeModel.t TreeSortable.t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTreeModelSort : 'a t -> base t
(*  24*)    val new_with_model : 'a t -> base t
(*  24*)    val get_model : 'a t -> base t
(*  24*)    val convert_child_path_to_path : 'a t -> tree_path -> tree_path
(*  24*)    val convert_childiter_toiter
(*  24*)      : 'a t -> treeiter option -> treeiter -> unit
(*  24*)    val convert_childiter_toiter' : 'a t -> treeiter -> unit
(*  24*)    val convert_path_to_child_path : 'a t -> tree_path -> tree_path
(*  24*)    val convertiter_to_childiter
(*  24*)      : 'a t -> treeiter option -> treeiter -> unit
(*  24*)    val convertiter_to_childiter' : 'a t -> treeiter -> unit
(*  24*)    val reset_default_sort_func : 'a t -> unit
(*  24*)    val clear_cache : 'a t -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a treemodelsort_t = unit
(*  24*)    type 'a t = 'a treemodelsort_t TreeModel.t TreeSortable.t GObject.t
(*  24*)    fun inherit w con
(*  24*)      = let val con = let val ptr = con () in fn () => ptr end
(*  24*)	    val witness = ()
(*  24*)	    val witness = TreeModel.inherit witness con
(*  24*)	    val witness = TreeSortable.inherit witness con
(*  24*)	in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTreeModelSort obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val new_with_model_ : cptr -> cptr
(*  24*)	= _import "gtk_tree_model_sort_new_with_model" : cptr -> cptr;
(*  24*)    val new_with_model : 'a t -> base t
(*  24*)	= fn child_model =>
(*  24*)	     make (GObject.withPtr (child_model, 
(*  24*)				    fn child_model =>
(*  24*)				       new_with_model_ child_model))
(*  24*)    val get_model_ : cptr -> cptr
(*  24*)	= _import "gtk_tree_model_sort_get_model" : cptr -> cptr;
(*  24*)    val get_model : 'a t -> base t
(*  24*)	= fn self => make (GObject.withPtr
(*  24*)			     (self, fn self => get_model_ self))
(*  24*)    val convert_child_path_to_path_ : cptr * cptr -> cptr
(*  24*)	= _import "gtk_tree_model_sort_convert_child_path_to_path"
(*  24*)		  : cptr * cptr -> cptr;
(*  24*)    val convert_child_path_to_path : 'a t -> tree_path -> tree_path
(*  24*)	= fn self => fn child_path =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => convert_child_path_to_path_
(*  24*)					   (self, child_path))
(*  24*)    val convert_childiter_toiter_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_model_sort_convert_child_iter_to_iter"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val convert_childiter_toiter
(*  24*)      : 'a t -> treeiter option -> treeiter -> unit
(*  24*)	= fn self => fn sort_iter => fn child_iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => convert_childiter_toiter_
(*  24*)			     (self, getOpt (sort_iter, GObject.null), 
(*  24*)			      child_iter))
(*  24*)    val convert_childiter_toiter' : 'a t -> treeiter -> unit
(*  24*)	= fn self => fn child_iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => convert_childiter_toiter_
(*  24*)			     (self, GObject.null, child_iter))
(*  24*)    val convert_path_to_child_path_ : cptr * cptr -> cptr
(*  24*)	= _import "gtk_tree_model_sort_convert_path_to_child_path"
(*  24*)		  : cptr * cptr -> cptr;
(*  24*)    val convert_path_to_child_path : 'a t -> tree_path -> tree_path
(*  24*)	= fn self => fn sorted_path =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => convert_path_to_child_path_
(*  24*)					   (self, sorted_path))
(*  24*)    val convertiter_to_childiter_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_model_sort_convert_iter_to_child_iter"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val convertiter_to_childiter
(*  24*)      : 'a t -> treeiter option -> treeiter -> unit
(*  24*)	= fn self => fn child_iter => fn sorted_iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => convertiter_to_childiter_
(*  24*)			     (self, getOpt (child_iter, GObject.null), 
(*  24*)			      sorted_iter))
(*  24*)    val convertiter_to_childiter' : 'a t -> treeiter -> unit
(*  24*)	= fn self => fn sorted_iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => convertiter_to_childiter_
(*  24*)			     (self, GObject.null, sorted_iter))
(*  24*)    val reset_default_sort_func_ : cptr -> unit
(*  24*)	= _import "gtk_tree_model_sort_reset_default_sort_func"
(*  24*)		  : cptr -> unit;
(*  24*)    val reset_default_sort_func : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => reset_default_sort_func_ self)
(*  24*)    val clear_cache_ : cptr -> unit
(*  24*)	= _import "gtk_tree_model_sort_clear_cache" : cptr -> unit;
(*  24*)    val clear_cache : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => clear_cache_ self)
(*  24*)end
(*  24*)structure TreeSelection :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a treeselection_t
(*  24*)    type 'a t = 'a treeselection_t Object.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTreeSelection : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val set_mode : 'a t -> selection_mode -> unit
(*  24*)    val get_mode : 'a t -> selection_mode
(*  24*)    val get_user_data : 'a t -> cptr
(*  24*)    val get_treeview : 'a t -> base t
(*  24*)    val get_selected : 'a t -> 'b TreeModel.t -> treeiter -> bool
(*  24*)    val select_path : 'a t -> tree_path -> unit
(*  24*)    val unselect_path : 'a t -> tree_path -> unit
(*  24*)    val selectiter : 'a t -> treeiter -> unit
(*  24*)    val unselectiter : 'a t -> treeiter -> unit
(*  24*)    val path_is_selected : 'a t -> tree_path -> bool
(*  24*)    val iter_is_selected : 'a t -> treeiter -> bool
(*  24*)    val select_all : 'a t -> unit
(*  24*)    val unselect_all : 'a t -> unit
(*  24*)    val select_range : 'a t -> tree_path -> tree_path -> unit
(*  24*)    val changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a treeselection_t = unit
(*  24*)    type 'a t = 'a treeselection_t Object.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Object.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTreeSelection obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_tree_selection_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val set_mode_ : cptr * int -> unit
(*  24*)	= _import "gtk_tree_selection_set_mode" : cptr * int -> unit;
(*  24*)    val set_mode : 'a t -> selection_mode -> unit
(*  24*)	= fn self => fn typ =>
(*  24*)	     GObject.withPtr (self, fn self => set_mode_ (self, typ))
(*  24*)    val get_mode_ : cptr -> int
(*  24*)	= _import "gtk_tree_selection_get_mode" : cptr -> int;
(*  24*)    val get_mode : 'a t -> selection_mode
(*  24*)	= fn self => GObject.withPtr (self, fn self => get_mode_ self)
(*  24*)    val get_user_data_ : cptr -> cptr
(*  24*)	= _import "gtk_tree_selection_get_user_data" : cptr -> cptr;
(*  24*)    val get_user_data : 'a t -> cptr
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_user_data_ self)
(*  24*)    val get_treeview_ : cptr -> cptr
(*  24*)	= _import "gtk_tree_selection_get_tree_view" : cptr -> cptr;
(*  24*)    val get_treeview : 'a t -> base t
(*  24*)	= fn self => make (GObject.withPtr
(*  24*)			     (self, fn self => get_treeview_ self))
(*  24*)    val get_selected_ : cptr * cptr * cptr -> bool
(*  24*)	= _import "gtk_tree_selection_get_selected"
(*  24*)		  : cptr * cptr * cptr -> bool;
(*  24*)    val get_selected : 'a t -> 'b TreeModel.t -> treeiter -> bool
(*  24*)	= fn self => fn model => fn iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (model, 
(*  24*)			      fn model => get_selected_
(*  24*)					    (self, model, iter)))
(*  24*)    val select_path_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_selection_select_path"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val select_path : 'a t -> tree_path -> unit
(*  24*)	= fn self => fn path =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => select_path_ (self, path))
(*  24*)    val unselect_path_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_selection_unselect_path"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val unselect_path : 'a t -> tree_path -> unit
(*  24*)	= fn self => fn path =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => unselect_path_ (self, path))
(*  24*)    val selectiter_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_selection_select_iter"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val selectiter : 'a t -> treeiter -> unit
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => selectiter_ (self, iter))
(*  24*)    val unselectiter_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_selection_unselect_iter"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val unselectiter : 'a t -> treeiter -> unit
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => unselectiter_ (self, iter))
(*  24*)    val path_is_selected_ : cptr * cptr -> bool
(*  24*)	= _import "gtk_tree_selection_path_is_selected"
(*  24*)		  : cptr * cptr -> bool;
(*  24*)    val path_is_selected : 'a t -> tree_path -> bool
(*  24*)	= fn self => fn path =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => path_is_selected_ (self, path))
(*  24*)    val iter_is_selected_ : cptr * cptr -> bool
(*  24*)	= _import "gtk_tree_selection_iter_is_selected"
(*  24*)		  : cptr * cptr -> bool;
(*  24*)    val iter_is_selected : 'a t -> treeiter -> bool
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => iter_is_selected_ (self, iter))
(*  24*)    val select_all_ : cptr -> unit
(*  24*)	= _import "gtk_tree_selection_select_all" : cptr -> unit;
(*  24*)    val select_all : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => select_all_ self)
(*  24*)    val unselect_all_ : cptr -> unit
(*  24*)	= _import "gtk_tree_selection_unselect_all" : cptr -> unit;
(*  24*)    val unselect_all : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => unselect_all_ self)
(*  24*)    val select_range_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_selection_select_range"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val select_range : 'a t -> tree_path -> tree_path -> unit
(*  24*)	= fn self => fn start_path => fn end_path =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => select_range_ (self, start_path, end_path))
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "changed" false (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure TreeStore :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a treestore_t
(*  24*)    type 'a t = 'a treestore_t TreeModel.t TreeDragSource.t
(*  24*)		  TreeDragDest.t
(*  24*)		  TreeSortable.t
(*  24*)		  GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTreeStore : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : int -> base t
(*  24*)    val set : 'a t -> treeiter -> unit
(*  24*)    val remove : 'a t -> treeiter -> unit
(*  24*)    val insert : 'a t -> treeiter -> treeiter -> int -> unit
(*  24*)    val insert_before
(*  24*)      : 'a t -> treeiter -> treeiter -> treeiter -> unit
(*  24*)    val insert_after : 'a t -> treeiter -> treeiter -> treeiter -> unit
(*  24*)    val prepend : 'a t -> treeiter -> treeiter -> unit
(*  24*)    val append : 'a t -> treeiter -> treeiter -> unit
(*  24*)    val is_ancestor : 'a t -> treeiter -> treeiter -> bool
(*  24*)    val storeiter_depth : 'a t -> treeiter -> int
(*  24*)    val clear : 'a t -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a treestore_t = unit
(*  24*)    type 'a t = 'a treestore_t TreeModel.t TreeDragSource.t
(*  24*)		  TreeDragDest.t
(*  24*)		  TreeSortable.t
(*  24*)		  GObject.t
(*  24*)    fun inherit w con
(*  24*)      = let val con = let val ptr = con () in fn () => ptr end
(*  24*)	    val witness = ()
(*  24*)	    val witness = TreeModel.inherit witness con
(*  24*)	    val witness = TreeDragSource.inherit witness con
(*  24*)	    val witness = TreeDragDest.inherit witness con
(*  24*)	    val witness = TreeSortable.inherit witness con
(*  24*)	in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTreeStore obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_tree_store_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : int -> cptr
(*  24*)	= _import "gtk_tree_store_new" : int -> cptr;
(*  24*)    val new : int -> base t = fn n_columns => make (new_ n_columns)
(*  24*)    val set_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_store_set" : cptr * cptr -> unit;
(*  24*)    val set : 'a t -> treeiter -> unit
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr (self, fn self => set_ (self, iter))
(*  24*)    val remove_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_store_remove" : cptr * cptr -> unit;
(*  24*)    val remove : 'a t -> treeiter -> unit
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr (self, fn self => remove_ (self, iter))
(*  24*)    val insert_ : cptr * cptr * cptr * int -> unit
(*  24*)	= _import "gtk_tree_store_insert"
(*  24*)		  : cptr * cptr * cptr * int -> unit;
(*  24*)    val insert : 'a t -> treeiter -> treeiter -> int -> unit
(*  24*)	= fn self => fn iter => fn parent => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => insert_ (self, iter, parent, position))
(*  24*)    val insert_before_ : cptr * cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_store_insert_before"
(*  24*)		  : cptr * cptr * cptr * cptr -> unit;
(*  24*)    val insert_before
(*  24*)      : 'a t -> treeiter -> treeiter -> treeiter -> unit
(*  24*)	= fn self => fn iter => fn parent => fn sibling =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => insert_before_
(*  24*)			     (self, iter, parent, sibling))
(*  24*)    val insert_after_ : cptr * cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_store_insert_after"
(*  24*)		  : cptr * cptr * cptr * cptr -> unit;
(*  24*)    val insert_after : 'a t -> treeiter -> treeiter -> treeiter -> unit
(*  24*)	= fn self => fn iter => fn parent => fn sibling =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => insert_after_ (self, iter, parent, sibling))
(*  24*)    val prepend_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_store_prepend"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val prepend : 'a t -> treeiter -> treeiter -> unit
(*  24*)	= fn self => fn iter => fn parent =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => prepend_ (self, iter, parent))
(*  24*)    val append_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_store_append" : cptr * cptr * cptr -> unit;
(*  24*)    val append : 'a t -> treeiter -> treeiter -> unit
(*  24*)	= fn self => fn iter => fn parent =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => append_ (self, iter, parent))
(*  24*)    val is_ancestor_ : cptr * cptr * cptr -> bool
(*  24*)	= _import "gtk_tree_store_is_ancestor"
(*  24*)		  : cptr * cptr * cptr -> bool;
(*  24*)    val is_ancestor : 'a t -> treeiter -> treeiter -> bool
(*  24*)	= fn self => fn iter => fn descendant =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => is_ancestor_ (self, iter, descendant))
(*  24*)    val storeiter_depth_ : cptr * cptr -> int
(*  24*)	= _import "gtk_tree_store_iter_depth" : cptr * cptr -> int;
(*  24*)    val storeiter_depth : 'a t -> treeiter -> int
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => storeiter_depth_ (self, iter))
(*  24*)    val clear_ : cptr -> unit
(*  24*)	= _import "gtk_tree_store_clear" : cptr -> unit;
(*  24*)    val clear : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => clear_ self)
(*  24*)end
(*  24*)structure TreeViewColumn :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a treeviewcolumn_t
(*  24*)    type 'a t = 'a treeviewcolumn_t Object.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTreeViewColumn : 'a t -> base t
(*  24*)    type sizing
(*  24*)    val TREE_VIEW_COLUMN_GROW_ONLY : sizing
(*  24*)    val TREE_VIEW_COLUMN_AUTOSIZE : sizing
(*  24*)    val TREE_VIEW_COLUMN_FIXED : sizing
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val new_with_attributes : string -> 'a CellRenderer.t -> base t
(*  24*)    val pack_start : 'a t -> 'b CellRenderer.t -> bool -> unit
(*  24*)    val pack_end : 'a t -> 'b CellRenderer.t -> bool -> unit
(*  24*)    val clear : 'a t -> unit
(*  24*)    val add_attribute
(*  24*)      : 'a t -> 'b CellRenderer.t -> string -> int -> unit
(*  24*)    val set_attributes : 'a t -> 'b CellRenderer.t -> unit
(*  24*)    val clear_attributes : 'a t -> 'b CellRenderer.t -> unit
(*  24*)    val set_spacing : 'a t -> int -> unit
(*  24*)    val get_spacing : 'a t -> int
(*  24*)    val set_visible : 'a t -> bool -> unit
(*  24*)    val get_visible : 'a t -> bool
(*  24*)    val set_resizable : 'a t -> bool -> unit
(*  24*)    val get_resizable : 'a t -> bool
(*  24*)    val set_sizing : 'a t -> sizing -> unit
(*  24*)    val get_sizing : 'a t -> int
(*  24*)    val get_width : 'a t -> int
(*  24*)    val get_fixed_width : 'a t -> int
(*  24*)    val set_fixed_width : 'a t -> int -> unit
(*  24*)    val set_min_width : 'a t -> int -> unit
(*  24*)    val get_min_width : 'a t -> int
(*  24*)    val set_max_width : 'a t -> int -> unit
(*  24*)    val get_max_width : 'a t -> int
(*  24*)    val clicked : 'a t -> unit
(*  24*)    val set_title : 'a t -> string -> unit
(*  24*)    val get_title : 'a t -> string
(*  24*)    val set_clickable : 'a t -> bool -> unit
(*  24*)    val get_clickable : 'a t -> bool
(*  24*)    val set_widget : 'a t -> 'b Widget.t option -> unit
(*  24*)    val set_widget' : 'a t -> unit
(*  24*)    val get_widget : 'a t -> base Widget.t
(*  24*)    val set_alignment : 'a t -> real -> unit
(*  24*)    val get_alignment : 'a t -> real
(*  24*)    val set_reorderable : 'a t -> bool -> unit
(*  24*)    val get_reorderable : 'a t -> bool
(*  24*)    val set_sort_column_id : 'a t -> int -> unit
(*  24*)    val get_sort_column_id : 'a t -> int
(*  24*)    val set_sort_indicator : 'a t -> bool -> unit
(*  24*)    val get_sort_indicator : 'a t -> bool
(*  24*)    val set_sort_order : 'a t -> sorttype -> unit
(*  24*)    val get_sort_order : 'a t -> sorttype
(*  24*)    val cell_set_cell_data
(*  24*)      : 'a t -> 'b TreeModel.t -> treeiter -> bool -> bool -> unit
(*  24*)    val cell_is_visible : 'a t -> bool
(*  24*)    val clicked_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a treeviewcolumn_t = unit
(*  24*)    type 'a t = 'a treeviewcolumn_t Object.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Object.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTreeViewColumn obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    type sizing = int
(*  24*)    val get_sizing_ : int ref * int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_treeviewcolumn_sizing"
(*  24*)		  : int ref * int ref * int ref -> unit;
(*  24*)    val (TREE_VIEW_COLUMN_GROW_ONLY, TREE_VIEW_COLUMN_AUTOSIZE, 
(*  24*)	 TREE_VIEW_COLUMN_FIXED)
(*  24*)	= let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
(*  24*)	  in get_sizing_ (x0, x1, x2)
(*  24*)	   ; (!x0, !x1, !x2)
(*  24*)	  end
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_tree_view_column_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_tree_view_column_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val new_with_attributes_ : CString.cstring * cptr -> cptr
(*  24*)	= _import "gtk_tree_view_column_new_with_attributes"
(*  24*)		  : CString.cstring * cptr -> cptr;
(*  24*)    val new_with_attributes : string -> 'a CellRenderer.t -> base t
(*  24*)	= fn title => fn cell =>
(*  24*)	     make (GObject.withPtr
(*  24*)		     (cell, 
(*  24*)		      fn cell => new_with_attributes_
(*  24*)				   (CString.fromString title, cell)))
(*  24*)    val pack_start_ : cptr * cptr * bool -> unit
(*  24*)	= _import "gtk_tree_view_column_pack_start"
(*  24*)		  : cptr * cptr * bool -> unit;
(*  24*)    val pack_start : 'a t -> 'b CellRenderer.t -> bool -> unit
(*  24*)	= fn self => fn cell => fn expand =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (cell, 
(*  24*)			      fn cell => pack_start_
(*  24*)					   (self, cell, expand)))
(*  24*)    val pack_end_ : cptr * cptr * bool -> unit
(*  24*)	= _import "gtk_tree_view_column_pack_end"
(*  24*)		  : cptr * cptr * bool -> unit;
(*  24*)    val pack_end : 'a t -> 'b CellRenderer.t -> bool -> unit
(*  24*)	= fn self => fn cell => fn expand =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (cell, fn cell => pack_end_ (self, cell, expand)))
(*  24*)    val clear_ : cptr -> unit
(*  24*)	= _import "gtk_tree_view_column_clear" : cptr -> unit;
(*  24*)    val clear : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => clear_ self)
(*  24*)    val add_attribute_ : cptr * cptr * CString.cstring * int -> unit
(*  24*)	= _import "gtk_tree_view_column_add_attribute"
(*  24*)		  : cptr * cptr * CString.cstring * int -> unit;
(*  24*)    val add_attribute
(*  24*)      : 'a t -> 'b CellRenderer.t -> string -> int -> unit
(*  24*)	= fn self => fn cell_renderer => fn attribute => fn column =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (cell_renderer, 
(*  24*)			      fn cell_renderer =>
(*  24*)				 add_attribute_
(*  24*)				   (self, cell_renderer, 
(*  24*)				    CString.fromString attribute, 
(*  24*)				    column)))
(*  24*)    val set_attributes_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_view_column_set_attributes"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val set_attributes : 'a t -> 'b CellRenderer.t -> unit
(*  24*)	= fn self => fn cell_renderer =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (cell_renderer, 
(*  24*)			      fn cell_renderer =>
(*  24*)				 set_attributes_
(*  24*)				   (self, cell_renderer)))
(*  24*)    val clear_attributes_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_view_column_clear_attributes"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val clear_attributes : 'a t -> 'b CellRenderer.t -> unit
(*  24*)	= fn self => fn cell_renderer =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (cell_renderer, 
(*  24*)			      fn cell_renderer =>
(*  24*)				 clear_attributes_
(*  24*)				   (self, cell_renderer)))
(*  24*)    val set_spacing_ : cptr * int -> unit
(*  24*)	= _import "gtk_tree_view_column_set_spacing"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_spacing : 'a t -> int -> unit
(*  24*)	= fn self => fn spacing =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_spacing_ (self, spacing))
(*  24*)    val get_spacing_ : cptr -> int
(*  24*)	= _import "gtk_tree_view_column_get_spacing" : cptr -> int;
(*  24*)    val get_spacing : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_spacing_ self)
(*  24*)    val set_visible_ : cptr * bool -> unit
(*  24*)	= _import "gtk_tree_view_column_set_visible"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_visible : 'a t -> bool -> unit
(*  24*)	= fn self => fn visible =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_visible_ (self, visible))
(*  24*)    val get_visible_ : cptr -> bool
(*  24*)	= _import "gtk_tree_view_column_get_visible" : cptr -> bool;
(*  24*)    val get_visible : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_visible_ self)
(*  24*)    val set_resizable_ : cptr * bool -> unit
(*  24*)	= _import "gtk_tree_view_column_set_resizable"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_resizable : 'a t -> bool -> unit
(*  24*)	= fn self => fn resizable =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_resizable_ (self, resizable))
(*  24*)    val get_resizable_ : cptr -> bool
(*  24*)	= _import "gtk_tree_view_column_get_resizable" : cptr -> bool;
(*  24*)    val get_resizable : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_resizable_ self)
(*  24*)    val set_sizing_ : cptr * int -> unit
(*  24*)	= _import "gtk_tree_view_column_set_sizing"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_sizing : 'a t -> sizing -> unit
(*  24*)	= fn self => fn typ =>
(*  24*)	     GObject.withPtr (self, fn self => set_sizing_ (self, typ))
(*  24*)    val get_sizing_ : cptr -> int
(*  24*)	= _import "gtk_tree_view_column_get_sizing" : cptr -> int;
(*  24*)    val get_sizing : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_sizing_ self)
(*  24*)    val get_width_ : cptr -> int
(*  24*)	= _import "gtk_tree_view_column_get_width" : cptr -> int;
(*  24*)    val get_width : 'a t -> int
(*  24*)	= fn self => GObject.withPtr (self, fn self => get_width_ self)
(*  24*)    val get_fixed_width_ : cptr -> int
(*  24*)	= _import "gtk_tree_view_column_get_fixed_width" : cptr -> int;
(*  24*)    val get_fixed_width : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_fixed_width_ self)
(*  24*)    val set_fixed_width_ : cptr * int -> unit
(*  24*)	= _import "gtk_tree_view_column_set_fixed_width"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_fixed_width : 'a t -> int -> unit
(*  24*)	= fn self => fn fixed_width =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_fixed_width_ (self, fixed_width))
(*  24*)    val set_min_width_ : cptr * int -> unit
(*  24*)	= _import "gtk_tree_view_column_set_min_width"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_min_width : 'a t -> int -> unit
(*  24*)	= fn self => fn min_width =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_min_width_ (self, min_width))
(*  24*)    val get_min_width_ : cptr -> int
(*  24*)	= _import "gtk_tree_view_column_get_min_width" : cptr -> int;
(*  24*)    val get_min_width : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_min_width_ self)
(*  24*)    val set_max_width_ : cptr * int -> unit
(*  24*)	= _import "gtk_tree_view_column_set_max_width"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_max_width : 'a t -> int -> unit
(*  24*)	= fn self => fn max_width =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_max_width_ (self, max_width))
(*  24*)    val get_max_width_ : cptr -> int
(*  24*)	= _import "gtk_tree_view_column_get_max_width" : cptr -> int;
(*  24*)    val get_max_width : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_max_width_ self)
(*  24*)    val clicked_ : cptr -> unit
(*  24*)	= _import "gtk_tree_view_column_clicked" : cptr -> unit;
(*  24*)    val clicked : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => clicked_ self)
(*  24*)    val set_title_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_tree_view_column_set_title"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_title : 'a t -> string -> unit
(*  24*)	= fn self => fn title =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_title_ (self, CString.fromString title))
(*  24*)    val get_title_ : cptr -> CString.t
(*  24*)	= _import "gtk_tree_view_column_get_title" : cptr -> CString.t;
(*  24*)    val get_title : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_title_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val set_clickable_ : cptr * bool -> unit
(*  24*)	= _import "gtk_tree_view_column_set_clickable"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_clickable : 'a t -> bool -> unit
(*  24*)	= fn self => fn active =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_clickable_ (self, active))
(*  24*)    val get_clickable_ : cptr -> bool
(*  24*)	= _import "gtk_tree_view_column_get_clickable" : cptr -> bool;
(*  24*)    val get_clickable : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_clickable_ self)
(*  24*)    val set_widget_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_view_column_set_widget"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val set_widget : 'a t -> 'b Widget.t option -> unit
(*  24*)	= fn self => fn widget =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withOpt
(*  24*)		     (widget, fn widget => set_widget_ (self, widget)))
(*  24*)    val set_widget' : 'a t -> unit
(*  24*)	= fn self =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_widget_ (self, GObject.null))
(*  24*)    val get_widget_ : cptr -> cptr
(*  24*)	= _import "gtk_tree_view_column_get_widget" : cptr -> cptr;
(*  24*)    val get_widget : 'a t -> base Widget.t
(*  24*)	= fn self => Widget.inherit
(*  24*)		       ()
(*  24*)		       (fn () => GObject.withPtr
(*  24*)				   (self, fn self => get_widget_ self))
(*  24*)    val set_alignment_ : cptr * real -> unit
(*  24*)	= _import "gtk_tree_view_column_set_alignment"
(*  24*)		  : cptr * real -> unit;
(*  24*)    val set_alignment : 'a t -> real -> unit
(*  24*)	= fn self => fn xalign =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_alignment_ (self, xalign))
(*  24*)    val get_alignment_ : cptr -> real
(*  24*)	= _import "gtk_tree_view_column_get_alignment" : cptr -> real;
(*  24*)    val get_alignment : 'a t -> real
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_alignment_ self)
(*  24*)    val set_reorderable_ : cptr * bool -> unit
(*  24*)	= _import "gtk_tree_view_column_set_reorderable"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_reorderable : 'a t -> bool -> unit
(*  24*)	= fn self => fn reorderable =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_reorderable_ (self, reorderable))
(*  24*)    val get_reorderable_ : cptr -> bool
(*  24*)	= _import "gtk_tree_view_column_get_reorderable"
(*  24*)		  : cptr -> bool;
(*  24*)    val get_reorderable : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_reorderable_ self)
(*  24*)    val set_sort_column_id_ : cptr * int -> unit
(*  24*)	= _import "gtk_tree_view_column_set_sort_column_id"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_sort_column_id : 'a t -> int -> unit
(*  24*)	= fn self => fn sort_column_id =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_sort_column_id_
(*  24*)					   (self, sort_column_id))
(*  24*)    val get_sort_column_id_ : cptr -> int
(*  24*)	= _import "gtk_tree_view_column_get_sort_column_id"
(*  24*)		  : cptr -> int;
(*  24*)    val get_sort_column_id : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_sort_column_id_ self)
(*  24*)    val set_sort_indicator_ : cptr * bool -> unit
(*  24*)	= _import "gtk_tree_view_column_set_sort_indicator"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_sort_indicator : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_sort_indicator_ (self, setting))
(*  24*)    val get_sort_indicator_ : cptr -> bool
(*  24*)	= _import "gtk_tree_view_column_get_sort_indicator"
(*  24*)		  : cptr -> bool;
(*  24*)    val get_sort_indicator : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_sort_indicator_ self)
(*  24*)    val set_sort_order_ : cptr * int -> unit
(*  24*)	= _import "gtk_tree_view_column_set_sort_order"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_sort_order : 'a t -> sorttype -> unit
(*  24*)	= fn self => fn order =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_sort_order_ (self, order))
(*  24*)    val get_sort_order_ : cptr -> int
(*  24*)	= _import "gtk_tree_view_column_get_sort_order" : cptr -> int;
(*  24*)    val get_sort_order : 'a t -> sorttype
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_sort_order_ self)
(*  24*)    val cell_set_cell_data_ : cptr * cptr * cptr * bool * bool -> unit
(*  24*)	= _import "gtk_tree_view_column_cell_set_cell_data"
(*  24*)		  : cptr * cptr * cptr * bool * bool -> unit;
(*  24*)    val cell_set_cell_data
(*  24*)      : 'a t -> 'b TreeModel.t -> treeiter -> bool -> bool -> unit
(*  24*)	= fn self => fn tree_model => fn iter => fn is_expander => 
(*  24*)	  fn is_expanded =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (tree_model, 
(*  24*)			      fn tree_model =>
(*  24*)				 cell_set_cell_data_
(*  24*)				   (self, tree_model, iter, 
(*  24*)				    is_expander, is_expanded)))
(*  24*)    val cell_is_visible_ : cptr -> bool
(*  24*)	= _import "gtk_tree_view_column_cell_is_visible"
(*  24*)		  : cptr -> bool;
(*  24*)    val cell_is_visible : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => cell_is_visible_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val clicked_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "clicked" false (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Separator :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a separator_t
(*  24*)    type 'a t = 'a separator_t Widget.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toSeparator : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val menu_item_get_type : unit -> int
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a separator_t = unit
(*  24*)    type 'a t = 'a separator_t Widget.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Widget.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toSeparator obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_separator_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val menu_item_get_type_ : unit -> int
(*  24*)	= _import "gtk_separator_menu_item_get_type" : unit -> int;
(*  24*)    val menu_item_get_type : unit -> int
(*  24*)	= fn dummy => menu_item_get_type_ dummy
(*  24*)end
(*  24*)structure VSeparator :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a vseparator_t
(*  24*)    type 'a t = 'a vseparator_t Separator.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toVSeparator : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a vseparator_t = unit
(*  24*)    type 'a t = 'a vseparator_t Separator.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Separator.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toVSeparator obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_vseparator_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_vseparator_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)end
(*  24*)structure HSeparator :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a hseparator_t
(*  24*)    type 'a t = 'a hseparator_t Separator.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toHSeparator : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a hseparator_t = unit
(*  24*)    type 'a t = 'a hseparator_t Separator.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Separator.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toHSeparator obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_hseparator_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_hseparator_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)end
(*  24*)structure Ruler :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a ruler_t
(*  24*)    type 'a t = 'a ruler_t Widget.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toRuler : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val set_metric : 'a t -> metrictype -> unit
(*  24*)    val set_range : 'a t -> real -> real -> real -> real -> unit
(*  24*)    val draw_ticks : 'a t -> unit
(*  24*)    val draw_pos : 'a t -> unit
(*  24*)    val get_metric : 'a t -> metrictype
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a ruler_t = unit
(*  24*)    type 'a t = 'a ruler_t Widget.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Widget.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toRuler obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_ruler_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val set_metric_ : cptr * int -> unit
(*  24*)	= _import "gtk_ruler_set_metric" : cptr * int -> unit;
(*  24*)    val set_metric : 'a t -> metrictype -> unit
(*  24*)	= fn self => fn metric =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_metric_ (self, metric))
(*  24*)    val set_range_ : cptr * real * real * real * real -> unit
(*  24*)	= _import "gtk_ruler_set_range"
(*  24*)		  : cptr * real * real * real * real -> unit;
(*  24*)    val set_range : 'a t -> real -> real -> real -> real -> unit
(*  24*)	= fn self => fn lower => fn upper => fn position => 
(*  24*)	  fn max_size =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_range_
(*  24*)					   (self, lower, upper, 
(*  24*)					    position, max_size))
(*  24*)    val draw_ticks_ : cptr -> unit
(*  24*)	= _import "gtk_ruler_draw_ticks" : cptr -> unit;
(*  24*)    val draw_ticks : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => draw_ticks_ self)
(*  24*)    val draw_pos_ : cptr -> unit
(*  24*)	= _import "gtk_ruler_draw_pos" : cptr -> unit;
(*  24*)    val draw_pos : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => draw_pos_ self)
(*  24*)    val get_metric_ : cptr -> int
(*  24*)	= _import "gtk_ruler_get_metric" : cptr -> int;
(*  24*)    val get_metric : 'a t -> metrictype
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_metric_ self)
(*  24*)end
(*  24*)structure VRuler :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a vruler_t
(*  24*)    type 'a t = 'a vruler_t Ruler.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toVRuler : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a vruler_t = unit
(*  24*)    type 'a t = 'a vruler_t Ruler.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Ruler.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toVRuler obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_vruler_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr = _import "gtk_vruler_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)end
(*  24*)structure HRuler :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a hruler_t
(*  24*)    type 'a t = 'a hruler_t Ruler.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toHRuler : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a hruler_t = unit
(*  24*)    type 'a t = 'a hruler_t Ruler.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Ruler.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toHRuler obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_hruler_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr = _import "gtk_hruler_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)end
(*  24*)structure Range :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a range_t
(*  24*)    type 'a t = 'a range_t Widget.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toRange : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val set_update_policy : 'a t -> updatetype -> unit
(*  24*)    val get_update_policy : 'a t -> updatetype
(*  24*)    val set_adjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)    val get_adjustment : 'a t -> base Adjustment.t
(*  24*)    val set_inverted : 'a t -> bool -> unit
(*  24*)    val get_inverted : 'a t -> bool
(*  24*)    val set_increments : 'a t -> real -> real -> unit
(*  24*)    val set_range : 'a t -> real -> real -> unit
(*  24*)    val set_value : 'a t -> real -> unit
(*  24*)    val get_value : 'a t -> real
(*  24*)    val value_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val adjust_bounds_sig : (real -> unit) -> 'a t Signal.signal
(*  24*)    val move_slider_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a range_t = unit
(*  24*)    type 'a t = 'a range_t Widget.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Widget.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toRange obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_range_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val set_update_policy_ : cptr * int -> unit
(*  24*)	= _import "gtk_range_set_update_policy" : cptr * int -> unit;
(*  24*)    val set_update_policy : 'a t -> updatetype -> unit
(*  24*)	= fn self => fn policy =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_update_policy_ (self, policy))
(*  24*)    val get_update_policy_ : cptr -> int
(*  24*)	= _import "gtk_range_get_update_policy" : cptr -> int;
(*  24*)    val get_update_policy : 'a t -> updatetype
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_update_policy_ self)
(*  24*)    val set_adjustment_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_range_set_adjustment" : cptr * cptr -> unit;
(*  24*)    val set_adjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)	= fn self => fn adjustment =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (adjustment, 
(*  24*)			      fn adjustment =>
(*  24*)				 set_adjustment_ (self, adjustment)))
(*  24*)    val get_adjustment_ : cptr -> cptr
(*  24*)	= _import "gtk_range_get_adjustment" : cptr -> cptr;
(*  24*)    val get_adjustment : 'a t -> base Adjustment.t
(*  24*)	= fn self =>
(*  24*)	     Adjustment.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, fn self => get_adjustment_ self))
(*  24*)    val set_inverted_ : cptr * bool -> unit
(*  24*)	= _import "gtk_range_set_inverted" : cptr * bool -> unit;
(*  24*)    val set_inverted : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_inverted_ (self, setting))
(*  24*)    val get_inverted_ : cptr -> bool
(*  24*)	= _import "gtk_range_get_inverted" : cptr -> bool;
(*  24*)    val get_inverted : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_inverted_ self)
(*  24*)    val set_increments_ : cptr * real * real -> unit
(*  24*)	= _import "gtk_range_set_increments"
(*  24*)		  : cptr * real * real -> unit;
(*  24*)    val set_increments : 'a t -> real -> real -> unit
(*  24*)	= fn self => fn step => fn page =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_increments_ (self, step, page))
(*  24*)    val set_range_ : cptr * real * real -> unit
(*  24*)	= _import "gtk_range_set_range" : cptr * real * real -> unit;
(*  24*)    val set_range : 'a t -> real -> real -> unit
(*  24*)	= fn self => fn min => fn max =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_range_ (self, min, max))
(*  24*)    val set_value_ : cptr * real -> unit
(*  24*)	= _import "gtk_range_set_value" : cptr * real -> unit;
(*  24*)    val set_value : 'a t -> real -> unit
(*  24*)	= fn self => fn value =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_value_ (self, value))
(*  24*)    val get_value_ : cptr -> real
(*  24*)	= _import "gtk_range_get_value" : cptr -> real;
(*  24*)    val get_value : 'a t -> real
(*  24*)	= fn self => GObject.withPtr (self, fn self => get_value_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val value_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "value-changed" false (void --> return_void) f
(*  24*)       val adjust_bounds_sig : (real -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "adjust-bounds" false (real --> return_void) f
(*  24*)       val move_slider_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "move-slider" false (unit --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Scrollbar :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a scrollbar_t
(*  24*)    type 'a t = 'a scrollbar_t Range.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toScrollbar : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a scrollbar_t = unit
(*  24*)    type 'a t = 'a scrollbar_t Range.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Range.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toScrollbar obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_scrollbar_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)end
(*  24*)structure VScrollbar :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a vscrollbar_t
(*  24*)    type 'a t = 'a vscrollbar_t Scrollbar.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toVScrollbar : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : 'a Adjustment.t option -> base t
(*  24*)    val new' : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a vscrollbar_t = unit
(*  24*)    type 'a t = 'a vscrollbar_t Scrollbar.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Scrollbar.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toVScrollbar obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_vscrollbar_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : cptr -> cptr
(*  24*)	= _import "gtk_vscrollbar_new" : cptr -> cptr;
(*  24*)    val new : 'a Adjustment.t option -> base t
(*  24*)	= fn adjustment =>
(*  24*)	     make (GObject.withOpt
(*  24*)		     (adjustment, fn adjustment => new_ adjustment))
(*  24*)    val new' : unit -> base t = fn dummy => make (new_ GObject.null)
(*  24*)end
(*  24*)structure HScrollbar :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a hscrollbar_t
(*  24*)    type 'a t = 'a hscrollbar_t Scrollbar.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toHScrollbar : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : 'a Adjustment.t option -> base t
(*  24*)    val new' : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a hscrollbar_t = unit
(*  24*)    type 'a t = 'a hscrollbar_t Scrollbar.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Scrollbar.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toHScrollbar obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_hscrollbar_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : cptr -> cptr
(*  24*)	= _import "gtk_hscrollbar_new" : cptr -> cptr;
(*  24*)    val new : 'a Adjustment.t option -> base t
(*  24*)	= fn adjustment =>
(*  24*)	     make (GObject.withOpt
(*  24*)		     (adjustment, fn adjustment => new_ adjustment))
(*  24*)    val new' : unit -> base t = fn dummy => make (new_ GObject.null)
(*  24*)end
(*  24*)structure Scale :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a scale_t
(*  24*)    type 'a t = 'a scale_t Range.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toScale : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val set_digits : 'a t -> int -> unit
(*  24*)    val get_digits : 'a t -> int
(*  24*)    val set_draw_value : 'a t -> bool -> unit
(*  24*)    val get_draw_value : 'a t -> bool
(*  24*)    val set_value_pos : 'a t -> positiontype -> unit
(*  24*)    val get_value_pos : 'a t -> positiontype
(*  24*)    val format_value_sig : (real -> char) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a scale_t = unit
(*  24*)    type 'a t = 'a scale_t Range.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Range.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toScale obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_scale_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val set_digits_ : cptr * int -> unit
(*  24*)	= _import "gtk_scale_set_digits" : cptr * int -> unit;
(*  24*)    val set_digits : 'a t -> int -> unit
(*  24*)	= fn self => fn digits =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_digits_ (self, digits))
(*  24*)    val get_digits_ : cptr -> int
(*  24*)	= _import "gtk_scale_get_digits" : cptr -> int;
(*  24*)    val get_digits : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_digits_ self)
(*  24*)    val set_draw_value_ : cptr * bool -> unit
(*  24*)	= _import "gtk_scale_set_draw_value" : cptr * bool -> unit;
(*  24*)    val set_draw_value : 'a t -> bool -> unit
(*  24*)	= fn self => fn draw_value =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_draw_value_ (self, draw_value))
(*  24*)    val get_draw_value_ : cptr -> bool
(*  24*)	= _import "gtk_scale_get_draw_value" : cptr -> bool;
(*  24*)    val get_draw_value : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_draw_value_ self)
(*  24*)    val set_value_pos_ : cptr * int -> unit
(*  24*)	= _import "gtk_scale_set_value_pos" : cptr * int -> unit;
(*  24*)    val set_value_pos : 'a t -> positiontype -> unit
(*  24*)	= fn self => fn pos =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_value_pos_ (self, pos))
(*  24*)    val get_value_pos_ : cptr -> int
(*  24*)	= _import "gtk_scale_get_value_pos" : cptr -> int;
(*  24*)    val get_value_pos : 'a t -> positiontype
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_value_pos_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val format_value_sig : (real -> char) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "format-value" false (real --> return_char) f
(*  24*)    end
(*  24*)end
(*  24*)structure VScale :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a vscale_t
(*  24*)    type 'a t = 'a vscale_t Scale.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toVScale : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : 'a Adjustment.t option -> base t
(*  24*)    val new' : unit -> base t
(*  24*)    val new_with_range : real -> real -> real -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a vscale_t = unit
(*  24*)    type 'a t = 'a vscale_t Scale.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Scale.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toVScale obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_vscale_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : cptr -> cptr = _import "gtk_vscale_new" : cptr -> cptr;
(*  24*)    val new : 'a Adjustment.t option -> base t
(*  24*)	= fn adjustment =>
(*  24*)	     make (GObject.withOpt
(*  24*)		     (adjustment, fn adjustment => new_ adjustment))
(*  24*)    val new' : unit -> base t = fn dummy => make (new_ GObject.null)
(*  24*)    val new_with_range_ : real * real * real -> cptr
(*  24*)	= _import "gtk_vscale_new_with_range"
(*  24*)		  : real * real * real -> cptr;
(*  24*)    val new_with_range : real -> real -> real -> base t
(*  24*)	= fn min => fn max => fn step =>
(*  24*)	     make (new_with_range_ (min, max, step))
(*  24*)end
(*  24*)structure HScale :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a hscale_t
(*  24*)    type 'a t = 'a hscale_t Scale.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toHScale : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : 'a Adjustment.t option -> base t
(*  24*)    val new' : unit -> base t
(*  24*)    val new_with_range : real -> real -> real -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a hscale_t = unit
(*  24*)    type 'a t = 'a hscale_t Scale.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Scale.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toHScale obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_hscale_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : cptr -> cptr = _import "gtk_hscale_new" : cptr -> cptr;
(*  24*)    val new : 'a Adjustment.t option -> base t
(*  24*)	= fn adjustment =>
(*  24*)	     make (GObject.withOpt
(*  24*)		     (adjustment, fn adjustment => new_ adjustment))
(*  24*)    val new' : unit -> base t = fn dummy => make (new_ GObject.null)
(*  24*)    val new_with_range_ : real * real * real -> cptr
(*  24*)	= _import "gtk_hscale_new_with_range"
(*  24*)		  : real * real * real -> cptr;
(*  24*)    val new_with_range : real -> real -> real -> base t
(*  24*)	= fn min => fn max => fn step =>
(*  24*)	     make (new_with_range_ (min, max, step))
(*  24*)end
(*  24*)structure Progress :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a progress_t
(*  24*)    type 'a t = 'a progress_t Widget.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toProgress : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val set_show_text : 'a t -> bool -> unit
(*  24*)    val set_text_alignment : 'a t -> real -> real -> unit
(*  24*)    val set_format_string : 'a t -> string -> unit
(*  24*)    val set_adjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)    val configure : 'a t -> real -> real -> real -> unit
(*  24*)    val set_percentage : 'a t -> real -> unit
(*  24*)    val set_value : 'a t -> real -> unit
(*  24*)    val get_value : 'a t -> real
(*  24*)    val set_activity_mode : 'a t -> bool -> unit
(*  24*)    val get_current_text : 'a t -> string
(*  24*)    val get_text_from_value : 'a t -> real -> string
(*  24*)    val get_current_percentage : 'a t -> real
(*  24*)    val get_percentage_from_value : 'a t -> real -> real
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a progress_t = unit
(*  24*)    type 'a t = 'a progress_t Widget.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Widget.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toProgress obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_progress_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val set_show_text_ : cptr * bool -> unit
(*  24*)	= _import "gtk_progress_set_show_text" : cptr * bool -> unit;
(*  24*)    val set_show_text : 'a t -> bool -> unit
(*  24*)	= fn self => fn show_text =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_show_text_ (self, show_text))
(*  24*)    val set_text_alignment_ : cptr * real * real -> unit
(*  24*)	= _import "gtk_progress_set_text_alignment"
(*  24*)		  : cptr * real * real -> unit;
(*  24*)    val set_text_alignment : 'a t -> real -> real -> unit
(*  24*)	= fn self => fn x_align => fn y_align =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_text_alignment_
(*  24*)					   (self, x_align, y_align))
(*  24*)    val set_format_string_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_progress_set_format_string"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_format_string : 'a t -> string -> unit
(*  24*)	= fn self => fn format =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_format_string_
(*  24*)			     (self, CString.fromString format))
(*  24*)    val set_adjustment_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_progress_set_adjustment" : cptr * cptr -> unit;
(*  24*)    val set_adjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)	= fn self => fn adjustment =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (adjustment, 
(*  24*)			      fn adjustment =>
(*  24*)				 set_adjustment_ (self, adjustment)))
(*  24*)    val configure_ : cptr * real * real * real -> unit
(*  24*)	= _import "gtk_progress_configure"
(*  24*)		  : cptr * real * real * real -> unit;
(*  24*)    val configure : 'a t -> real -> real -> real -> unit
(*  24*)	= fn self => fn value => fn min => fn max =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => configure_ (self, value, min, max))
(*  24*)    val set_percentage_ : cptr * real -> unit
(*  24*)	= _import "gtk_progress_set_percentage" : cptr * real -> unit;
(*  24*)    val set_percentage : 'a t -> real -> unit
(*  24*)	= fn self => fn percentage =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_percentage_ (self, percentage))
(*  24*)    val set_value_ : cptr * real -> unit
(*  24*)	= _import "gtk_progress_set_value" : cptr * real -> unit;
(*  24*)    val set_value : 'a t -> real -> unit
(*  24*)	= fn self => fn value =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_value_ (self, value))
(*  24*)    val get_value_ : cptr -> real
(*  24*)	= _import "gtk_progress_get_value" : cptr -> real;
(*  24*)    val get_value : 'a t -> real
(*  24*)	= fn self => GObject.withPtr (self, fn self => get_value_ self)
(*  24*)    val set_activity_mode_ : cptr * bool -> unit
(*  24*)	= _import "gtk_progress_set_activity_mode"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_activity_mode : 'a t -> bool -> unit
(*  24*)	= fn self => fn activity_mode =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_activity_mode_
(*  24*)					   (self, activity_mode))
(*  24*)    val get_current_text_ : cptr -> CString.t
(*  24*)	= _import "gtk_progress_get_current_text" : cptr -> CString.t;
(*  24*)    val get_current_text : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_current_text_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val get_text_from_value_ : cptr * real -> CString.t
(*  24*)	= _import "gtk_progress_get_text_from_value"
(*  24*)		  : cptr * real -> CString.t;
(*  24*)    val get_text_from_value : 'a t -> real -> string
(*  24*)	= fn self => fn value =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => let val t = get_text_from_value_
(*  24*)					 (self, value)
(*  24*)			   in CString.toString t end)
(*  24*)    val get_current_percentage_ : cptr -> real
(*  24*)	= _import "gtk_progress_get_current_percentage" : cptr -> real;
(*  24*)    val get_current_percentage : 'a t -> real
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_current_percentage_ self)
(*  24*)    val get_percentage_from_value_ : cptr * real -> real
(*  24*)	= _import "gtk_progress_get_percentage_from_value"
(*  24*)		  : cptr * real -> real;
(*  24*)    val get_percentage_from_value : 'a t -> real -> real
(*  24*)	= fn self => fn value =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => get_percentage_from_value_
(*  24*)					   (self, value))
(*  24*)end
(*  24*)structure ProgressBar :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a progressbar_t
(*  24*)    type 'a t = 'a progressbar_t Progress.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toProgressBar : 'a t -> base t
(*  24*)    type style
(*  24*)    val PROGRESS_CONTINUOUS : style
(*  24*)    val PROGRESS_DISCRETE : style
(*  24*)    type orientation
(*  24*)    val PROGRESS_LEFT_TO_RIGHT : orientation
(*  24*)    val PROGRESS_RIGHT_TO_LEFT : orientation
(*  24*)    val PROGRESS_BOTTOM_TO_TOP : orientation
(*  24*)    val PROGRESS_TOP_TO_BOTTOM : orientation
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val pulse : 'a t -> unit
(*  24*)    val set_text : 'a t -> string -> unit
(*  24*)    val set_fraction : 'a t -> real -> unit
(*  24*)    val set_pulse_step : 'a t -> real -> unit
(*  24*)    val set_orientation : 'a t -> orientation -> unit
(*  24*)    val get_text : 'a t -> string
(*  24*)    val get_fraction : 'a t -> real
(*  24*)    val get_pulse_step : 'a t -> real
(*  24*)    val get_orientation : 'a t -> orientation
(*  24*)    val new_with_adjustment : 'a Adjustment.t option -> base t
(*  24*)    val new_with_adjustment' : unit -> base t
(*  24*)    val set_bar_style : 'a t -> style -> unit
(*  24*)    val set_discrete_blocks : 'a t -> int -> unit
(*  24*)    val set_activity_step : 'a t -> int -> unit
(*  24*)    val set_activity_blocks : 'a t -> int -> unit
(*  24*)    val update : 'a t -> real -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a progressbar_t = unit
(*  24*)    type 'a t = 'a progressbar_t Progress.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Progress.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toProgressBar obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    type style = int
(*  24*)    val get_style_ : int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_progressbar_style"
(*  24*)		  : int ref * int ref -> unit;
(*  24*)    val (PROGRESS_CONTINUOUS, PROGRESS_DISCRETE)
(*  24*)	= let val (x0, x1) = (ref 0, ref 0) in get_style_ (x0, x1)
(*  24*)					     ; (!x0, !x1)
(*  24*)					    end
(*  24*)    type orientation = int
(*  24*)    val get_orientation_
(*  24*)      : int ref * int ref * int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_progressbar_orientation"
(*  24*)		  : int ref * int ref * int ref * int ref -> unit;
(*  24*)    val (PROGRESS_LEFT_TO_RIGHT, PROGRESS_RIGHT_TO_LEFT, 
(*  24*)	 PROGRESS_BOTTOM_TO_TOP, PROGRESS_TOP_TO_BOTTOM)
(*  24*)	= let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)	  in get_orientation_ (x0, x1, x2, x3)
(*  24*)	   ; (!x0, !x1, !x2, !x3)
(*  24*)	  end
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_progress_bar_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_progress_bar_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val pulse_ : cptr -> unit
(*  24*)	= _import "gtk_progress_bar_pulse" : cptr -> unit;
(*  24*)    val pulse : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => pulse_ self)
(*  24*)    val set_text_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_progress_bar_set_text"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_text : 'a t -> string -> unit
(*  24*)	= fn self => fn text =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_text_ (self, CString.fromString text))
(*  24*)    val set_fraction_ : cptr * real -> unit
(*  24*)	= _import "gtk_progress_bar_set_fraction"
(*  24*)		  : cptr * real -> unit;
(*  24*)    val set_fraction : 'a t -> real -> unit
(*  24*)	= fn self => fn fraction =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_fraction_ (self, fraction))
(*  24*)    val set_pulse_step_ : cptr * real -> unit
(*  24*)	= _import "gtk_progress_bar_set_pulse_step"
(*  24*)		  : cptr * real -> unit;
(*  24*)    val set_pulse_step : 'a t -> real -> unit
(*  24*)	= fn self => fn fraction =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_pulse_step_ (self, fraction))
(*  24*)    val set_orientation_ : cptr * int -> unit
(*  24*)	= _import "gtk_progress_bar_set_orientation"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_orientation : 'a t -> orientation -> unit
(*  24*)	= fn self => fn orientation =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_orientation_ (self, orientation))
(*  24*)    val get_text_ : cptr -> CString.t
(*  24*)	= _import "gtk_progress_bar_get_text" : cptr -> CString.t;
(*  24*)    val get_text : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_text_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val get_fraction_ : cptr -> real
(*  24*)	= _import "gtk_progress_bar_get_fraction" : cptr -> real;
(*  24*)    val get_fraction : 'a t -> real
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_fraction_ self)
(*  24*)    val get_pulse_step_ : cptr -> real
(*  24*)	= _import "gtk_progress_bar_get_pulse_step" : cptr -> real;
(*  24*)    val get_pulse_step : 'a t -> real
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_pulse_step_ self)
(*  24*)    val get_orientation_ : cptr -> int
(*  24*)	= _import "gtk_progress_bar_get_orientation" : cptr -> int;
(*  24*)    val get_orientation : 'a t -> orientation
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_orientation_ self)
(*  24*)    val new_with_adjustment_ : cptr -> cptr
(*  24*)	= _import "gtk_progress_bar_new_with_adjustment"
(*  24*)		  : cptr -> cptr;
(*  24*)    val new_with_adjustment : 'a Adjustment.t option -> base t
(*  24*)	= fn adjustment =>
(*  24*)	     make (GObject.withOpt
(*  24*)		     (adjustment, 
(*  24*)		      fn adjustment =>
(*  24*)			 new_with_adjustment_ adjustment))
(*  24*)    val new_with_adjustment' : unit -> base t
(*  24*)	= fn dummy => make (new_with_adjustment_ GObject.null)
(*  24*)    val set_bar_style_ : cptr * int -> unit
(*  24*)	= _import "gtk_progress_bar_set_bar_style"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_bar_style : 'a t -> style -> unit
(*  24*)	= fn self => fn style =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_bar_style_ (self, style))
(*  24*)    val set_discrete_blocks_ : cptr * int -> unit
(*  24*)	= _import "gtk_progress_bar_set_discrete_blocks"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_discrete_blocks : 'a t -> int -> unit
(*  24*)	= fn self => fn blocks =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_discrete_blocks_ (self, blocks))
(*  24*)    val set_activity_step_ : cptr * int -> unit
(*  24*)	= _import "gtk_progress_bar_set_activity_step"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_activity_step : 'a t -> int -> unit
(*  24*)	= fn self => fn step =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_activity_step_ (self, step))
(*  24*)    val set_activity_blocks_ : cptr * int -> unit
(*  24*)	= _import "gtk_progress_bar_set_activity_blocks"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_activity_blocks : 'a t -> int -> unit
(*  24*)	= fn self => fn blocks =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_activity_blocks_ (self, blocks))
(*  24*)    val update_ : cptr * real -> unit
(*  24*)	= _import "gtk_progress_bar_update" : cptr * real -> unit;
(*  24*)    val update : 'a t -> real -> unit
(*  24*)	= fn self => fn percentage =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => update_ (self, percentage))
(*  24*)end
(*  24*)structure Preview :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a preview_t
(*  24*)    type 'a t = 'a preview_t Widget.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toPreview : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val uninit : unit -> unit
(*  24*)    val new : previewtype -> base t
(*  24*)    val size : 'a t -> int -> int -> unit
(*  24*)    val set_expand : 'a t -> bool -> unit
(*  24*)    val set_gamma : real -> unit
(*  24*)    val set_color_cube : int -> int -> int -> int -> unit
(*  24*)    val set_install_cmap : int -> unit
(*  24*)    val set_reserved : int -> unit
(*  24*)    val reset : unit -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a preview_t = unit
(*  24*)    type 'a t = 'a preview_t Widget.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Widget.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toPreview obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_preview_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val uninit_ : unit -> unit
(*  24*)	= _import "gtk_preview_uninit" : unit -> unit;
(*  24*)    val uninit : unit -> unit = fn dummy => uninit_ dummy
(*  24*)    val new_ : int -> cptr = _import "gtk_preview_new" : int -> cptr;
(*  24*)    val new : previewtype -> base t = fn typ => make (new_ typ)
(*  24*)    val size_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_preview_size" : cptr * int * int -> unit;
(*  24*)    val size : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn width => fn height =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => size_ (self, width, height))
(*  24*)    val set_expand_ : cptr * bool -> unit
(*  24*)	= _import "gtk_preview_set_expand" : cptr * bool -> unit;
(*  24*)    val set_expand : 'a t -> bool -> unit
(*  24*)	= fn self => fn expand =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_expand_ (self, expand))
(*  24*)    val set_gamma_ : real -> unit
(*  24*)	= _import "gtk_preview_set_gamma" : real -> unit;
(*  24*)    val set_gamma : real -> unit = fn gamma => set_gamma_ gamma
(*  24*)    val set_color_cube_ : int * int * int * int -> unit
(*  24*)	= _import "gtk_preview_set_color_cube"
(*  24*)		  : int * int * int * int -> unit;
(*  24*)    val set_color_cube : int -> int -> int -> int -> unit
(*  24*)	= fn nred_shades => fn ngreen_shades => fn nblue_shades => 
(*  24*)	  fn ngray_shades =>
(*  24*)	     set_color_cube_ (nred_shades, ngreen_shades, 
(*  24*)			      nblue_shades, ngray_shades)
(*  24*)    val set_install_cmap_ : int -> unit
(*  24*)	= _import "gtk_preview_set_install_cmap" : int -> unit;
(*  24*)    val set_install_cmap : int -> unit
(*  24*)	= fn install_cmap => set_install_cmap_ install_cmap
(*  24*)    val set_reserved_ : int -> unit
(*  24*)	= _import "gtk_preview_set_reserved" : int -> unit;
(*  24*)    val set_reserved : int -> unit
(*  24*)	= fn nreserved => set_reserved_ nreserved
(*  24*)    val reset_ : unit -> unit
(*  24*)	= _import "gtk_preview_reset" : unit -> unit;
(*  24*)    val reset : unit -> unit = fn dummy => reset_ dummy
(*  24*)end
(*  24*)structure OldEditable :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a oldeditable_t
(*  24*)    type 'a t = 'a oldeditable_t Editable.t Widget.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toOldEditable : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val changed : 'a t -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a oldeditable_t = unit
(*  24*)    type 'a t = 'a oldeditable_t Editable.t Widget.t
(*  24*)    fun inherit w con
(*  24*)      = let val con = let val ptr = con () in fn () => ptr end
(*  24*)	    val witness = ()
(*  24*)	    val witness = Editable.inherit witness con
(*  24*)	in Widget.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toOldEditable obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_old_editable_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val changed_ : cptr -> unit
(*  24*)	= _import "gtk_old_editable_changed" : cptr -> unit;
(*  24*)    val changed : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => changed_ self)
(*  24*)end
(*  24*)structure Misc :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a misc_t
(*  24*)    type 'a t = 'a misc_t Widget.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toMisc : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val set_alignment : 'a t -> real -> real -> unit
(*  24*)    val set_padding : 'a t -> int -> int -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a misc_t = unit
(*  24*)    type 'a t = 'a misc_t Widget.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Widget.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toMisc obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_misc_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val set_alignment_ : cptr * real * real -> unit
(*  24*)	= _import "gtk_misc_set_alignment"
(*  24*)		  : cptr * real * real -> unit;
(*  24*)    val set_alignment : 'a t -> real -> real -> unit
(*  24*)	= fn self => fn xalign => fn yalign =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_alignment_ (self, xalign, yalign))
(*  24*)    val set_padding_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_misc_set_padding" : cptr * int * int -> unit;
(*  24*)    val set_padding : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn xpad => fn ypad =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_padding_ (self, xpad, ypad))
(*  24*)end
(*  24*)structure Pixmap :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a pixmap_t
(*  24*)    type 'a t = 'a pixmap_t Misc.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toPixmap : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val set_build_insensitive : 'a t -> bool -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a pixmap_t = unit
(*  24*)    type 'a t = 'a pixmap_t Misc.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Misc.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toPixmap obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_pixmap_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val set_build_insensitive_ : cptr * bool -> unit
(*  24*)	= _import "gtk_pixmap_set_build_insensitive"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_build_insensitive : 'a t -> bool -> unit
(*  24*)	= fn self => fn build =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_build_insensitive_ (self, build))
(*  24*)end
(*  24*)structure Arrow :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a arrow_t
(*  24*)    type 'a t = 'a arrow_t Misc.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toArrow : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : arrowtype -> shadowtype -> base t
(*  24*)    val set : 'a t -> arrowtype -> shadowtype -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a arrow_t = unit
(*  24*)    type 'a t = 'a arrow_t Misc.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Misc.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toArrow obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_arrow_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : int * int -> cptr
(*  24*)	= _import "gtk_arrow_new" : int * int -> cptr;
(*  24*)    val new : arrowtype -> shadowtype -> base t
(*  24*)	= fn arrow_type => fn shadow_type =>
(*  24*)	     make (new_ (arrow_type, shadow_type))
(*  24*)    val set_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_arrow_set" : cptr * int * int -> unit;
(*  24*)    val set : 'a t -> arrowtype -> shadowtype -> unit
(*  24*)	= fn self => fn arrow_type => fn shadow_type =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_ (self, arrow_type, shadow_type))
(*  24*)end
(*  24*)structure Image :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a image_t
(*  24*)    type 'a t = 'a image_t Misc.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toImage : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val new_from_file : string -> base t
(*  24*)    val new_from_stock : string -> icon_size -> base t
(*  24*)    val new_from_icon_set : icon_set -> icon_size -> base t
(*  24*)    val set_from_file : 'a t -> string option -> unit
(*  24*)    val set_from_file' : 'a t -> unit
(*  24*)    val set_from_stock : 'a t -> string -> icon_size -> unit
(*  24*)    val set_from_icon_set : 'a t -> icon_set -> icon_size -> unit
(*  24*)    val get_storagetype : 'a t -> imagetype
(*  24*)    val menu_item_get_type : unit -> int
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a image_t = unit
(*  24*)    type 'a t = 'a image_t Misc.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Misc.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toImage obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_image_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr = _import "gtk_image_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val new_from_file_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_image_new_from_file" : CString.cstring -> cptr;
(*  24*)    val new_from_file : string -> base t
(*  24*)	= fn filename => make (new_from_file_
(*  24*)				 (CString.fromString filename))
(*  24*)    val new_from_stock_ : CString.cstring * int -> cptr
(*  24*)	= _import "gtk_image_new_from_stock"
(*  24*)		  : CString.cstring * int -> cptr;
(*  24*)    val new_from_stock : string -> icon_size -> base t
(*  24*)	= fn stock_id => fn size =>
(*  24*)	     make (new_from_stock_ (CString.fromString stock_id, size))
(*  24*)    val new_from_icon_set_ : cptr * int -> cptr
(*  24*)	= _import "gtk_image_new_from_icon_set" : cptr * int -> cptr;
(*  24*)    val new_from_icon_set : icon_set -> icon_size -> base t
(*  24*)	= fn icon_set => fn size =>
(*  24*)	     make (new_from_icon_set_ (icon_set, size))
(*  24*)    val set_from_file_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_image_set_from_file"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_from_file : 'a t -> string option -> unit
(*  24*)	= fn self => fn filename =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_from_file_ (self, 
(*  24*)					   CString.fromString
(*  24*)					     (getOpt (filename, ""))))
(*  24*)    val set_from_file' : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => set_from_file_
(*  24*)				     (self, CString.fromString ""))
(*  24*)    val set_from_stock_ : cptr * CString.cstring * int -> unit
(*  24*)	= _import "gtk_image_set_from_stock"
(*  24*)		  : cptr * CString.cstring * int -> unit;
(*  24*)    val set_from_stock : 'a t -> string -> icon_size -> unit
(*  24*)	= fn self => fn stock_id => fn size =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_from_stock_
(*  24*)			     (self, CString.fromString stock_id, size))
(*  24*)    val set_from_icon_set_ : cptr * cptr * int -> unit
(*  24*)	= _import "gtk_image_set_from_icon_set"
(*  24*)		  : cptr * cptr * int -> unit;
(*  24*)    val set_from_icon_set : 'a t -> icon_set -> icon_size -> unit
(*  24*)	= fn self => fn icon_set => fn size =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_from_icon_set_
(*  24*)					   (self, icon_set, size))
(*  24*)    val get_storagetype_ : cptr -> int
(*  24*)	= _import "gtk_image_get_storage_type" : cptr -> int;
(*  24*)    val get_storagetype : 'a t -> imagetype
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_storagetype_ self)
(*  24*)    val menu_item_get_type_ : unit -> int
(*  24*)	= _import "gtk_image_menu_item_get_type" : unit -> int;
(*  24*)    val menu_item_get_type : unit -> int
(*  24*)	= fn dummy => menu_item_get_type_ dummy
(*  24*)end
(*  24*)structure Label :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a label_t
(*  24*)    type 'a t = 'a label_t Misc.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toLabel : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : string option -> base t
(*  24*)    val new' : unit -> base t
(*  24*)    val new_with_mnemonic : string option -> base t
(*  24*)    val new_with_mnemonic' : unit -> base t
(*  24*)    val set_text : 'a t -> string -> unit
(*  24*)    val get_text : 'a t -> string
(*  24*)    val set_label : 'a t -> string -> unit
(*  24*)    val get_label : 'a t -> string
(*  24*)    val set_markup : 'a t -> string -> unit
(*  24*)    val set_use_markup : 'a t -> bool -> unit
(*  24*)    val get_use_markup : 'a t -> bool
(*  24*)    val set_use_underline : 'a t -> bool -> unit
(*  24*)    val get_use_underline : 'a t -> bool
(*  24*)    val set_markup_with_mnemonic : 'a t -> string -> unit
(*  24*)    val get_mnemonic_keyval : 'a t -> int
(*  24*)    val set_mnemonic_widget : 'a t -> 'b Widget.t -> unit
(*  24*)    val get_mnemonic_widget : 'a t -> base Widget.t
(*  24*)    val set_text_with_mnemonic : 'a t -> string -> unit
(*  24*)    val set_justify : 'a t -> justification -> unit
(*  24*)    val get_justify : 'a t -> justification
(*  24*)    val set_pattern : 'a t -> string -> unit
(*  24*)    val set_line_wrap : 'a t -> bool -> unit
(*  24*)    val get_line_wrap : 'a t -> bool
(*  24*)    val set_selectable : 'a t -> bool -> unit
(*  24*)    val get_selectable : 'a t -> bool
(*  24*)    val select_region : 'a t -> int -> int -> unit
(*  24*)    val set : 'a t -> string -> unit
(*  24*)    val parse_uline : 'a t -> string -> int
(*  24*)    val move_cursor_sig : (unit -> int -> bool -> unit)
(*  24*)			  -> 'a t Signal.signal
(*  24*)    val copy_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val populate_popup_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a label_t = unit
(*  24*)    type 'a t = 'a label_t Misc.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Misc.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toLabel obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_label_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_label_new" : CString.cstring -> cptr;
(*  24*)    val new : string option -> base t
(*  24*)	= fn str => make (new_ (CString.fromString (getOpt (str, ""))))
(*  24*)    val new' : unit -> base t
(*  24*)	= fn dummy => make (new_ (CString.fromString ""))
(*  24*)    val new_with_mnemonic_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_label_new_with_mnemonic"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val new_with_mnemonic : string option -> base t
(*  24*)	= fn str => make (new_with_mnemonic_
(*  24*)			    (CString.fromString (getOpt (str, ""))))
(*  24*)    val new_with_mnemonic' : unit -> base t
(*  24*)	= fn dummy => make (new_with_mnemonic_ (CString.fromString ""))
(*  24*)    val set_text_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_label_set_text"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_text : 'a t -> string -> unit
(*  24*)	= fn self => fn str =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_text_ (self, CString.fromString str))
(*  24*)    val get_text_ : cptr -> CString.t
(*  24*)	= _import "gtk_label_get_text" : cptr -> CString.t;
(*  24*)    val get_text : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_text_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val set_label_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_label_set_label"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_label : 'a t -> string -> unit
(*  24*)	= fn self => fn str =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_label_ (self, CString.fromString str))
(*  24*)    val get_label_ : cptr -> CString.t
(*  24*)	= _import "gtk_label_get_label" : cptr -> CString.t;
(*  24*)    val get_label : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_label_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val set_markup_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_label_set_markup"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_markup : 'a t -> string -> unit
(*  24*)	= fn self => fn str =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_markup_ (self, CString.fromString str))
(*  24*)    val set_use_markup_ : cptr * bool -> unit
(*  24*)	= _import "gtk_label_set_use_markup" : cptr * bool -> unit;
(*  24*)    val set_use_markup : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_use_markup_ (self, setting))
(*  24*)    val get_use_markup_ : cptr -> bool
(*  24*)	= _import "gtk_label_get_use_markup" : cptr -> bool;
(*  24*)    val get_use_markup : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_use_markup_ self)
(*  24*)    val set_use_underline_ : cptr * bool -> unit
(*  24*)	= _import "gtk_label_set_use_underline" : cptr * bool -> unit;
(*  24*)    val set_use_underline : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_use_underline_ (self, setting))
(*  24*)    val get_use_underline_ : cptr -> bool
(*  24*)	= _import "gtk_label_get_use_underline" : cptr -> bool;
(*  24*)    val get_use_underline : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_use_underline_ self)
(*  24*)    val set_markup_with_mnemonic_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_label_set_markup_with_mnemonic"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_markup_with_mnemonic : 'a t -> string -> unit
(*  24*)	= fn self => fn str =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_markup_with_mnemonic_
(*  24*)			     (self, CString.fromString str))
(*  24*)    val get_mnemonic_keyval_ : cptr -> int
(*  24*)	= _import "gtk_label_get_mnemonic_keyval" : cptr -> int;
(*  24*)    val get_mnemonic_keyval : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_mnemonic_keyval_ self)
(*  24*)    val set_mnemonic_widget_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_label_set_mnemonic_widget"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val set_mnemonic_widget : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn widget =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (widget, 
(*  24*)			      fn widget => set_mnemonic_widget_
(*  24*)					     (self, widget)))
(*  24*)    val get_mnemonic_widget_ : cptr -> cptr
(*  24*)	= _import "gtk_label_get_mnemonic_widget" : cptr -> cptr;
(*  24*)    val get_mnemonic_widget : 'a t -> base Widget.t
(*  24*)	= fn self =>
(*  24*)	     Widget.inherit
(*  24*)	       ()
(*  24*)	       (fn () =>
(*  24*)		   GObject.withPtr
(*  24*)		     (self, fn self => get_mnemonic_widget_ self))
(*  24*)    val set_text_with_mnemonic_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_label_set_text_with_mnemonic"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_text_with_mnemonic : 'a t -> string -> unit
(*  24*)	= fn self => fn str =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_text_with_mnemonic_
(*  24*)			     (self, CString.fromString str))
(*  24*)    val set_justify_ : cptr * int -> unit
(*  24*)	= _import "gtk_label_set_justify" : cptr * int -> unit;
(*  24*)    val set_justify : 'a t -> justification -> unit
(*  24*)	= fn self => fn jtype =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_justify_ (self, jtype))
(*  24*)    val get_justify_ : cptr -> int
(*  24*)	= _import "gtk_label_get_justify" : cptr -> int;
(*  24*)    val get_justify : 'a t -> justification
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_justify_ self)
(*  24*)    val set_pattern_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_label_set_pattern"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_pattern : 'a t -> string -> unit
(*  24*)	= fn self => fn pattern =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_pattern_
(*  24*)			     (self, CString.fromString pattern))
(*  24*)    val set_line_wrap_ : cptr * bool -> unit
(*  24*)	= _import "gtk_label_set_line_wrap" : cptr * bool -> unit;
(*  24*)    val set_line_wrap : 'a t -> bool -> unit
(*  24*)	= fn self => fn wrap =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_line_wrap_ (self, wrap))
(*  24*)    val get_line_wrap_ : cptr -> bool
(*  24*)	= _import "gtk_label_get_line_wrap" : cptr -> bool;
(*  24*)    val get_line_wrap : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_line_wrap_ self)
(*  24*)    val set_selectable_ : cptr * bool -> unit
(*  24*)	= _import "gtk_label_set_selectable" : cptr * bool -> unit;
(*  24*)    val set_selectable : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_selectable_ (self, setting))
(*  24*)    val get_selectable_ : cptr -> bool
(*  24*)	= _import "gtk_label_get_selectable" : cptr -> bool;
(*  24*)    val get_selectable : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_selectable_ self)
(*  24*)    val select_region_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_label_select_region" : cptr * int * int -> unit;
(*  24*)    val select_region : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn start_offset => fn end_offset =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => select_region_
(*  24*)			     (self, start_offset, end_offset))
(*  24*)    val set_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_label_set" : cptr * CString.cstring -> unit;
(*  24*)    val set : 'a t -> string -> unit
(*  24*)	= fn self => fn str =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_ (self, CString.fromString str))
(*  24*)    val parse_uline_ : cptr * CString.cstring -> int
(*  24*)	= _import "gtk_label_parse_uline"
(*  24*)		  : cptr * CString.cstring -> int;
(*  24*)    val parse_uline : 'a t -> string -> int
(*  24*)	= fn self => fn string =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => parse_uline_
(*  24*)			     (self, CString.fromString string))
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val move_cursor_sig : (unit -> int -> bool -> unit)
(*  24*)			     -> 'a t Signal.signal
(*  24*)	   = fn f => signal "move-cursor" false
(*  24*)			    (unit --> int --> bool --> return_void) f
(*  24*)       val copy_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "copy-clipboard" false
(*  24*)			    (void --> return_void) f
(*  24*)       val populate_popup_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "populate-popup" false
(*  24*)			    (unit --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure AccelLabel :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a accellabel_t
(*  24*)    type 'a t = 'a accellabel_t Label.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toAccelLabel : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : string -> base t
(*  24*)    val accelerator_width : 'a t -> int
(*  24*)    val get_accel_widget : 'a t -> base Widget.t
(*  24*)    val get_accel_width : 'a t -> int
(*  24*)    val set_accel_widget : 'a t -> 'b Widget.t -> unit
(*  24*)    val refetch : 'a t -> bool
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a accellabel_t = unit
(*  24*)    type 'a t = 'a accellabel_t Label.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Label.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toAccelLabel obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_accel_label_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_accel_label_new" : CString.cstring -> cptr;
(*  24*)    val new : string -> base t
(*  24*)	= fn string => make (new_ (CString.fromString string))
(*  24*)    val accelerator_width_ : cptr -> int
(*  24*)	= _import "gtk_accel_label_accelerator_width" : cptr -> int;
(*  24*)    val accelerator_width : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => accelerator_width_ self)
(*  24*)    val get_accel_widget_ : cptr -> cptr
(*  24*)	= _import "gtk_accel_label_get_accel_widget" : cptr -> cptr;
(*  24*)    val get_accel_widget : 'a t -> base Widget.t
(*  24*)	= fn self =>
(*  24*)	     Widget.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, fn self => get_accel_widget_ self))
(*  24*)    val get_accel_width_ : cptr -> int
(*  24*)	= _import "gtk_accel_label_get_accel_width" : cptr -> int;
(*  24*)    val get_accel_width : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_accel_width_ self)
(*  24*)    val set_accel_widget_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_accel_label_set_accel_widget"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val set_accel_widget : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn accel_widget =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr (accel_widget, 
(*  24*)					    fn accel_widget =>
(*  24*)					       set_accel_widget_
(*  24*)						 (self, accel_widget)))
(*  24*)    val refetch_ : cptr -> bool
(*  24*)	= _import "gtk_accel_label_refetch" : cptr -> bool;
(*  24*)    val refetch : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr (self, fn self => refetch_ self)
(*  24*)end
(*  24*)structure Invisible :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a invisible_t
(*  24*)    type 'a t = 'a invisible_t Widget.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toInvisible : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a invisible_t = unit
(*  24*)    type 'a t = 'a invisible_t Widget.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Widget.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toInvisible obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_invisible_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_invisible_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)end
(*  24*)structure Entry :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a entry_t
(*  24*)    type 'a t = 'a entry_t Editable.t CellEditable.t Widget.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toEntry : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val new_with_max_length : int option -> base t
(*  24*)    val new_with_max_length' : unit -> base t
(*  24*)    val set_visibility : 'a t -> bool -> unit
(*  24*)    val get_visibility : 'a t -> bool
(*  24*)    val set_invisible_char : 'a t -> char -> unit
(*  24*)    val get_invisible_char : 'a t -> char
(*  24*)    val set_has_frame : 'a t -> bool -> unit
(*  24*)    val get_has_frame : 'a t -> bool
(*  24*)    val set_max_length : 'a t -> int -> unit
(*  24*)    val get_max_length : 'a t -> int
(*  24*)    val set_activates_default : 'a t -> bool -> unit
(*  24*)    val get_activates_default : 'a t -> bool
(*  24*)    val set_width_chars : 'a t -> int -> unit
(*  24*)    val get_width_chars : 'a t -> int
(*  24*)    val set_text : 'a t -> string -> unit
(*  24*)    val get_text : 'a t -> string
(*  24*)    val append_text : 'a t -> string -> unit
(*  24*)    val prepend_text : 'a t -> string -> unit
(*  24*)    val move_cursor_sig : (unit -> int -> bool -> unit)
(*  24*)			  -> 'a t Signal.signal
(*  24*)    val insert_at_cursor_sig : (char -> unit) -> 'a t Signal.signal
(*  24*)    val delete_from_cursor_sig
(*  24*)      : (unit -> int -> unit) -> 'a t Signal.signal
(*  24*)    val cut_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val copy_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val paste_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val toggle_overwrite_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val populate_popup_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val activate_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a entry_t = unit
(*  24*)    type 'a t = 'a entry_t Editable.t CellEditable.t Widget.t
(*  24*)    fun inherit w con
(*  24*)      = let val con = let val ptr = con () in fn () => ptr end
(*  24*)	    val witness = ()
(*  24*)	    val witness = Editable.inherit witness con
(*  24*)	    val witness = CellEditable.inherit witness con
(*  24*)	in Widget.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toEntry obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_entry_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr = _import "gtk_entry_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val new_with_max_length_ : int -> cptr
(*  24*)	= _import "gtk_entry_new_with_max_length" : int -> cptr;
(*  24*)    val new_with_max_length : int option -> base t
(*  24*)	= fn max => make (new_with_max_length_ (getOpt (max, 0)))
(*  24*)    val new_with_max_length' : unit -> base t
(*  24*)	= fn dummy => make (new_with_max_length_ 0)
(*  24*)    val set_visibility_ : cptr * bool -> unit
(*  24*)	= _import "gtk_entry_set_visibility" : cptr * bool -> unit;
(*  24*)    val set_visibility : 'a t -> bool -> unit
(*  24*)	= fn self => fn visible =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_visibility_ (self, visible))
(*  24*)    val get_visibility_ : cptr -> bool
(*  24*)	= _import "gtk_entry_get_visibility" : cptr -> bool;
(*  24*)    val get_visibility : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_visibility_ self)
(*  24*)    val set_invisible_char_ : cptr * char -> unit
(*  24*)	= _import "gtk_entry_set_invisible_char" : cptr * char -> unit;
(*  24*)    val set_invisible_char : 'a t -> char -> unit
(*  24*)	= fn self => fn ch =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_invisible_char_ (self, ch))
(*  24*)    val get_invisible_char_ : cptr -> char
(*  24*)	= _import "gtk_entry_get_invisible_char" : cptr -> char;
(*  24*)    val get_invisible_char : 'a t -> char
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_invisible_char_ self)
(*  24*)    val set_has_frame_ : cptr * bool -> unit
(*  24*)	= _import "gtk_entry_set_has_frame" : cptr * bool -> unit;
(*  24*)    val set_has_frame : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_has_frame_ (self, setting))
(*  24*)    val get_has_frame_ : cptr -> bool
(*  24*)	= _import "gtk_entry_get_has_frame" : cptr -> bool;
(*  24*)    val get_has_frame : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_has_frame_ self)
(*  24*)    val set_max_length_ : cptr * int -> unit
(*  24*)	= _import "gtk_entry_set_max_length" : cptr * int -> unit;
(*  24*)    val set_max_length : 'a t -> int -> unit
(*  24*)	= fn self => fn max =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_max_length_ (self, max))
(*  24*)    val get_max_length_ : cptr -> int
(*  24*)	= _import "gtk_entry_get_max_length" : cptr -> int;
(*  24*)    val get_max_length : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_max_length_ self)
(*  24*)    val set_activates_default_ : cptr * bool -> unit
(*  24*)	= _import "gtk_entry_set_activates_default"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_activates_default : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_activates_default_
(*  24*)					   (self, setting))
(*  24*)    val get_activates_default_ : cptr -> bool
(*  24*)	= _import "gtk_entry_get_activates_default" : cptr -> bool;
(*  24*)    val get_activates_default : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_activates_default_ self)
(*  24*)    val set_width_chars_ : cptr * int -> unit
(*  24*)	= _import "gtk_entry_set_width_chars" : cptr * int -> unit;
(*  24*)    val set_width_chars : 'a t -> int -> unit
(*  24*)	= fn self => fn n_chars =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_width_chars_ (self, n_chars))
(*  24*)    val get_width_chars_ : cptr -> int
(*  24*)	= _import "gtk_entry_get_width_chars" : cptr -> int;
(*  24*)    val get_width_chars : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_width_chars_ self)
(*  24*)    val set_text_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_entry_set_text"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_text : 'a t -> string -> unit
(*  24*)	= fn self => fn text =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_text_ (self, CString.fromString text))
(*  24*)    val get_text_ : cptr -> CString.t
(*  24*)	= _import "gtk_entry_get_text" : cptr -> CString.t;
(*  24*)    val get_text : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_text_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val append_text_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_entry_append_text"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val append_text : 'a t -> string -> unit
(*  24*)	= fn self => fn text =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => append_text_
(*  24*)			     (self, CString.fromString text))
(*  24*)    val prepend_text_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_entry_prepend_text"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val prepend_text : 'a t -> string -> unit
(*  24*)	= fn self => fn text =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => prepend_text_
(*  24*)			     (self, CString.fromString text))
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val move_cursor_sig : (unit -> int -> bool -> unit)
(*  24*)			     -> 'a t Signal.signal
(*  24*)	   = fn f => signal "move-cursor" false
(*  24*)			    (unit --> int --> bool --> return_void) f
(*  24*)       val insert_at_cursor_sig : (char -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "insert-at-cursor" false
(*  24*)			    (char --> return_void) f
(*  24*)       val delete_from_cursor_sig
(*  24*)	 : (unit -> int -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "delete-from-cursor" false
(*  24*)			    (unit --> int --> return_void) f
(*  24*)       val cut_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "cut-clipboard" false (void --> return_void) f
(*  24*)       val copy_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "copy-clipboard" false
(*  24*)			    (void --> return_void) f
(*  24*)       val paste_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "paste-clipboard" false
(*  24*)			    (void --> return_void) f
(*  24*)       val toggle_overwrite_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "toggle-overwrite" false
(*  24*)			    (void --> return_void) f
(*  24*)       val populate_popup_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "populate-popup" false
(*  24*)			    (unit --> return_void) f
(*  24*)       val activate_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "activate" false (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure SpinButton :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a spinbutton_t
(*  24*)    type 'a t = 'a spinbutton_t Entry.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toSpinButton : 'a t -> base t
(*  24*)    type update_policy
(*  24*)    val UPDATE_ALWAYS : update_policy
(*  24*)    val UPDATE_IF_VALID : update_policy
(*  24*)    val get_type : unit -> int
(*  24*)    val configure
(*  24*)      : 'a t -> 'b Adjustment.t option -> real -> int -> unit
(*  24*)    val configure' : 'a t -> real -> int -> unit
(*  24*)    val new : 'a Adjustment.t option -> real option -> int option
(*  24*)	      -> base t
(*  24*)    val new' : unit -> base t
(*  24*)    val new_with_range : real -> real -> real -> base t
(*  24*)    val set_adjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)    val get_adjustment : 'a t -> base Adjustment.t
(*  24*)    val set_digits : 'a t -> int -> unit
(*  24*)    val get_digits : 'a t -> int
(*  24*)    val set_increments : 'a t -> real -> real -> unit
(*  24*)    val set_range : 'a t -> real -> real -> unit
(*  24*)    val get_value : 'a t -> real
(*  24*)    val get_value_as_int : 'a t -> int
(*  24*)    val set_value : 'a t -> real -> unit
(*  24*)    val set_update_policy : 'a t -> update_policy -> unit
(*  24*)    val get_update_policy : 'a t -> int
(*  24*)    val set_numeric : 'a t -> bool -> unit
(*  24*)    val get_numeric : 'a t -> bool
(*  24*)    val spin : 'a t -> spintype -> real -> unit
(*  24*)    val set_wrap : 'a t -> bool -> unit
(*  24*)    val get_wrap : 'a t -> bool
(*  24*)    val set_snap_to_ticks : 'a t -> bool -> unit
(*  24*)    val get_snap_to_ticks : 'a t -> bool
(*  24*)    val update : 'a t -> unit
(*  24*)    val input_sig : (real -> int) -> 'a t Signal.signal
(*  24*)    val output_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val value_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val change_value_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a spinbutton_t = unit
(*  24*)    type 'a t = 'a spinbutton_t Entry.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Entry.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toSpinButton obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    type update_policy = int
(*  24*)    val get_update_policy_ : int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_spin_button_update_policy"
(*  24*)		  : int ref * int ref -> unit;
(*  24*)    val (UPDATE_ALWAYS, UPDATE_IF_VALID)
(*  24*)	= let val (x0, x1) = (ref 0, ref 0)
(*  24*)	  in get_update_policy_ (x0, x1)
(*  24*)	   ; (!x0, !x1)
(*  24*)	  end
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_spin_button_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val configure_ : cptr * cptr * real * int -> unit
(*  24*)	= _import "gtk_spin_button_configure"
(*  24*)		  : cptr * cptr * real * int -> unit;
(*  24*)    val configure
(*  24*)      : 'a t -> 'b Adjustment.t option -> real -> int -> unit
(*  24*)	= fn self => fn adjustment => fn climb_rate => fn digits =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withOpt
(*  24*)			     (adjustment, 
(*  24*)			      fn adjustment => configure_
(*  24*)						 (self, adjustment, 
(*  24*)						  climb_rate, digits)))
(*  24*)    val configure' : 'a t -> real -> int -> unit
(*  24*)	= fn self => fn climb_rate => fn digits =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => configure_
(*  24*)					   (self, GObject.null, 
(*  24*)					    climb_rate, digits))
(*  24*)    val new_ : cptr * real * int -> cptr
(*  24*)	= _import "gtk_spin_button_new" : cptr * real * int -> cptr;
(*  24*)    val new : 'a Adjustment.t option -> real option -> int option
(*  24*)	      -> base t
(*  24*)	= fn adjustment => fn climb_rate => fn digits =>
(*  24*)	     make (GObject.withOpt
(*  24*)		     (adjustment, 
(*  24*)		      fn adjustment =>
(*  24*)			 new_ (adjustment, getOpt (climb_rate, 0.0), 
(*  24*)			       getOpt (digits, 0))))
(*  24*)    val new' : unit -> base t
(*  24*)	= fn dummy => make (new_ (GObject.null, 0.0, 0))
(*  24*)    val new_with_range_ : real * real * real -> cptr
(*  24*)	= _import "gtk_spin_button_new_with_range"
(*  24*)		  : real * real * real -> cptr;
(*  24*)    val new_with_range : real -> real -> real -> base t
(*  24*)	= fn min => fn max => fn step =>
(*  24*)	     make (new_with_range_ (min, max, step))
(*  24*)    val set_adjustment_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_spin_button_set_adjustment"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val set_adjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)	= fn self => fn adjustment =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (adjustment, 
(*  24*)			      fn adjustment =>
(*  24*)				 set_adjustment_ (self, adjustment)))
(*  24*)    val get_adjustment_ : cptr -> cptr
(*  24*)	= _import "gtk_spin_button_get_adjustment" : cptr -> cptr;
(*  24*)    val get_adjustment : 'a t -> base Adjustment.t
(*  24*)	= fn self =>
(*  24*)	     Adjustment.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, fn self => get_adjustment_ self))
(*  24*)    val set_digits_ : cptr * int -> unit
(*  24*)	= _import "gtk_spin_button_set_digits" : cptr * int -> unit;
(*  24*)    val set_digits : 'a t -> int -> unit
(*  24*)	= fn self => fn digits =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_digits_ (self, digits))
(*  24*)    val get_digits_ : cptr -> int
(*  24*)	= _import "gtk_spin_button_get_digits" : cptr -> int;
(*  24*)    val get_digits : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_digits_ self)
(*  24*)    val set_increments_ : cptr * real * real -> unit
(*  24*)	= _import "gtk_spin_button_set_increments"
(*  24*)		  : cptr * real * real -> unit;
(*  24*)    val set_increments : 'a t -> real -> real -> unit
(*  24*)	= fn self => fn step => fn page =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_increments_ (self, step, page))
(*  24*)    val set_range_ : cptr * real * real -> unit
(*  24*)	= _import "gtk_spin_button_set_range"
(*  24*)		  : cptr * real * real -> unit;
(*  24*)    val set_range : 'a t -> real -> real -> unit
(*  24*)	= fn self => fn min => fn max =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_range_ (self, min, max))
(*  24*)    val get_value_ : cptr -> real
(*  24*)	= _import "gtk_spin_button_get_value" : cptr -> real;
(*  24*)    val get_value : 'a t -> real
(*  24*)	= fn self => GObject.withPtr (self, fn self => get_value_ self)
(*  24*)    val get_value_as_int_ : cptr -> int
(*  24*)	= _import "gtk_spin_button_get_value_as_int" : cptr -> int;
(*  24*)    val get_value_as_int : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_value_as_int_ self)
(*  24*)    val set_value_ : cptr * real -> unit
(*  24*)	= _import "gtk_spin_button_set_value" : cptr * real -> unit;
(*  24*)    val set_value : 'a t -> real -> unit
(*  24*)	= fn self => fn value =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_value_ (self, value))
(*  24*)    val set_update_policy_ : cptr * int -> unit
(*  24*)	= _import "gtk_spin_button_set_update_policy"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_update_policy : 'a t -> update_policy -> unit
(*  24*)	= fn self => fn policy =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_update_policy_ (self, policy))
(*  24*)    val get_update_policy_ : cptr -> int
(*  24*)	= _import "gtk_spin_button_get_update_policy" : cptr -> int;
(*  24*)    val get_update_policy : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_update_policy_ self)
(*  24*)    val set_numeric_ : cptr * bool -> unit
(*  24*)	= _import "gtk_spin_button_set_numeric" : cptr * bool -> unit;
(*  24*)    val set_numeric : 'a t -> bool -> unit
(*  24*)	= fn self => fn numeric =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_numeric_ (self, numeric))
(*  24*)    val get_numeric_ : cptr -> bool
(*  24*)	= _import "gtk_spin_button_get_numeric" : cptr -> bool;
(*  24*)    val get_numeric : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_numeric_ self)
(*  24*)    val spin_ : cptr * int * real -> unit
(*  24*)	= _import "gtk_spin_button_spin" : cptr * int * real -> unit;
(*  24*)    val spin : 'a t -> spintype -> real -> unit
(*  24*)	= fn self => fn direction => fn increment =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => spin_ (self, direction, increment))
(*  24*)    val set_wrap_ : cptr * bool -> unit
(*  24*)	= _import "gtk_spin_button_set_wrap" : cptr * bool -> unit;
(*  24*)    val set_wrap : 'a t -> bool -> unit
(*  24*)	= fn self => fn wrap =>
(*  24*)	     GObject.withPtr (self, fn self => set_wrap_ (self, wrap))
(*  24*)    val get_wrap_ : cptr -> bool
(*  24*)	= _import "gtk_spin_button_get_wrap" : cptr -> bool;
(*  24*)    val get_wrap : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr (self, fn self => get_wrap_ self)
(*  24*)    val set_snap_to_ticks_ : cptr * bool -> unit
(*  24*)	= _import "gtk_spin_button_set_snap_to_ticks"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_snap_to_ticks : 'a t -> bool -> unit
(*  24*)	= fn self => fn snap_to_ticks =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_snap_to_ticks_
(*  24*)					   (self, snap_to_ticks))
(*  24*)    val get_snap_to_ticks_ : cptr -> bool
(*  24*)	= _import "gtk_spin_button_get_snap_to_ticks" : cptr -> bool;
(*  24*)    val get_snap_to_ticks : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_snap_to_ticks_ self)
(*  24*)    val update_ : cptr -> unit
(*  24*)	= _import "gtk_spin_button_update" : cptr -> unit;
(*  24*)    val update : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => update_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val input_sig : (real -> int) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "input" false (real --> return_int) f
(*  24*)       val output_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "output" false (void --> return_bool) f
(*  24*)       val value_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "value-changed" false (void --> return_void) f
(*  24*)       val change_value_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "change-value" false (unit --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure DrawingArea :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a drawingarea_t
(*  24*)    type 'a t = 'a drawingarea_t Widget.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toDrawingArea : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val size : 'a t -> int -> int -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a drawingarea_t = unit
(*  24*)    type 'a t = 'a drawingarea_t Widget.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Widget.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toDrawingArea obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_drawing_area_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_drawing_area_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val size_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_drawing_area_size" : cptr * int * int -> unit;
(*  24*)    val size : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn width => fn height =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => size_ (self, width, height))
(*  24*)end
(*  24*)structure Curve :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a curve_t
(*  24*)    type 'a t = 'a curve_t DrawingArea.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toCurve : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val reset : 'a t -> unit
(*  24*)    val set_gamma : 'a t -> real -> unit
(*  24*)    val set_range : 'a t -> real -> real -> real -> real -> unit
(*  24*)    val set_curvetype : 'a t -> curvetype -> unit
(*  24*)    val curve_type_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a curve_t = unit
(*  24*)    type 'a t = 'a curve_t DrawingArea.t
(*  24*)    fun inherit w con
(*  24*)      = let val con = let val ptr = con () in fn () => ptr end
(*  24*)	    val witness = ()
(*  24*)	in DrawingArea.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toCurve obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_curve_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr = _import "gtk_curve_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val reset_ : cptr -> unit
(*  24*)	= _import "gtk_curve_reset" : cptr -> unit;
(*  24*)    val reset : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => reset_ self)
(*  24*)    val set_gamma_ : cptr * real -> unit
(*  24*)	= _import "gtk_curve_set_gamma" : cptr * real -> unit;
(*  24*)    val set_gamma : 'a t -> real -> unit
(*  24*)	= fn self => fn gamma =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_gamma_ (self, gamma))
(*  24*)    val set_range_ : cptr * real * real * real * real -> unit
(*  24*)	= _import "gtk_curve_set_range"
(*  24*)		  : cptr * real * real * real * real -> unit;
(*  24*)    val set_range : 'a t -> real -> real -> real -> real -> unit
(*  24*)	= fn self => fn min_x => fn max_x => fn min_y => fn max_y =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_range_
(*  24*)			     (self, min_x, max_x, min_y, max_y))
(*  24*)    val set_curvetype_ : cptr * int -> unit
(*  24*)	= _import "gtk_curve_set_curve_type" : cptr * int -> unit;
(*  24*)    val set_curvetype : 'a t -> curvetype -> unit
(*  24*)	= fn self => fn typ =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_curvetype_ (self, typ))
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val curve_type_changed_sig
(*  24*)	 : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "curve-type-changed" false
(*  24*)			    (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Container :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a container_t
(*  24*)    type 'a t = 'a container_t Widget.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toContainer : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val set_border_width : 'a t -> int -> unit
(*  24*)    val get_border_width : 'a t -> int
(*  24*)    val add : 'a t -> 'b Widget.t -> unit
(*  24*)    val remove : 'a t -> 'b Widget.t -> unit
(*  24*)    val set_resize_mode : 'a t -> resize_mode -> unit
(*  24*)    val get_resize_mode : 'a t -> resize_mode
(*  24*)    val check_resize : 'a t -> unit
(*  24*)    val unset_focus_chain : 'a t -> unit
(*  24*)    val set_reallocate_redraws : 'a t -> bool -> unit
(*  24*)    val set_focus_child : 'a t -> 'b Widget.t -> unit
(*  24*)    val set_focus_vadjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)    val get_focus_vadjustment : 'a t -> base Adjustment.t
(*  24*)    val set_focus_hadjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)    val get_focus_hadjustment : 'a t -> base Adjustment.t
(*  24*)    val resize_children : 'a t -> unit
(*  24*)    val childtype : 'a t -> int
(*  24*)    val add_with_properties : 'a t -> 'b Widget.t -> string -> unit
(*  24*)    val child_set : 'a t -> 'b Widget.t -> string -> unit
(*  24*)    val child_get : 'a t -> 'b Widget.t -> string -> unit
(*  24*)    val add_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val remove_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val check_resize_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val set_focus_child_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a container_t = unit
(*  24*)    type 'a t = 'a container_t Widget.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Widget.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toContainer obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_container_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val set_border_width_ : cptr * int -> unit
(*  24*)	= _import "gtk_container_set_border_width"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_border_width : 'a t -> int -> unit
(*  24*)	= fn self => fn border_width =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_border_width_
(*  24*)					   (self, border_width))
(*  24*)    val get_border_width_ : cptr -> int
(*  24*)	= _import "gtk_container_get_border_width" : cptr -> int;
(*  24*)    val get_border_width : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_border_width_ self)
(*  24*)    val add_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_container_add" : cptr * cptr -> unit;
(*  24*)    val add : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn widget =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (widget, fn widget => add_ (self, widget)))
(*  24*)    val remove_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_container_remove" : cptr * cptr -> unit;
(*  24*)    val remove : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn widget =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (widget, fn widget => remove_ (self, widget)))
(*  24*)    val set_resize_mode_ : cptr * int -> unit
(*  24*)	= _import "gtk_container_set_resize_mode" : cptr * int -> unit;
(*  24*)    val set_resize_mode : 'a t -> resize_mode -> unit
(*  24*)	= fn self => fn resize_mode =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_resize_mode_ (self, resize_mode))
(*  24*)    val get_resize_mode_ : cptr -> int
(*  24*)	= _import "gtk_container_get_resize_mode" : cptr -> int;
(*  24*)    val get_resize_mode : 'a t -> resize_mode
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_resize_mode_ self)
(*  24*)    val check_resize_ : cptr -> unit
(*  24*)	= _import "gtk_container_check_resize" : cptr -> unit;
(*  24*)    val check_resize : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => check_resize_ self)
(*  24*)    val unset_focus_chain_ : cptr -> unit
(*  24*)	= _import "gtk_container_unset_focus_chain" : cptr -> unit;
(*  24*)    val unset_focus_chain : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => unset_focus_chain_ self)
(*  24*)    val set_reallocate_redraws_ : cptr * bool -> unit
(*  24*)	= _import "gtk_container_set_reallocate_redraws"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_reallocate_redraws : 'a t -> bool -> unit
(*  24*)	= fn self => fn needs_redraws =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_reallocate_redraws_
(*  24*)					   (self, needs_redraws))
(*  24*)    val set_focus_child_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_container_set_focus_child"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val set_focus_child : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn child =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => set_focus_child_
(*  24*)					    (self, child)))
(*  24*)    val set_focus_vadjustment_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_container_set_focus_vadjustment"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val set_focus_vadjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)	= fn self => fn adjustment =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (adjustment, 
(*  24*)			      fn adjustment => set_focus_vadjustment_
(*  24*)						 (self, adjustment)))
(*  24*)    val get_focus_vadjustment_ : cptr -> cptr
(*  24*)	= _import "gtk_container_get_focus_vadjustment" : cptr -> cptr;
(*  24*)    val get_focus_vadjustment : 'a t -> base Adjustment.t
(*  24*)	= fn self =>
(*  24*)	     Adjustment.inherit
(*  24*)	       ()
(*  24*)	       (fn () =>
(*  24*)		   GObject.withPtr
(*  24*)		     (self, fn self => get_focus_vadjustment_ self))
(*  24*)    val set_focus_hadjustment_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_container_set_focus_hadjustment"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val set_focus_hadjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)	= fn self => fn adjustment =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (adjustment, 
(*  24*)			      fn adjustment => set_focus_hadjustment_
(*  24*)						 (self, adjustment)))
(*  24*)    val get_focus_hadjustment_ : cptr -> cptr
(*  24*)	= _import "gtk_container_get_focus_hadjustment" : cptr -> cptr;
(*  24*)    val get_focus_hadjustment : 'a t -> base Adjustment.t
(*  24*)	= fn self =>
(*  24*)	     Adjustment.inherit
(*  24*)	       ()
(*  24*)	       (fn () =>
(*  24*)		   GObject.withPtr
(*  24*)		     (self, fn self => get_focus_hadjustment_ self))
(*  24*)    val resize_children_ : cptr -> unit
(*  24*)	= _import "gtk_container_resize_children" : cptr -> unit;
(*  24*)    val resize_children : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => resize_children_ self)
(*  24*)    val childtype_ : cptr -> int
(*  24*)	= _import "gtk_container_child_type" : cptr -> int;
(*  24*)    val childtype : 'a t -> int
(*  24*)	= fn self => GObject.withPtr (self, fn self => childtype_ self)
(*  24*)    val add_with_properties_ : cptr * cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_container_add_with_properties"
(*  24*)		  : cptr * cptr * CString.cstring -> unit;
(*  24*)    val add_with_properties : 'a t -> 'b Widget.t -> string -> unit
(*  24*)	= fn self => fn widget => fn first_prop_name =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (widget, 
(*  24*)			      fn widget => add_with_properties_
(*  24*)					     (self, widget, 
(*  24*)					      CString.fromString
(*  24*)						first_prop_name)))
(*  24*)    val child_set_ : cptr * cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_container_child_set"
(*  24*)		  : cptr * cptr * CString.cstring -> unit;
(*  24*)    val child_set : 'a t -> 'b Widget.t -> string -> unit
(*  24*)	= fn self => fn child => fn first_prop_name =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child =>
(*  24*)				 child_set_ (self, child, 
(*  24*)					     CString.fromString
(*  24*)					       first_prop_name)))
(*  24*)    val child_get_ : cptr * cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_container_child_get"
(*  24*)		  : cptr * cptr * CString.cstring -> unit;
(*  24*)    val child_get : 'a t -> 'b Widget.t -> string -> unit
(*  24*)	= fn self => fn child => fn first_prop_name =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child =>
(*  24*)				 child_get_ (self, child, 
(*  24*)					     CString.fromString
(*  24*)					       first_prop_name)))
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val add_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "add" false (unit --> return_void) f
(*  24*)       val remove_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "remove" false (unit --> return_void) f
(*  24*)       val check_resize_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "check-resize" false (void --> return_void) f
(*  24*)       val set_focus_child_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "set-focus-child" false
(*  24*)			    (unit --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure TreeView :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a treeview_t
(*  24*)    type 'a t = 'a treeview_t Container.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTreeView : 'a t -> base t
(*  24*)    type drop_position
(*  24*)    val TREE_VIEW_DROP_BEFORE : drop_position
(*  24*)    val TREE_VIEW_DROP_AFTER : drop_position
(*  24*)    val TREE_VIEW_DROP_INTO_OR_BEFORE : drop_position
(*  24*)    val TREE_VIEW_DROP_INTO_OR_AFTER : drop_position
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val new_with_model : 'a TreeModel.t option -> base t
(*  24*)    val new_with_model' : unit -> base t
(*  24*)    val get_model : 'a t -> base TreeModel.t
(*  24*)    val set_model : 'a t -> 'b TreeModel.t option -> unit
(*  24*)    val set_model' : 'a t -> unit
(*  24*)    val get_selection : 'a t -> base TreeSelection.t
(*  24*)    val get_hadjustment : 'a t -> base Adjustment.t
(*  24*)    val set_hadjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)    val get_vadjustment : 'a t -> base Adjustment.t
(*  24*)    val set_vadjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)    val get_headers_visible : 'a t -> bool
(*  24*)    val set_headers_visible : 'a t -> bool -> unit
(*  24*)    val columns_autosize : 'a t -> unit
(*  24*)    val set_headers_clickable : 'a t -> bool -> unit
(*  24*)    val set_rules_hint : 'a t -> bool -> unit
(*  24*)    val get_rules_hint : 'a t -> bool
(*  24*)    val append_column : 'a t -> 'b TreeViewColumn.t -> int
(*  24*)    val remove_column : 'a t -> 'b TreeViewColumn.t -> int
(*  24*)    val insert_column : 'a t -> 'b TreeViewColumn.t -> int -> int
(*  24*)    val insert_column_with_attributes
(*  24*)      : 'a t -> int -> string -> 'b CellRenderer.t -> int
(*  24*)    val get_column : 'a t -> int -> base TreeViewColumn.t
(*  24*)    val move_column_after
(*  24*)      : 'a t -> 'b TreeViewColumn.t -> 'c TreeViewColumn.t -> unit
(*  24*)    val set_expander_column : 'a t -> 'b TreeViewColumn.t -> unit
(*  24*)    val get_expander_column : 'a t -> base TreeViewColumn.t
(*  24*)    val scroll_to_point : 'a t -> int -> int -> unit
(*  24*)    val scroll_to_cell : 'a t -> tree_path 
(*  24*)		      -> 'b TreeViewColumn.t option -> bool option 
(*  24*)		      -> real option -> real option
(*  24*)			 -> unit
(*  24*)    val scroll_to_cell' : 'a t -> tree_path -> unit
(*  24*)    val row_activated
(*  24*)      : 'a t -> tree_path -> 'b TreeViewColumn.t -> unit
(*  24*)    val expand_all : 'a t -> unit
(*  24*)    val collapse_all : 'a t -> unit
(*  24*)    val expand_row : 'a t -> tree_path -> bool -> unit
(*  24*)    val collapse_row : 'a t -> tree_path -> unit
(*  24*)    val row_expanded : 'a t -> tree_path -> bool
(*  24*)    val set_reorderable : 'a t -> bool -> unit
(*  24*)    val get_reorderable : 'a t -> bool
(*  24*)    val set_cursor : 'a t -> tree_path -> 'b TreeViewColumn.t option 
(*  24*)		  -> bool option
(*  24*)		     -> unit
(*  24*)    val set_cursor' : 'a t -> tree_path -> unit
(*  24*)    val get_cursor : 'a t -> tree_path -> 'b TreeViewColumn.t -> unit
(*  24*)    val unset_rows_drag_source : 'a t -> unit
(*  24*)    val unset_rows_drag_dest : 'a t -> unit
(*  24*)    val set_drag_dest_row : 'a t -> tree_path -> drop_position -> unit
(*  24*)    val set_enable_search : 'a t -> bool -> unit
(*  24*)    val get_enable_search : 'a t -> bool
(*  24*)    val get_search_column : 'a t -> int
(*  24*)    val set_search_column : 'a t -> int -> unit
(*  24*)    val set_scroll_adjustments_sig
(*  24*)      : (unit -> unit -> unit) -> 'a t Signal.signal
(*  24*)    val row_activated_sig : (unit -> unit -> unit)
(*  24*)			    -> 'a t Signal.signal
(*  24*)    val test_expand_row_sig
(*  24*)      : (unit -> unit -> bool) -> 'a t Signal.signal
(*  24*)    val test_collapse_row_sig
(*  24*)      : (unit -> unit -> bool) -> 'a t Signal.signal
(*  24*)    val row_expanded_sig : (unit -> unit -> unit) -> 'a t Signal.signal
(*  24*)    val row_collapsed_sig : (unit -> unit -> unit)
(*  24*)			    -> 'a t Signal.signal
(*  24*)    val columns_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val cursor_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val move_cursor_sig : (unit -> int -> bool) -> 'a t Signal.signal
(*  24*)    val select_all_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val unselect_all_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val select_cursor_row_sig : (bool -> bool) -> 'a t Signal.signal
(*  24*)    val toggle_cursor_row_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val expand_collapse_cursor_row_sig
(*  24*)      : (bool -> bool -> bool -> bool) -> 'a t Signal.signal
(*  24*)    val select_cursor_parent_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val start_interactive_search_sig
(*  24*)      : (unit -> bool) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a treeview_t = unit
(*  24*)    type 'a t = 'a treeview_t Container.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Container.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTreeView obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    type drop_position = int
(*  24*)    val get_drop_position_
(*  24*)      : int ref * int ref * int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_treeview_drop_position"
(*  24*)		  : int ref * int ref * int ref * int ref -> unit;
(*  24*)    val (TREE_VIEW_DROP_BEFORE, TREE_VIEW_DROP_AFTER, 
(*  24*)	 TREE_VIEW_DROP_INTO_OR_BEFORE, TREE_VIEW_DROP_INTO_OR_AFTER)
(*  24*)	= let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)	  in get_drop_position_ (x0, x1, x2, x3)
(*  24*)	   ; (!x0, !x1, !x2, !x3)
(*  24*)	  end
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_tree_view_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_tree_view_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val new_with_model_ : cptr -> cptr
(*  24*)	= _import "gtk_tree_view_new_with_model" : cptr -> cptr;
(*  24*)    val new_with_model : 'a TreeModel.t option -> base t
(*  24*)	= fn model =>
(*  24*)	     make (GObject.withOpt
(*  24*)		     (model, fn model => new_with_model_ model))
(*  24*)    val new_with_model' : unit -> base t
(*  24*)	= fn dummy => make (new_with_model_ GObject.null)
(*  24*)    val get_model_ : cptr -> cptr
(*  24*)	= _import "gtk_tree_view_get_model" : cptr -> cptr;
(*  24*)    val get_model : 'a t -> base TreeModel.t
(*  24*)	= fn self => TreeModel.inherit
(*  24*)		       ()
(*  24*)		       (fn () => GObject.withPtr
(*  24*)				   (self, fn self => get_model_ self))
(*  24*)    val set_model_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_view_set_model" : cptr * cptr -> unit;
(*  24*)    val set_model : 'a t -> 'b TreeModel.t option -> unit
(*  24*)	= fn self => fn model =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withOpt
(*  24*)		     (model, fn model => set_model_ (self, model)))
(*  24*)    val set_model' : 'a t -> unit
(*  24*)	= fn self =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_model_ (self, GObject.null))
(*  24*)    val get_selection_ : cptr -> cptr
(*  24*)	= _import "gtk_tree_view_get_selection" : cptr -> cptr;
(*  24*)    val get_selection : 'a t -> base TreeSelection.t
(*  24*)	= fn self =>
(*  24*)	     TreeSelection.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, fn self => get_selection_ self))
(*  24*)    val get_hadjustment_ : cptr -> cptr
(*  24*)	= _import "gtk_tree_view_get_hadjustment" : cptr -> cptr;
(*  24*)    val get_hadjustment : 'a t -> base Adjustment.t
(*  24*)	= fn self =>
(*  24*)	     Adjustment.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, fn self => get_hadjustment_ self))
(*  24*)    val set_hadjustment_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_view_set_hadjustment"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val set_hadjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)	= fn self => fn adjustment =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (adjustment, 
(*  24*)			      fn adjustment => set_hadjustment_
(*  24*)						 (self, adjustment)))
(*  24*)    val get_vadjustment_ : cptr -> cptr
(*  24*)	= _import "gtk_tree_view_get_vadjustment" : cptr -> cptr;
(*  24*)    val get_vadjustment : 'a t -> base Adjustment.t
(*  24*)	= fn self =>
(*  24*)	     Adjustment.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, fn self => get_vadjustment_ self))
(*  24*)    val set_vadjustment_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_view_set_vadjustment"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val set_vadjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)	= fn self => fn adjustment =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (adjustment, 
(*  24*)			      fn adjustment => set_vadjustment_
(*  24*)						 (self, adjustment)))
(*  24*)    val get_headers_visible_ : cptr -> bool
(*  24*)	= _import "gtk_tree_view_get_headers_visible" : cptr -> bool;
(*  24*)    val get_headers_visible : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_headers_visible_ self)
(*  24*)    val set_headers_visible_ : cptr * bool -> unit
(*  24*)	= _import "gtk_tree_view_set_headers_visible"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_headers_visible : 'a t -> bool -> unit
(*  24*)	= fn self => fn headers_visible =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_headers_visible_
(*  24*)					   (self, headers_visible))
(*  24*)    val columns_autosize_ : cptr -> unit
(*  24*)	= _import "gtk_tree_view_columns_autosize" : cptr -> unit;
(*  24*)    val columns_autosize : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => columns_autosize_ self)
(*  24*)    val set_headers_clickable_ : cptr * bool -> unit
(*  24*)	= _import "gtk_tree_view_set_headers_clickable"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_headers_clickable : 'a t -> bool -> unit
(*  24*)	= fn self => fn active =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_headers_clickable_ (self, active))
(*  24*)    val set_rules_hint_ : cptr * bool -> unit
(*  24*)	= _import "gtk_tree_view_set_rules_hint" : cptr * bool -> unit;
(*  24*)    val set_rules_hint : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_rules_hint_ (self, setting))
(*  24*)    val get_rules_hint_ : cptr -> bool
(*  24*)	= _import "gtk_tree_view_get_rules_hint" : cptr -> bool;
(*  24*)    val get_rules_hint : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_rules_hint_ self)
(*  24*)    val append_column_ : cptr * cptr -> int
(*  24*)	= _import "gtk_tree_view_append_column" : cptr * cptr -> int;
(*  24*)    val append_column : 'a t -> 'b TreeViewColumn.t -> int
(*  24*)	= fn self => fn column =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (column, 
(*  24*)			      fn column => append_column_
(*  24*)					     (self, column)))
(*  24*)    val remove_column_ : cptr * cptr -> int
(*  24*)	= _import "gtk_tree_view_remove_column" : cptr * cptr -> int;
(*  24*)    val remove_column : 'a t -> 'b TreeViewColumn.t -> int
(*  24*)	= fn self => fn column =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (column, 
(*  24*)			      fn column => remove_column_
(*  24*)					     (self, column)))
(*  24*)    val insert_column_ : cptr * cptr * int -> int
(*  24*)	= _import "gtk_tree_view_insert_column"
(*  24*)		  : cptr * cptr * int -> int;
(*  24*)    val insert_column : 'a t -> 'b TreeViewColumn.t -> int -> int
(*  24*)	= fn self => fn column => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (column, 
(*  24*)			      fn column => insert_column_
(*  24*)					     (self, column, position)))
(*  24*)    val insert_column_with_attributes_
(*  24*)      : cptr * int * CString.cstring * cptr -> int
(*  24*)	= _import "gtk_tree_view_insert_column_with_attributes"
(*  24*)		  : cptr * int * CString.cstring * cptr -> int;
(*  24*)    val insert_column_with_attributes
(*  24*)      : 'a t -> int -> string -> 'b CellRenderer.t -> int
(*  24*)	= fn self => fn position => fn title => fn cell =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (cell, 
(*  24*)		      fn cell => insert_column_with_attributes_
(*  24*)				   (self, position, 
(*  24*)				    CString.fromString title, cell)))
(*  24*)    val get_column_ : cptr * int -> cptr
(*  24*)	= _import "gtk_tree_view_get_column" : cptr * int -> cptr;
(*  24*)    val get_column : 'a t -> int -> base TreeViewColumn.t
(*  24*)	= fn self => fn n =>
(*  24*)	     TreeViewColumn.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, fn self => get_column_ (self, n)))
(*  24*)    val move_column_after_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_view_move_column_after"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val move_column_after
(*  24*)      : 'a t -> 'b TreeViewColumn.t -> 'c TreeViewColumn.t -> unit
(*  24*)	= fn self => fn column => fn base_column =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (column, 
(*  24*)			      fn column => GObject.withPtr
(*  24*)					     (base_column, 
(*  24*)					      fn base_column =>
(*  24*)						 move_column_after_
(*  24*)						   (self, column, 
(*  24*)						    base_column))))
(*  24*)    val set_expander_column_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_view_set_expander_column"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val set_expander_column : 'a t -> 'b TreeViewColumn.t -> unit
(*  24*)	= fn self => fn column =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (column, 
(*  24*)			      fn column => set_expander_column_
(*  24*)					     (self, column)))
(*  24*)    val get_expander_column_ : cptr -> cptr
(*  24*)	= _import "gtk_tree_view_get_expander_column" : cptr -> cptr;
(*  24*)    val get_expander_column : 'a t -> base TreeViewColumn.t
(*  24*)	= fn self =>
(*  24*)	     TreeViewColumn.inherit
(*  24*)	       ()
(*  24*)	       (fn () =>
(*  24*)		   GObject.withPtr
(*  24*)		     (self, fn self => get_expander_column_ self))
(*  24*)    val scroll_to_point_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_tree_view_scroll_to_point"
(*  24*)		  : cptr * int * int -> unit;
(*  24*)    val scroll_to_point : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn tree_x => fn tree_y =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => scroll_to_point_
(*  24*)					   (self, tree_x, tree_y))
(*  24*)    val scroll_to_cell_
(*  24*)      : cptr * cptr * cptr * bool * real * real -> unit
(*  24*)	= _import "gtk_tree_view_scroll_to_cell"
(*  24*)		  : cptr * cptr * cptr * bool * real * real -> unit;
(*  24*)    val scroll_to_cell : 'a t -> tree_path 
(*  24*)		      -> 'b TreeViewColumn.t option -> bool option 
(*  24*)		      -> real option -> real option
(*  24*)			 -> unit
(*  24*)	= fn self => fn path => fn column => fn use_align => 
(*  24*)	  fn row_align => fn col_align =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withOpt
(*  24*)			     (column, 
(*  24*)			      fn column =>
(*  24*)				 scroll_to_cell_
(*  24*)				   (self, path, column, 
(*  24*)				    getOpt (use_align, false), 
(*  24*)				    getOpt (row_align, 0.0), 
(*  24*)				    getOpt (col_align, 0.0))))
(*  24*)    val scroll_to_cell' : 'a t -> tree_path -> unit
(*  24*)	= fn self => fn path =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => scroll_to_cell_ (self, path, GObject.null, 
(*  24*)					    false, 0.0, 0.0))
(*  24*)    val row_activated_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_view_row_activated"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val row_activated
(*  24*)      : 'a t -> tree_path -> 'b TreeViewColumn.t -> unit
(*  24*)	= fn self => fn path => fn column =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (column, 
(*  24*)			      fn column => row_activated_
(*  24*)					     (self, path, column)))
(*  24*)    val expand_all_ : cptr -> unit
(*  24*)	= _import "gtk_tree_view_expand_all" : cptr -> unit;
(*  24*)    val expand_all : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => expand_all_ self)
(*  24*)    val collapse_all_ : cptr -> unit
(*  24*)	= _import "gtk_tree_view_collapse_all" : cptr -> unit;
(*  24*)    val collapse_all : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => collapse_all_ self)
(*  24*)    val expand_row_ : cptr * cptr * bool -> unit
(*  24*)	= _import "gtk_tree_view_expand_row"
(*  24*)		  : cptr * cptr * bool -> unit;
(*  24*)    val expand_row : 'a t -> tree_path -> bool -> unit
(*  24*)	= fn self => fn path => fn open_all =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => expand_row_ (self, path, open_all))
(*  24*)    val collapse_row_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_view_collapse_row" : cptr * cptr -> unit;
(*  24*)    val collapse_row : 'a t -> tree_path -> unit
(*  24*)	= fn self => fn path =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => collapse_row_ (self, path))
(*  24*)    val row_expanded_ : cptr * cptr -> bool
(*  24*)	= _import "gtk_tree_view_row_expanded" : cptr * cptr -> bool;
(*  24*)    val row_expanded : 'a t -> tree_path -> bool
(*  24*)	= fn self => fn path =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => row_expanded_ (self, path))
(*  24*)    val set_reorderable_ : cptr * bool -> unit
(*  24*)	= _import "gtk_tree_view_set_reorderable"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_reorderable : 'a t -> bool -> unit
(*  24*)	= fn self => fn reorderable =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_reorderable_ (self, reorderable))
(*  24*)    val get_reorderable_ : cptr -> bool
(*  24*)	= _import "gtk_tree_view_get_reorderable" : cptr -> bool;
(*  24*)    val get_reorderable : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_reorderable_ self)
(*  24*)    val set_cursor_ : cptr * cptr * cptr * bool -> unit
(*  24*)	= _import "gtk_tree_view_set_cursor"
(*  24*)		  : cptr * cptr * cptr * bool -> unit;
(*  24*)    val set_cursor : 'a t -> tree_path -> 'b TreeViewColumn.t option 
(*  24*)		  -> bool option
(*  24*)		     -> unit
(*  24*)	= fn self => fn path => fn focus_column => fn start_editing =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withOpt
(*  24*)			     (focus_column, 
(*  24*)			      fn focus_column =>
(*  24*)				 set_cursor_
(*  24*)				   (self, path, focus_column, 
(*  24*)				    getOpt (start_editing, false))))
(*  24*)    val set_cursor' : 'a t -> tree_path -> unit
(*  24*)	= fn self => fn path =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_cursor_
(*  24*)			     (self, path, GObject.null, false))
(*  24*)    val get_cursor_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_tree_view_get_cursor"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val get_cursor : 'a t -> tree_path -> 'b TreeViewColumn.t -> unit
(*  24*)	= fn self => fn path => fn focus_column =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (focus_column, 
(*  24*)			      fn focus_column =>
(*  24*)				 get_cursor_ (self, path, 
(*  24*)					      focus_column)))
(*  24*)    val unset_rows_drag_source_ : cptr -> unit
(*  24*)	= _import "gtk_tree_view_unset_rows_drag_source"
(*  24*)		  : cptr -> unit;
(*  24*)    val unset_rows_drag_source : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => unset_rows_drag_source_ self)
(*  24*)    val unset_rows_drag_dest_ : cptr -> unit
(*  24*)	= _import "gtk_tree_view_unset_rows_drag_dest" : cptr -> unit;
(*  24*)    val unset_rows_drag_dest : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => unset_rows_drag_dest_ self)
(*  24*)    val set_drag_dest_row_ : cptr * cptr * int -> unit
(*  24*)	= _import "gtk_tree_view_set_drag_dest_row"
(*  24*)		  : cptr * cptr * int -> unit;
(*  24*)    val set_drag_dest_row : 'a t -> tree_path -> drop_position -> unit
(*  24*)	= fn self => fn path => fn pos =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_drag_dest_row_ (self, path, pos))
(*  24*)    val set_enable_search_ : cptr * bool -> unit
(*  24*)	= _import "gtk_tree_view_set_enable_search"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_enable_search : 'a t -> bool -> unit
(*  24*)	= fn self => fn enable_search =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_enable_search_
(*  24*)					   (self, enable_search))
(*  24*)    val get_enable_search_ : cptr -> bool
(*  24*)	= _import "gtk_tree_view_get_enable_search" : cptr -> bool;
(*  24*)    val get_enable_search : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_enable_search_ self)
(*  24*)    val get_search_column_ : cptr -> int
(*  24*)	= _import "gtk_tree_view_get_search_column" : cptr -> int;
(*  24*)    val get_search_column : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_search_column_ self)
(*  24*)    val set_search_column_ : cptr * int -> unit
(*  24*)	= _import "gtk_tree_view_set_search_column"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_search_column : 'a t -> int -> unit
(*  24*)	= fn self => fn column =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_search_column_ (self, column))
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in
(*  24*)      val set_scroll_adjustments_sig
(*  24*)	: (unit -> unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "set-scroll-adjustments" false
(*  24*)			   (unit --> unit --> return_void) f
(*  24*)      val row_activated_sig : (unit -> unit -> unit)
(*  24*)			      -> 'a t Signal.signal
(*  24*)	  = fn f => signal "row-activated" false
(*  24*)			   (unit --> unit --> return_void) f
(*  24*)      val test_expand_row_sig
(*  24*)	: (unit -> unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "test-expand-row" false
(*  24*)			   (unit --> unit --> return_bool) f
(*  24*)      val test_collapse_row_sig
(*  24*)	: (unit -> unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "test-collapse-row" false
(*  24*)			   (unit --> unit --> return_bool) f
(*  24*)      val row_expanded_sig : (unit -> unit -> unit)
(*  24*)			     -> 'a t Signal.signal
(*  24*)	  = fn f => signal "row-expanded" false
(*  24*)			   (unit --> unit --> return_void) f
(*  24*)      val row_collapsed_sig : (unit -> unit -> unit)
(*  24*)			      -> 'a t Signal.signal
(*  24*)	  = fn f => signal "row-collapsed" false
(*  24*)			   (unit --> unit --> return_void) f
(*  24*)      val columns_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "columns-changed" false
(*  24*)			   (void --> return_void) f
(*  24*)      val cursor_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "cursor-changed" false
(*  24*)			   (void --> return_void) f
(*  24*)      val move_cursor_sig : (unit -> int -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "move-cursor" false
(*  24*)			   (unit --> int --> return_bool) f
(*  24*)      val select_all_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "select-all" false (void --> return_bool) f
(*  24*)      val unselect_all_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f =>
(*  24*)	       signal "unselect-all" false (void --> return_bool) f
(*  24*)      val select_cursor_row_sig : (bool -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "select-cursor-row" false
(*  24*)			   (bool --> return_bool) f
(*  24*)      val toggle_cursor_row_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "toggle-cursor-row" false
(*  24*)			   (void --> return_bool) f
(*  24*)      val expand_collapse_cursor_row_sig
(*  24*)	: (bool -> bool -> bool -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "expand-collapse-cursor-row" false
(*  24*)			   (bool --> bool --> bool --> return_bool) f
(*  24*)      val select_cursor_parent_sig
(*  24*)	: (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "select-cursor-parent" false
(*  24*)			   (void --> return_bool) f
(*  24*)      val start_interactive_search_sig
(*  24*)	: (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "start-interactive-search" false
(*  24*)			   (void --> return_bool) f
(*  24*)    end
(*  24*)end
(*  24*)structure Toolbar :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a toolbar_t
(*  24*)    type 'a t = 'a toolbar_t Container.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toToolbar : 'a t -> base t
(*  24*)    type style
(*  24*)    val ICONS : style
(*  24*)    val TEXT : style
(*  24*)    val BOTH : style
(*  24*)    val BOTH_HORIZ : style
(*  24*)    type childtype
(*  24*)    val CHILD_SPACE : childtype
(*  24*)    val CHILD_BUTTON : childtype
(*  24*)    val CHILD_TOGGLEBUTTON : childtype
(*  24*)    val CHILD_RADIOBUTTON : childtype
(*  24*)    val CHILD_WIDGET : childtype
(*  24*)    type space_style
(*  24*)    val SPACE_EMPTY : space_style
(*  24*)    val SPACE_LINE : space_style
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val append_space : 'a t -> unit
(*  24*)    val prepend_space : 'a t -> unit
(*  24*)    val insert_space : 'a t -> int -> unit
(*  24*)    val remove_space : 'a t -> int -> unit
(*  24*)    val append_widget
(*  24*)      : 'a t -> 'b Widget.t -> string option -> string option -> unit
(*  24*)    val append_widget' : 'a t -> 'b Widget.t -> unit
(*  24*)    val prepend_widget
(*  24*)      : 'a t -> 'b Widget.t -> string option -> string option -> unit
(*  24*)    val prepend_widget' : 'a t -> 'b Widget.t -> unit
(*  24*)    val insert_widget : 'a t -> 'b Widget.t -> string option 
(*  24*)		     -> string option -> int
(*  24*)			-> unit
(*  24*)    val insert_widget' : 'a t -> 'b Widget.t -> int -> unit
(*  24*)    val set_orientation : 'a t -> orientation -> unit
(*  24*)    val set_style : 'a t -> style -> unit
(*  24*)    val set_icon_size : 'a t -> icon_size -> unit
(*  24*)    val set_tooltips : 'a t -> bool -> unit
(*  24*)    val unset_style : 'a t -> unit
(*  24*)    val unset_icon_size : 'a t -> unit
(*  24*)    val get_orientation : 'a t -> orientation
(*  24*)    val get_style : 'a t -> style
(*  24*)    val get_icon_size : 'a t -> icon_size
(*  24*)    val get_tooltips : 'a t -> bool
(*  24*)    val orientation_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val style_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a toolbar_t = unit
(*  24*)    type 'a t = 'a toolbar_t Container.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Container.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toToolbar obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    type style = int
(*  24*)    val get_style_ : int ref * int ref * int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_toolbar_style"
(*  24*)		  : int ref * int ref * int ref * int ref -> unit;
(*  24*)    val (ICONS, TEXT, BOTH, BOTH_HORIZ)
(*  24*)	= let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)	  in get_style_ (x0, x1, x2, x3)
(*  24*)	   ; (!x0, !x1, !x2, !x3)
(*  24*)	  end
(*  24*)    type childtype = int
(*  24*)    val get_childtype_
(*  24*)      : int ref * int ref * int ref * int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_toolbar_childtype"
(*  24*)		  : int ref * int ref * int ref * int ref * int ref
(*  24*)		    -> unit;
(*  24*)    val (CHILD_SPACE, CHILD_BUTTON, CHILD_TOGGLEBUTTON, 
(*  24*)	 CHILD_RADIOBUTTON, CHILD_WIDGET)
(*  24*)	= let val (x0, x1, x2, x3, x4)
(*  24*)		  = (ref 0, ref 0, ref 0, ref 0, ref 0)
(*  24*)	  in get_childtype_ (x0, x1, x2, x3, x4)
(*  24*)	   ; (!x0, !x1, !x2, !x3, !x4)
(*  24*)	  end
(*  24*)    type space_style = int
(*  24*)    val get_space_style_ : int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_toolbar_space_style"
(*  24*)		  : int ref * int ref -> unit;
(*  24*)    val (SPACE_EMPTY, SPACE_LINE) = let val (x0, x1) = (ref 0, ref 0)
(*  24*)				    in get_space_style_ (x0, x1)
(*  24*)				     ; (!x0, !x1)
(*  24*)				    end
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_toolbar_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr = _import "gtk_toolbar_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val append_space_ : cptr -> unit
(*  24*)	= _import "gtk_toolbar_append_space" : cptr -> unit;
(*  24*)    val append_space : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => append_space_ self)
(*  24*)    val prepend_space_ : cptr -> unit
(*  24*)	= _import "gtk_toolbar_prepend_space" : cptr -> unit;
(*  24*)    val prepend_space : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => prepend_space_ self)
(*  24*)    val insert_space_ : cptr * int -> unit
(*  24*)	= _import "gtk_toolbar_insert_space" : cptr * int -> unit;
(*  24*)    val insert_space : 'a t -> int -> unit
(*  24*)	= fn self => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => insert_space_ (self, position))
(*  24*)    val remove_space_ : cptr * int -> unit
(*  24*)	= _import "gtk_toolbar_remove_space" : cptr * int -> unit;
(*  24*)    val remove_space : 'a t -> int -> unit
(*  24*)	= fn self => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => remove_space_ (self, position))
(*  24*)    val append_widget_
(*  24*)      : cptr * cptr * CString.cstring * CString.cstring -> unit
(*  24*)	= _import "gtk_toolbar_append_widget"
(*  24*)		  : cptr * cptr * CString.cstring * CString.cstring
(*  24*)		    -> unit;
(*  24*)    val append_widget
(*  24*)      : 'a t -> 'b Widget.t -> string option -> string option -> unit
(*  24*)	= fn self => fn widget => fn tooltip_text => 
(*  24*)	  fn tooltip_private_text =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (widget, 
(*  24*)		      fn widget =>
(*  24*)			 append_widget_
(*  24*)			   (self, widget, 
(*  24*)			    CString.fromString
(*  24*)			      (getOpt (tooltip_text, "")), 
(*  24*)			    CString.fromString
(*  24*)			      (getOpt (tooltip_private_text, "")))))
(*  24*)    val append_widget' : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn widget =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (widget, 
(*  24*)			      fn widget => append_widget_
(*  24*)					     (self, widget, 
(*  24*)					      CString.fromString "", 
(*  24*)					      CString.fromString "")))
(*  24*)    val prepend_widget_
(*  24*)      : cptr * cptr * CString.cstring * CString.cstring -> unit
(*  24*)	= _import "gtk_toolbar_prepend_widget"
(*  24*)		  : cptr * cptr * CString.cstring * CString.cstring
(*  24*)		    -> unit;
(*  24*)    val prepend_widget
(*  24*)      : 'a t -> 'b Widget.t -> string option -> string option -> unit
(*  24*)	= fn self => fn widget => fn tooltip_text => 
(*  24*)	  fn tooltip_private_text =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (widget, 
(*  24*)		      fn widget =>
(*  24*)			 prepend_widget_
(*  24*)			   (self, widget, 
(*  24*)			    CString.fromString
(*  24*)			      (getOpt (tooltip_text, "")), 
(*  24*)			    CString.fromString
(*  24*)			      (getOpt (tooltip_private_text, "")))))
(*  24*)    val prepend_widget' : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn widget =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (widget, 
(*  24*)			      fn widget => prepend_widget_
(*  24*)					     (self, widget, 
(*  24*)					      CString.fromString "", 
(*  24*)					      CString.fromString "")))
(*  24*)    val insert_widget_
(*  24*)      : cptr * cptr * CString.cstring * CString.cstring * int -> unit
(*  24*)	= _import
(*  24*)	    "gtk_toolbar_insert_widget"
(*  24*)	    : cptr * cptr * CString.cstring * CString.cstring * int
(*  24*)	      -> unit;
(*  24*)    val insert_widget : 'a t -> 'b Widget.t -> string option 
(*  24*)		     -> string option -> int
(*  24*)			-> unit
(*  24*)	= fn self => fn widget => fn tooltip_text => 
(*  24*)	  fn tooltip_private_text => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (widget, 
(*  24*)		      fn widget =>
(*  24*)			 insert_widget_
(*  24*)			   (self, widget, 
(*  24*)			    CString.fromString
(*  24*)			      (getOpt (tooltip_text, "")), 
(*  24*)			    CString.fromString
(*  24*)			      (getOpt (tooltip_private_text, "")), 
(*  24*)			    position)))
(*  24*)    val insert_widget' : 'a t -> 'b Widget.t -> int -> unit
(*  24*)	= fn self => fn widget => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (widget, 
(*  24*)		      fn widget =>
(*  24*)			 insert_widget_
(*  24*)			   (self, widget, CString.fromString "", 
(*  24*)			    CString.fromString "", position)))
(*  24*)    val set_orientation_ : cptr * int -> unit
(*  24*)	= _import "gtk_toolbar_set_orientation" : cptr * int -> unit;
(*  24*)    val set_orientation : 'a t -> orientation -> unit
(*  24*)	= fn self => fn orientation =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_orientation_ (self, orientation))
(*  24*)    val set_style_ : cptr * int -> unit
(*  24*)	= _import "gtk_toolbar_set_style" : cptr * int -> unit;
(*  24*)    val set_style : 'a t -> style -> unit
(*  24*)	= fn self => fn style =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_style_ (self, style))
(*  24*)    val set_icon_size_ : cptr * int -> unit
(*  24*)	= _import "gtk_toolbar_set_icon_size" : cptr * int -> unit;
(*  24*)    val set_icon_size : 'a t -> icon_size -> unit
(*  24*)	= fn self => fn icon_size =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_icon_size_ (self, icon_size))
(*  24*)    val set_tooltips_ : cptr * bool -> unit
(*  24*)	= _import "gtk_toolbar_set_tooltips" : cptr * bool -> unit;
(*  24*)    val set_tooltips : 'a t -> bool -> unit
(*  24*)	= fn self => fn enable =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_tooltips_ (self, enable))
(*  24*)    val unset_style_ : cptr -> unit
(*  24*)	= _import "gtk_toolbar_unset_style" : cptr -> unit;
(*  24*)    val unset_style : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => unset_style_ self)
(*  24*)    val unset_icon_size_ : cptr -> unit
(*  24*)	= _import "gtk_toolbar_unset_icon_size" : cptr -> unit;
(*  24*)    val unset_icon_size : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => unset_icon_size_ self)
(*  24*)    val get_orientation_ : cptr -> int
(*  24*)	= _import "gtk_toolbar_get_orientation" : cptr -> int;
(*  24*)    val get_orientation : 'a t -> orientation
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_orientation_ self)
(*  24*)    val get_style_ : cptr -> int
(*  24*)	= _import "gtk_toolbar_get_style" : cptr -> int;
(*  24*)    val get_style : 'a t -> style
(*  24*)	= fn self => GObject.withPtr (self, fn self => get_style_ self)
(*  24*)    val get_icon_size_ : cptr -> int
(*  24*)	= _import "gtk_toolbar_get_icon_size" : cptr -> int;
(*  24*)    val get_icon_size : 'a t -> icon_size
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_icon_size_ self)
(*  24*)    val get_tooltips_ : cptr -> bool
(*  24*)	= _import "gtk_toolbar_get_tooltips" : cptr -> bool;
(*  24*)    val get_tooltips : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_tooltips_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val orientation_changed_sig
(*  24*)	 : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "orientation-changed" false
(*  24*)			    (unit --> return_void) f
(*  24*)       val style_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "style-changed" false (unit --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure TextView :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a textview_t
(*  24*)    type 'a t = 'a textview_t Container.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTextView : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val new_withbuffer : 'a TextBuffer.t option -> base t
(*  24*)    val new_withbuffer' : unit -> base t
(*  24*)    val setbuffer : 'a t -> 'b TextBuffer.t -> unit
(*  24*)    val get_buffer : 'a t -> base TextBuffer.t
(*  24*)    val scroll_toiter : 'a t -> textiter -> real -> bool option 
(*  24*)		     -> real option -> real option
(*  24*)			-> bool
(*  24*)    val scroll_toiter' : 'a t -> textiter -> real -> bool
(*  24*)    val scroll_to_mark : 'a t -> 'b TextMark.t -> real -> bool option 
(*  24*)		      -> real option -> real option
(*  24*)			 -> unit
(*  24*)    val scroll_to_mark' : 'a t -> 'b TextMark.t -> real -> unit
(*  24*)    val scroll_mark_onscreen : 'a t -> 'b TextMark.t -> unit
(*  24*)    val move_mark_onscreen : 'a t -> 'b TextMark.t -> bool
(*  24*)    val place_cursor_onscreen : 'a t -> bool
(*  24*)    val set_cursor_visible : 'a t -> bool -> unit
(*  24*)    val get_cursor_visible : 'a t -> bool
(*  24*)    val getiter_at_location : 'a t -> textiter -> int -> int -> unit
(*  24*)    val set_border_window_size
(*  24*)      : 'a t -> text_window_type_t -> int -> unit
(*  24*)    val get_border_window_size : 'a t -> text_window_type_t -> int
(*  24*)    val forward_display_line : 'a t -> textiter -> bool
(*  24*)    val backward_display_line : 'a t -> textiter -> bool
(*  24*)    val forward_display_line_end : 'a t -> textiter -> bool
(*  24*)    val backward_display_line_start : 'a t -> textiter -> bool
(*  24*)    val starts_display_line : 'a t -> textiter -> bool
(*  24*)    val move_visually : 'a t -> textiter -> int -> bool
(*  24*)    val add_child_at_anchor
(*  24*)      : 'a t -> 'b Widget.t -> 'c TextChildAnchor.t -> unit
(*  24*)    val add_child_in_window
(*  24*)      : 'a t -> 'b Widget.t -> text_window_type_t -> int -> int -> unit
(*  24*)    val move_child : 'a t -> 'b Widget.t -> int -> int -> unit
(*  24*)    val set_wrap_mode : 'a t -> wrap_mode -> unit
(*  24*)    val get_wrap_mode : 'a t -> wrap_mode
(*  24*)    val set_editable : 'a t -> bool -> unit
(*  24*)    val get_editable : 'a t -> bool
(*  24*)    val set_pixels_above_lines : 'a t -> int -> unit
(*  24*)    val get_pixels_above_lines : 'a t -> int
(*  24*)    val set_pixels_below_lines : 'a t -> int -> unit
(*  24*)    val get_pixels_below_lines : 'a t -> int
(*  24*)    val set_pixels_inside_wrap : 'a t -> int -> unit
(*  24*)    val get_pixels_inside_wrap : 'a t -> int
(*  24*)    val set_justification : 'a t -> justification -> unit
(*  24*)    val get_justification : 'a t -> justification
(*  24*)    val set_left_margin : 'a t -> int -> unit
(*  24*)    val get_left_margin : 'a t -> int
(*  24*)    val set_right_margin : 'a t -> int -> unit
(*  24*)    val get_right_margin : 'a t -> int
(*  24*)    val set_indent : 'a t -> int -> unit
(*  24*)    val get_indent : 'a t -> int
(*  24*)    val get_default_attributes : 'a t -> text_attributes
(*  24*)    val move_focus_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val set_scroll_adjustments_sig
(*  24*)      : (unit -> unit -> unit) -> 'a t Signal.signal
(*  24*)    val move_cursor_sig : (unit -> int -> bool -> unit)
(*  24*)			  -> 'a t Signal.signal
(*  24*)    val page_horizontally_sig
(*  24*)      : (int -> bool -> unit) -> 'a t Signal.signal
(*  24*)    val set_anchor_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val insert_at_cursor_sig : (char -> unit) -> 'a t Signal.signal
(*  24*)    val delete_from_cursor_sig
(*  24*)      : (unit -> int -> unit) -> 'a t Signal.signal
(*  24*)    val cut_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val copy_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val paste_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val toggle_overwrite_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val populate_popup_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a textview_t = unit
(*  24*)    type 'a t = 'a textview_t Container.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Container.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTextView obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_text_view_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_text_view_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val new_withbuffer_ : cptr -> cptr
(*  24*)	= _import "gtk_text_view_new_with_buffer" : cptr -> cptr;
(*  24*)    val new_withbuffer : 'a TextBuffer.t option -> base t
(*  24*)	= fn buffer =>
(*  24*)	     make (GObject.withOpt
(*  24*)		     (buffer, fn buffer => new_withbuffer_ buffer))
(*  24*)    val new_withbuffer' : unit -> base t
(*  24*)	= fn dummy => make (new_withbuffer_ GObject.null)
(*  24*)    val setbuffer_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_text_view_set_buffer" : cptr * cptr -> unit;
(*  24*)    val setbuffer : 'a t -> 'b TextBuffer.t -> unit
(*  24*)	= fn self => fn buffer =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (buffer, fn buffer => setbuffer_ (self, buffer)))
(*  24*)    val get_buffer_ : cptr -> cptr
(*  24*)	= _import "gtk_text_view_get_buffer" : cptr -> cptr;
(*  24*)    val get_buffer : 'a t -> base TextBuffer.t
(*  24*)	= fn self => TextBuffer.inherit
(*  24*)		       ()
(*  24*)		       (fn () => GObject.withPtr
(*  24*)				   (self, fn self => get_buffer_ self))
(*  24*)    val scroll_toiter_
(*  24*)      : cptr * cptr * real * bool * real * real -> bool
(*  24*)	= _import "gtk_text_view_scroll_to_iter"
(*  24*)		  : cptr * cptr * real * bool * real * real -> bool;
(*  24*)    val scroll_toiter : 'a t -> textiter -> real -> bool option 
(*  24*)		     -> real option -> real option
(*  24*)			-> bool
(*  24*)	= fn self => fn iter => fn within_margin => fn use_align => 
(*  24*)	  fn xalign => fn yalign =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => scroll_toiter_ (self, iter, within_margin, 
(*  24*)					   getOpt (use_align, false), 
(*  24*)					   getOpt (xalign, 0.5), 
(*  24*)					   getOpt (yalign, 0.5)))
(*  24*)    val scroll_toiter' : 'a t -> textiter -> real -> bool
(*  24*)	= fn self => fn iter => fn within_margin =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => scroll_toiter_ (self, iter, within_margin, 
(*  24*)					   false, 0.5, 0.5))
(*  24*)    val scroll_to_mark_
(*  24*)      : cptr * cptr * real * bool * real * real -> unit
(*  24*)	= _import "gtk_text_view_scroll_to_mark"
(*  24*)		  : cptr * cptr * real * bool * real * real -> unit;
(*  24*)    val scroll_to_mark : 'a t -> 'b TextMark.t -> real -> bool option 
(*  24*)		      -> real option -> real option
(*  24*)			 -> unit
(*  24*)	= fn self => fn mark => fn within_margin => fn use_align => 
(*  24*)	  fn xalign => fn yalign =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (mark, 
(*  24*)			      fn mark => scroll_to_mark_
(*  24*)					   (self, mark, within_margin, 
(*  24*)					    getOpt (use_align, false), 
(*  24*)					    getOpt (xalign, 0.5), 
(*  24*)					    getOpt (yalign, 0.5))))
(*  24*)    val scroll_to_mark' : 'a t -> 'b TextMark.t -> real -> unit
(*  24*)	= fn self => fn mark => fn within_margin =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (mark, 
(*  24*)			      fn mark => scroll_to_mark_
(*  24*)					   (self, mark, within_margin, 
(*  24*)					    false, 0.5, 0.5)))
(*  24*)    val scroll_mark_onscreen_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_text_view_scroll_mark_onscreen"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val scroll_mark_onscreen : 'a t -> 'b TextMark.t -> unit
(*  24*)	= fn self => fn mark =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (mark, 
(*  24*)			      fn mark => scroll_mark_onscreen_
(*  24*)					   (self, mark)))
(*  24*)    val move_mark_onscreen_ : cptr * cptr -> bool
(*  24*)	= _import "gtk_text_view_move_mark_onscreen"
(*  24*)		  : cptr * cptr -> bool;
(*  24*)    val move_mark_onscreen : 'a t -> 'b TextMark.t -> bool
(*  24*)	= fn self => fn mark =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (mark, 
(*  24*)			      fn mark => move_mark_onscreen_
(*  24*)					   (self, mark)))
(*  24*)    val place_cursor_onscreen_ : cptr -> bool
(*  24*)	= _import "gtk_text_view_place_cursor_onscreen" : cptr -> bool;
(*  24*)    val place_cursor_onscreen : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => place_cursor_onscreen_ self)
(*  24*)    val set_cursor_visible_ : cptr * bool -> unit
(*  24*)	= _import "gtk_text_view_set_cursor_visible"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_cursor_visible : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_cursor_visible_ (self, setting))
(*  24*)    val get_cursor_visible_ : cptr -> bool
(*  24*)	= _import "gtk_text_view_get_cursor_visible" : cptr -> bool;
(*  24*)    val get_cursor_visible : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_cursor_visible_ self)
(*  24*)    val getiter_at_location_ : cptr * cptr * int * int -> unit
(*  24*)	= _import "gtk_text_view_get_iter_at_location"
(*  24*)		  : cptr * cptr * int * int -> unit;
(*  24*)    val getiter_at_location : 'a t -> textiter -> int -> int -> unit
(*  24*)	= fn self => fn iter => fn x => fn y =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => getiter_at_location_
(*  24*)					   (self, iter, x, y))
(*  24*)    val set_border_window_size_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_text_view_set_border_window_size"
(*  24*)		  : cptr * int * int -> unit;
(*  24*)    val set_border_window_size
(*  24*)      : 'a t -> text_window_type_t -> int -> unit
(*  24*)	= fn self => fn typ => fn size =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_border_window_size_
(*  24*)					   (self, typ, size))
(*  24*)    val get_border_window_size_ : cptr * int -> int
(*  24*)	= _import "gtk_text_view_get_border_window_size"
(*  24*)		  : cptr * int -> int;
(*  24*)    val get_border_window_size : 'a t -> text_window_type_t -> int
(*  24*)	= fn self => fn typ =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => get_border_window_size_ (self, typ))
(*  24*)    val forward_display_line_ : cptr * cptr -> bool
(*  24*)	= _import "gtk_text_view_forward_display_line"
(*  24*)		  : cptr * cptr -> bool;
(*  24*)    val forward_display_line : 'a t -> textiter -> bool
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => forward_display_line_ (self, iter))
(*  24*)    val backward_display_line_ : cptr * cptr -> bool
(*  24*)	= _import "gtk_text_view_backward_display_line"
(*  24*)		  : cptr * cptr -> bool;
(*  24*)    val backward_display_line : 'a t -> textiter -> bool
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => backward_display_line_ (self, iter))
(*  24*)    val forward_display_line_end_ : cptr * cptr -> bool
(*  24*)	= _import "gtk_text_view_forward_display_line_end"
(*  24*)		  : cptr * cptr -> bool;
(*  24*)    val forward_display_line_end : 'a t -> textiter -> bool
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => forward_display_line_end_ (self, iter))
(*  24*)    val backward_display_line_start_ : cptr * cptr -> bool
(*  24*)	= _import "gtk_text_view_backward_display_line_start"
(*  24*)		  : cptr * cptr -> bool;
(*  24*)    val backward_display_line_start : 'a t -> textiter -> bool
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => backward_display_line_start_ (self, iter))
(*  24*)    val starts_display_line_ : cptr * cptr -> bool
(*  24*)	= _import "gtk_text_view_starts_display_line"
(*  24*)		  : cptr * cptr -> bool;
(*  24*)    val starts_display_line : 'a t -> textiter -> bool
(*  24*)	= fn self => fn iter =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => starts_display_line_ (self, iter))
(*  24*)    val move_visually_ : cptr * cptr * int -> bool
(*  24*)	= _import "gtk_text_view_move_visually"
(*  24*)		  : cptr * cptr * int -> bool;
(*  24*)    val move_visually : 'a t -> textiter -> int -> bool
(*  24*)	= fn self => fn iter => fn count =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => move_visually_ (self, iter, count))
(*  24*)    val add_child_at_anchor_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_text_view_add_child_at_anchor"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val add_child_at_anchor
(*  24*)      : 'a t -> 'b Widget.t -> 'c TextChildAnchor.t -> unit
(*  24*)	= fn self => fn child => fn anchor =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child =>
(*  24*)				 GObject.withPtr
(*  24*)				   (anchor, 
(*  24*)				    fn anchor =>
(*  24*)				       add_child_at_anchor_
(*  24*)					 (self, child, anchor))))
(*  24*)    val add_child_in_window_ : cptr * cptr * int * int * int -> unit
(*  24*)	= _import "gtk_text_view_add_child_in_window"
(*  24*)		  : cptr * cptr * int * int * int -> unit;
(*  24*)    val add_child_in_window
(*  24*)      : 'a t -> 'b Widget.t -> text_window_type_t -> int -> int -> unit
(*  24*)	= fn self => fn child => fn which_window => fn xpos => 
(*  24*)	  fn ypos =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child =>
(*  24*)				 add_child_in_window_
(*  24*)				   (self, child, which_window, 
(*  24*)				    xpos, ypos)))
(*  24*)    val move_child_ : cptr * cptr * int * int -> unit
(*  24*)	= _import "gtk_text_view_move_child"
(*  24*)		  : cptr * cptr * int * int -> unit;
(*  24*)    val move_child : 'a t -> 'b Widget.t -> int -> int -> unit
(*  24*)	= fn self => fn child => fn xpos => fn ypos =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => move_child_
(*  24*)					    (self, child, xpos, ypos)))
(*  24*)    val set_wrap_mode_ : cptr * int -> unit
(*  24*)	= _import "gtk_text_view_set_wrap_mode" : cptr * int -> unit;
(*  24*)    val set_wrap_mode : 'a t -> wrap_mode -> unit
(*  24*)	= fn self => fn wrap_mode =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_wrap_mode_ (self, wrap_mode))
(*  24*)    val get_wrap_mode_ : cptr -> int
(*  24*)	= _import "gtk_text_view_get_wrap_mode" : cptr -> int;
(*  24*)    val get_wrap_mode : 'a t -> wrap_mode
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_wrap_mode_ self)
(*  24*)    val set_editable_ : cptr * bool -> unit
(*  24*)	= _import "gtk_text_view_set_editable" : cptr * bool -> unit;
(*  24*)    val set_editable : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_editable_ (self, setting))
(*  24*)    val get_editable_ : cptr -> bool
(*  24*)	= _import "gtk_text_view_get_editable" : cptr -> bool;
(*  24*)    val get_editable : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_editable_ self)
(*  24*)    val set_pixels_above_lines_ : cptr * int -> unit
(*  24*)	= _import "gtk_text_view_set_pixels_above_lines"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_pixels_above_lines : 'a t -> int -> unit
(*  24*)	= fn self => fn pixels_above_lines =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_pixels_above_lines_
(*  24*)					   (self, pixels_above_lines))
(*  24*)    val get_pixels_above_lines_ : cptr -> int
(*  24*)	= _import "gtk_text_view_get_pixels_above_lines" : cptr -> int;
(*  24*)    val get_pixels_above_lines : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_pixels_above_lines_ self)
(*  24*)    val set_pixels_below_lines_ : cptr * int -> unit
(*  24*)	= _import "gtk_text_view_set_pixels_below_lines"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_pixels_below_lines : 'a t -> int -> unit
(*  24*)	= fn self => fn pixels_below_lines =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_pixels_below_lines_
(*  24*)					   (self, pixels_below_lines))
(*  24*)    val get_pixels_below_lines_ : cptr -> int
(*  24*)	= _import "gtk_text_view_get_pixels_below_lines" : cptr -> int;
(*  24*)    val get_pixels_below_lines : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_pixels_below_lines_ self)
(*  24*)    val set_pixels_inside_wrap_ : cptr * int -> unit
(*  24*)	= _import "gtk_text_view_set_pixels_inside_wrap"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_pixels_inside_wrap : 'a t -> int -> unit
(*  24*)	= fn self => fn pixels_inside_wrap =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_pixels_inside_wrap_
(*  24*)					   (self, pixels_inside_wrap))
(*  24*)    val get_pixels_inside_wrap_ : cptr -> int
(*  24*)	= _import "gtk_text_view_get_pixels_inside_wrap" : cptr -> int;
(*  24*)    val get_pixels_inside_wrap : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_pixels_inside_wrap_ self)
(*  24*)    val set_justification_ : cptr * int -> unit
(*  24*)	= _import "gtk_text_view_set_justification"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_justification : 'a t -> justification -> unit
(*  24*)	= fn self => fn justification =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_justification_
(*  24*)					   (self, justification))
(*  24*)    val get_justification_ : cptr -> int
(*  24*)	= _import "gtk_text_view_get_justification" : cptr -> int;
(*  24*)    val get_justification : 'a t -> justification
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_justification_ self)
(*  24*)    val set_left_margin_ : cptr * int -> unit
(*  24*)	= _import "gtk_text_view_set_left_margin" : cptr * int -> unit;
(*  24*)    val set_left_margin : 'a t -> int -> unit
(*  24*)	= fn self => fn left_margin =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_left_margin_ (self, left_margin))
(*  24*)    val get_left_margin_ : cptr -> int
(*  24*)	= _import "gtk_text_view_get_left_margin" : cptr -> int;
(*  24*)    val get_left_margin : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_left_margin_ self)
(*  24*)    val set_right_margin_ : cptr * int -> unit
(*  24*)	= _import "gtk_text_view_set_right_margin"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_right_margin : 'a t -> int -> unit
(*  24*)	= fn self => fn right_margin =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_right_margin_
(*  24*)					   (self, right_margin))
(*  24*)    val get_right_margin_ : cptr -> int
(*  24*)	= _import "gtk_text_view_get_right_margin" : cptr -> int;
(*  24*)    val get_right_margin : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_right_margin_ self)
(*  24*)    val set_indent_ : cptr * int -> unit
(*  24*)	= _import "gtk_text_view_set_indent" : cptr * int -> unit;
(*  24*)    val set_indent : 'a t -> int -> unit
(*  24*)	= fn self => fn indent =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_indent_ (self, indent))
(*  24*)    val get_indent_ : cptr -> int
(*  24*)	= _import "gtk_text_view_get_indent" : cptr -> int;
(*  24*)    val get_indent : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_indent_ self)
(*  24*)    val get_default_attributes_ : cptr -> cptr
(*  24*)	= _import "gtk_text_view_get_default_attributes"
(*  24*)		  : cptr -> cptr;
(*  24*)    val get_default_attributes : 'a t -> text_attributes
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_default_attributes_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val move_focus_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "move-focus" false (unit --> return_void) f
(*  24*)       val set_scroll_adjustments_sig
(*  24*)	 : (unit -> unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "set-scroll-adjustments" false
(*  24*)			    (unit --> unit --> return_void) f
(*  24*)       val move_cursor_sig : (unit -> int -> bool -> unit)
(*  24*)			     -> 'a t Signal.signal
(*  24*)	   = fn f => signal "move-cursor" false
(*  24*)			    (unit --> int --> bool --> return_void) f
(*  24*)       val page_horizontally_sig
(*  24*)	 : (int -> bool -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "page-horizontally" false
(*  24*)			    (int --> bool --> return_void) f
(*  24*)       val set_anchor_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "set-anchor" false (void --> return_void) f
(*  24*)       val insert_at_cursor_sig : (char -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "insert-at-cursor" false
(*  24*)			    (char --> return_void) f
(*  24*)       val delete_from_cursor_sig
(*  24*)	 : (unit -> int -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "delete-from-cursor" false
(*  24*)			    (unit --> int --> return_void) f
(*  24*)       val cut_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "cut-clipboard" false (void --> return_void) f
(*  24*)       val copy_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "copy-clipboard" false
(*  24*)			    (void --> return_void) f
(*  24*)       val paste_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "paste-clipboard" false
(*  24*)			    (void --> return_void) f
(*  24*)       val toggle_overwrite_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "toggle-overwrite" false
(*  24*)			    (void --> return_void) f
(*  24*)       val populate_popup_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "populate-popup" false
(*  24*)			    (unit --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Table :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a table_t
(*  24*)    type 'a t = 'a table_t Container.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTable : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : int option -> int option -> bool option -> base t
(*  24*)    val new' : unit -> base t
(*  24*)    val resize : 'a t -> int -> int -> unit
(*  24*)    val attach_defaults
(*  24*)      : 'a t -> 'b Widget.t -> int -> int -> int -> int -> unit
(*  24*)    val set_row_spacing : 'a t -> int -> int -> unit
(*  24*)    val get_row_spacing : 'a t -> int -> int
(*  24*)    val set_col_spacing : 'a t -> int -> int -> unit
(*  24*)    val get_col_spacing : 'a t -> int -> int
(*  24*)    val set_row_spacings : 'a t -> int -> unit
(*  24*)    val get_default_row_spacing : 'a t -> int
(*  24*)    val set_col_spacings : 'a t -> int -> unit
(*  24*)    val get_default_col_spacing : 'a t -> int
(*  24*)    val set_homogeneous : 'a t -> bool -> unit
(*  24*)    val get_homogeneous : 'a t -> bool
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a table_t = unit
(*  24*)    type 'a t = 'a table_t Container.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Container.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTable obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_table_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : int * int * bool -> cptr
(*  24*)	= _import "gtk_table_new" : int * int * bool -> cptr;
(*  24*)    val new : int option -> int option -> bool option -> base t
(*  24*)	= fn rows => fn columns => fn homogeneous =>
(*  24*)	     make (new_ (getOpt (rows, 1), getOpt (columns, 1), 
(*  24*)			 getOpt (homogeneous, false)))
(*  24*)    val new' : unit -> base t = fn dummy => make (new_ (1, 1, false))
(*  24*)    val resize_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_table_resize" : cptr * int * int -> unit;
(*  24*)    val resize : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn rows => fn columns =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => resize_ (self, rows, columns))
(*  24*)    val attach_defaults_ : cptr * cptr * int * int * int * int -> unit
(*  24*)	= _import "gtk_table_attach_defaults"
(*  24*)		  : cptr * cptr * int * int * int * int -> unit;
(*  24*)    val attach_defaults
(*  24*)      : 'a t -> 'b Widget.t -> int -> int -> int -> int -> unit
(*  24*)	= fn self => fn widget => fn left_attach => fn right_attach => 
(*  24*)	  fn top_attach => fn bottom_attach =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (widget, 
(*  24*)			      fn widget =>
(*  24*)				 attach_defaults_
(*  24*)				   (self, widget, left_attach, 
(*  24*)				    right_attach, top_attach, 
(*  24*)				    bottom_attach)))
(*  24*)    val set_row_spacing_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_table_set_row_spacing"
(*  24*)		  : cptr * int * int -> unit;
(*  24*)    val set_row_spacing : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn row => fn spacing =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_row_spacing_ (self, row, spacing))
(*  24*)    val get_row_spacing_ : cptr * int -> int
(*  24*)	= _import "gtk_table_get_row_spacing" : cptr * int -> int;
(*  24*)    val get_row_spacing : 'a t -> int -> int
(*  24*)	= fn self => fn row =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => get_row_spacing_ (self, row))
(*  24*)    val set_col_spacing_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_table_set_col_spacing"
(*  24*)		  : cptr * int * int -> unit;
(*  24*)    val set_col_spacing : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn column => fn spacing =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_col_spacing_
(*  24*)					   (self, column, spacing))
(*  24*)    val get_col_spacing_ : cptr * int -> int
(*  24*)	= _import "gtk_table_get_col_spacing" : cptr * int -> int;
(*  24*)    val get_col_spacing : 'a t -> int -> int
(*  24*)	= fn self => fn column =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => get_col_spacing_ (self, column))
(*  24*)    val set_row_spacings_ : cptr * int -> unit
(*  24*)	= _import "gtk_table_set_row_spacings" : cptr * int -> unit;
(*  24*)    val set_row_spacings : 'a t -> int -> unit
(*  24*)	= fn self => fn spacing =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_row_spacings_ (self, spacing))
(*  24*)    val get_default_row_spacing_ : cptr -> int
(*  24*)	= _import "gtk_table_get_default_row_spacing" : cptr -> int;
(*  24*)    val get_default_row_spacing : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_default_row_spacing_ self)
(*  24*)    val set_col_spacings_ : cptr * int -> unit
(*  24*)	= _import "gtk_table_set_col_spacings" : cptr * int -> unit;
(*  24*)    val set_col_spacings : 'a t -> int -> unit
(*  24*)	= fn self => fn spacing =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_col_spacings_ (self, spacing))
(*  24*)    val get_default_col_spacing_ : cptr -> int
(*  24*)	= _import "gtk_table_get_default_col_spacing" : cptr -> int;
(*  24*)    val get_default_col_spacing : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_default_col_spacing_ self)
(*  24*)    val set_homogeneous_ : cptr * bool -> unit
(*  24*)	= _import "gtk_table_set_homogeneous" : cptr * bool -> unit;
(*  24*)    val set_homogeneous : 'a t -> bool -> unit
(*  24*)	= fn self => fn homogeneous =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_homogeneous_ (self, homogeneous))
(*  24*)    val get_homogeneous_ : cptr -> bool
(*  24*)	= _import "gtk_table_get_homogeneous" : cptr -> bool;
(*  24*)    val get_homogeneous : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_homogeneous_ self)
(*  24*)end
(*  24*)structure Socket :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a socket_t
(*  24*)    type 'a t = 'a socket_t Container.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toSocket : 'a t -> base t
(*  24*)    val new : unit -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val plug_added_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val plug_removed_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a socket_t = unit
(*  24*)    type 'a t = 'a socket_t Container.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Container.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toSocket obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val new_ : unit -> cptr = _import "gtk_socket_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_socket_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val plug_added_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "plug-added" false (void --> return_void) f
(*  24*)       val plug_removed_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "plug-removed" false (void --> return_bool) f
(*  24*)    end
(*  24*)end
(*  24*)structure Paned :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a paned_t
(*  24*)    type 'a t = 'a paned_t Container.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toPaned : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val add1 : 'a t -> 'b Widget.t -> unit
(*  24*)    val add2 : 'a t -> 'b Widget.t -> unit
(*  24*)    val pack1
(*  24*)      : 'a t -> 'b Widget.t -> bool option -> bool option -> unit
(*  24*)    val pack1' : 'a t -> 'b Widget.t -> unit
(*  24*)    val pack2
(*  24*)      : 'a t -> 'b Widget.t -> bool option -> bool option -> unit
(*  24*)    val pack2' : 'a t -> 'b Widget.t -> unit
(*  24*)    val get_position : 'a t -> int
(*  24*)    val set_position : 'a t -> int -> unit
(*  24*)    val compute_position : 'a t -> int -> int -> int -> unit
(*  24*)    val cycle_child_focus_sig : (bool -> bool) -> 'a t Signal.signal
(*  24*)    val toggle_handle_focus_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val move_handle_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val cycle_handle_focus_sig : (bool -> bool) -> 'a t Signal.signal
(*  24*)    val accept_position_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val cancel_position_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a paned_t = unit
(*  24*)    type 'a t = 'a paned_t Container.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Container.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toPaned obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_paned_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val add1_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_paned_add1" : cptr * cptr -> unit;
(*  24*)    val add1 : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn child =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, fn child => add1_ (self, child)))
(*  24*)    val add2_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_paned_add2" : cptr * cptr -> unit;
(*  24*)    val add2 : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn child =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, fn child => add2_ (self, child)))
(*  24*)    val pack1_ : cptr * cptr * bool * bool -> unit
(*  24*)	= _import "gtk_paned_pack1"
(*  24*)		  : cptr * cptr * bool * bool -> unit;
(*  24*)    val pack1
(*  24*)      : 'a t -> 'b Widget.t -> bool option -> bool option -> unit
(*  24*)	= fn self => fn child => fn resize => fn shrink =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child =>
(*  24*)				 pack1_ (self, child, 
(*  24*)					 getOpt (resize, false), 
(*  24*)					 getOpt (shrink, true))))
(*  24*)    val pack1' : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn child =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child =>
(*  24*)				 pack1_ (self, child, false, true)))
(*  24*)    val pack2_ : cptr * cptr * bool * bool -> unit
(*  24*)	= _import "gtk_paned_pack2"
(*  24*)		  : cptr * cptr * bool * bool -> unit;
(*  24*)    val pack2
(*  24*)      : 'a t -> 'b Widget.t -> bool option -> bool option -> unit
(*  24*)	= fn self => fn child => fn resize => fn shrink =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child =>
(*  24*)				 pack2_ (self, child, 
(*  24*)					 getOpt (resize, true), 
(*  24*)					 getOpt (shrink, true))))
(*  24*)    val pack2' : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn child =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child =>
(*  24*)				 pack2_ (self, child, true, true)))
(*  24*)    val get_position_ : cptr -> int
(*  24*)	= _import "gtk_paned_get_position" : cptr -> int;
(*  24*)    val get_position : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_position_ self)
(*  24*)    val set_position_ : cptr * int -> unit
(*  24*)	= _import "gtk_paned_set_position" : cptr * int -> unit;
(*  24*)    val set_position : 'a t -> int -> unit
(*  24*)	= fn self => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_position_ (self, position))
(*  24*)    val compute_position_ : cptr * int * int * int -> unit
(*  24*)	= _import "gtk_paned_compute_position"
(*  24*)		  : cptr * int * int * int -> unit;
(*  24*)    val compute_position : 'a t -> int -> int -> int -> unit
(*  24*)	= fn self => fn allocation => fn child1_req => fn child2_req =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => compute_position_
(*  24*)					   (self, allocation, 
(*  24*)					    child1_req, child2_req))
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in
(*  24*)      val cycle_child_focus_sig : (bool -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "cycle-child-focus" false
(*  24*)			   (bool --> return_bool) f
(*  24*)      val toggle_handle_focus_sig
(*  24*)	: (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "toggle-handle-focus" false
(*  24*)			   (void --> return_bool) f
(*  24*)      val move_handle_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "move-handle" false (unit --> return_bool) f
(*  24*)      val cycle_handle_focus_sig : (bool -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "cycle-handle-focus" false
(*  24*)			   (bool --> return_bool) f
(*  24*)      val accept_position_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "accept-position" false
(*  24*)			   (void --> return_bool) f
(*  24*)      val cancel_position_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "cancel-position" false
(*  24*)			   (void --> return_bool) f
(*  24*)    end
(*  24*)end
(*  24*)structure VPaned :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a vpaned_t
(*  24*)    type 'a t = 'a vpaned_t Paned.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toVPaned : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a vpaned_t = unit
(*  24*)    type 'a t = 'a vpaned_t Paned.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Paned.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toVPaned obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_vpaned_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr = _import "gtk_vpaned_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)end
(*  24*)structure HPaned :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a hpaned_t
(*  24*)    type 'a t = 'a hpaned_t Paned.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toHPaned : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a hpaned_t = unit
(*  24*)    type 'a t = 'a hpaned_t Paned.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Paned.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toHPaned obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_hpaned_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr = _import "gtk_hpaned_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)end
(*  24*)structure Notebook :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a notebook_t
(*  24*)    type 'a t = 'a notebook_t Container.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toNotebook : 'a t -> base t
(*  24*)    type tab
(*  24*)    val TAB_FIRST : tab
(*  24*)    val TAB_LAST : tab
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val append_page : 'a t -> 'b Widget.t -> 'c Widget.t -> unit
(*  24*)    val append_page_menu
(*  24*)      : 'a t -> 'b Widget.t -> 'c Widget.t -> 'd Widget.t -> unit
(*  24*)    val prepend_page : 'a t -> 'b Widget.t -> 'c Widget.t -> unit
(*  24*)    val prepend_page_menu
(*  24*)      : 'a t -> 'b Widget.t -> 'c Widget.t -> 'd Widget.t -> unit
(*  24*)    val insert_page : 'a t -> 'b Widget.t -> 'c Widget.t -> int -> unit
(*  24*)    val insert_page_menu : 'a t -> 'b Widget.t -> 'c Widget.t 
(*  24*)			-> 'd Widget.t -> int
(*  24*)			   -> unit
(*  24*)    val remove_page : 'a t -> int -> unit
(*  24*)    val get_current_page : 'a t -> int
(*  24*)    val get_nth_page : 'a t -> int -> base Widget.t
(*  24*)    val page_num : 'a t -> 'b Widget.t -> int
(*  24*)    val set_current_page : 'a t -> int -> unit
(*  24*)    val next_page : 'a t -> unit
(*  24*)    val prev_page : 'a t -> unit
(*  24*)    val set_show_border : 'a t -> bool -> unit
(*  24*)    val get_show_border : 'a t -> bool
(*  24*)    val set_show_tabs : 'a t -> bool -> unit
(*  24*)    val get_show_tabs : 'a t -> bool
(*  24*)    val set_tab_pos : 'a t -> positiontype -> unit
(*  24*)    val get_tab_pos : 'a t -> positiontype
(*  24*)    val set_homogeneous_tabs : 'a t -> bool -> unit
(*  24*)    val set_tab_border : 'a t -> int -> unit
(*  24*)    val set_tab_hborder : 'a t -> int -> unit
(*  24*)    val set_tab_vborder : 'a t -> int -> unit
(*  24*)    val set_scrollable : 'a t -> bool -> unit
(*  24*)    val get_scrollable : 'a t -> bool
(*  24*)    val popup_enable : 'a t -> unit
(*  24*)    val popup_disable : 'a t -> unit
(*  24*)    val get_tab_label : 'a t -> 'b Widget.t -> base Widget.t
(*  24*)    val set_tab_label : 'a t -> 'b Widget.t -> 'c Widget.t -> unit
(*  24*)    val set_tab_label_text : 'a t -> 'b Widget.t -> string -> unit
(*  24*)    val get_tab_label_text : 'a t -> 'b Widget.t -> string
(*  24*)    val get_menu_label : 'a t -> 'b Widget.t -> base Widget.t
(*  24*)    val set_menu_label : 'a t -> 'b Widget.t -> 'c Widget.t -> unit
(*  24*)    val set_menu_label_text : 'a t -> 'b Widget.t -> string -> unit
(*  24*)    val get_menu_label_text : 'a t -> 'b Widget.t -> string
(*  24*)    val set_tab_label_packing
(*  24*)      : 'a t -> 'b Widget.t -> bool -> bool -> packtype -> unit
(*  24*)    val reorder_child : 'a t -> 'b Widget.t -> int -> unit
(*  24*)    val current_page : 'a t -> int
(*  24*)    val set_page : 'a t -> int -> unit
(*  24*)    val move_focus_out_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val switch_page_sig : (unit -> int -> unit) -> 'a t Signal.signal
(*  24*)    val focus_tab_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val select_page_sig : (bool -> bool) -> 'a t Signal.signal
(*  24*)    val change_current_page_sig : (int -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a notebook_t = unit
(*  24*)    type 'a t = 'a notebook_t Container.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Container.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toNotebook obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    type tab = int
(*  24*)    val get_tab_ : int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_notebook_tab"
(*  24*)		  : int ref * int ref -> unit;
(*  24*)    val (TAB_FIRST, TAB_LAST)
(*  24*)	= let val (x0, x1) = (ref 0, ref 0) in get_tab_ (x0, x1)
(*  24*)					     ; (!x0, !x1)
(*  24*)					    end
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_notebook_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_notebook_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val append_page_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_notebook_append_page"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val append_page : 'a t -> 'b Widget.t -> 'c Widget.t -> unit
(*  24*)	= fn self => fn child => fn tab_label =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => GObject.withPtr
(*  24*)					    (tab_label, 
(*  24*)					     fn tab_label =>
(*  24*)						append_page_
(*  24*)						  (self, child, 
(*  24*)						   tab_label))))
(*  24*)    val append_page_menu_ : cptr * cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_notebook_append_page_menu"
(*  24*)		  : cptr * cptr * cptr * cptr -> unit;
(*  24*)    val append_page_menu
(*  24*)      : 'a t -> 'b Widget.t -> 'c Widget.t -> 'd Widget.t -> unit
(*  24*)	= fn self => fn child => fn tab_label => fn menu_label =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (child, 
(*  24*)		      fn child =>
(*  24*)			 GObject.withPtr
(*  24*)			   (tab_label, 
(*  24*)			    fn tab_label =>
(*  24*)			       GObject.withPtr
(*  24*)				 (menu_label, 
(*  24*)				  fn menu_label =>
(*  24*)				     append_page_menu_
(*  24*)				       (self, child, tab_label, 
(*  24*)					menu_label)))))
(*  24*)    val prepend_page_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_notebook_prepend_page"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val prepend_page : 'a t -> 'b Widget.t -> 'c Widget.t -> unit
(*  24*)	= fn self => fn child => fn tab_label =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => GObject.withPtr
(*  24*)					    (tab_label, 
(*  24*)					     fn tab_label =>
(*  24*)						prepend_page_
(*  24*)						  (self, child, 
(*  24*)						   tab_label))))
(*  24*)    val prepend_page_menu_ : cptr * cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_notebook_prepend_page_menu"
(*  24*)		  : cptr * cptr * cptr * cptr -> unit;
(*  24*)    val prepend_page_menu
(*  24*)      : 'a t -> 'b Widget.t -> 'c Widget.t -> 'd Widget.t -> unit
(*  24*)	= fn self => fn child => fn tab_label => fn menu_label =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child =>
(*  24*)				 GObject.withPtr
(*  24*)				   (tab_label, 
(*  24*)				    fn tab_label =>
(*  24*)				       GObject.withPtr
(*  24*)					 (menu_label, 
(*  24*)					  fn menu_label =>
(*  24*)					     prepend_page_menu_
(*  24*)					       (self, child, 
(*  24*)						tab_label, 
(*  24*)						menu_label)))))
(*  24*)    val insert_page_ : cptr * cptr * cptr * int -> unit
(*  24*)	= _import "gtk_notebook_insert_page"
(*  24*)		  : cptr * cptr * cptr * int -> unit;
(*  24*)    val insert_page : 'a t -> 'b Widget.t -> 'c Widget.t -> int -> unit
(*  24*)	= fn self => fn child => fn tab_label => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => GObject.withPtr
(*  24*)					    (tab_label, 
(*  24*)					     fn tab_label =>
(*  24*)						insert_page_
(*  24*)						  (self, child, 
(*  24*)						   tab_label, 
(*  24*)						   position))))
(*  24*)    val insert_page_menu_ : cptr * cptr * cptr * cptr * int -> unit
(*  24*)	= _import "gtk_notebook_insert_page_menu"
(*  24*)		  : cptr * cptr * cptr * cptr * int -> unit;
(*  24*)    val insert_page_menu : 'a t -> 'b Widget.t -> 'c Widget.t 
(*  24*)			-> 'd Widget.t -> int
(*  24*)			   -> unit
(*  24*)	= fn self => fn child => fn tab_label => fn menu_label => 
(*  24*)	  fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (child, 
(*  24*)		      fn child =>
(*  24*)			 GObject.withPtr
(*  24*)			   (tab_label, 
(*  24*)			    fn tab_label =>
(*  24*)			       GObject.withPtr
(*  24*)				 (menu_label, 
(*  24*)				  fn menu_label =>
(*  24*)				     insert_page_menu_
(*  24*)				       (self, child, tab_label, 
(*  24*)					menu_label, position)))))
(*  24*)    val remove_page_ : cptr * int -> unit
(*  24*)	= _import "gtk_notebook_remove_page" : cptr * int -> unit;
(*  24*)    val remove_page : 'a t -> int -> unit
(*  24*)	= fn self => fn page_num =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => remove_page_ (self, page_num))
(*  24*)    val get_current_page_ : cptr -> int
(*  24*)	= _import "gtk_notebook_get_current_page" : cptr -> int;
(*  24*)    val get_current_page : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_current_page_ self)
(*  24*)    val get_nth_page_ : cptr * int -> cptr
(*  24*)	= _import "gtk_notebook_get_nth_page" : cptr * int -> cptr;
(*  24*)    val get_nth_page : 'a t -> int -> base Widget.t
(*  24*)	= fn self => fn page_num =>
(*  24*)	     Widget.inherit
(*  24*)	       ()
(*  24*)	       (fn () =>
(*  24*)		   GObject.withPtr
(*  24*)		     (self, fn self => get_nth_page_ (self, page_num)))
(*  24*)    val page_num_ : cptr * cptr -> int
(*  24*)	= _import "gtk_notebook_page_num" : cptr * cptr -> int;
(*  24*)    val page_num : 'a t -> 'b Widget.t -> int
(*  24*)	= fn self => fn child =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (child, fn child => page_num_ (self, child)))
(*  24*)    val set_current_page_ : cptr * int -> unit
(*  24*)	= _import "gtk_notebook_set_current_page" : cptr * int -> unit;
(*  24*)    val set_current_page : 'a t -> int -> unit
(*  24*)	= fn self => fn page_num =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_current_page_ (self, page_num))
(*  24*)    val next_page_ : cptr -> unit
(*  24*)	= _import "gtk_notebook_next_page" : cptr -> unit;
(*  24*)    val next_page : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => next_page_ self)
(*  24*)    val prev_page_ : cptr -> unit
(*  24*)	= _import "gtk_notebook_prev_page" : cptr -> unit;
(*  24*)    val prev_page : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => prev_page_ self)
(*  24*)    val set_show_border_ : cptr * bool -> unit
(*  24*)	= _import "gtk_notebook_set_show_border" : cptr * bool -> unit;
(*  24*)    val set_show_border : 'a t -> bool -> unit
(*  24*)	= fn self => fn show_border =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_show_border_ (self, show_border))
(*  24*)    val get_show_border_ : cptr -> bool
(*  24*)	= _import "gtk_notebook_get_show_border" : cptr -> bool;
(*  24*)    val get_show_border : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_show_border_ self)
(*  24*)    val set_show_tabs_ : cptr * bool -> unit
(*  24*)	= _import "gtk_notebook_set_show_tabs" : cptr * bool -> unit;
(*  24*)    val set_show_tabs : 'a t -> bool -> unit
(*  24*)	= fn self => fn show_tabs =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_show_tabs_ (self, show_tabs))
(*  24*)    val get_show_tabs_ : cptr -> bool
(*  24*)	= _import "gtk_notebook_get_show_tabs" : cptr -> bool;
(*  24*)    val get_show_tabs : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_show_tabs_ self)
(*  24*)    val set_tab_pos_ : cptr * int -> unit
(*  24*)	= _import "gtk_notebook_set_tab_pos" : cptr * int -> unit;
(*  24*)    val set_tab_pos : 'a t -> positiontype -> unit
(*  24*)	= fn self => fn pos =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_tab_pos_ (self, pos))
(*  24*)    val get_tab_pos_ : cptr -> int
(*  24*)	= _import "gtk_notebook_get_tab_pos" : cptr -> int;
(*  24*)    val get_tab_pos : 'a t -> positiontype
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_tab_pos_ self)
(*  24*)    val set_homogeneous_tabs_ : cptr * bool -> unit
(*  24*)	= _import "gtk_notebook_set_homogeneous_tabs"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_homogeneous_tabs : 'a t -> bool -> unit
(*  24*)	= fn self => fn homogeneous =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_homogeneous_tabs_
(*  24*)					   (self, homogeneous))
(*  24*)    val set_tab_border_ : cptr * int -> unit
(*  24*)	= _import "gtk_notebook_set_tab_border" : cptr * int -> unit;
(*  24*)    val set_tab_border : 'a t -> int -> unit
(*  24*)	= fn self => fn border_width =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_tab_border_ (self, border_width))
(*  24*)    val set_tab_hborder_ : cptr * int -> unit
(*  24*)	= _import "gtk_notebook_set_tab_hborder" : cptr * int -> unit;
(*  24*)    val set_tab_hborder : 'a t -> int -> unit
(*  24*)	= fn self => fn tab_hborder =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_tab_hborder_ (self, tab_hborder))
(*  24*)    val set_tab_vborder_ : cptr * int -> unit
(*  24*)	= _import "gtk_notebook_set_tab_vborder" : cptr * int -> unit;
(*  24*)    val set_tab_vborder : 'a t -> int -> unit
(*  24*)	= fn self => fn tab_vborder =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_tab_vborder_ (self, tab_vborder))
(*  24*)    val set_scrollable_ : cptr * bool -> unit
(*  24*)	= _import "gtk_notebook_set_scrollable" : cptr * bool -> unit;
(*  24*)    val set_scrollable : 'a t -> bool -> unit
(*  24*)	= fn self => fn scrollable =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_scrollable_ (self, scrollable))
(*  24*)    val get_scrollable_ : cptr -> bool
(*  24*)	= _import "gtk_notebook_get_scrollable" : cptr -> bool;
(*  24*)    val get_scrollable : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_scrollable_ self)
(*  24*)    val popup_enable_ : cptr -> unit
(*  24*)	= _import "gtk_notebook_popup_enable" : cptr -> unit;
(*  24*)    val popup_enable : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => popup_enable_ self)
(*  24*)    val popup_disable_ : cptr -> unit
(*  24*)	= _import "gtk_notebook_popup_disable" : cptr -> unit;
(*  24*)    val popup_disable : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => popup_disable_ self)
(*  24*)    val get_tab_label_ : cptr * cptr -> cptr
(*  24*)	= _import "gtk_notebook_get_tab_label" : cptr * cptr -> cptr;
(*  24*)    val get_tab_label : 'a t -> 'b Widget.t -> base Widget.t
(*  24*)	= fn self => fn child =>
(*  24*)	     Widget.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, 
(*  24*)			    fn self => GObject.withPtr
(*  24*)					 (child, 
(*  24*)					  fn child =>
(*  24*)					     get_tab_label_
(*  24*)					       (self, child))))
(*  24*)    val set_tab_label_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_notebook_set_tab_label"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val set_tab_label : 'a t -> 'b Widget.t -> 'c Widget.t -> unit
(*  24*)	= fn self => fn child => fn tab_label =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => GObject.withPtr
(*  24*)					    (tab_label, 
(*  24*)					     fn tab_label =>
(*  24*)						set_tab_label_
(*  24*)						  (self, child, 
(*  24*)						   tab_label))))
(*  24*)    val set_tab_label_text_ : cptr * cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_notebook_set_tab_label_text"
(*  24*)		  : cptr * cptr * CString.cstring -> unit;
(*  24*)    val set_tab_label_text : 'a t -> 'b Widget.t -> string -> unit
(*  24*)	= fn self => fn child => fn tab_text =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => set_tab_label_text_
(*  24*)					    (self, child, 
(*  24*)					     CString.fromString
(*  24*)					       tab_text)))
(*  24*)    val get_tab_label_text_ : cptr * cptr -> CString.t
(*  24*)	= _import "gtk_notebook_get_tab_label_text"
(*  24*)		  : cptr * cptr -> CString.t;
(*  24*)    val get_tab_label_text : 'a t -> 'b Widget.t -> string
(*  24*)	= fn self => fn child =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child =>
(*  24*)				 let val t = get_tab_label_text_
(*  24*)					       (self, child)
(*  24*)				 in CString.toString t end))
(*  24*)    val get_menu_label_ : cptr * cptr -> cptr
(*  24*)	= _import "gtk_notebook_get_menu_label" : cptr * cptr -> cptr;
(*  24*)    val get_menu_label : 'a t -> 'b Widget.t -> base Widget.t
(*  24*)	= fn self => fn child =>
(*  24*)	     Widget.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, 
(*  24*)			    fn self => GObject.withPtr
(*  24*)					 (child, 
(*  24*)					  fn child =>
(*  24*)					     get_menu_label_
(*  24*)					       (self, child))))
(*  24*)    val set_menu_label_ : cptr * cptr * cptr -> unit
(*  24*)	= _import "gtk_notebook_set_menu_label"
(*  24*)		  : cptr * cptr * cptr -> unit;
(*  24*)    val set_menu_label : 'a t -> 'b Widget.t -> 'c Widget.t -> unit
(*  24*)	= fn self => fn child => fn menu_label =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => GObject.withPtr
(*  24*)					    (menu_label, 
(*  24*)					     fn menu_label =>
(*  24*)						set_menu_label_
(*  24*)						  (self, child, 
(*  24*)						   menu_label))))
(*  24*)    val set_menu_label_text_ : cptr * cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_notebook_set_menu_label_text"
(*  24*)		  : cptr * cptr * CString.cstring -> unit;
(*  24*)    val set_menu_label_text : 'a t -> 'b Widget.t -> string -> unit
(*  24*)	= fn self => fn child => fn menu_text =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => set_menu_label_text_
(*  24*)					    (self, child, 
(*  24*)					     CString.fromString
(*  24*)					       menu_text)))
(*  24*)    val get_menu_label_text_ : cptr * cptr -> CString.t
(*  24*)	= _import "gtk_notebook_get_menu_label_text"
(*  24*)		  : cptr * cptr -> CString.t;
(*  24*)    val get_menu_label_text : 'a t -> 'b Widget.t -> string
(*  24*)	= fn self => fn child =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child =>
(*  24*)				 let val t = get_menu_label_text_
(*  24*)					       (self, child)
(*  24*)				 in CString.toString t end))
(*  24*)    val set_tab_label_packing_
(*  24*)      : cptr * cptr * bool * bool * int -> unit
(*  24*)	= _import "gtk_notebook_set_tab_label_packing"
(*  24*)		  : cptr * cptr * bool * bool * int -> unit;
(*  24*)    val set_tab_label_packing
(*  24*)      : 'a t -> 'b Widget.t -> bool -> bool -> packtype -> unit
(*  24*)	= fn self => fn child => fn expand => fn fill => 
(*  24*)	  fn pack_type =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => set_tab_label_packing_
(*  24*)					    (self, child, expand, 
(*  24*)					     fill, pack_type)))
(*  24*)    val reorder_child_ : cptr * cptr * int -> unit
(*  24*)	= _import "gtk_notebook_reorder_child"
(*  24*)		  : cptr * cptr * int -> unit;
(*  24*)    val reorder_child : 'a t -> 'b Widget.t -> int -> unit
(*  24*)	= fn self => fn child => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => reorder_child_
(*  24*)					    (self, child, position)))
(*  24*)    val current_page_ : cptr -> int
(*  24*)	= _import "gtk_notebook_current_page" : cptr -> int;
(*  24*)    val current_page : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => current_page_ self)
(*  24*)    val set_page_ : cptr * int -> unit
(*  24*)	= _import "gtk_notebook_set_page" : cptr * int -> unit;
(*  24*)    val set_page : 'a t -> int -> unit
(*  24*)	= fn self => fn page_num =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_page_ (self, page_num))
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in
(*  24*)      val move_focus_out_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "move-focus-out" false
(*  24*)			   (unit --> return_void) f
(*  24*)      val switch_page_sig : (unit -> int -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "switch-page" false
(*  24*)			   (unit --> int --> return_void) f
(*  24*)      val focus_tab_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "focus-tab" false (unit --> return_bool) f
(*  24*)      val select_page_sig : (bool -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "select-page" false (bool --> return_bool) f
(*  24*)      val change_current_page_sig : (int -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "change-current-page" false
(*  24*)			   (int --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure MenuShell :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a menushell_t
(*  24*)    type 'a t = 'a menushell_t Container.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toMenuShell : 'a t -> base t
(*  24*)    val append : 'a t -> 'b Widget.t -> unit
(*  24*)    val prepend : 'a t -> 'b Widget.t -> unit
(*  24*)    val insert : 'a t -> 'b Widget.t -> int -> unit
(*  24*)    val deactivate : 'a t -> unit
(*  24*)    val select_item : 'a t -> 'b Widget.t -> unit
(*  24*)    val deselect : 'a t -> unit
(*  24*)    val activate_item : 'a t -> 'b Widget.t -> bool -> unit
(*  24*)    val deactivate_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val selection_done_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val move_current_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val activate_current_sig : (bool -> unit) -> 'a t Signal.signal
(*  24*)    val cancel_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val cycle_focus_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a menushell_t = unit
(*  24*)    type 'a t = 'a menushell_t Container.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Container.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toMenuShell obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val append_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_menu_shell_append" : cptr * cptr -> unit;
(*  24*)    val append : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn child =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (child, fn child => append_ (self, child)))
(*  24*)    val prepend_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_menu_shell_prepend" : cptr * cptr -> unit;
(*  24*)    val prepend : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn child =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (child, fn child => prepend_ (self, child)))
(*  24*)    val insert_ : cptr * cptr * int -> unit
(*  24*)	= _import "gtk_menu_shell_insert" : cptr * cptr * int -> unit;
(*  24*)    val insert : 'a t -> 'b Widget.t -> int -> unit
(*  24*)	= fn self => fn child => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => insert_ (self, child, 
(*  24*)						   position)))
(*  24*)    val deactivate_ : cptr -> unit
(*  24*)	= _import "gtk_menu_shell_deactivate" : cptr -> unit;
(*  24*)    val deactivate : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => deactivate_ self)
(*  24*)    val select_item_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_menu_shell_select_item" : cptr * cptr -> unit;
(*  24*)    val select_item : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn menu_item =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (menu_item, 
(*  24*)			      fn menu_item =>
(*  24*)				 select_item_ (self, menu_item)))
(*  24*)    val deselect_ : cptr -> unit
(*  24*)	= _import "gtk_menu_shell_deselect" : cptr -> unit;
(*  24*)    val deselect : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => deselect_ self)
(*  24*)    val activate_item_ : cptr * cptr * bool -> unit
(*  24*)	= _import "gtk_menu_shell_activate_item"
(*  24*)		  : cptr * cptr * bool -> unit;
(*  24*)    val activate_item : 'a t -> 'b Widget.t -> bool -> unit
(*  24*)	= fn self => fn menu_item => fn force_deactivate =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (menu_item, 
(*  24*)			      fn menu_item =>
(*  24*)				 activate_item_ (self, menu_item, 
(*  24*)						 force_deactivate)))
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in
(*  24*)      val deactivate_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "deactivate" false (void --> return_void) f
(*  24*)      val selection_done_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "selection-done" false
(*  24*)			   (void --> return_void) f
(*  24*)      val move_current_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f =>
(*  24*)	       signal "move-current" false (unit --> return_void) f
(*  24*)      val activate_current_sig : (bool -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "activate-current" false
(*  24*)			   (bool --> return_void) f
(*  24*)      val cancel_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "cancel" false (void --> return_void) f
(*  24*)      val cycle_focus_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "cycle-focus" false (unit --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Menu :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a menu_t
(*  24*)    type 'a t = 'a menu_t MenuShell.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toMenu : 'a t -> base t
(*  24*)    type directiontype
(*  24*)    val DIR_PARENT : directiontype
(*  24*)    val DIR_CHILD : directiontype
(*  24*)    val DIR_NEXT : directiontype
(*  24*)    val DIR_PREV : directiontype
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val reposition : 'a t -> unit
(*  24*)    val popdown : 'a t -> unit
(*  24*)    val get_active : 'a t -> base Widget.t
(*  24*)    val set_active : 'a t -> int -> unit
(*  24*)    val set_accelgroup : 'a t -> 'b AccelGroup.t -> unit
(*  24*)    val get_accelgroup : 'a t -> base AccelGroup.t
(*  24*)    val set_accel_path : 'a t -> string -> unit
(*  24*)    val detach : 'a t -> unit
(*  24*)    val get_attach_widget : 'a t -> base Widget.t
(*  24*)    val set_tearoff_state : 'a t -> bool -> unit
(*  24*)    val get_tearoff_state : 'a t -> bool
(*  24*)    val set_title : 'a t -> string -> unit
(*  24*)    val get_title : 'a t -> string
(*  24*)    val reorder_child : 'a t -> 'b Widget.t -> int -> unit
(*  24*)    val bar_get_type : unit -> int
(*  24*)    val item_get_type : unit -> int
(*  24*)    val shell_get_type : unit -> int
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a menu_t = unit
(*  24*)    type 'a t = 'a menu_t MenuShell.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in MenuShell.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toMenu obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    type directiontype = int
(*  24*)    val get_directiontype_
(*  24*)      : int ref * int ref * int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_menu_directiontype"
(*  24*)		  : int ref * int ref * int ref * int ref -> unit;
(*  24*)    val (DIR_PARENT, DIR_CHILD, DIR_NEXT, DIR_PREV)
(*  24*)	= let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)	  in get_directiontype_ (x0, x1, x2, x3)
(*  24*)	   ; (!x0, !x1, !x2, !x3)
(*  24*)	  end
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_menu_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr = _import "gtk_menu_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val reposition_ : cptr -> unit
(*  24*)	= _import "gtk_menu_reposition" : cptr -> unit;
(*  24*)    val reposition : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => reposition_ self)
(*  24*)    val popdown_ : cptr -> unit
(*  24*)	= _import "gtk_menu_popdown" : cptr -> unit;
(*  24*)    val popdown : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => popdown_ self)
(*  24*)    val get_active_ : cptr -> cptr
(*  24*)	= _import "gtk_menu_get_active" : cptr -> cptr;
(*  24*)    val get_active : 'a t -> base Widget.t
(*  24*)	= fn self => Widget.inherit
(*  24*)		       ()
(*  24*)		       (fn () => GObject.withPtr
(*  24*)				   (self, fn self => get_active_ self))
(*  24*)    val set_active_ : cptr * int -> unit
(*  24*)	= _import "gtk_menu_set_active" : cptr * int -> unit;
(*  24*)    val set_active : 'a t -> int -> unit
(*  24*)	= fn self => fn index =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_active_ (self, index))
(*  24*)    val set_accelgroup_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_menu_set_accel_group" : cptr * cptr -> unit;
(*  24*)    val set_accelgroup : 'a t -> 'b AccelGroup.t -> unit
(*  24*)	= fn self => fn accel_group =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (accel_group, 
(*  24*)			      fn accel_group =>
(*  24*)				 set_accelgroup_ (self, accel_group)))
(*  24*)    val get_accelgroup_ : cptr -> cptr
(*  24*)	= _import "gtk_menu_get_accel_group" : cptr -> cptr;
(*  24*)    val get_accelgroup : 'a t -> base AccelGroup.t
(*  24*)	= fn self =>
(*  24*)	     AccelGroup.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, fn self => get_accelgroup_ self))
(*  24*)    val set_accel_path_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_menu_set_accel_path"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_accel_path : 'a t -> string -> unit
(*  24*)	= fn self => fn accel_path =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_accel_path_
(*  24*)			     (self, CString.fromString accel_path))
(*  24*)    val detach_ : cptr -> unit
(*  24*)	= _import "gtk_menu_detach" : cptr -> unit;
(*  24*)    val detach : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => detach_ self)
(*  24*)    val get_attach_widget_ : cptr -> cptr
(*  24*)	= _import "gtk_menu_get_attach_widget" : cptr -> cptr;
(*  24*)    val get_attach_widget : 'a t -> base Widget.t
(*  24*)	= fn self =>
(*  24*)	     Widget.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, fn self => get_attach_widget_ self))
(*  24*)    val set_tearoff_state_ : cptr * bool -> unit
(*  24*)	= _import "gtk_menu_set_tearoff_state" : cptr * bool -> unit;
(*  24*)    val set_tearoff_state : 'a t -> bool -> unit
(*  24*)	= fn self => fn torn_off =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_tearoff_state_ (self, torn_off))
(*  24*)    val get_tearoff_state_ : cptr -> bool
(*  24*)	= _import "gtk_menu_get_tearoff_state" : cptr -> bool;
(*  24*)    val get_tearoff_state : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_tearoff_state_ self)
(*  24*)    val set_title_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_menu_set_title"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_title : 'a t -> string -> unit
(*  24*)	= fn self => fn title =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_title_ (self, CString.fromString title))
(*  24*)    val get_title_ : cptr -> CString.t
(*  24*)	= _import "gtk_menu_get_title" : cptr -> CString.t;
(*  24*)    val get_title : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_title_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val reorder_child_ : cptr * cptr * int -> unit
(*  24*)	= _import "gtk_menu_reorder_child" : cptr * cptr * int -> unit;
(*  24*)    val reorder_child : 'a t -> 'b Widget.t -> int -> unit
(*  24*)	= fn self => fn child => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => reorder_child_
(*  24*)					    (self, child, position)))
(*  24*)    val bar_get_type_ : unit -> int
(*  24*)	= _import "gtk_menu_bar_get_type" : unit -> int;
(*  24*)    val bar_get_type : unit -> int = fn dummy => bar_get_type_ dummy
(*  24*)    val item_get_type_ : unit -> int
(*  24*)	= _import "gtk_menu_item_get_type" : unit -> int;
(*  24*)    val item_get_type : unit -> int = fn dummy => item_get_type_ dummy
(*  24*)    val shell_get_type_ : unit -> int
(*  24*)	= _import "gtk_menu_shell_get_type" : unit -> int;
(*  24*)    val shell_get_type : unit -> int
(*  24*)	= fn dummy => shell_get_type_ dummy
(*  24*)end
(*  24*)structure MenuBar :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a menubar_t
(*  24*)    type 'a t = 'a menubar_t MenuShell.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toMenuBar : 'a t -> base t
(*  24*)    val new : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a menubar_t = unit
(*  24*)    type 'a t = 'a menubar_t MenuShell.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in MenuShell.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toMenuBar obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_menu_bar_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)end
(*  24*)structure List :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a list_t
(*  24*)    type 'a t = 'a list_t Container.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toList : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val clear_items : 'a t -> int -> int -> unit
(*  24*)    val select_item : 'a t -> int -> unit
(*  24*)    val unselect_item : 'a t -> int -> unit
(*  24*)    val select_child : 'a t -> 'b Widget.t -> unit
(*  24*)    val unselect_child : 'a t -> 'b Widget.t -> unit
(*  24*)    val child_position : 'a t -> 'b Widget.t -> int
(*  24*)    val set_selection_mode : 'a t -> selection_mode -> unit
(*  24*)    val extend_selection : 'a t -> scrolltype -> real -> bool -> unit
(*  24*)    val start_selection : 'a t -> unit
(*  24*)    val end_selection : 'a t -> unit
(*  24*)    val select_all : 'a t -> unit
(*  24*)    val unselect_all : 'a t -> unit
(*  24*)    val scroll_horizontal : 'a t -> scrolltype -> real -> unit
(*  24*)    val scroll_vertical : 'a t -> scrolltype -> real -> unit
(*  24*)    val toggle_add_mode : 'a t -> unit
(*  24*)    val toggle_focus_row : 'a t -> unit
(*  24*)    val toggle_row : 'a t -> 'b Widget.t -> unit
(*  24*)    val undo_selection : 'a t -> unit
(*  24*)    val end_drag_selection : 'a t -> unit
(*  24*)    val item_get_type : unit -> int
(*  24*)    val store_get_type : unit -> int
(*  24*)    val selection_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val select_child_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val unselect_child_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a list_t = unit
(*  24*)    type 'a t = 'a list_t Container.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Container.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toList obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_list_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr = _import "gtk_list_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val clear_items_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_list_clear_items" : cptr * int * int -> unit;
(*  24*)    val clear_items : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn start => fn en =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => clear_items_ (self, start, en))
(*  24*)    val select_item_ : cptr * int -> unit
(*  24*)	= _import "gtk_list_select_item" : cptr * int -> unit;
(*  24*)    val select_item : 'a t -> int -> unit
(*  24*)	= fn self => fn item =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => select_item_ (self, item))
(*  24*)    val unselect_item_ : cptr * int -> unit
(*  24*)	= _import "gtk_list_unselect_item" : cptr * int -> unit;
(*  24*)    val unselect_item : 'a t -> int -> unit
(*  24*)	= fn self => fn item =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => unselect_item_ (self, item))
(*  24*)    val select_child_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_list_select_child" : cptr * cptr -> unit;
(*  24*)    val select_child : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn child =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (child, fn child => select_child_ (self, child)))
(*  24*)    val unselect_child_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_list_unselect_child" : cptr * cptr -> unit;
(*  24*)    val unselect_child : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn child =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => unselect_child_
(*  24*)					    (self, child)))
(*  24*)    val child_position_ : cptr * cptr -> int
(*  24*)	= _import "gtk_list_child_position" : cptr * cptr -> int;
(*  24*)    val child_position : 'a t -> 'b Widget.t -> int
(*  24*)	= fn self => fn child =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => child_position_
(*  24*)					    (self, child)))
(*  24*)    val set_selection_mode_ : cptr * int -> unit
(*  24*)	= _import "gtk_list_set_selection_mode" : cptr * int -> unit;
(*  24*)    val set_selection_mode : 'a t -> selection_mode -> unit
(*  24*)	= fn self => fn mode =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_selection_mode_ (self, mode))
(*  24*)    val extend_selection_ : cptr * int * real * bool -> unit
(*  24*)	= _import "gtk_list_extend_selection"
(*  24*)		  : cptr * int * real * bool -> unit;
(*  24*)    val extend_selection : 'a t -> scrolltype -> real -> bool -> unit
(*  24*)	= fn self => fn scroll_type => fn position => 
(*  24*)	  fn auto_start_selection =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => extend_selection_
(*  24*)			     (self, scroll_type, position, 
(*  24*)			      auto_start_selection))
(*  24*)    val start_selection_ : cptr -> unit
(*  24*)	= _import "gtk_list_start_selection" : cptr -> unit;
(*  24*)    val start_selection : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => start_selection_ self)
(*  24*)    val end_selection_ : cptr -> unit
(*  24*)	= _import "gtk_list_end_selection" : cptr -> unit;
(*  24*)    val end_selection : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => end_selection_ self)
(*  24*)    val select_all_ : cptr -> unit
(*  24*)	= _import "gtk_list_select_all" : cptr -> unit;
(*  24*)    val select_all : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => select_all_ self)
(*  24*)    val unselect_all_ : cptr -> unit
(*  24*)	= _import "gtk_list_unselect_all" : cptr -> unit;
(*  24*)    val unselect_all : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => unselect_all_ self)
(*  24*)    val scroll_horizontal_ : cptr * int * real -> unit
(*  24*)	= _import "gtk_list_scroll_horizontal"
(*  24*)		  : cptr * int * real -> unit;
(*  24*)    val scroll_horizontal : 'a t -> scrolltype -> real -> unit
(*  24*)	= fn self => fn scroll_type => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => scroll_horizontal_
(*  24*)			     (self, scroll_type, position))
(*  24*)    val scroll_vertical_ : cptr * int * real -> unit
(*  24*)	= _import "gtk_list_scroll_vertical"
(*  24*)		  : cptr * int * real -> unit;
(*  24*)    val scroll_vertical : 'a t -> scrolltype -> real -> unit
(*  24*)	= fn self => fn scroll_type => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => scroll_vertical_
(*  24*)			     (self, scroll_type, position))
(*  24*)    val toggle_add_mode_ : cptr -> unit
(*  24*)	= _import "gtk_list_toggle_add_mode" : cptr -> unit;
(*  24*)    val toggle_add_mode : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => toggle_add_mode_ self)
(*  24*)    val toggle_focus_row_ : cptr -> unit
(*  24*)	= _import "gtk_list_toggle_focus_row" : cptr -> unit;
(*  24*)    val toggle_focus_row : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => toggle_focus_row_ self)
(*  24*)    val toggle_row_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_list_toggle_row" : cptr * cptr -> unit;
(*  24*)    val toggle_row : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn item =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (item, fn item => toggle_row_ (self, item)))
(*  24*)    val undo_selection_ : cptr -> unit
(*  24*)	= _import "gtk_list_undo_selection" : cptr -> unit;
(*  24*)    val undo_selection : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => undo_selection_ self)
(*  24*)    val end_drag_selection_ : cptr -> unit
(*  24*)	= _import "gtk_list_end_drag_selection" : cptr -> unit;
(*  24*)    val end_drag_selection : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => end_drag_selection_ self)
(*  24*)    val item_get_type_ : unit -> int
(*  24*)	= _import "gtk_list_item_get_type" : unit -> int;
(*  24*)    val item_get_type : unit -> int = fn dummy => item_get_type_ dummy
(*  24*)    val store_get_type_ : unit -> int
(*  24*)	= _import "gtk_list_store_get_type" : unit -> int;
(*  24*)    val store_get_type : unit -> int
(*  24*)	= fn dummy => store_get_type_ dummy
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val selection_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "selection-changed" false
(*  24*)			    (void --> return_void) f
(*  24*)       val select_child_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "select-child" false (unit --> return_void) f
(*  24*)       val unselect_child_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "unselect-child" false
(*  24*)			    (unit --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Layout :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a layout_t
(*  24*)    type 'a t = 'a layout_t Container.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toLayout : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : 'a Adjustment.t option -> 'b Adjustment.t option
(*  24*)	      -> base t
(*  24*)    val new' : unit -> base t
(*  24*)    val put : 'a t -> 'b Widget.t -> int -> int -> unit
(*  24*)    val move : 'a t -> 'b Widget.t -> int -> int -> unit
(*  24*)    val set_size : 'a t -> int -> int -> unit
(*  24*)    val set_hadjustment : 'a t -> 'b Adjustment.t option -> unit
(*  24*)    val set_hadjustment' : 'a t -> unit
(*  24*)    val set_vadjustment : 'a t -> 'b Adjustment.t option -> unit
(*  24*)    val set_vadjustment' : 'a t -> unit
(*  24*)    val freeze : 'a t -> unit
(*  24*)    val thaw : 'a t -> unit
(*  24*)    val set_scroll_adjustments_sig
(*  24*)      : (unit -> unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a layout_t = unit
(*  24*)    type 'a t = 'a layout_t Container.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Container.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toLayout obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_layout_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : cptr * cptr -> cptr
(*  24*)	= _import "gtk_layout_new" : cptr * cptr -> cptr;
(*  24*)    val new : 'a Adjustment.t option -> 'b Adjustment.t option
(*  24*)	      -> base t
(*  24*)	= fn hadjustment => fn vadjustment =>
(*  24*)	     make (GObject.withOpt
(*  24*)		     (hadjustment, 
(*  24*)		      fn hadjustment =>
(*  24*)			 GObject.withOpt
(*  24*)			   (vadjustment, 
(*  24*)			    fn vadjustment =>
(*  24*)			       new_ (hadjustment, vadjustment))))
(*  24*)    val new' : unit -> base t
(*  24*)	= fn dummy => make (new_ (GObject.null, GObject.null))
(*  24*)    val put_ : cptr * cptr * int * int -> unit
(*  24*)	= _import "gtk_layout_put" : cptr * cptr * int * int -> unit;
(*  24*)    val put : 'a t -> 'b Widget.t -> int -> int -> unit
(*  24*)	= fn self => fn child_widget => fn x => fn y =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child_widget, 
(*  24*)			      fn child_widget =>
(*  24*)				 put_ (self, child_widget, x, y)))
(*  24*)    val move_ : cptr * cptr * int * int -> unit
(*  24*)	= _import "gtk_layout_move" : cptr * cptr * int * int -> unit;
(*  24*)    val move : 'a t -> 'b Widget.t -> int -> int -> unit
(*  24*)	= fn self => fn child_widget => fn x => fn y =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child_widget, 
(*  24*)			      fn child_widget =>
(*  24*)				 move_ (self, child_widget, x, y)))
(*  24*)    val set_size_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_layout_set_size" : cptr * int * int -> unit;
(*  24*)    val set_size : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn width => fn height =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_size_ (self, width, height))
(*  24*)    val set_hadjustment_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_layout_set_hadjustment" : cptr * cptr -> unit;
(*  24*)    val set_hadjustment : 'a t -> 'b Adjustment.t option -> unit
(*  24*)	= fn self => fn adjustment =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withOpt
(*  24*)			     (adjustment, 
(*  24*)			      fn adjustment => set_hadjustment_
(*  24*)						 (self, adjustment)))
(*  24*)    val set_hadjustment' : 'a t -> unit
(*  24*)	= fn self =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_hadjustment_ (self, GObject.null))
(*  24*)    val set_vadjustment_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_layout_set_vadjustment" : cptr * cptr -> unit;
(*  24*)    val set_vadjustment : 'a t -> 'b Adjustment.t option -> unit
(*  24*)	= fn self => fn adjustment =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withOpt
(*  24*)			     (adjustment, 
(*  24*)			      fn adjustment => set_vadjustment_
(*  24*)						 (self, adjustment)))
(*  24*)    val set_vadjustment' : 'a t -> unit
(*  24*)	= fn self =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_vadjustment_ (self, GObject.null))
(*  24*)    val freeze_ : cptr -> unit
(*  24*)	= _import "gtk_layout_freeze" : cptr -> unit;
(*  24*)    val freeze : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => freeze_ self)
(*  24*)    val thaw_ : cptr -> unit
(*  24*)	= _import "gtk_layout_thaw" : cptr -> unit;
(*  24*)    val thaw : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => thaw_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val set_scroll_adjustments_sig
(*  24*)	 : (unit -> unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "set-scroll-adjustments" false
(*  24*)			    (unit --> unit --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Fixed :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a fixed_t
(*  24*)    type 'a t = 'a fixed_t Container.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toFixed : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val put : 'a t -> 'b Widget.t -> int -> int -> unit
(*  24*)    val move : 'a t -> 'b Widget.t -> int -> int -> unit
(*  24*)    val set_has_window : 'a t -> bool -> unit
(*  24*)    val get_has_window : 'a t -> bool
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a fixed_t = unit
(*  24*)    type 'a t = 'a fixed_t Container.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Container.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toFixed obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_fixed_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr = _import "gtk_fixed_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val put_ : cptr * cptr * int * int -> unit
(*  24*)	= _import "gtk_fixed_put" : cptr * cptr * int * int -> unit;
(*  24*)    val put : 'a t -> 'b Widget.t -> int -> int -> unit
(*  24*)	= fn self => fn widget => fn x => fn y =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (widget, fn widget => put_ (self, widget, x, y)))
(*  24*)    val move_ : cptr * cptr * int * int -> unit
(*  24*)	= _import "gtk_fixed_move" : cptr * cptr * int * int -> unit;
(*  24*)    val move : 'a t -> 'b Widget.t -> int -> int -> unit
(*  24*)	= fn self => fn widget => fn x => fn y =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (widget, fn widget => move_ (self, widget, x, y)))
(*  24*)    val set_has_window_ : cptr * bool -> unit
(*  24*)	= _import "gtk_fixed_set_has_window" : cptr * bool -> unit;
(*  24*)    val set_has_window : 'a t -> bool -> unit
(*  24*)	= fn self => fn has_window =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_has_window_ (self, has_window))
(*  24*)    val get_has_window_ : cptr -> bool
(*  24*)	= _import "gtk_fixed_get_has_window" : cptr -> bool;
(*  24*)    val get_has_window : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_has_window_ self)
(*  24*)end
(*  24*)structure Bin :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a bin_t
(*  24*)    type 'a t = 'a bin_t Container.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toBin : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val get_child : 'a t -> base Widget.t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a bin_t = unit
(*  24*)    type 'a t = 'a bin_t Container.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Container.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toBin obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_bin_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val get_child_ : cptr -> cptr
(*  24*)	= _import "gtk_bin_get_child" : cptr -> cptr;
(*  24*)    val get_child : 'a t -> base Widget.t
(*  24*)	= fn self => Widget.inherit
(*  24*)		       ()
(*  24*)		       (fn () => GObject.withPtr
(*  24*)				   (self, fn self => get_child_ self))
(*  24*)end
(*  24*)structure Viewport :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a viewport_t
(*  24*)    type 'a t = 'a viewport_t Bin.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toViewport : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : 'a Adjustment.t option -> 'b Adjustment.t option
(*  24*)	      -> base t
(*  24*)    val new' : unit -> base t
(*  24*)    val get_hadjustment : 'a t -> base Adjustment.t
(*  24*)    val get_vadjustment : 'a t -> base Adjustment.t
(*  24*)    val set_hadjustment : 'a t -> 'b Adjustment.t option -> unit
(*  24*)    val set_hadjustment' : 'a t -> unit
(*  24*)    val set_vadjustment : 'a t -> 'b Adjustment.t option -> unit
(*  24*)    val set_vadjustment' : 'a t -> unit
(*  24*)    val set_shadowtype : 'a t -> shadowtype -> unit
(*  24*)    val get_shadowtype : 'a t -> shadowtype
(*  24*)    val set_scroll_adjustments_sig
(*  24*)      : (unit -> unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a viewport_t = unit
(*  24*)    type 'a t = 'a viewport_t Bin.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Bin.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toViewport obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_viewport_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : cptr * cptr -> cptr
(*  24*)	= _import "gtk_viewport_new" : cptr * cptr -> cptr;
(*  24*)    val new : 'a Adjustment.t option -> 'b Adjustment.t option
(*  24*)	      -> base t
(*  24*)	= fn hadjustment => fn vadjustment =>
(*  24*)	     make (GObject.withOpt
(*  24*)		     (hadjustment, 
(*  24*)		      fn hadjustment =>
(*  24*)			 GObject.withOpt
(*  24*)			   (vadjustment, 
(*  24*)			    fn vadjustment =>
(*  24*)			       new_ (hadjustment, vadjustment))))
(*  24*)    val new' : unit -> base t
(*  24*)	= fn dummy => make (new_ (GObject.null, GObject.null))
(*  24*)    val get_hadjustment_ : cptr -> cptr
(*  24*)	= _import "gtk_viewport_get_hadjustment" : cptr -> cptr;
(*  24*)    val get_hadjustment : 'a t -> base Adjustment.t
(*  24*)	= fn self =>
(*  24*)	     Adjustment.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, fn self => get_hadjustment_ self))
(*  24*)    val get_vadjustment_ : cptr -> cptr
(*  24*)	= _import "gtk_viewport_get_vadjustment" : cptr -> cptr;
(*  24*)    val get_vadjustment : 'a t -> base Adjustment.t
(*  24*)	= fn self =>
(*  24*)	     Adjustment.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, fn self => get_vadjustment_ self))
(*  24*)    val set_hadjustment_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_viewport_set_hadjustment" : cptr * cptr -> unit;
(*  24*)    val set_hadjustment : 'a t -> 'b Adjustment.t option -> unit
(*  24*)	= fn self => fn adjustment =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withOpt
(*  24*)			     (adjustment, 
(*  24*)			      fn adjustment => set_hadjustment_
(*  24*)						 (self, adjustment)))
(*  24*)    val set_hadjustment' : 'a t -> unit
(*  24*)	= fn self =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_hadjustment_ (self, GObject.null))
(*  24*)    val set_vadjustment_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_viewport_set_vadjustment" : cptr * cptr -> unit;
(*  24*)    val set_vadjustment : 'a t -> 'b Adjustment.t option -> unit
(*  24*)	= fn self => fn adjustment =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withOpt
(*  24*)			     (adjustment, 
(*  24*)			      fn adjustment => set_vadjustment_
(*  24*)						 (self, adjustment)))
(*  24*)    val set_vadjustment' : 'a t -> unit
(*  24*)	= fn self =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_vadjustment_ (self, GObject.null))
(*  24*)    val set_shadowtype_ : cptr * int -> unit
(*  24*)	= _import "gtk_viewport_set_shadow_type" : cptr * int -> unit;
(*  24*)    val set_shadowtype : 'a t -> shadowtype -> unit
(*  24*)	= fn self => fn typ =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_shadowtype_ (self, typ))
(*  24*)    val get_shadowtype_ : cptr -> int
(*  24*)	= _import "gtk_viewport_get_shadow_type" : cptr -> int;
(*  24*)    val get_shadowtype : 'a t -> shadowtype
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_shadowtype_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val set_scroll_adjustments_sig
(*  24*)	 : (unit -> unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "set-scroll-adjustments" false
(*  24*)			    (unit --> unit --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure ScrolledWindow :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a scrolledwindow_t
(*  24*)    type 'a t = 'a scrolledwindow_t Bin.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toScrolledWindow : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : 'a Adjustment.t option -> 'b Adjustment.t option
(*  24*)	      -> base t
(*  24*)    val new' : unit -> base t
(*  24*)    val set_hadjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)    val set_vadjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)    val get_hadjustment : 'a t -> base Adjustment.t
(*  24*)    val get_vadjustment : 'a t -> base Adjustment.t
(*  24*)    val set_policy : 'a t -> policytype -> policytype -> unit
(*  24*)    val set_placement : 'a t -> cornertype -> unit
(*  24*)    val get_placement : 'a t -> cornertype
(*  24*)    val set_shadowtype : 'a t -> shadowtype -> unit
(*  24*)    val get_shadowtype : 'a t -> shadowtype
(*  24*)    val add_with_viewport : 'a t -> 'b Widget.t -> unit
(*  24*)    val scroll_child_sig : (unit -> bool -> unit) -> 'a t Signal.signal
(*  24*)    val move_focus_out_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a scrolledwindow_t = unit
(*  24*)    type 'a t = 'a scrolledwindow_t Bin.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Bin.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toScrolledWindow obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_scrolled_window_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : cptr * cptr -> cptr
(*  24*)	= _import "gtk_scrolled_window_new" : cptr * cptr -> cptr;
(*  24*)    val new : 'a Adjustment.t option -> 'b Adjustment.t option
(*  24*)	      -> base t
(*  24*)	= fn hadjustment => fn vadjustment =>
(*  24*)	     make (GObject.withOpt
(*  24*)		     (hadjustment, 
(*  24*)		      fn hadjustment =>
(*  24*)			 GObject.withOpt
(*  24*)			   (vadjustment, 
(*  24*)			    fn vadjustment =>
(*  24*)			       new_ (hadjustment, vadjustment))))
(*  24*)    val new' : unit -> base t
(*  24*)	= fn dummy => make (new_ (GObject.null, GObject.null))
(*  24*)    val set_hadjustment_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_scrolled_window_set_hadjustment"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val set_hadjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)	= fn self => fn hadjustment =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr (hadjustment, 
(*  24*)					    fn hadjustment =>
(*  24*)					       set_hadjustment_
(*  24*)						 (self, hadjustment)))
(*  24*)    val set_vadjustment_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_scrolled_window_set_vadjustment"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val set_vadjustment : 'a t -> 'b Adjustment.t -> unit
(*  24*)	= fn self => fn hadjustment =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr (hadjustment, 
(*  24*)					    fn hadjustment =>
(*  24*)					       set_vadjustment_
(*  24*)						 (self, hadjustment)))
(*  24*)    val get_hadjustment_ : cptr -> cptr
(*  24*)	= _import "gtk_scrolled_window_get_hadjustment" : cptr -> cptr;
(*  24*)    val get_hadjustment : 'a t -> base Adjustment.t
(*  24*)	= fn self =>
(*  24*)	     Adjustment.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, fn self => get_hadjustment_ self))
(*  24*)    val get_vadjustment_ : cptr -> cptr
(*  24*)	= _import "gtk_scrolled_window_get_vadjustment" : cptr -> cptr;
(*  24*)    val get_vadjustment : 'a t -> base Adjustment.t
(*  24*)	= fn self =>
(*  24*)	     Adjustment.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, fn self => get_vadjustment_ self))
(*  24*)    val set_policy_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_scrolled_window_set_policy"
(*  24*)		  : cptr * int * int -> unit;
(*  24*)    val set_policy : 'a t -> policytype -> policytype -> unit
(*  24*)	= fn self => fn hscrollbar_policy => fn vscrollbar_policy =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_policy_ (self, hscrollbar_policy, 
(*  24*)					vscrollbar_policy))
(*  24*)    val set_placement_ : cptr * int -> unit
(*  24*)	= _import "gtk_scrolled_window_set_placement"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_placement : 'a t -> cornertype -> unit
(*  24*)	= fn self => fn window_placement =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_placement_ (self, window_placement))
(*  24*)    val get_placement_ : cptr -> int
(*  24*)	= _import "gtk_scrolled_window_get_placement" : cptr -> int;
(*  24*)    val get_placement : 'a t -> cornertype
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_placement_ self)
(*  24*)    val set_shadowtype_ : cptr * int -> unit
(*  24*)	= _import "gtk_scrolled_window_set_shadow_type"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_shadowtype : 'a t -> shadowtype -> unit
(*  24*)	= fn self => fn typ =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_shadowtype_ (self, typ))
(*  24*)    val get_shadowtype_ : cptr -> int
(*  24*)	= _import "gtk_scrolled_window_get_shadow_type" : cptr -> int;
(*  24*)    val get_shadowtype : 'a t -> shadowtype
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_shadowtype_ self)
(*  24*)    val add_with_viewport_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_scrolled_window_add_with_viewport"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val add_with_viewport : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn child =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => add_with_viewport_
(*  24*)					    (self, child)))
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val scroll_child_sig : (unit -> bool -> unit)
(*  24*)			      -> 'a t Signal.signal
(*  24*)	   = fn f => signal "scroll-child" false
(*  24*)			    (unit --> bool --> return_void) f
(*  24*)       val move_focus_out_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "move-focus-out" false
(*  24*)			    (unit --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Item :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a item_t
(*  24*)    type 'a t = 'a item_t Bin.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toItem : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val select : 'a t -> unit
(*  24*)    val deselect : 'a t -> unit
(*  24*)    val toggle : 'a t -> unit
(*  24*)    val factory_get_type : unit -> int
(*  24*)    val factory_path_from_widget : 'a Widget.t -> string
(*  24*)    val factory_popup_data_from_widget : 'a Widget.t -> cptr
(*  24*)    val factories_path_delete : string -> string -> unit
(*  24*)    val select_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val deselect_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val toggle_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a item_t = unit
(*  24*)    type 'a t = 'a item_t Bin.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Bin.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toItem obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_item_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val select_ : cptr -> unit
(*  24*)	= _import "gtk_item_select" : cptr -> unit;
(*  24*)    val select : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => select_ self)
(*  24*)    val deselect_ : cptr -> unit
(*  24*)	= _import "gtk_item_deselect" : cptr -> unit;
(*  24*)    val deselect : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => deselect_ self)
(*  24*)    val toggle_ : cptr -> unit
(*  24*)	= _import "gtk_item_toggle" : cptr -> unit;
(*  24*)    val toggle : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => toggle_ self)
(*  24*)    val factory_get_type_ : unit -> int
(*  24*)	= _import "gtk_item_factory_get_type" : unit -> int;
(*  24*)    val factory_get_type : unit -> int
(*  24*)	= fn dummy => factory_get_type_ dummy
(*  24*)    val factory_path_from_widget_ : cptr -> CString.t
(*  24*)	= _import "gtk_item_factory_path_from_widget"
(*  24*)		  : cptr -> CString.t;
(*  24*)    val factory_path_from_widget : 'a Widget.t -> string
(*  24*)	= fn widget =>
(*  24*)	     GObject.withPtr
(*  24*)	       (widget, 
(*  24*)		fn widget =>
(*  24*)		   let val t = factory_path_from_widget_ widget
(*  24*)		   in CString.toString t end)
(*  24*)    val factory_popup_data_from_widget_ : cptr -> cptr
(*  24*)	= _import "gtk_item_factory_popup_data_from_widget"
(*  24*)		  : cptr -> cptr;
(*  24*)    val factory_popup_data_from_widget : 'a Widget.t -> cptr
(*  24*)	= fn widget =>
(*  24*)	     GObject.withPtr
(*  24*)	       (widget, 
(*  24*)		fn widget => factory_popup_data_from_widget_ widget)
(*  24*)    val factories_path_delete_
(*  24*)      : CString.cstring * CString.cstring -> unit
(*  24*)	= _import "gtk_item_factories_path_delete"
(*  24*)		  : CString.cstring * CString.cstring -> unit;
(*  24*)    val factories_path_delete : string -> string -> unit
(*  24*)	= fn ifactory_path => fn path =>
(*  24*)	     factories_path_delete_ (CString.fromString ifactory_path, 
(*  24*)				     CString.fromString path)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val select_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "select" false (void --> return_void) f
(*  24*)       val deselect_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "deselect" false (void --> return_void) f
(*  24*)       val toggle_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "toggle" false (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure MenuItem :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a menuitem_t
(*  24*)    type 'a t = 'a menuitem_t Item.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toMenuItem : 'a t -> base t
(*  24*)    val new : unit -> base t
(*  24*)    val new_with_label : string -> base t
(*  24*)    val new_with_mnemonic : string -> base t
(*  24*)    val set_submenu : 'a t -> 'b Widget.t -> unit
(*  24*)    val get_submenu : 'a t -> base Widget.t
(*  24*)    val remove_submenu : 'a t -> unit
(*  24*)    val select : 'a t -> unit
(*  24*)    val deselect : 'a t -> unit
(*  24*)    val activate : 'a t -> unit
(*  24*)    val toggle_size_allocate : 'a t -> int -> unit
(*  24*)    val set_right_justified : 'a t -> bool -> unit
(*  24*)    val get_right_justified : 'a t -> bool
(*  24*)    val set_accel_path : 'a t -> string -> unit
(*  24*)    val right_justify : 'a t -> unit
(*  24*)    val activate_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val activate_item_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val toggle_size_request_sig : (int -> unit) -> 'a t Signal.signal
(*  24*)    val toggle_size_allocate_sig : (int -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a menuitem_t = unit
(*  24*)    type 'a t = 'a menuitem_t Item.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Item.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toMenuItem obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_menu_item_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val new_with_label_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_menu_item_new_with_label"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val new_with_label : string -> base t
(*  24*)	= fn label => make (new_with_label_ (CString.fromString label))
(*  24*)    val new_with_mnemonic_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_menu_item_new_with_mnemonic"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val new_with_mnemonic : string -> base t
(*  24*)	= fn label => make (new_with_mnemonic_
(*  24*)			      (CString.fromString label))
(*  24*)    val set_submenu_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_menu_item_set_submenu" : cptr * cptr -> unit;
(*  24*)    val set_submenu : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn submenu =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (submenu, 
(*  24*)			      fn submenu =>
(*  24*)				 set_submenu_ (self, submenu)))
(*  24*)    val get_submenu_ : cptr -> cptr
(*  24*)	= _import "gtk_menu_item_get_submenu" : cptr -> cptr;
(*  24*)    val get_submenu : 'a t -> base Widget.t
(*  24*)	= fn self =>
(*  24*)	     Widget.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, fn self => get_submenu_ self))
(*  24*)    val remove_submenu_ : cptr -> unit
(*  24*)	= _import "gtk_menu_item_remove_submenu" : cptr -> unit;
(*  24*)    val remove_submenu : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => remove_submenu_ self)
(*  24*)    val select_ : cptr -> unit
(*  24*)	= _import "gtk_menu_item_select" : cptr -> unit;
(*  24*)    val select : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => select_ self)
(*  24*)    val deselect_ : cptr -> unit
(*  24*)	= _import "gtk_menu_item_deselect" : cptr -> unit;
(*  24*)    val deselect : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => deselect_ self)
(*  24*)    val activate_ : cptr -> unit
(*  24*)	= _import "gtk_menu_item_activate" : cptr -> unit;
(*  24*)    val activate : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => activate_ self)
(*  24*)    val toggle_size_allocate_ : cptr * int -> unit
(*  24*)	= _import "gtk_menu_item_toggle_size_allocate"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val toggle_size_allocate : 'a t -> int -> unit
(*  24*)	= fn self => fn allocation =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => toggle_size_allocate_
(*  24*)					   (self, allocation))
(*  24*)    val set_right_justified_ : cptr * bool -> unit
(*  24*)	= _import "gtk_menu_item_set_right_justified"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_right_justified : 'a t -> bool -> unit
(*  24*)	= fn self => fn right_justified =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_right_justified_
(*  24*)					   (self, right_justified))
(*  24*)    val get_right_justified_ : cptr -> bool
(*  24*)	= _import "gtk_menu_item_get_right_justified" : cptr -> bool;
(*  24*)    val get_right_justified : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_right_justified_ self)
(*  24*)    val set_accel_path_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_menu_item_set_accel_path"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_accel_path : 'a t -> string -> unit
(*  24*)	= fn self => fn accel_path =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_accel_path_
(*  24*)			     (self, CString.fromString accel_path))
(*  24*)    val right_justify_ : cptr -> unit
(*  24*)	= _import "gtk_menu_item_right_justify" : cptr -> unit;
(*  24*)    val right_justify : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => right_justify_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val activate_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "activate" false (void --> return_void) f
(*  24*)       val activate_item_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "activate-item" false (void --> return_void) f
(*  24*)       val toggle_size_request_sig
(*  24*)	 : (int -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "toggle-size-request" false
(*  24*)			    (int --> return_void) f
(*  24*)       val toggle_size_allocate_sig
(*  24*)	 : (int -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "toggle-size-allocate" false
(*  24*)			    (int --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure TearoffMenuItem :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a tearoffmenuitem_t
(*  24*)    type 'a t = 'a tearoffmenuitem_t MenuItem.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toTearoffMenuItem : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a tearoffmenuitem_t = unit
(*  24*)    type 'a t = 'a tearoffmenuitem_t MenuItem.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in MenuItem.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toTearoffMenuItem obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_tearoff_menu_item_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_tearoff_menu_item_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)end
(*  24*)structure SeparatorMenuItem :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a separatormenuitem_t
(*  24*)    type 'a t = 'a separatormenuitem_t MenuItem.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toSeparatorMenuItem : 'a t -> base t
(*  24*)    val new : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a separatormenuitem_t = unit
(*  24*)    type 'a t = 'a separatormenuitem_t MenuItem.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in MenuItem.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toSeparatorMenuItem obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_separator_menu_item_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)end
(*  24*)structure CheckMenuItem :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a checkmenuitem_t
(*  24*)    type 'a t = 'a checkmenuitem_t MenuItem.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toCheckMenuItem : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val new_with_label : string -> base t
(*  24*)    val new_with_mnemonic : string -> base t
(*  24*)    val set_active : 'a t -> bool -> unit
(*  24*)    val get_active : 'a t -> bool
(*  24*)    val toggled : 'a t -> unit
(*  24*)    val set_inconsistent : 'a t -> bool -> unit
(*  24*)    val get_inconsistent : 'a t -> bool
(*  24*)    val set_show_toggle : 'a t -> bool -> unit
(*  24*)    val set_state : 'a t -> bool -> unit
(*  24*)    val toggled_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a checkmenuitem_t = unit
(*  24*)    type 'a t = 'a checkmenuitem_t MenuItem.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in MenuItem.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toCheckMenuItem obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_check_menu_item_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_check_menu_item_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val new_with_label_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_check_menu_item_new_with_label"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val new_with_label : string -> base t
(*  24*)	= fn label => make (new_with_label_ (CString.fromString label))
(*  24*)    val new_with_mnemonic_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_check_menu_item_new_with_mnemonic"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val new_with_mnemonic : string -> base t
(*  24*)	= fn label => make (new_with_mnemonic_
(*  24*)			      (CString.fromString label))
(*  24*)    val set_active_ : cptr * bool -> unit
(*  24*)	= _import "gtk_check_menu_item_set_active"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_active : 'a t -> bool -> unit
(*  24*)	= fn self => fn is_active =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_active_ (self, is_active))
(*  24*)    val get_active_ : cptr -> bool
(*  24*)	= _import "gtk_check_menu_item_get_active" : cptr -> bool;
(*  24*)    val get_active : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_active_ self)
(*  24*)    val toggled_ : cptr -> unit
(*  24*)	= _import "gtk_check_menu_item_toggled" : cptr -> unit;
(*  24*)    val toggled : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => toggled_ self)
(*  24*)    val set_inconsistent_ : cptr * bool -> unit
(*  24*)	= _import "gtk_check_menu_item_set_inconsistent"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_inconsistent : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_inconsistent_ (self, setting))
(*  24*)    val get_inconsistent_ : cptr -> bool
(*  24*)	= _import "gtk_check_menu_item_get_inconsistent"
(*  24*)		  : cptr -> bool;
(*  24*)    val get_inconsistent : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_inconsistent_ self)
(*  24*)    val set_show_toggle_ : cptr * bool -> unit
(*  24*)	= _import "gtk_check_menu_item_set_show_toggle"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_show_toggle : 'a t -> bool -> unit
(*  24*)	= fn self => fn always =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_show_toggle_ (self, always))
(*  24*)    val set_state_ : cptr * bool -> unit
(*  24*)	= _import "gtk_check_menu_item_set_state"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_state : 'a t -> bool -> unit
(*  24*)	= fn self => fn is_active =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_state_ (self, is_active))
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val toggled_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "toggled" false (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure RadioMenuItem :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a radiomenuitem_t
(*  24*)    type 'a t = 'a radiomenuitem_t CheckMenuItem.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toRadioMenuItem : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a radiomenuitem_t = unit
(*  24*)    type 'a t = 'a radiomenuitem_t CheckMenuItem.t
(*  24*)    fun inherit w con
(*  24*)      = let val con = let val ptr = con () in fn () => ptr end
(*  24*)	    val witness = ()
(*  24*)	in CheckMenuItem.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toRadioMenuItem obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_radio_menu_item_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)end
(*  24*)structure ImageMenuItem :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a imagemenuitem_t
(*  24*)    type 'a t = 'a imagemenuitem_t MenuItem.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toImageMenuItem : 'a t -> base t
(*  24*)    val new : unit -> base t
(*  24*)    val new_with_label : string -> base t
(*  24*)    val new_with_mnemonic : string -> base t
(*  24*)    val new_from_stock : string -> 'a AccelGroup.t -> base t
(*  24*)    val set_image : 'a t -> 'b Widget.t option -> unit
(*  24*)    val set_image' : 'a t -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a imagemenuitem_t = unit
(*  24*)    type 'a t = 'a imagemenuitem_t MenuItem.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in MenuItem.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toImageMenuItem obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_image_menu_item_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val new_with_label_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_image_menu_item_new_with_label"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val new_with_label : string -> base t
(*  24*)	= fn label => make (new_with_label_ (CString.fromString label))
(*  24*)    val new_with_mnemonic_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_image_menu_item_new_with_mnemonic"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val new_with_mnemonic : string -> base t
(*  24*)	= fn label => make (new_with_mnemonic_
(*  24*)			      (CString.fromString label))
(*  24*)    val new_from_stock_ : CString.cstring * cptr -> cptr
(*  24*)	= _import "gtk_image_menu_item_new_from_stock"
(*  24*)		  : CString.cstring * cptr -> cptr;
(*  24*)    val new_from_stock : string -> 'a AccelGroup.t -> base t
(*  24*)	= fn stock_id => fn accel_group =>
(*  24*)	     make (GObject.withPtr
(*  24*)		     (accel_group, 
(*  24*)		      fn accel_group =>
(*  24*)			 new_from_stock_
(*  24*)			   (CString.fromString stock_id, accel_group)))
(*  24*)    val set_image_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_image_menu_item_set_image"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val set_image : 'a t -> 'b Widget.t option -> unit
(*  24*)	= fn self => fn image =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withOpt
(*  24*)		     (image, fn image => set_image_ (self, image)))
(*  24*)    val set_image' : 'a t -> unit
(*  24*)	= fn self =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_image_ (self, GObject.null))
(*  24*)end
(*  24*)structure ListItem :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a listitem_t
(*  24*)    type 'a t = 'a listitem_t Item.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toListItem : 'a t -> base t
(*  24*)    val new : unit -> base t
(*  24*)    val new_with_label : string -> base t
(*  24*)    val select : 'a t -> unit
(*  24*)    val deselect : 'a t -> unit
(*  24*)    val select_all_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val unselect_all_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val toggle_focus_row_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val undo_selection_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val start_selection_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val end_selection_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val toggle_add_mode_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a listitem_t = unit
(*  24*)    type 'a t = 'a listitem_t Item.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Item.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toListItem obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_list_item_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val new_with_label_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_list_item_new_with_label"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val new_with_label : string -> base t
(*  24*)	= fn label => make (new_with_label_ (CString.fromString label))
(*  24*)    val select_ : cptr -> unit
(*  24*)	= _import "gtk_list_item_select" : cptr -> unit;
(*  24*)    val select : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => select_ self)
(*  24*)    val deselect_ : cptr -> unit
(*  24*)	= _import "gtk_list_item_deselect" : cptr -> unit;
(*  24*)    val deselect : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => deselect_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val select_all_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "select-all" false (void --> return_void) f
(*  24*)       val unselect_all_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "unselect-all" false (void --> return_void) f
(*  24*)       val toggle_focus_row_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "toggle-focus-row" false
(*  24*)			    (void --> return_void) f
(*  24*)       val undo_selection_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "undo-selection" false
(*  24*)			    (void --> return_void) f
(*  24*)       val start_selection_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "start-selection" false
(*  24*)			    (void --> return_void) f
(*  24*)       val end_selection_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "end-selection" false (void --> return_void) f
(*  24*)       val toggle_add_mode_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "toggle-add-mode" false
(*  24*)			    (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure HandleBox :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a handlebox_t
(*  24*)    type 'a t = 'a handlebox_t Bin.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toHandleBox : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val set_shadowtype : 'a t -> shadowtype -> unit
(*  24*)    val get_shadowtype : 'a t -> shadowtype
(*  24*)    val set_handle_position : 'a t -> positiontype -> unit
(*  24*)    val get_handle_position : 'a t -> positiontype
(*  24*)    val set_snap_edge : 'a t -> positiontype -> unit
(*  24*)    val get_snap_edge : 'a t -> positiontype
(*  24*)    val child_attached_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val child_detached_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a handlebox_t = unit
(*  24*)    type 'a t = 'a handlebox_t Bin.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Bin.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toHandleBox obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_handle_box_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_handle_box_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val set_shadowtype_ : cptr * int -> unit
(*  24*)	= _import "gtk_handle_box_set_shadow_type"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_shadowtype : 'a t -> shadowtype -> unit
(*  24*)	= fn self => fn typ =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_shadowtype_ (self, typ))
(*  24*)    val get_shadowtype_ : cptr -> int
(*  24*)	= _import "gtk_handle_box_get_shadow_type" : cptr -> int;
(*  24*)    val get_shadowtype : 'a t -> shadowtype
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_shadowtype_ self)
(*  24*)    val set_handle_position_ : cptr * int -> unit
(*  24*)	= _import "gtk_handle_box_set_handle_position"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_handle_position : 'a t -> positiontype -> unit
(*  24*)	= fn self => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_handle_position_ (self, position))
(*  24*)    val get_handle_position_ : cptr -> int
(*  24*)	= _import "gtk_handle_box_get_handle_position" : cptr -> int;
(*  24*)    val get_handle_position : 'a t -> positiontype
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_handle_position_ self)
(*  24*)    val set_snap_edge_ : cptr * int -> unit
(*  24*)	= _import "gtk_handle_box_set_snap_edge" : cptr * int -> unit;
(*  24*)    val set_snap_edge : 'a t -> positiontype -> unit
(*  24*)	= fn self => fn edge =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_snap_edge_ (self, edge))
(*  24*)    val get_snap_edge_ : cptr -> int
(*  24*)	= _import "gtk_handle_box_get_snap_edge" : cptr -> int;
(*  24*)    val get_snap_edge : 'a t -> positiontype
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_snap_edge_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val child_attached_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "child-attached" false
(*  24*)			    (unit --> return_void) f
(*  24*)       val child_detached_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "child-detached" false
(*  24*)			    (unit --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Frame :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a frame_t
(*  24*)    type 'a t = 'a frame_t Bin.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toFrame : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : string option -> base t
(*  24*)    val new' : unit -> base t
(*  24*)    val set_label : 'a t -> string option -> unit
(*  24*)    val set_label' : 'a t -> unit
(*  24*)    val get_label : 'a t -> string
(*  24*)    val set_label_widget : 'a t -> 'b Widget.t -> unit
(*  24*)    val get_label_widget : 'a t -> base Widget.t
(*  24*)    val set_label_align : 'a t -> real -> real -> unit
(*  24*)    val set_shadowtype : 'a t -> shadowtype -> unit
(*  24*)    val get_shadowtype : 'a t -> shadowtype
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a frame_t = unit
(*  24*)    type 'a t = 'a frame_t Bin.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Bin.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toFrame obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_frame_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_frame_new" : CString.cstring -> cptr;
(*  24*)    val new : string option -> base t
(*  24*)	= fn label => make (new_ (CString.fromString
(*  24*)				    (getOpt (label, ""))))
(*  24*)    val new' : unit -> base t
(*  24*)	= fn dummy => make (new_ (CString.fromString ""))
(*  24*)    val set_label_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_frame_set_label"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_label : 'a t -> string option -> unit
(*  24*)	= fn self => fn label =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_label_ (self, 
(*  24*)				       CString.fromString
(*  24*)					 (getOpt (label, ""))))
(*  24*)    val set_label' : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => set_label_
(*  24*)				     (self, CString.fromString ""))
(*  24*)    val get_label_ : cptr -> CString.t
(*  24*)	= _import "gtk_frame_get_label" : cptr -> CString.t;
(*  24*)    val get_label : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_label_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val set_label_widget_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_frame_set_label_widget" : cptr * cptr -> unit;
(*  24*)    val set_label_widget : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn label_widget =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr (label_widget, 
(*  24*)					    fn label_widget =>
(*  24*)					       set_label_widget_
(*  24*)						 (self, label_widget)))
(*  24*)    val get_label_widget_ : cptr -> cptr
(*  24*)	= _import "gtk_frame_get_label_widget" : cptr -> cptr;
(*  24*)    val get_label_widget : 'a t -> base Widget.t
(*  24*)	= fn self =>
(*  24*)	     Widget.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, fn self => get_label_widget_ self))
(*  24*)    val set_label_align_ : cptr * real * real -> unit
(*  24*)	= _import "gtk_frame_set_label_align"
(*  24*)		  : cptr * real * real -> unit;
(*  24*)    val set_label_align : 'a t -> real -> real -> unit
(*  24*)	= fn self => fn xalign => fn yalign =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_label_align_
(*  24*)					   (self, xalign, yalign))
(*  24*)    val set_shadowtype_ : cptr * int -> unit
(*  24*)	= _import "gtk_frame_set_shadow_type" : cptr * int -> unit;
(*  24*)    val set_shadowtype : 'a t -> shadowtype -> unit
(*  24*)	= fn self => fn typ =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_shadowtype_ (self, typ))
(*  24*)    val get_shadowtype_ : cptr -> int
(*  24*)	= _import "gtk_frame_get_shadow_type" : cptr -> int;
(*  24*)    val get_shadowtype : 'a t -> shadowtype
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_shadowtype_ self)
(*  24*)end
(*  24*)structure AspectFrame :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a aspectframe_t
(*  24*)    type 'a t = 'a aspectframe_t Frame.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toAspectFrame : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : string option -> real option -> real option 
(*  24*)	   -> real option -> bool option
(*  24*)	      -> base t
(*  24*)    val new' : unit -> base t
(*  24*)    val set : 'a t -> real option -> real option -> real option 
(*  24*)	   -> bool option
(*  24*)	      -> unit
(*  24*)    val set' : 'a t -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a aspectframe_t = unit
(*  24*)    type 'a t = 'a aspectframe_t Frame.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Frame.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toAspectFrame obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_aspect_frame_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : CString.cstring * real * real * real * bool -> cptr
(*  24*)	= _import "gtk_aspect_frame_new"
(*  24*)		  : CString.cstring * real * real * real * bool
(*  24*)		    -> cptr;
(*  24*)    val new : string option -> real option -> real option 
(*  24*)	   -> real option -> bool option
(*  24*)	      -> base t
(*  24*)	= fn label => fn xalign => fn yalign => fn ratio => 
(*  24*)	  fn obey_child =>
(*  24*)	     make (new_ (CString.fromString (getOpt (label, "")), 
(*  24*)			 getOpt (xalign, 0.5), getOpt (yalign, 0.5), 
(*  24*)			 getOpt (ratio, 1.0), 
(*  24*)			 getOpt (obey_child, true)))
(*  24*)    val new' : unit -> base t
(*  24*)	= fn dummy => make (new_ (CString.fromString "", 0.5, 0.5, 
(*  24*)				  1.0, true))
(*  24*)    val set_ : cptr * real * real * real * bool -> unit
(*  24*)	= _import "gtk_aspect_frame_set"
(*  24*)		  : cptr * real * real * real * bool -> unit;
(*  24*)    val set : 'a t -> real option -> real option -> real option 
(*  24*)	   -> bool option
(*  24*)	      -> unit
(*  24*)	= fn self => fn xalign => fn yalign => fn ratio => 
(*  24*)	  fn obey_child =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_ (self, getOpt (xalign, 0.0), 
(*  24*)				 getOpt (yalign, 0.0), 
(*  24*)				 getOpt (ratio, 1.0), 
(*  24*)				 getOpt (obey_child, true)))
(*  24*)    val set' : 'a t -> unit
(*  24*)	= fn self =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_ (self, 0.0, 0.0, 1.0, true))
(*  24*)end
(*  24*)structure EventBox :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a eventbox_t
(*  24*)    type 'a t = 'a eventbox_t Bin.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toEventBox : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a eventbox_t = unit
(*  24*)    type 'a t = 'a eventbox_t Bin.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Bin.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toEventBox obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_event_box_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_event_box_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)end
(*  24*)structure Alignment :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a alignment_t
(*  24*)    type 'a t = 'a alignment_t Bin.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toAlignment : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : real option -> real option -> real option -> real option
(*  24*)	      -> base t
(*  24*)    val new' : unit -> base t
(*  24*)    val set : 'a t -> real -> real -> real -> real -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a alignment_t = unit
(*  24*)    type 'a t = 'a alignment_t Bin.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Bin.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toAlignment obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_alignment_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : real * real * real * real -> cptr
(*  24*)	= _import "gtk_alignment_new"
(*  24*)		  : real * real * real * real -> cptr;
(*  24*)    val new : real option -> real option -> real option -> real option
(*  24*)	      -> base t
(*  24*)	= fn xalign => fn yalign => fn xscale => fn yscale =>
(*  24*)	     make (new_ (getOpt (xalign, 0.0), getOpt (yalign, 0.0), 
(*  24*)			 getOpt (xscale, 0.0), getOpt (yscale, 0.0)))
(*  24*)    val new' : unit -> base t
(*  24*)	= fn dummy => make (new_ (0.0, 0.0, 0.0, 0.0))
(*  24*)    val set_ : cptr * real * real * real * real -> unit
(*  24*)	= _import "gtk_alignment_set"
(*  24*)		  : cptr * real * real * real * real -> unit;
(*  24*)    val set : 'a t -> real -> real -> real -> real -> unit
(*  24*)	= fn self => fn xalign => fn yalign => fn xscale => 
(*  24*)	  fn yscale =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_ (self, xalign, yalign, 
(*  24*)					       xscale, yscale))
(*  24*)end
(*  24*)structure Button :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a button_t
(*  24*)    type 'a t = 'a button_t Bin.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toButton : 'a t -> base t
(*  24*)    type action
(*  24*)    val IGNORED : action
(*  24*)    val SELECTS : action
(*  24*)    val DRAGS : action
(*  24*)    val EXPANDS : action
(*  24*)    type box_style
(*  24*)    val BUTTONBOX_DEFAULT_STYLE : box_style
(*  24*)    val BUTTONBOX_SPREAD : box_style
(*  24*)    val BUTTONBOX_EDGE : box_style
(*  24*)    val BUTTONBOX_START : box_style
(*  24*)    val BUTTONBOX_END : box_style
(*  24*)    val box_get_type : unit -> int
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val new_with_label : string -> base t
(*  24*)    val new_from_stock : string -> base t
(*  24*)    val new_with_mnemonic : string -> base t
(*  24*)    val pressed : 'a t -> unit
(*  24*)    val released : 'a t -> unit
(*  24*)    val clicked : 'a t -> unit
(*  24*)    val enter : 'a t -> unit
(*  24*)    val leave : 'a t -> unit
(*  24*)    val set_relief : 'a t -> relief_style -> unit
(*  24*)    val get_relief : 'a t -> relief_style
(*  24*)    val set_label : 'a t -> string -> unit
(*  24*)    val get_label : 'a t -> string
(*  24*)    val set_use_underline : 'a t -> bool -> unit
(*  24*)    val get_use_underline : 'a t -> bool
(*  24*)    val set_use_stock : 'a t -> bool -> unit
(*  24*)    val get_use_stock : 'a t -> bool
(*  24*)    val clicked_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val pressed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val released_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val enter_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val leave_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val activate_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a button_t = unit
(*  24*)    type 'a t = 'a button_t Bin.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Bin.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toButton obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    type action = int
(*  24*)    val get_action_ : int ref * int ref * int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_button_action"
(*  24*)		  : int ref * int ref * int ref * int ref -> unit;
(*  24*)    val (IGNORED, SELECTS, DRAGS, EXPANDS)
(*  24*)	= let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
(*  24*)	  in get_action_ (x0, x1, x2, x3)
(*  24*)	   ; (!x0, !x1, !x2, !x3)
(*  24*)	  end
(*  24*)    type box_style = int
(*  24*)    val get_box_style_
(*  24*)      : int ref * int ref * int ref * int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_button_box_style"
(*  24*)		  : int ref * int ref * int ref * int ref * int ref
(*  24*)		    -> unit;
(*  24*)    val (BUTTONBOX_DEFAULT_STYLE, BUTTONBOX_SPREAD, BUTTONBOX_EDGE, 
(*  24*)	 BUTTONBOX_START, BUTTONBOX_END)
(*  24*)	= let val (x0, x1, x2, x3, x4)
(*  24*)		  = (ref 0, ref 0, ref 0, ref 0, ref 0)
(*  24*)	  in get_box_style_ (x0, x1, x2, x3, x4)
(*  24*)	   ; (!x0, !x1, !x2, !x3, !x4)
(*  24*)	  end
(*  24*)    val box_get_type_ : unit -> int
(*  24*)	= _import "gtk_button_box_get_type" : unit -> int;
(*  24*)    val box_get_type : unit -> int = fn dummy => box_get_type_ dummy
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_button_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr = _import "gtk_button_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val new_with_label_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_button_new_with_label"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val new_with_label : string -> base t
(*  24*)	= fn label => make (new_with_label_ (CString.fromString label))
(*  24*)    val new_from_stock_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_button_new_from_stock"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val new_from_stock : string -> base t
(*  24*)	= fn stock_id => make (new_from_stock_
(*  24*)				 (CString.fromString stock_id))
(*  24*)    val new_with_mnemonic_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_button_new_with_mnemonic"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val new_with_mnemonic : string -> base t
(*  24*)	= fn label => make (new_with_mnemonic_
(*  24*)			      (CString.fromString label))
(*  24*)    val pressed_ : cptr -> unit
(*  24*)	= _import "gtk_button_pressed" : cptr -> unit;
(*  24*)    val pressed : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => pressed_ self)
(*  24*)    val released_ : cptr -> unit
(*  24*)	= _import "gtk_button_released" : cptr -> unit;
(*  24*)    val released : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => released_ self)
(*  24*)    val clicked_ : cptr -> unit
(*  24*)	= _import "gtk_button_clicked" : cptr -> unit;
(*  24*)    val clicked : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => clicked_ self)
(*  24*)    val enter_ : cptr -> unit
(*  24*)	= _import "gtk_button_enter" : cptr -> unit;
(*  24*)    val enter : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => enter_ self)
(*  24*)    val leave_ : cptr -> unit
(*  24*)	= _import "gtk_button_leave" : cptr -> unit;
(*  24*)    val leave : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => leave_ self)
(*  24*)    val set_relief_ : cptr * int -> unit
(*  24*)	= _import "gtk_button_set_relief" : cptr * int -> unit;
(*  24*)    val set_relief : 'a t -> relief_style -> unit
(*  24*)	= fn self => fn newstyle =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_relief_ (self, newstyle))
(*  24*)    val get_relief_ : cptr -> int
(*  24*)	= _import "gtk_button_get_relief" : cptr -> int;
(*  24*)    val get_relief : 'a t -> relief_style
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_relief_ self)
(*  24*)    val set_label_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_button_set_label"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_label : 'a t -> string -> unit
(*  24*)	= fn self => fn label =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_label_ (self, CString.fromString label))
(*  24*)    val get_label_ : cptr -> CString.t
(*  24*)	= _import "gtk_button_get_label" : cptr -> CString.t;
(*  24*)    val get_label : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_label_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val set_use_underline_ : cptr * bool -> unit
(*  24*)	= _import "gtk_button_set_use_underline" : cptr * bool -> unit;
(*  24*)    val set_use_underline : 'a t -> bool -> unit
(*  24*)	= fn self => fn use_underline =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_use_underline_
(*  24*)					   (self, use_underline))
(*  24*)    val get_use_underline_ : cptr -> bool
(*  24*)	= _import "gtk_button_get_use_underline" : cptr -> bool;
(*  24*)    val get_use_underline : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_use_underline_ self)
(*  24*)    val set_use_stock_ : cptr * bool -> unit
(*  24*)	= _import "gtk_button_set_use_stock" : cptr * bool -> unit;
(*  24*)    val set_use_stock : 'a t -> bool -> unit
(*  24*)	= fn self => fn use_stock =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_use_stock_ (self, use_stock))
(*  24*)    val get_use_stock_ : cptr -> bool
(*  24*)	= _import "gtk_button_get_use_stock" : cptr -> bool;
(*  24*)    val get_use_stock : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_use_stock_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val clicked_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "clicked" false (void --> return_void) f
(*  24*)       val pressed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "pressed" false (void --> return_void) f
(*  24*)       val released_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "released" false (void --> return_void) f
(*  24*)       val enter_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "enter" false (void --> return_void) f
(*  24*)       val leave_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "leave" false (void --> return_void) f
(*  24*)       val activate_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "activate" false (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure ToggleButton :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a togglebutton_t
(*  24*)    type 'a t = 'a togglebutton_t Button.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toToggleButton : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val new_with_label : string -> base t
(*  24*)    val new_with_mnemonic : string -> base t
(*  24*)    val set_mode : 'a t -> bool -> unit
(*  24*)    val get_mode : 'a t -> bool
(*  24*)    val set_active : 'a t -> bool -> unit
(*  24*)    val get_active : 'a t -> bool
(*  24*)    val toggled : 'a t -> unit
(*  24*)    val set_inconsistent : 'a t -> bool -> unit
(*  24*)    val get_inconsistent : 'a t -> bool
(*  24*)    val set_state : 'a t -> bool -> unit
(*  24*)    val toggled_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a togglebutton_t = unit
(*  24*)    type 'a t = 'a togglebutton_t Button.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Button.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toToggleButton obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_toggle_button_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_toggle_button_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val new_with_label_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_toggle_button_new_with_label"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val new_with_label : string -> base t
(*  24*)	= fn label => make (new_with_label_ (CString.fromString label))
(*  24*)    val new_with_mnemonic_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_toggle_button_new_with_mnemonic"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val new_with_mnemonic : string -> base t
(*  24*)	= fn label => make (new_with_mnemonic_
(*  24*)			      (CString.fromString label))
(*  24*)    val set_mode_ : cptr * bool -> unit
(*  24*)	= _import "gtk_toggle_button_set_mode" : cptr * bool -> unit;
(*  24*)    val set_mode : 'a t -> bool -> unit
(*  24*)	= fn self => fn draw_indicator =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_mode_ (self, draw_indicator))
(*  24*)    val get_mode_ : cptr -> bool
(*  24*)	= _import "gtk_toggle_button_get_mode" : cptr -> bool;
(*  24*)    val get_mode : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr (self, fn self => get_mode_ self)
(*  24*)    val set_active_ : cptr * bool -> unit
(*  24*)	= _import "gtk_toggle_button_set_active" : cptr * bool -> unit;
(*  24*)    val set_active : 'a t -> bool -> unit
(*  24*)	= fn self => fn is_active =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_active_ (self, is_active))
(*  24*)    val get_active_ : cptr -> bool
(*  24*)	= _import "gtk_toggle_button_get_active" : cptr -> bool;
(*  24*)    val get_active : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_active_ self)
(*  24*)    val toggled_ : cptr -> unit
(*  24*)	= _import "gtk_toggle_button_toggled" : cptr -> unit;
(*  24*)    val toggled : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => toggled_ self)
(*  24*)    val set_inconsistent_ : cptr * bool -> unit
(*  24*)	= _import "gtk_toggle_button_set_inconsistent"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_inconsistent : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_inconsistent_ (self, setting))
(*  24*)    val get_inconsistent_ : cptr -> bool
(*  24*)	= _import "gtk_toggle_button_get_inconsistent" : cptr -> bool;
(*  24*)    val get_inconsistent : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_inconsistent_ self)
(*  24*)    val set_state_ : cptr * bool -> unit
(*  24*)	= _import "gtk_toggle_button_set_state" : cptr * bool -> unit;
(*  24*)    val set_state : 'a t -> bool -> unit
(*  24*)	= fn self => fn is_active =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_state_ (self, is_active))
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val toggled_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "toggled" false (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure CheckButton :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a checkbutton_t
(*  24*)    type 'a t = 'a checkbutton_t ToggleButton.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toCheckButton : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val new_with_label : string -> base t
(*  24*)    val new_with_mnemonic : string -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a checkbutton_t = unit
(*  24*)    type 'a t = 'a checkbutton_t ToggleButton.t
(*  24*)    fun inherit w con
(*  24*)      = let val con = let val ptr = con () in fn () => ptr end
(*  24*)	    val witness = ()
(*  24*)	in ToggleButton.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toCheckButton obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_check_button_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_check_button_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val new_with_label_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_check_button_new_with_label"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val new_with_label : string -> base t
(*  24*)	= fn label => make (new_with_label_ (CString.fromString label))
(*  24*)    val new_with_mnemonic_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_check_button_new_with_mnemonic"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val new_with_mnemonic : string -> base t
(*  24*)	= fn label => make (new_with_mnemonic_
(*  24*)			      (CString.fromString label))
(*  24*)end
(*  24*)structure RadioButton :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a radiobutton_t
(*  24*)    type 'a t = 'a radiobutton_t CheckButton.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toRadioButton : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new_from_widget : 'a t -> base t
(*  24*)    val new_with_label_from_widget : 'a t -> string -> base t
(*  24*)    val new_with_mnemonic_from_widget : 'a t -> string -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a radiobutton_t = unit
(*  24*)    type 'a t = 'a radiobutton_t CheckButton.t
(*  24*)    fun inherit w con
(*  24*)      = let val con = let val ptr = con () in fn () => ptr end
(*  24*)	    val witness = ()
(*  24*)	in CheckButton.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toRadioButton obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_radio_button_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_from_widget_ : cptr -> cptr
(*  24*)	= _import "gtk_radio_button_new_from_widget" : cptr -> cptr;
(*  24*)    val new_from_widget : 'a t -> base t
(*  24*)	= fn group =>
(*  24*)	     make (GObject.withPtr
(*  24*)		     (group, fn group => new_from_widget_ group))
(*  24*)    val new_with_label_from_widget_ : cptr * CString.cstring -> cptr
(*  24*)	= _import "gtk_radio_button_new_with_label_from_widget"
(*  24*)		  : cptr * CString.cstring -> cptr;
(*  24*)    val new_with_label_from_widget : 'a t -> string -> base t
(*  24*)	= fn group => fn label =>
(*  24*)	     make (GObject.withPtr
(*  24*)		     (group, 
(*  24*)		      fn group => new_with_label_from_widget_
(*  24*)				    (group, CString.fromString label)))
(*  24*)    val new_with_mnemonic_from_widget_ : cptr * CString.cstring -> cptr
(*  24*)	= _import "gtk_radio_button_new_with_mnemonic_from_widget"
(*  24*)		  : cptr * CString.cstring -> cptr;
(*  24*)    val new_with_mnemonic_from_widget : 'a t -> string -> base t
(*  24*)	= fn group => fn label =>
(*  24*)	     make (GObject.withPtr
(*  24*)		     (group, 
(*  24*)		      fn group => new_with_mnemonic_from_widget_
(*  24*)				    (group, CString.fromString label)))
(*  24*)end
(*  24*)structure OptionMenu :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a optionmenu_t
(*  24*)    type 'a t = 'a optionmenu_t Button.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toOptionMenu : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val get_menu : 'a t -> base Widget.t
(*  24*)    val set_menu : 'a t -> 'b Widget.t -> unit
(*  24*)    val remove_menu : 'a t -> unit
(*  24*)    val get_history : 'a t -> int
(*  24*)    val set_history : 'a t -> int -> unit
(*  24*)    val changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a optionmenu_t = unit
(*  24*)    type 'a t = 'a optionmenu_t Button.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Button.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toOptionMenu obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_option_menu_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_option_menu_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val get_menu_ : cptr -> cptr
(*  24*)	= _import "gtk_option_menu_get_menu" : cptr -> cptr;
(*  24*)    val get_menu : 'a t -> base Widget.t
(*  24*)	= fn self => Widget.inherit
(*  24*)		       ()
(*  24*)		       (fn () => GObject.withPtr
(*  24*)				   (self, fn self => get_menu_ self))
(*  24*)    val set_menu_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_option_menu_set_menu" : cptr * cptr -> unit;
(*  24*)    val set_menu : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn menu =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (menu, fn menu => set_menu_ (self, menu)))
(*  24*)    val remove_menu_ : cptr -> unit
(*  24*)	= _import "gtk_option_menu_remove_menu" : cptr -> unit;
(*  24*)    val remove_menu : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => remove_menu_ self)
(*  24*)    val get_history_ : cptr -> int
(*  24*)	= _import "gtk_option_menu_get_history" : cptr -> int;
(*  24*)    val get_history : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_history_ self)
(*  24*)    val set_history_ : cptr * int -> unit
(*  24*)	= _import "gtk_option_menu_set_history" : cptr * int -> unit;
(*  24*)    val set_history : 'a t -> int -> unit
(*  24*)	= fn self => fn index =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_history_ (self, index))
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "changed" false (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Box :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a box_t
(*  24*)    type 'a t = 'a box_t Container.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toBox : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val pack_start
(*  24*)      : 'a t -> 'b Widget.t -> bool option -> bool option -> int option
(*  24*)	-> unit
(*  24*)    val pack_start' : 'a t -> 'b Widget.t -> unit
(*  24*)    val pack_end : 'a t -> 'b Widget.t -> bool option -> bool option 
(*  24*)		-> int option
(*  24*)		   -> unit
(*  24*)    val pack_end' : 'a t -> 'b Widget.t -> unit
(*  24*)    val pack_start_defaults : 'a t -> 'b Widget.t -> unit
(*  24*)    val pack_end_defaults : 'a t -> 'b Widget.t -> unit
(*  24*)    val set_homogeneous : 'a t -> bool -> unit
(*  24*)    val get_homogeneous : 'a t -> bool
(*  24*)    val set_spacing : 'a t -> int -> unit
(*  24*)    val get_spacing : 'a t -> int
(*  24*)    val reorder_child : 'a t -> 'b Widget.t -> int -> unit
(*  24*)    val set_child_packing
(*  24*)      : 'a t -> 'b Widget.t -> bool -> bool -> int -> packtype -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a box_t = unit
(*  24*)    type 'a t = 'a box_t Container.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Container.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toBox obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_box_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val pack_start_ : cptr * cptr * bool * bool * int -> unit
(*  24*)	= _import "gtk_box_pack_start"
(*  24*)		  : cptr * cptr * bool * bool * int -> unit;
(*  24*)    val pack_start
(*  24*)      : 'a t -> 'b Widget.t -> bool option -> bool option -> int option
(*  24*)	-> unit
(*  24*)	= fn self => fn child => fn expand => fn fill => fn padding =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => pack_start_
(*  24*)					    (self, child, 
(*  24*)					     getOpt (expand, true), 
(*  24*)					     getOpt (fill, true), 
(*  24*)					     getOpt (padding, 0))))
(*  24*)    val pack_start' : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn child =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => pack_start_ (self, child, 
(*  24*)						       true, true, 0)))
(*  24*)    val pack_end_ : cptr * cptr * bool * bool * int -> unit
(*  24*)	= _import "gtk_box_pack_end"
(*  24*)		  : cptr * cptr * bool * bool * int -> unit;
(*  24*)    val pack_end : 'a t -> 'b Widget.t -> bool option -> bool option 
(*  24*)		-> int option
(*  24*)		   -> unit
(*  24*)	= fn self => fn child => fn expand => fn fill => fn padding =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child =>
(*  24*)				 pack_end_ (self, child, 
(*  24*)					    getOpt (expand, true), 
(*  24*)					    getOpt (fill, true), 
(*  24*)					    getOpt (padding, 0))))
(*  24*)    val pack_end' : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn child =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => pack_end_ (self, child, 
(*  24*)						     true, true, 0)))
(*  24*)    val pack_start_defaults_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_box_pack_start_defaults" : cptr * cptr -> unit;
(*  24*)    val pack_start_defaults : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn widget =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (widget, 
(*  24*)			      fn widget => pack_start_defaults_
(*  24*)					     (self, widget)))
(*  24*)    val pack_end_defaults_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_box_pack_end_defaults" : cptr * cptr -> unit;
(*  24*)    val pack_end_defaults : 'a t -> 'b Widget.t -> unit
(*  24*)	= fn self => fn widget =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (widget, 
(*  24*)			      fn widget => pack_end_defaults_
(*  24*)					     (self, widget)))
(*  24*)    val set_homogeneous_ : cptr * bool -> unit
(*  24*)	= _import "gtk_box_set_homogeneous" : cptr * bool -> unit;
(*  24*)    val set_homogeneous : 'a t -> bool -> unit
(*  24*)	= fn self => fn homogeneous =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_homogeneous_ (self, homogeneous))
(*  24*)    val get_homogeneous_ : cptr -> bool
(*  24*)	= _import "gtk_box_get_homogeneous" : cptr -> bool;
(*  24*)    val get_homogeneous : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_homogeneous_ self)
(*  24*)    val set_spacing_ : cptr * int -> unit
(*  24*)	= _import "gtk_box_set_spacing" : cptr * int -> unit;
(*  24*)    val set_spacing : 'a t -> int -> unit
(*  24*)	= fn self => fn spacing =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_spacing_ (self, spacing))
(*  24*)    val get_spacing_ : cptr -> int
(*  24*)	= _import "gtk_box_get_spacing" : cptr -> int;
(*  24*)    val get_spacing : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_spacing_ self)
(*  24*)    val reorder_child_ : cptr * cptr * int -> unit
(*  24*)	= _import "gtk_box_reorder_child" : cptr * cptr * int -> unit;
(*  24*)    val reorder_child : 'a t -> 'b Widget.t -> int -> unit
(*  24*)	= fn self => fn child => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => reorder_child_
(*  24*)					    (self, child, position)))
(*  24*)    val set_child_packing_
(*  24*)      : cptr * cptr * bool * bool * int * int -> unit
(*  24*)	= _import "gtk_box_set_child_packing"
(*  24*)		  : cptr * cptr * bool * bool * int * int -> unit;
(*  24*)    val set_child_packing
(*  24*)      : 'a t -> 'b Widget.t -> bool -> bool -> int -> packtype -> unit
(*  24*)	= fn self => fn child => fn expand => fn fill => fn padding => 
(*  24*)	  fn pack_type =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child =>
(*  24*)				 set_child_packing_
(*  24*)				   (self, child, expand, fill, 
(*  24*)				    padding, pack_type)))
(*  24*)end
(*  24*)structure VBox :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a vbox_t
(*  24*)    type 'a t = 'a vbox_t Box.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toVBox : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : bool option -> int option -> base t
(*  24*)    val new' : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a vbox_t = unit
(*  24*)    type 'a t = 'a vbox_t Box.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Box.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toVBox obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_vbox_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : bool * int -> cptr
(*  24*)	= _import "gtk_vbox_new" : bool * int -> cptr;
(*  24*)    val new : bool option -> int option -> base t
(*  24*)	= fn homogeneous => fn spacing =>
(*  24*)	     make (new_ (getOpt (homogeneous, false), 
(*  24*)			 getOpt (spacing, 0)))
(*  24*)    val new' : unit -> base t = fn dummy => make (new_ (false, 0))
(*  24*)end
(*  24*)structure ColorSelection :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a colorselection_t
(*  24*)    type 'a t = 'a colorselection_t VBox.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toColorSelection : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val get_has_opacity_control : 'a t -> bool
(*  24*)    val set_has_opacity_control : 'a t -> bool -> unit
(*  24*)    val get_has_palette : 'a t -> bool
(*  24*)    val set_has_palette : 'a t -> bool -> unit
(*  24*)    val set_current_alpha : 'a t -> int -> unit
(*  24*)    val get_current_alpha : 'a t -> int
(*  24*)    val set_previous_alpha : 'a t -> int -> unit
(*  24*)    val get_previous_alpha : 'a t -> int
(*  24*)    val is_adjusting : 'a t -> bool
(*  24*)    val set_update_policy : 'a t -> updatetype -> unit
(*  24*)    val dialog_get_type : unit -> int
(*  24*)    val color_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a colorselection_t = unit
(*  24*)    type 'a t = 'a colorselection_t VBox.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in VBox.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toColorSelection obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_color_selection_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_color_selection_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val get_has_opacity_control_ : cptr -> bool
(*  24*)	= _import "gtk_color_selection_get_has_opacity_control"
(*  24*)		  : cptr -> bool;
(*  24*)    val get_has_opacity_control : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_has_opacity_control_ self)
(*  24*)    val set_has_opacity_control_ : cptr * bool -> unit
(*  24*)	= _import "gtk_color_selection_set_has_opacity_control"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_has_opacity_control : 'a t -> bool -> unit
(*  24*)	= fn self => fn has_opacity =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_has_opacity_control_
(*  24*)					   (self, has_opacity))
(*  24*)    val get_has_palette_ : cptr -> bool
(*  24*)	= _import "gtk_color_selection_get_has_palette" : cptr -> bool;
(*  24*)    val get_has_palette : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_has_palette_ self)
(*  24*)    val set_has_palette_ : cptr * bool -> unit
(*  24*)	= _import "gtk_color_selection_set_has_palette"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_has_palette : 'a t -> bool -> unit
(*  24*)	= fn self => fn has_palette =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_has_palette_ (self, has_palette))
(*  24*)    val set_current_alpha_ : cptr * int -> unit
(*  24*)	= _import "gtk_color_selection_set_current_alpha"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_current_alpha : 'a t -> int -> unit
(*  24*)	= fn self => fn alpha =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_current_alpha_ (self, alpha))
(*  24*)    val get_current_alpha_ : cptr -> int
(*  24*)	= _import "gtk_color_selection_get_current_alpha"
(*  24*)		  : cptr -> int;
(*  24*)    val get_current_alpha : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_current_alpha_ self)
(*  24*)    val set_previous_alpha_ : cptr * int -> unit
(*  24*)	= _import "gtk_color_selection_set_previous_alpha"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_previous_alpha : 'a t -> int -> unit
(*  24*)	= fn self => fn alpha =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_previous_alpha_ (self, alpha))
(*  24*)    val get_previous_alpha_ : cptr -> int
(*  24*)	= _import "gtk_color_selection_get_previous_alpha"
(*  24*)		  : cptr -> int;
(*  24*)    val get_previous_alpha : 'a t -> int
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_previous_alpha_ self)
(*  24*)    val is_adjusting_ : cptr -> bool
(*  24*)	= _import "gtk_color_selection_is_adjusting" : cptr -> bool;
(*  24*)    val is_adjusting : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => is_adjusting_ self)
(*  24*)    val set_update_policy_ : cptr * int -> unit
(*  24*)	= _import "gtk_color_selection_set_update_policy"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_update_policy : 'a t -> updatetype -> unit
(*  24*)	= fn self => fn policy =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_update_policy_ (self, policy))
(*  24*)    val dialog_get_type_ : unit -> int
(*  24*)	= _import "gtk_color_selection_dialog_get_type" : unit -> int;
(*  24*)    val dialog_get_type : unit -> int
(*  24*)	= fn dummy => dialog_get_type_ dummy
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val color_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "color-changed" false (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure FontSelection :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a fontselection_t
(*  24*)    type 'a t = 'a fontselection_t VBox.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toFontSelection : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val get_font_name : 'a t -> string
(*  24*)    val set_font_name : 'a t -> string -> bool
(*  24*)    val get_preview_text : 'a t -> string
(*  24*)    val set_preview_text : 'a t -> string -> unit
(*  24*)    val dialog_get_type : unit -> int
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a fontselection_t = unit
(*  24*)    type 'a t = 'a fontselection_t VBox.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in VBox.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toFontSelection obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_font_selection_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_font_selection_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val get_font_name_ : cptr -> CString.t
(*  24*)	= _import "gtk_font_selection_get_font_name"
(*  24*)		  : cptr -> CString.t;
(*  24*)    val get_font_name : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_font_name_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val set_font_name_ : cptr * CString.cstring -> bool
(*  24*)	= _import "gtk_font_selection_set_font_name"
(*  24*)		  : cptr * CString.cstring -> bool;
(*  24*)    val set_font_name : 'a t -> string -> bool
(*  24*)	= fn self => fn fontname =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_font_name_
(*  24*)			     (self, CString.fromString fontname))
(*  24*)    val get_preview_text_ : cptr -> CString.t
(*  24*)	= _import "gtk_font_selection_get_preview_text"
(*  24*)		  : cptr -> CString.t;
(*  24*)    val get_preview_text : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_preview_text_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val set_preview_text_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_font_selection_set_preview_text"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_preview_text : 'a t -> string -> unit
(*  24*)	= fn self => fn text =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_preview_text_
(*  24*)			     (self, CString.fromString text))
(*  24*)    val dialog_get_type_ : unit -> int
(*  24*)	= _import "gtk_font_selection_dialog_get_type" : unit -> int;
(*  24*)    val dialog_get_type : unit -> int
(*  24*)	= fn dummy => dialog_get_type_ dummy
(*  24*)end
(*  24*)structure GammaCurve :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a gammacurve_t
(*  24*)    type 'a t = 'a gammacurve_t VBox.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toGammaCurve : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a gammacurve_t = unit
(*  24*)    type 'a t = 'a gammacurve_t VBox.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in VBox.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toGammaCurve obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_gamma_curve_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_gamma_curve_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)end
(*  24*)structure HBox :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a hbox_t
(*  24*)    type 'a t = 'a hbox_t Box.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toHBox : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : bool option -> int option -> base t
(*  24*)    val new' : unit -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a hbox_t = unit
(*  24*)    type 'a t = 'a hbox_t Box.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Box.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toHBox obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_hbox_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : bool * int -> cptr
(*  24*)	= _import "gtk_hbox_new" : bool * int -> cptr;
(*  24*)    val new : bool option -> int option -> base t
(*  24*)	= fn homogeneous => fn spacing =>
(*  24*)	     make (new_ (getOpt (homogeneous, false), 
(*  24*)			 getOpt (spacing, 0)))
(*  24*)    val new' : unit -> base t = fn dummy => make (new_ (false, 0))
(*  24*)end
(*  24*)structure Statusbar :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a statusbar_t
(*  24*)    type 'a t = 'a statusbar_t HBox.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toStatusbar : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val get_context_id : 'a t -> string -> int
(*  24*)    val push : 'a t -> int -> string -> int
(*  24*)    val pop : 'a t -> int -> unit
(*  24*)    val remove : 'a t -> int -> int -> unit
(*  24*)    val set_has_resize_grip : 'a t -> bool -> unit
(*  24*)    val get_has_resize_grip : 'a t -> bool
(*  24*)    val text_pushed_sig : (int -> char -> unit) -> 'a t Signal.signal
(*  24*)    val text_popped_sig : (int -> char -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a statusbar_t = unit
(*  24*)    type 'a t = 'a statusbar_t HBox.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in HBox.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toStatusbar obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_statusbar_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_statusbar_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val get_context_id_ : cptr * CString.cstring -> int
(*  24*)	= _import "gtk_statusbar_get_context_id"
(*  24*)		  : cptr * CString.cstring -> int;
(*  24*)    val get_context_id : 'a t -> string -> int
(*  24*)	= fn self => fn context_description =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => get_context_id_ (self, 
(*  24*)					    CString.fromString
(*  24*)					      context_description))
(*  24*)    val push_ : cptr * int * CString.cstring -> int
(*  24*)	= _import "gtk_statusbar_push"
(*  24*)		  : cptr * int * CString.cstring -> int;
(*  24*)    val push : 'a t -> int -> string -> int
(*  24*)	= fn self => fn context_id => fn text =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => push_ (self, context_id, 
(*  24*)				  CString.fromString text))
(*  24*)    val pop_ : cptr * int -> unit
(*  24*)	= _import "gtk_statusbar_pop" : cptr * int -> unit;
(*  24*)    val pop : 'a t -> int -> unit
(*  24*)	= fn self => fn context_id =>
(*  24*)	     GObject.withPtr (self, fn self => pop_ (self, context_id))
(*  24*)    val remove_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_statusbar_remove" : cptr * int * int -> unit;
(*  24*)    val remove : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn context_id => fn message_id =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => remove_ (self, context_id, message_id))
(*  24*)    val set_has_resize_grip_ : cptr * bool -> unit
(*  24*)	= _import "gtk_statusbar_set_has_resize_grip"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_has_resize_grip : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_has_resize_grip_ (self, setting))
(*  24*)    val get_has_resize_grip_ : cptr -> bool
(*  24*)	= _import "gtk_statusbar_get_has_resize_grip" : cptr -> bool;
(*  24*)    val get_has_resize_grip : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_has_resize_grip_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in
(*  24*)      val text_pushed_sig : (int -> char -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "text-pushed" false
(*  24*)			   (int --> char --> return_void) f
(*  24*)      val text_popped_sig : (int -> char -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "text-popped" false
(*  24*)			   (int --> char --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Combo :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a combo_t
(*  24*)    type 'a t = 'a combo_t HBox.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toCombo : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val set_value_in_list : 'a t -> bool -> bool -> unit
(*  24*)    val set_use_arrows : 'a t -> bool -> unit
(*  24*)    val set_use_arrows_always : 'a t -> bool -> unit
(*  24*)    val set_case_sensitive : 'a t -> bool -> unit
(*  24*)    val set_item_string : 'a t -> 'b Item.t -> string -> unit
(*  24*)    val disable_activate : 'a t -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a combo_t = unit
(*  24*)    type 'a t = 'a combo_t HBox.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in HBox.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toCombo obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_combo_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr = _import "gtk_combo_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val set_value_in_list_ : cptr * bool * bool -> unit
(*  24*)	= _import "gtk_combo_set_value_in_list"
(*  24*)		  : cptr * bool * bool -> unit;
(*  24*)    val set_value_in_list : 'a t -> bool -> bool -> unit
(*  24*)	= fn self => fn valu => fn ok_if_empty =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_value_in_list_
(*  24*)					   (self, valu, ok_if_empty))
(*  24*)    val set_use_arrows_ : cptr * bool -> unit
(*  24*)	= _import "gtk_combo_set_use_arrows" : cptr * bool -> unit;
(*  24*)    val set_use_arrows : 'a t -> bool -> unit
(*  24*)	= fn self => fn valu =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_use_arrows_ (self, valu))
(*  24*)    val set_use_arrows_always_ : cptr * bool -> unit
(*  24*)	= _import "gtk_combo_set_use_arrows_always"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_use_arrows_always : 'a t -> bool -> unit
(*  24*)	= fn self => fn valu =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_use_arrows_always_ (self, valu))
(*  24*)    val set_case_sensitive_ : cptr * bool -> unit
(*  24*)	= _import "gtk_combo_set_case_sensitive" : cptr * bool -> unit;
(*  24*)    val set_case_sensitive : 'a t -> bool -> unit
(*  24*)	= fn self => fn valu =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_case_sensitive_ (self, valu))
(*  24*)    val set_item_string_ : cptr * cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_combo_set_item_string"
(*  24*)		  : cptr * cptr * CString.cstring -> unit;
(*  24*)    val set_item_string : 'a t -> 'b Item.t -> string -> unit
(*  24*)	= fn self => fn item => fn item_value =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (item, 
(*  24*)			      fn item => set_item_string_
(*  24*)					   (self, item, 
(*  24*)					    CString.fromString
(*  24*)					      item_value)))
(*  24*)    val disable_activate_ : cptr -> unit
(*  24*)	= _import "gtk_combo_disable_activate" : cptr -> unit;
(*  24*)    val disable_activate : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => disable_activate_ self)
(*  24*)end
(*  24*)structure ButtonBox :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a buttonbox_t
(*  24*)    type 'a t = 'a buttonbox_t Box.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toButtonBox : 'a t -> base t
(*  24*)    val set_child_secondary : 'a t -> 'b Widget.t -> bool -> unit
(*  24*)    val set_child_size : 'a t -> int -> int -> unit
(*  24*)    val set_child_ipadding : 'a t -> int -> int -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a buttonbox_t = unit
(*  24*)    type 'a t = 'a buttonbox_t Box.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Box.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toButtonBox obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val set_child_secondary_ : cptr * cptr * bool -> unit
(*  24*)	= _import "gtk_button_box_set_child_secondary"
(*  24*)		  : cptr * cptr * bool -> unit;
(*  24*)    val set_child_secondary : 'a t -> 'b Widget.t -> bool -> unit
(*  24*)	= fn self => fn child => fn is_secondary =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => set_child_secondary_
(*  24*)					    (self, child, 
(*  24*)					     is_secondary)))
(*  24*)    val set_child_size_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_button_box_set_child_size"
(*  24*)		  : cptr * int * int -> unit;
(*  24*)    val set_child_size : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn min_width => fn min_height =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_child_size_
(*  24*)			     (self, min_width, min_height))
(*  24*)    val set_child_ipadding_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_button_box_set_child_ipadding"
(*  24*)		  : cptr * int * int -> unit;
(*  24*)    val set_child_ipadding : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn ipad_x => fn ipad_y =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_child_ipadding_
(*  24*)					   (self, ipad_x, ipad_y))
(*  24*)end
(*  24*)structure VButtonBox :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a vbuttonbox_t
(*  24*)    type 'a t = 'a vbuttonbox_t ButtonBox.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toVButtonBox : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val get_spacing_default : unit -> int
(*  24*)    val set_spacing_default : int -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a vbuttonbox_t = unit
(*  24*)    type 'a t = 'a vbuttonbox_t ButtonBox.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in ButtonBox.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toVButtonBox obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_vbutton_box_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_vbutton_box_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val get_spacing_default_ : unit -> int
(*  24*)	= _import "gtk_vbutton_box_get_spacing_default" : unit -> int;
(*  24*)    val get_spacing_default : unit -> int
(*  24*)	= fn dummy => get_spacing_default_ dummy
(*  24*)    val set_spacing_default_ : int -> unit
(*  24*)	= _import "gtk_vbutton_box_set_spacing_default" : int -> unit;
(*  24*)    val set_spacing_default : int -> unit
(*  24*)	= fn spacing => set_spacing_default_ spacing
(*  24*)end
(*  24*)structure HButtonBox :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a hbuttonbox_t
(*  24*)    type 'a t = 'a hbuttonbox_t ButtonBox.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toHButtonBox : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val get_spacing_default : unit -> int
(*  24*)    val set_spacing_default : int -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a hbuttonbox_t = unit
(*  24*)    type 'a t = 'a hbuttonbox_t ButtonBox.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in ButtonBox.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toHButtonBox obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_hbutton_box_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_hbutton_box_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val get_spacing_default_ : unit -> int
(*  24*)	= _import "gtk_hbutton_box_get_spacing_default" : unit -> int;
(*  24*)    val get_spacing_default : unit -> int
(*  24*)	= fn dummy => get_spacing_default_ dummy
(*  24*)    val set_spacing_default_ : int -> unit
(*  24*)	= _import "gtk_hbutton_box_set_spacing_default" : int -> unit;
(*  24*)    val set_spacing_default : int -> unit
(*  24*)	= fn spacing => set_spacing_default_ spacing
(*  24*)end
(*  24*)structure Calendar :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a calendar_t
(*  24*)    type 'a t = 'a calendar_t Widget.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toCalendar : 'a t -> base t
(*  24*)    type display_options
(*  24*)    val SHOW_HEADING : display_options
(*  24*)    val SHOW_DAY_NAMES : display_options
(*  24*)    val NO_MONTH_CHANGE : display_options
(*  24*)    val SHOW_WEEK_NUMBERS : display_options
(*  24*)    val WEEK_START_MONDAY : display_options
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val select_month : 'a t -> int -> int -> bool
(*  24*)    val select_day : 'a t -> int -> unit
(*  24*)    val mark_day : 'a t -> int -> bool
(*  24*)    val unmark_day : 'a t -> int -> bool
(*  24*)    val clear_marks : 'a t -> unit
(*  24*)    val display_options : 'a t -> display_options list -> unit
(*  24*)    val freeze : 'a t -> unit
(*  24*)    val thaw : 'a t -> unit
(*  24*)    val month_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val day_selected_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val day_selected_double_click_sig
(*  24*)      : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val prev_month_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val next_month_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val prev_year_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val next_year_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a calendar_t = unit
(*  24*)    type 'a t = 'a calendar_t Widget.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Widget.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toCalendar obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    type display_options = int
(*  24*)    val get_display_options_
(*  24*)      : int ref * int ref * int ref * int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_calendar_display_options"
(*  24*)		  : int ref * int ref * int ref * int ref * int ref
(*  24*)		    -> unit;
(*  24*)    val (SHOW_HEADING, SHOW_DAY_NAMES, NO_MONTH_CHANGE, 
(*  24*)	 SHOW_WEEK_NUMBERS, WEEK_START_MONDAY)
(*  24*)	= let val (x0, x1, x2, x3, x4)
(*  24*)		  = (ref 0, ref 0, ref 0, ref 0, ref 0)
(*  24*)	  in get_display_options_ (x0, x1, x2, x3, x4)
(*  24*)	   ; (!x0, !x1, !x2, !x3, !x4)
(*  24*)	  end
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_calendar_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_calendar_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val select_month_ : cptr * int * int -> bool
(*  24*)	= _import "gtk_calendar_select_month"
(*  24*)		  : cptr * int * int -> bool;
(*  24*)    val select_month : 'a t -> int -> int -> bool
(*  24*)	= fn self => fn month => fn year =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => select_month_ (self, month, year))
(*  24*)    val select_day_ : cptr * int -> unit
(*  24*)	= _import "gtk_calendar_select_day" : cptr * int -> unit;
(*  24*)    val select_day : 'a t -> int -> unit
(*  24*)	= fn self => fn day =>
(*  24*)	     GObject.withPtr (self, fn self => select_day_ (self, day))
(*  24*)    val mark_day_ : cptr * int -> bool
(*  24*)	= _import "gtk_calendar_mark_day" : cptr * int -> bool;
(*  24*)    val mark_day : 'a t -> int -> bool
(*  24*)	= fn self => fn day =>
(*  24*)	     GObject.withPtr (self, fn self => mark_day_ (self, day))
(*  24*)    val unmark_day_ : cptr * int -> bool
(*  24*)	= _import "gtk_calendar_unmark_day" : cptr * int -> bool;
(*  24*)    val unmark_day : 'a t -> int -> bool
(*  24*)	= fn self => fn day =>
(*  24*)	     GObject.withPtr (self, fn self => unmark_day_ (self, day))
(*  24*)    val clear_marks_ : cptr -> unit
(*  24*)	= _import "gtk_calendar_clear_marks" : cptr -> unit;
(*  24*)    val clear_marks : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => clear_marks_ self)
(*  24*)    val display_options_ : cptr * int -> unit
(*  24*)	= _import "gtk_calendar_display_options" : cptr * int -> unit;
(*  24*)    val display_options : 'a t -> display_options list -> unit
(*  24*)	= fn self => fn flags =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => display_options_
(*  24*)					   (self, Flags.set flags))
(*  24*)    val freeze_ : cptr -> unit
(*  24*)	= _import "gtk_calendar_freeze" : cptr -> unit;
(*  24*)    val freeze : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => freeze_ self)
(*  24*)    val thaw_ : cptr -> unit
(*  24*)	= _import "gtk_calendar_thaw" : cptr -> unit;
(*  24*)    val thaw : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => thaw_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val month_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "month-changed" false (void --> return_void) f
(*  24*)       val day_selected_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "day-selected" false (void --> return_void) f
(*  24*)       val day_selected_double_click_sig
(*  24*)	 : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "day-selected-double-click" false
(*  24*)			    (void --> return_void) f
(*  24*)       val prev_month_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "prev-month" false (void --> return_void) f
(*  24*)       val next_month_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "next-month" false (void --> return_void) f
(*  24*)       val prev_year_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "prev-year" false (void --> return_void) f
(*  24*)       val next_year_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "next-year" false (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Window :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a window_t
(*  24*)    type 'a t = 'a window_t Bin.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toWindow : 'a t -> base t
(*  24*)    type position
(*  24*)    val WIN_POS_NONE : position
(*  24*)    val WIN_POS_CENTER : position
(*  24*)    val WIN_POS_MOUSE : position
(*  24*)    val WIN_POS_CENTER_ALWAYS : position
(*  24*)    val WIN_POS_CENTER_ON_PARENT : position
(*  24*)    type type_t
(*  24*)    val TOPLEVEL : type_t
(*  24*)    val POPUP : type_t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : type_t option -> base t
(*  24*)    val new' : unit -> base t
(*  24*)    val set_title : 'a t -> string -> unit
(*  24*)    val get_title : 'a t -> string
(*  24*)    val set_wmclass : 'a t -> string -> string -> unit
(*  24*)    val set_role : 'a t -> string -> unit
(*  24*)    val get_role : 'a t -> string
(*  24*)    val add_accelgroup : 'a t -> 'b AccelGroup.t -> unit
(*  24*)    val remove_accelgroup : 'a t -> 'b AccelGroup.t -> unit
(*  24*)    val set_position : 'a t -> position -> unit
(*  24*)    val activate_focus : 'a t -> bool
(*  24*)    val set_focus : 'a t -> 'b Widget.t option -> unit
(*  24*)    val set_focus' : 'a t -> unit
(*  24*)    val get_focus : 'a t -> base Widget.t
(*  24*)    val set_default : 'a t -> 'b Widget.t option -> unit
(*  24*)    val set_default' : 'a t -> unit
(*  24*)    val activate_default : 'a t -> bool
(*  24*)    val set_transient_for : 'a t -> 'b t option -> unit
(*  24*)    val set_transient_for' : 'a t -> unit
(*  24*)    val get_transient_for : 'a t -> base t
(*  24*)    val set_destroy_with_parent : 'a t -> bool -> unit
(*  24*)    val get_destroy_with_parent : 'a t -> bool
(*  24*)    val set_resizable : 'a t -> bool -> unit
(*  24*)    val get_resizable : 'a t -> bool
(*  24*)    val set_has_frame : 'a t -> bool -> unit
(*  24*)    val get_has_frame : 'a t -> bool
(*  24*)    val set_frame_dimensions : 'a t -> int -> int -> int -> int -> unit
(*  24*)    val set_decorated : 'a t -> bool -> unit
(*  24*)    val get_decorated : 'a t -> bool
(*  24*)    val set_modal : 'a t -> bool -> unit
(*  24*)    val get_modal : 'a t -> bool
(*  24*)    val add_mnemonic : 'a t -> int -> 'b Widget.t -> unit
(*  24*)    val remove_mnemonic : 'a t -> int -> 'b Widget.t -> unit
(*  24*)    val present : 'a t -> unit
(*  24*)    val iconify : 'a t -> unit
(*  24*)    val deiconify : 'a t -> unit
(*  24*)    val stick : 'a t -> unit
(*  24*)    val unstick : 'a t -> unit
(*  24*)    val maximize : 'a t -> unit
(*  24*)    val unmaximize : 'a t -> unit
(*  24*)    val begin_move_drag : 'a t -> int -> int -> int -> int -> unit
(*  24*)    val set_policy : 'a t -> int -> int -> int -> unit
(*  24*)    val set_default_size : 'a t -> int -> int -> unit
(*  24*)    val resize : 'a t -> int -> int -> unit
(*  24*)    val move : 'a t -> int -> int -> unit
(*  24*)    val parse_geometry : 'a t -> string -> bool
(*  24*)    val reshow_with_initial_size : 'a t -> unit
(*  24*)    val group_get_type : unit -> int
(*  24*)    val remove_embedded_xid : 'a t -> int -> unit
(*  24*)    val add_embedded_xid : 'a t -> int -> unit
(*  24*)    val set_focus_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val frame_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)    val activate_focus_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val activate_default_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val move_focus_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val keys_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a window_t = unit
(*  24*)    type 'a t = 'a window_t Bin.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Bin.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toWindow obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    type position = int
(*  24*)    val get_position_
(*  24*)      : int ref * int ref * int ref * int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_window_position"
(*  24*)		  : int ref * int ref * int ref * int ref * int ref
(*  24*)		    -> unit;
(*  24*)    val (WIN_POS_NONE, WIN_POS_CENTER, WIN_POS_MOUSE, 
(*  24*)	 WIN_POS_CENTER_ALWAYS, WIN_POS_CENTER_ON_PARENT)
(*  24*)	= let val (x0, x1, x2, x3, x4)
(*  24*)		  = (ref 0, ref 0, ref 0, ref 0, ref 0)
(*  24*)	  in get_position_ (x0, x1, x2, x3, x4)
(*  24*)	   ; (!x0, !x1, !x2, !x3, !x4)
(*  24*)	  end
(*  24*)    type type_t = int
(*  24*)    val get_type_t_ : int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_window_type"
(*  24*)		  : int ref * int ref -> unit;
(*  24*)    val (TOPLEVEL, POPUP) = let val (x0, x1) = (ref 0, ref 0)
(*  24*)			    in get_type_t_ (x0, x1)
(*  24*)			     ; (!x0, !x1)
(*  24*)			    end
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_window_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : int -> cptr = _import "gtk_window_new" : int -> cptr;
(*  24*)    val new : type_t option -> base t
(*  24*)	= fn typ => make (new_ (getOpt (typ, TOPLEVEL)))
(*  24*)    val new' : unit -> base t = fn dummy => make (new_ TOPLEVEL)
(*  24*)    val set_title_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_window_set_title"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_title : 'a t -> string -> unit
(*  24*)	= fn self => fn title =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_title_ (self, CString.fromString title))
(*  24*)    val get_title_ : cptr -> CString.t
(*  24*)	= _import "gtk_window_get_title" : cptr -> CString.t;
(*  24*)    val get_title : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_title_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val set_wmclass_ : cptr * CString.cstring * CString.cstring -> unit
(*  24*)	= _import "gtk_window_set_wmclass"
(*  24*)		  : cptr * CString.cstring * CString.cstring -> unit;
(*  24*)    val set_wmclass : 'a t -> string -> string -> unit
(*  24*)	= fn self => fn wmclass_name => fn wmclass_class =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_wmclass_
(*  24*)			     (self, CString.fromString wmclass_name, 
(*  24*)			      CString.fromString wmclass_class))
(*  24*)    val set_role_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_window_set_role"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_role : 'a t -> string -> unit
(*  24*)	= fn self => fn role =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_role_ (self, CString.fromString role))
(*  24*)    val get_role_ : cptr -> CString.t
(*  24*)	= _import "gtk_window_get_role" : cptr -> CString.t;
(*  24*)    val get_role : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_role_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val add_accelgroup_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_window_add_accel_group" : cptr * cptr -> unit;
(*  24*)    val add_accelgroup : 'a t -> 'b AccelGroup.t -> unit
(*  24*)	= fn self => fn accel_group =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (accel_group, 
(*  24*)			      fn accel_group =>
(*  24*)				 add_accelgroup_ (self, accel_group)))
(*  24*)    val remove_accelgroup_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_window_remove_accel_group"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val remove_accelgroup : 'a t -> 'b AccelGroup.t -> unit
(*  24*)	= fn self => fn accel_group =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr (accel_group, 
(*  24*)					    fn accel_group =>
(*  24*)					       remove_accelgroup_
(*  24*)						 (self, accel_group)))
(*  24*)    val set_position_ : cptr * int -> unit
(*  24*)	= _import "gtk_window_set_position" : cptr * int -> unit;
(*  24*)    val set_position : 'a t -> position -> unit
(*  24*)	= fn self => fn position =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_position_ (self, position))
(*  24*)    val activate_focus_ : cptr -> bool
(*  24*)	= _import "gtk_window_activate_focus" : cptr -> bool;
(*  24*)    val activate_focus : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => activate_focus_ self)
(*  24*)    val set_focus_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_window_set_focus" : cptr * cptr -> unit;
(*  24*)    val set_focus : 'a t -> 'b Widget.t option -> unit
(*  24*)	= fn self => fn focus =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withOpt
(*  24*)		     (focus, fn focus => set_focus_ (self, focus)))
(*  24*)    val set_focus' : 'a t -> unit
(*  24*)	= fn self =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_focus_ (self, GObject.null))
(*  24*)    val get_focus_ : cptr -> cptr
(*  24*)	= _import "gtk_window_get_focus" : cptr -> cptr;
(*  24*)    val get_focus : 'a t -> base Widget.t
(*  24*)	= fn self => Widget.inherit
(*  24*)		       ()
(*  24*)		       (fn () => GObject.withPtr
(*  24*)				   (self, fn self => get_focus_ self))
(*  24*)    val set_default_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_window_set_default" : cptr * cptr -> unit;
(*  24*)    val set_default : 'a t -> 'b Widget.t option -> unit
(*  24*)	= fn self => fn default_widget =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withOpt
(*  24*)			     (default_widget, 
(*  24*)			      fn default_widget =>
(*  24*)				 set_default_ (self, default_widget)))
(*  24*)    val set_default' : 'a t -> unit
(*  24*)	= fn self =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_default_ (self, GObject.null))
(*  24*)    val activate_default_ : cptr -> bool
(*  24*)	= _import "gtk_window_activate_default" : cptr -> bool;
(*  24*)    val activate_default : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => activate_default_ self)
(*  24*)    val set_transient_for_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_window_set_transient_for" : cptr * cptr -> unit;
(*  24*)    val set_transient_for : 'a t -> 'b t option -> unit
(*  24*)	= fn self => fn parent =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withOpt
(*  24*)			     (parent, 
(*  24*)			      fn parent => set_transient_for_
(*  24*)					     (self, parent)))
(*  24*)    val set_transient_for' : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => set_transient_for_
(*  24*)				     (self, GObject.null))
(*  24*)    val get_transient_for_ : cptr -> cptr
(*  24*)	= _import "gtk_window_get_transient_for" : cptr -> cptr;
(*  24*)    val get_transient_for : 'a t -> base t
(*  24*)	= fn self =>
(*  24*)	     make (GObject.withPtr
(*  24*)		     (self, fn self => get_transient_for_ self))
(*  24*)    val set_destroy_with_parent_ : cptr * bool -> unit
(*  24*)	= _import "gtk_window_set_destroy_with_parent"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_destroy_with_parent : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_destroy_with_parent_
(*  24*)					   (self, setting))
(*  24*)    val get_destroy_with_parent_ : cptr -> bool
(*  24*)	= _import "gtk_window_get_destroy_with_parent" : cptr -> bool;
(*  24*)    val get_destroy_with_parent : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_destroy_with_parent_ self)
(*  24*)    val set_resizable_ : cptr * bool -> unit
(*  24*)	= _import "gtk_window_set_resizable" : cptr * bool -> unit;
(*  24*)    val set_resizable : 'a t -> bool -> unit
(*  24*)	= fn self => fn resizable =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_resizable_ (self, resizable))
(*  24*)    val get_resizable_ : cptr -> bool
(*  24*)	= _import "gtk_window_get_resizable" : cptr -> bool;
(*  24*)    val get_resizable : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_resizable_ self)
(*  24*)    val set_has_frame_ : cptr * bool -> unit
(*  24*)	= _import "gtk_window_set_has_frame" : cptr * bool -> unit;
(*  24*)    val set_has_frame : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_has_frame_ (self, setting))
(*  24*)    val get_has_frame_ : cptr -> bool
(*  24*)	= _import "gtk_window_get_has_frame" : cptr -> bool;
(*  24*)    val get_has_frame : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_has_frame_ self)
(*  24*)    val set_frame_dimensions_ : cptr * int * int * int * int -> unit
(*  24*)	= _import "gtk_window_set_frame_dimensions"
(*  24*)		  : cptr * int * int * int * int -> unit;
(*  24*)    val set_frame_dimensions : 'a t -> int -> int -> int -> int -> unit
(*  24*)	= fn self => fn left => fn top => fn right => fn bottom =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_frame_dimensions_
(*  24*)			     (self, left, top, right, bottom))
(*  24*)    val set_decorated_ : cptr * bool -> unit
(*  24*)	= _import "gtk_window_set_decorated" : cptr * bool -> unit;
(*  24*)    val set_decorated : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_decorated_ (self, setting))
(*  24*)    val get_decorated_ : cptr -> bool
(*  24*)	= _import "gtk_window_get_decorated" : cptr -> bool;
(*  24*)    val get_decorated : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_decorated_ self)
(*  24*)    val set_modal_ : cptr * bool -> unit
(*  24*)	= _import "gtk_window_set_modal" : cptr * bool -> unit;
(*  24*)    val set_modal : 'a t -> bool -> unit
(*  24*)	= fn self => fn modal =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_modal_ (self, modal))
(*  24*)    val get_modal_ : cptr -> bool
(*  24*)	= _import "gtk_window_get_modal" : cptr -> bool;
(*  24*)    val get_modal : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr (self, fn self => get_modal_ self)
(*  24*)    val add_mnemonic_ : cptr * int * cptr -> unit
(*  24*)	= _import "gtk_window_add_mnemonic"
(*  24*)		  : cptr * int * cptr -> unit;
(*  24*)    val add_mnemonic : 'a t -> int -> 'b Widget.t -> unit
(*  24*)	= fn self => fn keyval => fn target =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (target, 
(*  24*)			      fn target => add_mnemonic_
(*  24*)					     (self, keyval, target)))
(*  24*)    val remove_mnemonic_ : cptr * int * cptr -> unit
(*  24*)	= _import "gtk_window_remove_mnemonic"
(*  24*)		  : cptr * int * cptr -> unit;
(*  24*)    val remove_mnemonic : 'a t -> int -> 'b Widget.t -> unit
(*  24*)	= fn self => fn keyval => fn target =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (target, 
(*  24*)			      fn target => remove_mnemonic_
(*  24*)					     (self, keyval, target)))
(*  24*)    val present_ : cptr -> unit
(*  24*)	= _import "gtk_window_present" : cptr -> unit;
(*  24*)    val present : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => present_ self)
(*  24*)    val iconify_ : cptr -> unit
(*  24*)	= _import "gtk_window_iconify" : cptr -> unit;
(*  24*)    val iconify : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => iconify_ self)
(*  24*)    val deiconify_ : cptr -> unit
(*  24*)	= _import "gtk_window_deiconify" : cptr -> unit;
(*  24*)    val deiconify : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => deiconify_ self)
(*  24*)    val stick_ : cptr -> unit
(*  24*)	= _import "gtk_window_stick" : cptr -> unit;
(*  24*)    val stick : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => stick_ self)
(*  24*)    val unstick_ : cptr -> unit
(*  24*)	= _import "gtk_window_unstick" : cptr -> unit;
(*  24*)    val unstick : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => unstick_ self)
(*  24*)    val maximize_ : cptr -> unit
(*  24*)	= _import "gtk_window_maximize" : cptr -> unit;
(*  24*)    val maximize : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => maximize_ self)
(*  24*)    val unmaximize_ : cptr -> unit
(*  24*)	= _import "gtk_window_unmaximize" : cptr -> unit;
(*  24*)    val unmaximize : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => unmaximize_ self)
(*  24*)    val begin_move_drag_ : cptr * int * int * int * int -> unit
(*  24*)	= _import "gtk_window_begin_move_drag"
(*  24*)		  : cptr * int * int * int * int -> unit;
(*  24*)    val begin_move_drag : 'a t -> int -> int -> int -> int -> unit
(*  24*)	= fn self => fn button => fn root_x => fn root_y => 
(*  24*)	  fn timestamp =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => begin_move_drag_
(*  24*)					   (self, button, root_x, 
(*  24*)					    root_y, timestamp))
(*  24*)    val set_policy_ : cptr * int * int * int -> unit
(*  24*)	= _import "gtk_window_set_policy"
(*  24*)		  : cptr * int * int * int -> unit;
(*  24*)    val set_policy : 'a t -> int -> int -> int -> unit
(*  24*)	= fn self => fn allow_shrink => fn allow_grow => 
(*  24*)	  fn auto_shrink =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_policy_ (self, allow_shrink, 
(*  24*)					allow_grow, auto_shrink))
(*  24*)    val set_default_size_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_window_set_default_size"
(*  24*)		  : cptr * int * int -> unit;
(*  24*)    val set_default_size : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn width => fn height =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_default_size_
(*  24*)					   (self, width, height))
(*  24*)    val resize_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_window_resize" : cptr * int * int -> unit;
(*  24*)    val resize : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn width => fn height =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => resize_ (self, width, height))
(*  24*)    val move_ : cptr * int * int -> unit
(*  24*)	= _import "gtk_window_move" : cptr * int * int -> unit;
(*  24*)    val move : 'a t -> int -> int -> unit
(*  24*)	= fn self => fn x => fn y =>
(*  24*)	     GObject.withPtr (self, fn self => move_ (self, x, y))
(*  24*)    val parse_geometry_ : cptr * CString.cstring -> bool
(*  24*)	= _import "gtk_window_parse_geometry"
(*  24*)		  : cptr * CString.cstring -> bool;
(*  24*)    val parse_geometry : 'a t -> string -> bool
(*  24*)	= fn self => fn geometry =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => parse_geometry_
(*  24*)			     (self, CString.fromString geometry))
(*  24*)    val reshow_with_initial_size_ : cptr -> unit
(*  24*)	= _import "gtk_window_reshow_with_initial_size" : cptr -> unit;
(*  24*)    val reshow_with_initial_size : 'a t -> unit
(*  24*)	= fn self =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => reshow_with_initial_size_ self)
(*  24*)    val group_get_type_ : unit -> int
(*  24*)	= _import "gtk_window_group_get_type" : unit -> int;
(*  24*)    val group_get_type : unit -> int
(*  24*)	= fn dummy => group_get_type_ dummy
(*  24*)    val remove_embedded_xid_ : cptr * int -> unit
(*  24*)	= _import "gtk_window_remove_embedded_xid"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val remove_embedded_xid : 'a t -> int -> unit
(*  24*)	= fn self => fn xid =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => remove_embedded_xid_ (self, xid))
(*  24*)    val add_embedded_xid_ : cptr * int -> unit
(*  24*)	= _import "gtk_window_add_embedded_xid" : cptr * int -> unit;
(*  24*)    val add_embedded_xid : 'a t -> int -> unit
(*  24*)	= fn self => fn xid =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => add_embedded_xid_ (self, xid))
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in
(*  24*)      val set_focus_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "set-focus" false (unit --> return_void) f
(*  24*)      val frame_event_sig : (unit -> bool) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "frame-event" false (unit --> return_bool) f
(*  24*)      val activate_focus_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "activate-focus" false
(*  24*)			   (void --> return_void) f
(*  24*)      val activate_default_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "activate-default" false
(*  24*)			   (void --> return_void) f
(*  24*)      val move_focus_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f => signal "move-focus" false (unit --> return_void) f
(*  24*)      val keys_changed_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	  = fn f =>
(*  24*)	       signal "keys-changed" false (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Plug :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a plug_t
(*  24*)    type 'a t = 'a plug_t Window.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toPlug : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val embedded_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a plug_t = unit
(*  24*)    type 'a t = 'a plug_t Window.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Window.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toPlug obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_plug_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val embedded_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "embedded" false (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure Dialog :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a dialog_t
(*  24*)    type 'a t = 'a dialog_t Window.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toDialog : 'a t -> base t
(*  24*)    type flags
(*  24*)    val MODAL : flags
(*  24*)    val DESTROY_WITH_PARENT : flags
(*  24*)    val NO_SEPARATOR : flags
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val new_with_buttons : string option -> 'a Window.t option 
(*  24*)			-> flags list option -> string option
(*  24*)			   -> base t
(*  24*)    val new_with_buttons' : unit -> base t
(*  24*)    val add_action_widget : 'a t -> 'b Widget.t -> int -> unit
(*  24*)    val add_button : 'a t -> string -> int -> base Widget.t
(*  24*)    val add_buttons : 'a t -> string -> unit
(*  24*)    val set_response_sensitive : 'a t -> int -> bool -> unit
(*  24*)    val set_default_response : 'a t -> int -> unit
(*  24*)    val set_has_separator : 'a t -> bool -> unit
(*  24*)    val get_has_separator : 'a t -> bool
(*  24*)    val response : 'a t -> int -> unit
(*  24*)    val run : 'a t -> int
(*  24*)    val response_sig : (int -> unit) -> 'a t Signal.signal
(*  24*)    val close_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a dialog_t = unit
(*  24*)    type 'a t = 'a dialog_t Window.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Window.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toDialog obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    type flags = int
(*  24*)    val get_flags_ : int ref * int ref * int ref -> unit
(*  24*)	= _import "mgtk_get_gtk_dialog_flags"
(*  24*)		  : int ref * int ref * int ref -> unit;
(*  24*)    val (MODAL, DESTROY_WITH_PARENT, NO_SEPARATOR)
(*  24*)	= let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
(*  24*)	  in get_flags_ (x0, x1, x2)
(*  24*)	   ; (!x0, !x1, !x2)
(*  24*)	  end
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_dialog_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr = _import "gtk_dialog_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val new_with_buttons_
(*  24*)      : CString.cstring * cptr * int * CString.cstring -> cptr
(*  24*)	= _import "gtk_dialog_new_with_buttons"
(*  24*)		  : CString.cstring * cptr * int * CString.cstring
(*  24*)		    -> cptr;
(*  24*)    val new_with_buttons : string option -> 'a Window.t option 
(*  24*)			-> flags list option -> string option
(*  24*)			   -> base t
(*  24*)	= fn title => fn parent => fn flags => fn first_button_text =>
(*  24*)	     make (GObject.withOpt
(*  24*)		     (parent, 
(*  24*)		      fn parent =>
(*  24*)			 new_with_buttons_
(*  24*)			   (CString.fromString (getOpt (title, "")), 
(*  24*)			    parent, 
(*  24*)			    getOpt (Option.map Flags.set flags, 0), 
(*  24*)			    CString.fromString
(*  24*)			      (getOpt (first_button_text, "")))))
(*  24*)    val new_with_buttons' : unit -> base t
(*  24*)	= fn dummy => make (new_with_buttons_ (CString.fromString "", 
(*  24*)					       GObject.null, 0, 
(*  24*)					       CString.fromString ""))
(*  24*)    val add_action_widget_ : cptr * cptr * int -> unit
(*  24*)	= _import "gtk_dialog_add_action_widget"
(*  24*)		  : cptr * cptr * int -> unit;
(*  24*)    val add_action_widget : 'a t -> 'b Widget.t -> int -> unit
(*  24*)	= fn self => fn child => fn response_id =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (child, 
(*  24*)			      fn child => add_action_widget_
(*  24*)					    (self, child, 
(*  24*)					     response_id)))
(*  24*)    val add_button_ : cptr * CString.cstring * int -> cptr
(*  24*)	= _import "gtk_dialog_add_button"
(*  24*)		  : cptr * CString.cstring * int -> cptr;
(*  24*)    val add_button : 'a t -> string -> int -> base Widget.t
(*  24*)	= fn self => fn button_text => fn response_id =>
(*  24*)	     Widget.inherit
(*  24*)	       ()
(*  24*)	       (fn () => GObject.withPtr
(*  24*)			   (self, 
(*  24*)			    fn self => add_button_ (self, 
(*  24*)						    CString.fromString
(*  24*)						      button_text, 
(*  24*)						    response_id)))
(*  24*)    val add_buttons_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_dialog_add_buttons"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val add_buttons : 'a t -> string -> unit
(*  24*)	= fn self => fn first_button_text =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => add_buttons_ (self, 
(*  24*)					 CString.fromString
(*  24*)					   first_button_text))
(*  24*)    val set_response_sensitive_ : cptr * int * bool -> unit
(*  24*)	= _import "gtk_dialog_set_response_sensitive"
(*  24*)		  : cptr * int * bool -> unit;
(*  24*)    val set_response_sensitive : 'a t -> int -> bool -> unit
(*  24*)	= fn self => fn response_id => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_response_sensitive_
(*  24*)			     (self, response_id, setting))
(*  24*)    val set_default_response_ : cptr * int -> unit
(*  24*)	= _import "gtk_dialog_set_default_response"
(*  24*)		  : cptr * int -> unit;
(*  24*)    val set_default_response : 'a t -> int -> unit
(*  24*)	= fn self => fn response_id =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_default_response_
(*  24*)					   (self, response_id))
(*  24*)    val set_has_separator_ : cptr * bool -> unit
(*  24*)	= _import "gtk_dialog_set_has_separator" : cptr * bool -> unit;
(*  24*)    val set_has_separator : 'a t -> bool -> unit
(*  24*)	= fn self => fn setting =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => set_has_separator_ (self, setting))
(*  24*)    val get_has_separator_ : cptr -> bool
(*  24*)	= _import "gtk_dialog_get_has_separator" : cptr -> bool;
(*  24*)    val get_has_separator : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_has_separator_ self)
(*  24*)    val response_ : cptr * int -> unit
(*  24*)	= _import "gtk_dialog_response" : cptr * int -> unit;
(*  24*)    val response : 'a t -> int -> unit
(*  24*)	= fn self => fn response_id =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, fn self => response_ (self, response_id))
(*  24*)    val run_ : cptr -> int = _import "gtk_dialog_run" : cptr -> int;
(*  24*)    val run : 'a t -> int
(*  24*)	= fn self => GObject.withPtr (self, fn self => run_ self)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val response_sig : (int -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "response" false (int --> return_void) f
(*  24*)       val close_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "close" false (void --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure MessageDialog :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a messagedialog_t
(*  24*)    type 'a t = 'a messagedialog_t Dialog.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toMessageDialog : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a messagedialog_t = unit
(*  24*)    type 'a t = 'a messagedialog_t Dialog.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Dialog.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toMessageDialog obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_message_dialog_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)end
(*  24*)structure InputDialog :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a inputdialog_t
(*  24*)    type 'a t = 'a inputdialog_t Dialog.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toInputDialog : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : unit -> base t
(*  24*)    val enable_device_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)    val disable_device_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a inputdialog_t = unit
(*  24*)    type 'a t = 'a inputdialog_t Dialog.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Dialog.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toInputDialog obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_input_dialog_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_input_dialog_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    local open Signal
(*  24*)	  infixr -->
(*  24*)    in val enable_device_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f =>
(*  24*)		signal "enable-device" false (unit --> return_void) f
(*  24*)       val disable_device_sig : (unit -> unit) -> 'a t Signal.signal
(*  24*)	   = fn f => signal "disable-device" false
(*  24*)			    (unit --> return_void) f
(*  24*)    end
(*  24*)end
(*  24*)structure FontSelectionDialog :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a fontselectiondialog_t
(*  24*)    type 'a t = 'a fontselectiondialog_t Dialog.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toFontSelectionDialog : 'a t -> base t
(*  24*)    val new : string -> base t
(*  24*)    val get_font_name : 'a t -> string
(*  24*)    val set_font_name : 'a t -> string -> bool
(*  24*)    val get_preview_text : 'a t -> string
(*  24*)    val set_preview_text : 'a t -> string -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a fontselectiondialog_t = unit
(*  24*)    type 'a t = 'a fontselectiondialog_t Dialog.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Dialog.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toFontSelectionDialog obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val new_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_font_selection_dialog_new"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val new : string -> base t
(*  24*)	= fn title => make (new_ (CString.fromString title))
(*  24*)    val get_font_name_ : cptr -> CString.t
(*  24*)	= _import "gtk_font_selection_dialog_get_font_name"
(*  24*)		  : cptr -> CString.t;
(*  24*)    val get_font_name : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_font_name_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val set_font_name_ : cptr * CString.cstring -> bool
(*  24*)	= _import "gtk_font_selection_dialog_set_font_name"
(*  24*)		  : cptr * CString.cstring -> bool;
(*  24*)    val set_font_name : 'a t -> string -> bool
(*  24*)	= fn self => fn fontname =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_font_name_
(*  24*)			     (self, CString.fromString fontname))
(*  24*)    val get_preview_text_ : cptr -> CString.t
(*  24*)	= _import "gtk_font_selection_dialog_get_preview_text"
(*  24*)		  : cptr -> CString.t;
(*  24*)    val get_preview_text : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_preview_text_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val set_preview_text_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_font_selection_dialog_set_preview_text"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_preview_text : 'a t -> string -> unit
(*  24*)	= fn self => fn text =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_preview_text_
(*  24*)			     (self, CString.fromString text))
(*  24*)end
(*  24*)structure FileSelection :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a fileselection_t
(*  24*)    type 'a t = 'a fileselection_t Dialog.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toFileSelection : 'a t -> base t
(*  24*)    val get_type : unit -> int
(*  24*)    val new : string option -> base t
(*  24*)    val new' : unit -> base t
(*  24*)    val set_filename : 'a t -> string -> unit
(*  24*)    val get_filename : 'a t -> string
(*  24*)    val complete : 'a t -> string -> unit
(*  24*)    val show_fileop_buttons : 'a t -> unit
(*  24*)    val hide_fileop_buttons : 'a t -> unit
(*  24*)    val set_select_multiple : 'a t -> bool -> unit
(*  24*)    val get_select_multiple : 'a t -> bool
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a fileselection_t = unit
(*  24*)    type 'a t = 'a fileselection_t Dialog.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Dialog.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toFileSelection obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_type_ : unit -> int
(*  24*)	= _import "gtk_file_selection_get_type" : unit -> int;
(*  24*)    val get_type : unit -> int = fn dummy => get_type_ dummy
(*  24*)    val new_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_file_selection_new" : CString.cstring -> cptr;
(*  24*)    val new : string option -> base t
(*  24*)	= fn title => make (new_ (CString.fromString
(*  24*)				    (getOpt (title, ""))))
(*  24*)    val new' : unit -> base t
(*  24*)	= fn dummy => make (new_ (CString.fromString ""))
(*  24*)    val set_filename_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_file_selection_set_filename"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val set_filename : 'a t -> string -> unit
(*  24*)	= fn self => fn filename =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => set_filename_
(*  24*)			     (self, CString.fromString filename))
(*  24*)    val get_filename_ : cptr -> CString.t
(*  24*)	= _import "gtk_file_selection_get_filename"
(*  24*)		  : cptr -> CString.t;
(*  24*)    val get_filename : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = get_filename_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val complete_ : cptr * CString.cstring -> unit
(*  24*)	= _import "gtk_file_selection_complete"
(*  24*)		  : cptr * CString.cstring -> unit;
(*  24*)    val complete : 'a t -> string -> unit
(*  24*)	= fn self => fn pattern =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   complete_ (self, CString.fromString pattern))
(*  24*)    val show_fileop_buttons_ : cptr -> unit
(*  24*)	= _import "gtk_file_selection_show_fileop_buttons"
(*  24*)		  : cptr -> unit;
(*  24*)    val show_fileop_buttons : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => show_fileop_buttons_ self)
(*  24*)    val hide_fileop_buttons_ : cptr -> unit
(*  24*)	= _import "gtk_file_selection_hide_fileop_buttons"
(*  24*)		  : cptr -> unit;
(*  24*)    val hide_fileop_buttons : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => hide_fileop_buttons_ self)
(*  24*)    val set_select_multiple_ : cptr * bool -> unit
(*  24*)	= _import "gtk_file_selection_set_select_multiple"
(*  24*)		  : cptr * bool -> unit;
(*  24*)    val set_select_multiple : 'a t -> bool -> unit
(*  24*)	= fn self => fn select_multiple =>
(*  24*)	     GObject.withPtr (self, 
(*  24*)			      fn self => set_select_multiple_
(*  24*)					   (self, select_multiple))
(*  24*)    val get_select_multiple_ : cptr -> bool
(*  24*)	= _import "gtk_file_selection_get_select_multiple"
(*  24*)		  : cptr -> bool;
(*  24*)    val get_select_multiple : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => get_select_multiple_ self)
(*  24*)end
(*  24*)structure ColorSelectionDialog :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a colorselectiondialog_t
(*  24*)    type 'a t = 'a colorselectiondialog_t Dialog.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toColorSelectionDialog : 'a t -> base t
(*  24*)    val new : string -> base t
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a colorselectiondialog_t = unit
(*  24*)    type 'a t = 'a colorselectiondialog_t Dialog.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in Dialog.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toColorSelectionDialog obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val new_ : CString.cstring -> cptr
(*  24*)	= _import "gtk_color_selection_dialog_new"
(*  24*)		  : CString.cstring -> cptr;
(*  24*)    val new : string -> base t
(*  24*)	= fn title => make (new_ (CString.fromString title))
(*  24*)end
(*  24*)structure WindowGroup :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a windowgroup_t
(*  24*)    type 'a t = 'a windowgroup_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toWindowGroup : 'a t -> base t
(*  24*)    val new : unit -> base t
(*  24*)    val add_window : 'a t -> 'b t -> unit
(*  24*)    val remove_window : 'a t -> 'b t -> unit
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a windowgroup_t = unit
(*  24*)    type 'a t = 'a windowgroup_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toWindowGroup obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val new_ : unit -> cptr
(*  24*)	= _import "gtk_window_group_new" : unit -> cptr;
(*  24*)    val new : unit -> base t = fn dummy => make (new_ dummy)
(*  24*)    val add_window_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_window_group_add_window" : cptr * cptr -> unit;
(*  24*)    val add_window : 'a t -> 'b t -> unit
(*  24*)	= fn self => fn window =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   GObject.withPtr
(*  24*)		     (window, fn window => add_window_ (self, window)))
(*  24*)    val remove_window_ : cptr * cptr -> unit
(*  24*)	= _import "gtk_window_group_remove_window"
(*  24*)		  : cptr * cptr -> unit;
(*  24*)    val remove_window : 'a t -> 'b t -> unit
(*  24*)	= fn self => fn window =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self => GObject.withPtr
(*  24*)			     (window, 
(*  24*)			      fn window => remove_window_
(*  24*)					     (self, window)))
(*  24*)end
(*  24*)structure Clipboard :>
(*  24*)  sig
(*  24*)    type base
(*  24*)    type 'a clipboard_t
(*  24*)    type 'a t = 'a clipboard_t GObject.t
(*  24*)    val inherit : 'a -> GObject.constructor -> 'a t
(*  24*)    val toClipboard : 'a t -> base t
(*  24*)    val get_owner : 'a t -> base GObject.t
(*  24*)    val clear : 'a t -> unit
(*  24*)    val set_text : 'a t -> string -> int -> unit
(*  24*)    val wait_for_text : 'a t -> string
(*  24*)    val wait_is_text_available : 'a t -> bool
(*  24*)  end = struct
(*  24*)    type cptr = GObject.cptr
(*  24*)    type base = unit
(*  24*)    type 'a clipboard_t = unit
(*  24*)    type 'a t = 'a clipboard_t GObject.t
(*  24*)    fun inherit w con = let val con = let val ptr = con ()
(*  24*)				      in fn () => ptr end
(*  24*)			    val witness = ()
(*  24*)			in GObject.inherit witness con end
(*  24*)    fun make ptr = inherit () (fn () => ptr)
(*  24*)    fun toClipboard obj
(*  24*)      = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
(*  24*)    val get_owner_ : cptr -> cptr
(*  24*)	= _import "gtk_clipboard_get_owner" : cptr -> cptr;
(*  24*)    val get_owner : 'a t -> base GObject.t
(*  24*)	= fn self => GObject.inherit
(*  24*)		       ()
(*  24*)		       (fn () => GObject.withPtr
(*  24*)				   (self, fn self => get_owner_ self))
(*  24*)    val clear_ : cptr -> unit
(*  24*)	= _import "gtk_clipboard_clear" : cptr -> unit;
(*  24*)    val clear : 'a t -> unit
(*  24*)	= fn self => GObject.withPtr (self, fn self => clear_ self)
(*  24*)    val set_text_ : cptr * CString.cstring * int -> unit
(*  24*)	= _import "gtk_clipboard_set_text"
(*  24*)		  : cptr * CString.cstring * int -> unit;
(*  24*)    val set_text : 'a t -> string -> int -> unit
(*  24*)	= fn self => fn text => fn len =>
(*  24*)	     GObject.withPtr
(*  24*)	       (self, 
(*  24*)		fn self =>
(*  24*)		   set_text_ (self, CString.fromString text, len))
(*  24*)    val wait_for_text_ : cptr -> CString.t
(*  24*)	= _import "gtk_clipboard_wait_for_text" : cptr -> CString.t;
(*  24*)    val wait_for_text : 'a t -> string
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, 
(*  24*)			fn self => let val t = wait_for_text_ self
(*  24*)				   in CString.toString t end)
(*  24*)    val wait_is_text_available_ : cptr -> bool
(*  24*)	= _import "gtk_clipboard_wait_is_text_available"
(*  24*)		  : cptr -> bool;
(*  24*)    val wait_is_text_available : 'a t -> bool
(*  24*)	= fn self => GObject.withPtr
(*  24*)		       (self, fn self => wait_is_text_available_ self)
(*  24*)end
end
