structure Gtk  = struct
    structure CString :> 
      sig
        type cstring
        val fromString : string -> cstring
    
        type t
        val null : t
        val toString : t -> string
    
        val free : t -> unit
      end = struct
        type cstring = string 
        fun fromString s = s ^ "\000"
    
        type t = MLton.Pointer.t
        val null = MLton.Pointer.null
        val sub = _import "mgtk_stringsub" : t * int -> char;
    
        fun toVector t =
            let fun size i = if sub(t, i) = #"\000" then i
                             else size(i+1)
            in  CharVector.tabulate(size 0, fn i => sub(t, i))
            end
    
        val toString = toVector
    
        val free = _import "free" : t -> unit;
    end
    
    structure GtkBasis :> 
      sig
        val init : string list -> unit
        val main : unit -> unit
        val main_quit : unit -> unit
      end = struct
        structure AS = ArraySlice
        (* Basic GTK stuff *)
        val gtk_init_ = _import "mgtk_init" 
                      : CString.cstring array * int -> unit;
        fun init args = 
    	let val args =
    	        case args of
    		    [] => (print "mGTK Warning: Gtk.init called\
                                      \ with empty list\n";
    			   [CommandLine.name()])
    		  | _  => args
                val argv = Array.fromList(List.map CString.fromString args)
            in  gtk_init_(argv, Array.length argv)
    	end
    
        val main      = _import "gtk_main" : unit -> unit;
        val main_quit = _import "gtk_main_quit" : unit -> unit; 
    end
    
    structure GObject :> 
      sig
        type cptr
        type base
        type 'a t
        type constructor = unit -> cptr
    
        val null : cptr
    
        val withPtr  : 'a t * (cptr -> 'b) -> 'b
        val withOpt  : 'a t option * (cptr -> 'b) -> 'b
        val inherit  : 'a -> constructor -> 'a t
        val toObject : 'a t -> base t
      end = struct
        structure F = MLton.Finalizable
    
        type cptr = MLton.Pointer.t
        type base = unit
    
        (* A litle type cleverness *)
        datatype 'a t = OBJ of cptr F.t
        type constructor = unit -> cptr
    
        val null = MLton.Pointer.null
    
        fun withPtr (OBJ ptr, f) = F.withValue(ptr, f)
        fun withOpt (SOME(OBJ ptr), f) = F.withValue(ptr, f)
          | withOpt (NONE, f)          = F.withValue(F.new null, f)
    
        val object_ref = _import "g_object_ref" : cptr -> cptr;
        val object_unref = _import "g_object_unref" : cptr -> unit;
        
        fun inherit _ con = let val ptr = object_ref(con())
                                val obj = F.new ptr
                            in  F.addFinalizer(obj, object_unref)
                              ; OBJ obj
                            end
    
        fun toObject (OBJ ptr) = OBJ ptr
    end
    
    structure GValue :>
      sig
        type GValues
        type GValue
    
        val int    : int    -> GValue
        val string : string -> GValue
        val real   : real   -> GValue
    
        type 'a setter = GValue -> 'a -> unit
        val setBool   : bool setter
        val setInt    : int setter
        val setChar   : char setter
        val setReal   : real setter
    
        type 'a getter = GValues -> int -> 'a
        val getBool   : bool getter
        val getInt    : int getter
        val getChar   : char getter
        val getReal   : real getter
      end = struct
        type GValues = MLton.Pointer.t
        type GValue = MLton.Pointer.t
    
        fun curry f x y = f(x,y)
    
        val int    : int    -> GValue 
          = _import "mgtk_g_value_set_int"    : int    -> GValue;
        val string : string -> GValue 
          = _import "mgtk_g_value_set_string" : string -> GValue;
        val real   : real   -> GValue 
          = _import "mgtk_g_value_set_real"   : real   -> GValue;
    
        (* UNSAFE: no error checking in the set and get functions! *)
        type 'a setter_ = GValue * 'a -> unit
        type 'a setter = GValue -> 'a -> unit
        val setBool = _import "g_value_set_boolean" : bool setter_;
        val setBool = curry setBool
        val setInt  = _import "g_value_set_long" : int setter_;  
        val setInt  = curry setInt
        val setChar = _import "g_value_set_char" : char setter_;
        val setChar = curry setChar
        val setReal = _import "g_value_set_double" : real setter_;
        val setReal = curry setReal
    
        type 'a getter_ = GValues * int -> 'a
        type 'a getter = GValues -> int -> 'a
        val getBool = _import "mgtk_get_pos_bool" : bool getter_;
        val getBool = curry getBool
        val getInt  = _import "mgtk_get_pos_int"  : int getter_;
        val getInt  = curry getInt 
        val getChar = _import "mgtk_get_pos_char" : char getter_;
        val getChar = curry getChar
        val getReal = _import "mgtk_get_pos_real" : real getter_;
        val getReal = curry getReal
    
    (*
            val getLong   : int getter    = app2(symb "mgtk_get_pos_long")
            val getChar   : char getter   = app2(symb "mgtk_get_pos_char")
            val getString : string getter = app2(symb "mgtk_get_pos_string")
    *)
    
    end
    
    structure Signal :> 
      sig
        type state
        type 'a t = 'a GObject.t
    
    
        type ('a, 'b) trans   = 'a * state -> 'b * state
        type ('a, 'rest) read = ('a -> 'rest, 'rest) trans
        type 'a return        = ('a, unit) trans
    
        val bool   : (bool,   'rest) read
        val int    : (int,    'rest) read
        val char   : (char,   'rest) read
        val real   : (real,   'rest) read
        val unit   : (unit,   'rest) read
    
        val void : (unit, 'rest) read
    
        val return_bool   : bool   return
        val return_int    : int    return
        val return_char   : char   return
        val return_real   : real   return
        val return_unit   : unit   return
    
        val return_void : unit return
    
        val --> : ('a, 'b) read * ('b, 'c) trans -> ('a -> 'b, 'c) trans 
    
        type 'a signal
        type signal_id
        val signal  : string -> bool -> ('b -> 'c) return -> ('b -> 'c) ->
                                                      'a t signal
        val connect : 'a t -> 'a t signal -> signal_id
      end = struct
    
        type 'a t = 'a GObject.t
        local
            structure GO = GObject
            structure CT = Callbacktable
    
            open GValue
    
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
    	    val localId
    		= fn f => f (!intern before intern := !intern + 1)
    	end
    
    	val dispatch : callback_id * GValue * GValues * int -> unit =
                fn (id, value, values, size) =>
    	    case peek id of
    		SOME f => f (value, values, size)   
                  (* FIXME: we need a handle here, but what 
                            should it do 
                   *)
    	      | NONE   => 
    		  ( print ("mgtk: Unknown callback function (id: "^
    			         Int.toString id^")\n")
                      ; TextIO.flushOut TextIO.stdOut)
    
    	val _ = _export "mgtk_callback_dispatch_smlside" 
                            : callback_id * GValue * GValues * int -> unit;
                    dispatch
            val _ = _export "mgtk_callback_destroy_smlside"
                             : callback_id -> unit; 
                    destroy
    		
    
    	fun register f = localId(fn id => (add (id, f); id))
    	val signal_connect 
    	    = _import "mgtk_signal_connect"
                  : GO.cptr * CString.cstring * int * bool -> int;
        in
        datatype state = S of GValue * GValues * int * int
        type ('a, 'b) trans   = 'a * state -> 'b * state
        type ('a, 'rest) read = ('a -> 'rest, 'rest) trans
        type 'a return        = ('a, unit) trans
    
        fun state f ret arg max = (f, S(ret, arg, max, 0+1))
        (* NOTE: the +1 is for the object connected to *)
    
        fun wrap conv f (ret, arg, max) = ignore(conv(state f ret arg max)) 
    
        fun getter get (f, S(ret, arg, max, next)) = 
            if next < max  (* FIXME: it should be < but that gives problems
                              with return_unit.  Currently unsafe! *)
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
    
        fun real x        = getter getReal x
        fun return_real x = setter setReal x
    
        
        (* FIXME: convince Ken that this correct *)
        fun void (f, state) = (f(), state)
        fun return_void (f, S(ret, arg, max, next)) = 
    	(f, S(ret, arg,max+1,next))
    
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
            in  GO.withPtr(wid, fn wid=>signal_connect(wid,csign,id,after))
            end
    
        end	
    end
    
    structure Flags :> 
      sig
        val setGeneral : int -> int list -> int list -> int 
        val set : int list -> int
        val get : int -> int list
        val isSet : int list -> int -> int list
        val areTheseSet : int list -> int -> bool
      end = struct
        (* convert a list of flags to a word *)
        infix orb andb
        val notb = Word.notb 
        val op orb = Word.orb 
        val op andb = Word.andb
        val W = Word.fromInt
        fun set(f,res) = W f orb res
        fun unset(f, res) = notb(W f) andb res
    
        fun setGeneral init pos neg =
    	let val res = List.foldl set (W init) pos
    	    val res = List.foldl unset res neg
    	in  Word.toInt res
    	end
    
        (* we only need to set flags from the no flag *)
        val set = fn pos =>
    	let val res = List.foldl set 0w0 pos
    	in  Word.toInt res
    	end
    
        (* Checks which flags from possible is set in flag *)
        fun isSet possible flag =
    	let val f = W flag
    	    fun isIn pos = (f andb (W pos)) <> 0w0
    	in  List.filter isIn possible
    	end
    
        fun get f =
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
    
        fun areTheseSet flags flag = 
    	((W(set flags)) andb (W flag)) <> 0w0
    end
    
    structure GType :>
      sig
        type t
        val int    : t
        val real   : t
        val string : t
    
        val toInt : t -> int
        val toString : t -> string
      end = struct
    
        type t = int
    
        val int    = _import "mgtk_g_type_int"    : unit -> int;
        val int    = int ()
        val real   = _import "mgtk_g_type_real"   : unit -> int;
        val real   = real ()
        val string = _import "mgtk_g_type_string" : unit -> int;
        val string = string ()
    
        val toInt = fn t => t
        val toString : t -> CString.t 
            = _import "mgtk_g_type_name" : int -> CString.t;
        val toString = CString.toString o toString
    
    end
    type cptr = GObject.cptr
    type anchortype = int
    val get_anchortype_ : int ref * int ref * int ref * int ref * int ref 
			* int ref * int ref * int ref * int ref * int ref 
			* int ref * int ref * int ref * int ref * int ref 
			* int ref * int ref
			  -> unit
	= _import "mgtk_get_gtk_anchortype"
		  : int ref * int ref * int ref * int ref * int ref * int ref 
		  * int ref * int ref * int ref * int ref * int ref * int ref 
		  * int ref * int ref * int ref * int ref * int ref
		    -> unit;
    val (ANCHOR_CENTER, ANCHOR_NORTH, ANCHOR_NORTH_WEST, ANCHOR_NORTH_EAST, 
	 ANCHOR_SOUTH, ANCHOR_SOUTH_WEST, ANCHOR_SOUTH_EAST, ANCHOR_WEST, 
	 ANCHOR_EAST, ANCHOR_N, ANCHOR_NW, ANCHOR_NE, ANCHOR_S, ANCHOR_SW, 
	 ANCHOR_SE, ANCHOR_W, ANCHOR_E)
	= let val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, 
		   x14, x15, x16)
		  = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
		     ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
		     ref 0)
	  in get_anchortype_ (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, 
			      x11, x12, x13, x14, x15, x16)
	   ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6, !x7, !x8, !x9, !x10, !x11, 
	      !x12, !x13, !x14, !x15, !x16)
	  end
    type arrowtype = int
    val get_arrowtype_ : int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_arrowtype"
		  : int ref * int ref * int ref * int ref -> unit;
    val (ARROW_UP, ARROW_DOWN, ARROW_LEFT, ARROW_RIGHT)
	= let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	  in get_arrowtype_ (x0, x1, x2, x3)
	   ; (!x0, !x1, !x2, !x3)
	  end
    type buttonstype = int
    val get_buttonstype_
      : int ref * int ref * int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_buttonstype"
		  : int ref * int ref * int ref * int ref * int ref * int ref
		    -> unit;
    val (BUTTONS_NONE, BUTTONS_OK, BUTTONS_CLOSE, BUTTONS_CANCEL, 
	 BUTTONS_YES_NO, BUTTONS_OK_CANCEL)
	= let val (x0, x1, x2, x3, x4, x5)
		  = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_buttonstype_ (x0, x1, x2, x3, x4, x5)
	   ; (!x0, !x1, !x2, !x3, !x4, !x5)
	  end
    type celltype = int
    val get_celltype_ : int ref * int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_celltype"
		  : int ref * int ref * int ref * int ref * int ref -> unit;
    val (CELL_EMPTY, CELL_TEXT, CELL_PIXMAP, CELL_PIXTEXT, CELL_WIDGET)
	= let val (x0, x1, x2, x3, x4) = (ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_celltype_ (x0, x1, x2, x3, x4)
	   ; (!x0, !x1, !x2, !x3, !x4)
	  end
    type cornertype = int
    val get_cornertype_ : int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_cornertype"
		  : int ref * int ref * int ref * int ref -> unit;
    val (CORNER_TOP_LEFT, CORNER_BOTTOM_LEFT, CORNER_TOP_RIGHT, 
	 CORNER_BOTTOM_RIGHT)
	= let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	  in get_cornertype_ (x0, x1, x2, x3)
	   ; (!x0, !x1, !x2, !x3)
	  end
    type curvetype = int
    val get_curvetype_ : int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_curvetype"
		  : int ref * int ref * int ref -> unit;
    val (CURVE_TYPE_LINEAR, CURVE_TYPE_SPLINE, CURVE_TYPE_FREE)
	= let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
	  in get_curvetype_ (x0, x1, x2)
	   ; (!x0, !x1, !x2)
	  end
    type deletetype = int
    val get_deletetype_ : int ref * int ref * int ref * int ref * int ref 
			* int ref * int ref * int ref
			  -> unit
	= _import "mgtk_get_gtk_deletetype" : int ref * int ref * int ref 
					    * int ref * int ref * int ref 
					    * int ref * int ref
					      -> unit;
    val (DELETE_CHARS, DELETE_WORD_ENDS, DELETE_WORDS, DELETE_DISPLAY_LINES, 
	 DELETE_DISPLAY_LINE_ENDS, DELETE_PARAGRAPH_ENDS, DELETE_PARAGRAPHS, 
	 DELETE_WHITESPACE)
	= let val (x0, x1, x2, x3, x4, x5, x6, x7)
		  = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_deletetype_ (x0, x1, x2, x3, x4, x5, x6, x7)
	   ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6, !x7)
	  end
    type directiontype = int
    val get_directiontype_
      : int ref * int ref * int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_directiontype"
		  : int ref * int ref * int ref * int ref * int ref * int ref
		    -> unit;
    val (DIR_TAB_FORWARD, DIR_TAB_BACKWARD, DIR_UP, DIR_DOWN, DIR_LEFT, 
	 DIR_RIGHT)
	= let val (x0, x1, x2, x3, x4, x5)
		  = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_directiontype_ (x0, x1, x2, x3, x4, x5)
	   ; (!x0, !x1, !x2, !x3, !x4, !x5)
	  end
    type icon_size = int
    val get_icon_size_ : int ref * int ref * int ref * int ref * int ref 
		       * int ref * int ref
			 -> unit
	= _import "mgtk_get_gtk_icon_size" : int ref * int ref * int ref 
					   * int ref * int ref * int ref 
					   * int ref
					     -> unit;
    val (ICON_SIZE_INVALID, ICON_SIZE_MENU, ICON_SIZE_SMALL_TOOLBAR, 
	 ICON_SIZE_LARGE_TOOLBAR, ICON_SIZE_BUTTON, ICON_SIZE_DND, 
	 ICON_SIZE_DIALOG)
	= let val (x0, x1, x2, x3, x4, x5, x6)
		  = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_icon_size_ (x0, x1, x2, x3, x4, x5, x6)
	   ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6)
	  end
    type imagetype = int
    val get_imagetype_ : int ref * int ref * int ref * int ref * int ref 
		       * int ref * int ref
			 -> unit
	= _import "mgtk_get_gtk_imagetype" : int ref * int ref * int ref 
					   * int ref * int ref * int ref 
					   * int ref
					     -> unit;
    val (IMAGE_EMPTY, IMAGE_PIXMAP, IMAGE_IMAGE, IMAGE_PIXBUF, IMAGE_STOCK, 
	 IMAGE_ICON_SET, IMAGE_ANIMATION)
	= let val (x0, x1, x2, x3, x4, x5, x6)
		  = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_imagetype_ (x0, x1, x2, x3, x4, x5, x6)
	   ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6)
	  end
    type im_preedit_style = int
    val get_im_preedit_style_ : int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_im_preedit_style"
		  : int ref * int ref * int ref -> unit;
    val (IM_PREEDIT_NOTHING, IM_PREEDIT_CALLBACK, IM_PREEDIT_NONE)
	= let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
	  in get_im_preedit_style_ (x0, x1, x2)
	   ; (!x0, !x1, !x2)
	  end
    type im_status_style = int
    val get_im_status_style_ : int ref * int ref -> unit
	= _import "mgtk_get_gtk_im_status_style" : int ref * int ref -> unit;
    val (IM_STATUS_NOTHING, IM_STATUS_CALLBACK)
	= let val (x0, x1) = (ref 0, ref 0) in get_im_status_style_ (x0, x1)
					     ; (!x0, !x1)
					    end
    type justification = int
    val get_justification_ : int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_justification"
		  : int ref * int ref * int ref * int ref -> unit;
    val (JUSTIFY_LEFT, JUSTIFY_RIGHT, JUSTIFY_CENTER, JUSTIFY_FILL)
	= let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	  in get_justification_ (x0, x1, x2, x3)
	   ; (!x0, !x1, !x2, !x3)
	  end
    type matchtype = int
    val get_matchtype_
      : int ref * int ref * int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_matchtype"
		  : int ref * int ref * int ref * int ref * int ref * int ref
		    -> unit;
    val (MATCH_ALL, MATCH_ALL_TAIL, MATCH_HEAD, MATCH_TAIL, MATCH_EXACT, 
	 MATCH_LAST)
	= let val (x0, x1, x2, x3, x4, x5)
		  = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_matchtype_ (x0, x1, x2, x3, x4, x5)
	   ; (!x0, !x1, !x2, !x3, !x4, !x5)
	  end
    type messagetype = int
    val get_messagetype_ : int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_messagetype"
		  : int ref * int ref * int ref * int ref -> unit;
    val (MESSAGE_INFO, MESSAGE_WARNING, MESSAGE_QUESTION, MESSAGE_ERROR)
	= let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	  in get_messagetype_ (x0, x1, x2, x3)
	   ; (!x0, !x1, !x2, !x3)
	  end
    type metrictype = int
    val get_metrictype_ : int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_metrictype"
		  : int ref * int ref * int ref -> unit;
    val (PIXELS, INCHES, CENTIMETERS)
	= let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
	  in get_metrictype_ (x0, x1, x2)
	   ; (!x0, !x1, !x2)
	  end
    type movement_step = int
    val get_movement_step_ : int ref * int ref * int ref * int ref * int ref 
			   * int ref * int ref * int ref * int ref * int ref
			     -> unit
	= _import "mgtk_get_gtk_movement_step"
		  : int ref * int ref * int ref * int ref * int ref * int ref 
		  * int ref * int ref * int ref * int ref
		    -> unit;
    val (MOVEMENT_LOGICAL_POSITIONS, MOVEMENT_VISUAL_POSITIONS, 
	 MOVEMENT_WORDS, MOVEMENT_DISPLAY_LINES, MOVEMENT_DISPLAY_LINE_ENDS, 
	 MOVEMENT_PARAGRAPHS, MOVEMENT_PARAGRAPH_ENDS, MOVEMENT_PAGES, 
	 MOVEMENT_BUFFER_ENDS, MOVEMENT_HORIZONTAL_PAGES)
	= let val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9)
		  = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
		     ref 0, ref 0)
	  in get_movement_step_ (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9)
	   ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6, !x7, !x8, !x9)
	  end
    type orientation = int
    val get_orientation_ : int ref * int ref -> unit
	= _import "mgtk_get_gtk_orientation" : int ref * int ref -> unit;
    val (ORIENTATION_HORIZONTAL, ORIENTATION_VERTICAL)
	= let val (x0, x1) = (ref 0, ref 0) in get_orientation_ (x0, x1)
					     ; (!x0, !x1)
					    end
    type packtype = int
    val get_packtype_ : int ref * int ref -> unit
	= _import "mgtk_get_gtk_packtype" : int ref * int ref -> unit;
    val (PACK_START, PACK_END) = let val (x0, x1) = (ref 0, ref 0)
				 in get_packtype_ (x0, x1)
				  ; (!x0, !x1)
				 end
    type path_prioritytype = int
    val get_path_prioritytype_
      : int ref * int ref * int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_path_prioritytype"
		  : int ref * int ref * int ref * int ref * int ref * int ref
		    -> unit;
    val (PATH_PRIO_LOWEST, PATH_PRIO_GTK, PATH_PRIO_APPLICATION, 
	 PATH_PRIO_THEME, PATH_PRIO_RC, PATH_PRIO_HIGHEST)
	= let val (x0, x1, x2, x3, x4, x5)
		  = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_path_prioritytype_ (x0, x1, x2, x3, x4, x5)
	   ; (!x0, !x1, !x2, !x3, !x4, !x5)
	  end
    type pathtype = int
    val get_pathtype_ : int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_pathtype"
		  : int ref * int ref * int ref -> unit;
    val (PATH_WIDGET, PATH_WIDGET_CLASS, PATH_CLASS)
	= let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
	  in get_pathtype_ (x0, x1, x2)
	   ; (!x0, !x1, !x2)
	  end
    type policytype = int
    val get_policytype_ : int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_policytype"
		  : int ref * int ref * int ref -> unit;
    val (POLICY_ALWAYS, POLICY_AUTOMATIC, POLICY_NEVER)
	= let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
	  in get_policytype_ (x0, x1, x2)
	   ; (!x0, !x1, !x2)
	  end
    type positiontype = int
    val get_positiontype_ : int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_positiontype"
		  : int ref * int ref * int ref * int ref -> unit;
    val (POS_LEFT, POS_RIGHT, POS_TOP, POS_BOTTOM)
	= let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	  in get_positiontype_ (x0, x1, x2, x3)
	   ; (!x0, !x1, !x2, !x3)
	  end
    type previewtype = int
    val get_previewtype_ : int ref * int ref -> unit
	= _import "mgtk_get_gtk_previewtype" : int ref * int ref -> unit;
    val (PREVIEW_COLOR, PREVIEW_GRAYSCALE)
	= let val (x0, x1) = (ref 0, ref 0) in get_previewtype_ (x0, x1)
					     ; (!x0, !x1)
					    end
    type rc_tokentype = int
    val get_rc_tokentype_
      : int ref * int ref * int ref * int ref * int ref * int ref * int ref 
      * int ref * int ref * int ref * int ref * int ref * int ref * int ref 
      * int ref * int ref * int ref * int ref * int ref * int ref * int ref 
      * int ref * int ref * int ref * int ref * int ref * int ref * int ref 
      * int ref * int ref * int ref * int ref * int ref * int ref * int ref 
      * int ref * int ref * int ref
	-> unit
	= _import "mgtk_get_gtk_rc_tokentype"
		  : int ref * int ref * int ref * int ref * int ref * int ref 
		  * int ref * int ref * int ref * int ref * int ref * int ref 
		  * int ref * int ref * int ref * int ref * int ref * int ref 
		  * int ref * int ref * int ref * int ref * int ref * int ref 
		  * int ref * int ref * int ref * int ref * int ref * int ref 
		  * int ref * int ref * int ref * int ref * int ref * int ref 
		  * int ref * int ref
		    -> unit;
    val
      (RC_TOKEN_INVALID, RC_TOKEN_INCLUDE, RC_TOKEN_NORMAL, RC_TOKEN_ACTIVE, 
       RC_TOKEN_PRELIGHT, RC_TOKEN_SELECTED, RC_TOKEN_INSENSITIVE, 
       RC_TOKEN_FG, RC_TOKEN_BG, RC_TOKEN_TEXT, RC_TOKEN_BASE, 
       RC_TOKEN_XTHICKNESS, RC_TOKEN_YTHICKNESS, RC_TOKEN_FONT, 
       RC_TOKEN_FONTSET, RC_TOKEN_FONT_NAME, RC_TOKEN_BG_PIXMAP, 
       RC_TOKEN_PIXMAP_PATH, RC_TOKEN_STYLE, RC_TOKEN_BINDING, RC_TOKEN_BIND, 
       RC_TOKEN_WIDGET, RC_TOKEN_WIDGET_CLASS, RC_TOKEN_CLASS, 
       RC_TOKEN_LOWEST, RC_TOKEN_GTK, RC_TOKEN_APPLICATION, RC_TOKEN_THEME, 
       RC_TOKEN_RC, RC_TOKEN_HIGHEST, RC_TOKEN_ENGINE, RC_TOKEN_MODULE_PATH, 
       RC_TOKEN_IM_MODULE_PATH, RC_TOKEN_IM_MODULE_FILE, RC_TOKEN_STOCK, 
       RC_TOKEN_LTR, RC_TOKEN_RTL, RC_TOKEN_LAST)
	= let val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, 
		   x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, 
		   x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37)
		  = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
		     ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
		     ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
		     ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
		     ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_rc_tokentype_
	       (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, 
		x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, 
		x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37)
	   ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6, !x7, !x8, !x9, !x10, !x11, 
	      !x12, !x13, !x14, !x15, !x16, !x17, !x18, !x19, !x20, !x21, 
	      !x22, !x23, !x24, !x25, !x26, !x27, !x28, !x29, !x30, !x31, 
	      !x32, !x33, !x34, !x35, !x36, !x37)
	  end
    type relief_style = int
    val get_relief_style_ : int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_relief_style"
		  : int ref * int ref * int ref -> unit;
    val (RELIEF_NORMAL, RELIEF_HALF, RELIEF_NONE)
	= let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
	  in get_relief_style_ (x0, x1, x2)
	   ; (!x0, !x1, !x2)
	  end
    type resize_mode = int
    val get_resize_mode_ : int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_resize_mode"
		  : int ref * int ref * int ref -> unit;
    val (RESIZE_PARENT, RESIZE_QUEUE, RESIZE_IMMEDIATE)
	= let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
	  in get_resize_mode_ (x0, x1, x2)
	   ; (!x0, !x1, !x2)
	  end
    type responsetype = int
    val get_responsetype_
      : int ref * int ref * int ref * int ref * int ref * int ref * int ref 
      * int ref * int ref * int ref * int ref
	-> unit
	= _import "mgtk_get_gtk_responsetype"
		  : int ref * int ref * int ref * int ref * int ref * int ref 
		  * int ref * int ref * int ref * int ref * int ref
		    -> unit;
    val (RESPONSE_NONE, RESPONSE_REJECT, RESPONSE_ACCEPT, 
	 RESPONSE_DELETE_EVENT, RESPONSE_OK, RESPONSE_CANCEL, RESPONSE_CLOSE, 
	 RESPONSE_YES, RESPONSE_NO, RESPONSE_APPLY, RESPONSE_HELP)
	= let val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
		  = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
		     ref 0, ref 0, ref 0)
	  in get_responsetype_ (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
	   ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6, !x7, !x8, !x9, !x10)
	  end
    type scroll_step = int
    val get_scroll_step_
      : int ref * int ref * int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_scroll_step"
		  : int ref * int ref * int ref * int ref * int ref * int ref
		    -> unit;
    val (SCROLL_STEPS, SCROLL_PAGES, SCROLL_ENDS, SCROLL_HORIZONTAL_STEPS, 
	 SCROLL_HORIZONTAL_PAGES, SCROLL_HORIZONTAL_ENDS)
	= let val (x0, x1, x2, x3, x4, x5)
		  = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_scroll_step_ (x0, x1, x2, x3, x4, x5)
	   ; (!x0, !x1, !x2, !x3, !x4, !x5)
	  end
    type scrolltype = int
    val get_scrolltype_ : int ref * int ref * int ref * int ref * int ref 
			* int ref * int ref * int ref * int ref * int ref 
			* int ref * int ref * int ref * int ref * int ref 
			* int ref
			  -> unit
	= _import "mgtk_get_gtk_scrolltype"
		  : int ref * int ref * int ref * int ref * int ref * int ref 
		  * int ref * int ref * int ref * int ref * int ref * int ref 
		  * int ref * int ref * int ref * int ref
		    -> unit;
    val (SCROLL_NONE, SCROLL_JUMP, SCROLL_STEP_BACKWARD, SCROLL_STEP_FORWARD, 
	 SCROLL_PAGE_BACKWARD, SCROLL_PAGE_FORWARD, SCROLL_STEP_UP, 
	 SCROLL_STEP_DOWN, SCROLL_PAGE_UP, SCROLL_PAGE_DOWN, SCROLL_STEP_LEFT, 
	 SCROLL_STEP_RIGHT, SCROLL_PAGE_LEFT, SCROLL_PAGE_RIGHT, SCROLL_START, 
	 SCROLL_END)
	= let val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, 
		   x14, x15)
		  = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
		     ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_scrolltype_ (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, 
			      x11, x12, x13, x14, x15)
	   ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6, !x7, !x8, !x9, !x10, !x11, 
	      !x12, !x13, !x14, !x15)
	  end
    type selection_mode = int
    val get_selection_mode_
      : int ref * int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_selection_mode"
		  : int ref * int ref * int ref * int ref * int ref -> unit;
    val (SELECTION_NONE, SELECTION_SINGLE, SELECTION_BROWSE, 
	 SELECTION_MULTIPLE, SELECTION_EXTENDED)
	= let val (x0, x1, x2, x3, x4) = (ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_selection_mode_ (x0, x1, x2, x3, x4)
	   ; (!x0, !x1, !x2, !x3, !x4)
	  end
    type shadowtype = int
    val get_shadowtype_
      : int ref * int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_shadowtype"
		  : int ref * int ref * int ref * int ref * int ref -> unit;
    val (SHADOW_NONE, SHADOW_IN, SHADOW_OUT, SHADOW_ETCHED_IN, 
	 SHADOW_ETCHED_OUT)
	= let val (x0, x1, x2, x3, x4) = (ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_shadowtype_ (x0, x1, x2, x3, x4)
	   ; (!x0, !x1, !x2, !x3, !x4)
	  end
    type sidetype = int
    val get_sidetype_ : int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_sidetype"
		  : int ref * int ref * int ref * int ref -> unit;
    val (SIDE_TOP, SIDE_BOTTOM, SIDE_LEFT, SIDE_RIGHT)
	= let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	  in get_sidetype_ (x0, x1, x2, x3)
	   ; (!x0, !x1, !x2, !x3)
	  end
    type sorttype = int
    val get_sorttype_ : int ref * int ref -> unit
	= _import "mgtk_get_gtk_sorttype" : int ref * int ref -> unit;
    val (SORT_ASCENDING, SORT_DESCENDING)
	= let val (x0, x1) = (ref 0, ref 0) in get_sorttype_ (x0, x1)
					     ; (!x0, !x1)
					    end
    type spintype = int
    val get_spintype_ : int ref * int ref * int ref * int ref * int ref 
		      * int ref * int ref
			-> unit
	= _import "mgtk_get_gtk_spintype" : int ref * int ref * int ref 
					  * int ref * int ref * int ref 
					  * int ref
					    -> unit;
    val (SPIN_STEP_FORWARD, SPIN_STEP_BACKWARD, SPIN_PAGE_FORWARD, 
	 SPIN_PAGE_BACKWARD, SPIN_HOME, SPIN_END, SPIN_USER_DEFINED)
	= let val (x0, x1, x2, x3, x4, x5, x6)
		  = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_spintype_ (x0, x1, x2, x3, x4, x5, x6)
	   ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6)
	  end
    type statetype = int
    val get_statetype_
      : int ref * int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_statetype"
		  : int ref * int ref * int ref * int ref * int ref -> unit;
    val (STATE_NORMAL, STATE_ACTIVE, STATE_PRELIGHT, STATE_SELECTED, 
	 STATE_INSENSITIVE)
	= let val (x0, x1, x2, x3, x4) = (ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_statetype_ (x0, x1, x2, x3, x4)
	   ; (!x0, !x1, !x2, !x3, !x4)
	  end
    type submenu_direction = int
    val get_submenu_direction_ : int ref * int ref -> unit
	= _import "mgtk_get_gtk_submenu_direction" : int ref * int ref -> unit;
    val (DIRECTION_LEFT, DIRECTION_RIGHT)
	= let val (x0, x1) = (ref 0, ref 0) in get_submenu_direction_ (x0, x1)
					     ; (!x0, !x1)
					    end
    type submenu_placement = int
    val get_submenu_placement_ : int ref * int ref -> unit
	= _import "mgtk_get_gtk_submenu_placement" : int ref * int ref -> unit;
    val (TOP_BOTTOM, LEFT_RIGHT) = let val (x0, x1) = (ref 0, ref 0)
				   in get_submenu_placement_ (x0, x1)
				    ; (!x0, !x1)
				   end
    type text_direction = int
    val get_text_direction_ : int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_text_direction"
		  : int ref * int ref * int ref -> unit;
    val (TEXT_DIR_NONE, TEXT_DIR_LTR, TEXT_DIR_RTL)
	= let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
	  in get_text_direction_ (x0, x1, x2)
	   ; (!x0, !x1, !x2)
	  end
    type text_window_type_t = int
    val get_text_window_type_t_ : int ref * int ref * int ref * int ref 
				* int ref * int ref * int ref
				  -> unit
	= _import "mgtk_get_gtk_text_window_type"
		  : int ref * int ref * int ref * int ref * int ref * int ref 
		  * int ref
		    -> unit;
    val (TEXT_WINDOW_PRIVATE, TEXT_WINDOW_WIDGET, TEXT_WINDOW_TEXT, 
	 TEXT_WINDOW_LEFT, TEXT_WINDOW_RIGHT, TEXT_WINDOW_TOP, 
	 TEXT_WINDOW_BOTTOM)
	= let val (x0, x1, x2, x3, x4, x5, x6)
		  = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_text_window_type_t_ (x0, x1, x2, x3, x4, x5, x6)
	   ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6)
	  end
    type updatetype = int
    val get_updatetype_ : int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_updatetype"
		  : int ref * int ref * int ref -> unit;
    val (UPDATE_CONTINUOUS, UPDATE_DISCONTINUOUS, UPDATE_DELAYED)
	= let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
	  in get_updatetype_ (x0, x1, x2)
	   ; (!x0, !x1, !x2)
	  end
    type visibility = int
    val get_visibility_ : int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_visibility"
		  : int ref * int ref * int ref -> unit;
    val (VISIBILITY_NONE, VISIBILITY_PARTIAL, VISIBILITY_FULL)
	= let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
	  in get_visibility_ (x0, x1, x2)
	   ; (!x0, !x1, !x2)
	  end
    type wrap_mode = int
    val get_wrap_mode_ : int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_wrap_mode"
		  : int ref * int ref * int ref * int ref -> unit;
    val (WRAP_NONE, WRAP_CHAR, WRAP_WORD, WRAP_WORD_CHAR)
	= let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	  in get_wrap_mode_ (x0, x1, x2, x3)
	   ; (!x0, !x1, !x2, !x3)
	  end
    type accel_flags = int
    val get_accel_flags_ : int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_accel_flags"
		  : int ref * int ref * int ref -> unit;
    val (ACCEL_VISIBLE, ACCEL_LOCKED, ACCEL_MASK)
	= let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
	  in get_accel_flags_ (x0, x1, x2)
	   ; (!x0, !x1, !x2)
	  end
    type arg_flags = int
    val get_arg_flags_
      : int ref * int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_arg_flags"
		  : int ref * int ref * int ref * int ref * int ref -> unit;
    val (ARG_READABLE, ARG_WRITABLE, ARG_CONSTRUCT, ARG_CONSTRUCT_ONLY, 
	 ARG_CHILD_ARG)
	= let val (x0, x1, x2, x3, x4) = (ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_arg_flags_ (x0, x1, x2, x3, x4)
	   ; (!x0, !x1, !x2, !x3, !x4)
	  end
    type attach_options = int
    val get_attach_options_ : int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_attach_options"
		  : int ref * int ref * int ref -> unit;
    val (EXPAND, SHRINK, FILL) = let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
				 in get_attach_options_ (x0, x1, x2)
				  ; (!x0, !x1, !x2)
				 end
    type debug_flag = int
    val get_debug_flag_ : int ref * int ref * int ref * int ref * int ref 
			* int ref * int ref
			  -> unit
	= _import "mgtk_get_gtk_debug_flag" : int ref * int ref * int ref 
					    * int ref * int ref * int ref 
					    * int ref
					      -> unit;
    val (DEBUG_MISC, DEBUG_PLUGSOCKET, DEBUG_TEXT, DEBUG_TREE, DEBUG_UPDATES, 
	 DEBUG_KEYBINDINGS, DEBUG_MULTIHEAD)
	= let val (x0, x1, x2, x3, x4, x5, x6)
		  = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0)
	  in get_debug_flag_ (x0, x1, x2, x3, x4, x5, x6)
	   ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6)
	  end
    type dest_defaults = int
    val get_dest_defaults_ : int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_dest_defaults"
		  : int ref * int ref * int ref * int ref -> unit;
    val (DEST_DEFAULT_MOTION, DEST_DEFAULT_HIGHLIGHT, DEST_DEFAULT_DROP, 
	 DEST_DEFAULT_ALL)
	= let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	  in get_dest_defaults_ (x0, x1, x2, x3)
	   ; (!x0, !x1, !x2, !x3)
	  end
    type icon_lookup_flags = int
    val get_icon_lookup_flags_ : int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_icon_lookup_flags"
		  : int ref * int ref * int ref -> unit;
    val (ICON_LOOKUP_NO_SVG, ICON_LOOKUP_FORCE_SVG, ICON_LOOKUP_USE_BUILTIN)
	= let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
	  in get_icon_lookup_flags_ (x0, x1, x2)
	   ; (!x0, !x1, !x2)
	  end
    type rc_flags = int
    val get_rc_flags_ : int ref * int ref * int ref * int ref -> unit
	= _import "mgtk_get_gtk_rc_flags"
		  : int ref * int ref * int ref * int ref -> unit;
    val (RC_FG, RC_BG, RC_TEXT, RC_BASE)
	= let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	  in get_rc_flags_ (x0, x1, x2, x3)
	   ; (!x0, !x1, !x2, !x3)
	  end
    type target_flags = int
    val get_target_flags_ : int ref * int ref -> unit
	= _import "mgtk_get_gtk_target_flags" : int ref * int ref -> unit;
    val (TARGET_SAME_APP, TARGET_SAME_WIDGET)
	= let val (x0, x1) = (ref 0, ref 0) in get_target_flags_ (x0, x1)
					     ; (!x0, !x1)
					    end
    type text_search_flags = int
    val get_text_search_flags_ : int ref * int ref -> unit
	= _import "mgtk_get_gtk_text_search_flags" : int ref * int ref -> unit;
    val (TEXT_SEARCH_VISIBLE_ONLY, TEXT_SEARCH_TEXT_ONLY)
	= let val (x0, x1) = (ref 0, ref 0) in get_text_search_flags_ (x0, x1)
					     ; (!x0, !x1)
					    end
    structure BindingSet :>
      sig
	type base
	type 'a bindingset_t
	type 'a t = 'a bindingset_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toBindingSet : 'a t -> base t
	val new : string -> base t
	val by_class : cptr -> base t
	val find : string -> base t
	val add_path : 'a t -> pathtype -> string -> path_prioritytype -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a bindingset_t = unit
	type 'a t = 'a bindingset_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toBindingSet obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : CString.cstring -> cptr
	    = _import "gtk_binding_set_new" : CString.cstring -> cptr;
	val new : string -> base t
	    = fn set_name => make (new_ (CString.fromString set_name))
	val by_class_ : cptr -> cptr
	    = _import "gtk_binding_set_by_class" : cptr -> cptr;
	val by_class : cptr -> base t
	    = fn object_class => make (by_class_ object_class)
	val find_ : CString.cstring -> cptr
	    = _import "gtk_binding_set_find" : CString.cstring -> cptr;
	val find : string -> base t
	    = fn set_name => make (find_ (CString.fromString set_name))
	val add_path_ : cptr * int * CString.cstring * int -> unit
	    = _import "gtk_binding_set_add_path"
		      : cptr * int * CString.cstring * int -> unit;
	val add_path : 'a t -> pathtype -> string -> path_prioritytype -> unit
	    = fn self => fn path_type => fn path_pattern => fn priority =>
		 GObject.withPtr (self, 
				  fn self => add_path_ (self, path_type, 
							CString.fromString
							  path_pattern, 
							priority))
    end
    structure Object :>
      sig
	type base
	type 'a object_t
	type 'a t = 'a object_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toObject : 'a t -> base t
	type flags
	val IN_DESTRUCTION : flags
	val FLOATING : flags
	val RESERVED_1 : flags
	val RESERVED_2 : flags
	val get_type : unit -> GType.t
	val new : GType.t -> string -> base t
	val sink : 'a t -> unit
	val destroy : 'a t -> unit
	val destroy_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a object_t = unit
	type 'a t = 'a object_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toObject obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type flags = int
	val get_flags_ : int ref * int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_object_flags"
		      : int ref * int ref * int ref * int ref -> unit;
	val (IN_DESTRUCTION, FLOATING, RESERVED_1, RESERVED_2)
	    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	      in get_flags_ (x0, x1, x2, x3)
	       ; (!x0, !x1, !x2, !x3)
	      end
	val get_type_ : unit -> GType.t
	    = _import "gtk_object_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : GType.t * CString.cstring -> cptr
	    = _import "gtk_object_new" : GType.t * CString.cstring -> cptr;
	val new : GType.t -> string -> base t
	    = fn typ => fn first_property_name =>
		 make (new_ (typ, CString.fromString first_property_name))
	val sink_ : cptr -> unit = _import "gtk_object_sink" : cptr -> unit;
	val sink : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => sink_ self)
	val destroy_ : cptr -> unit
	    = _import "gtk_object_destroy" : cptr -> unit;
	val destroy : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => destroy_ self)
	local open Signal
	      infixr -->
	in val destroy_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "destroy" false (void --> return_void) f
	end
    end
    structure Settings :>
      sig
	type base
	type 'a settings_t
	type 'a t = 'a settings_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toSettings : 'a t -> base t
	val get_type : unit -> GType.t
	val get_default : unit -> base t
	val set_string_property : 'a t -> string -> string -> string -> unit
	val set_double_property : 'a t -> string -> real -> string -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a settings_t = unit
	type 'a t = 'a settings_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toSettings obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_settings_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val get_default_ : unit -> cptr
	    = _import "gtk_settings_get_default" : unit -> cptr;
	val get_default : unit -> base t
	    = fn dummy => make (get_default_ dummy)
	val set_string_property_
	  : cptr * CString.cstring * CString.cstring * CString.cstring -> unit
	    = _import "gtk_settings_set_string_property"
		      : cptr * CString.cstring * CString.cstring 
		      * CString.cstring
			-> unit;
	val set_string_property : 'a t -> string -> string -> string -> unit
	    = fn self => fn name => fn v_string => fn origin =>
		 GObject.withPtr (self, 
				  fn self => set_string_property_
					       (self, CString.fromString name, 
						CString.fromString v_string, 
						CString.fromString origin))
	val set_double_property_
	  : cptr * CString.cstring * real * CString.cstring -> unit
	    = _import "gtk_settings_set_double_property"
		      : cptr * CString.cstring * real * CString.cstring
			-> unit;
	val set_double_property : 'a t -> string -> real -> string -> unit
	    = fn self => fn name => fn v_double => fn origin =>
		 GObject.withPtr
		   (self, 
		    fn self => set_double_property_
				 (self, CString.fromString name, v_double, 
				  CString.fromString origin))
    end
    structure IconSet :>
      sig
	type t = GObject.cptr
	type base
	val alloc_GtkIconSet : unit -> t
	val get_type : unit -> GType.t
	val new : unit -> t
	val refe : t -> t
	val unref : t -> unit
	val copy : t -> t
      end = struct
	type cptr = GObject.cptr
	type t = GObject.cptr
	type base = unit
	val alloc_GtkIconSet = _import "alloc_GtkIconSet" : unit -> t;
	val get_type_ : unit -> GType.t
	    = _import "gtk_icon_set_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_icon_set_new" : unit -> cptr;
	val new : unit -> t = fn dummy => new_ dummy
	val ref_ : cptr -> cptr = _import "gtk_icon_set_ref" : cptr -> cptr;
	val refe : t -> t = fn self => ref_ self
	val unref_ : cptr -> unit
	    = _import "gtk_icon_set_unref" : cptr -> unit;
	val unref : t -> unit = fn self => unref_ self
	val copy_ : cptr -> cptr = _import "gtk_icon_set_copy" : cptr -> cptr;
	val copy : t -> t = fn self => copy_ self
    end
    structure AccelGroup :>
      sig
	type base
	type 'a accelgroup_t
	type 'a t = 'a accelgroup_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toAccelGroup : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val lock : 'a t -> unit
	val unlock : 'a t -> unit
	val accel_activate_sig : (unit -> int -> unit -> bool)
				 -> 'a t Signal.signal
	val accel_changed_sig : (int -> unit -> unit -> unit)
				-> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a accelgroup_t = unit
	type 'a t = 'a accelgroup_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toAccelGroup obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_accel_group_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_accel_group_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val lock_ : cptr -> unit
	    = _import "gtk_accel_group_lock" : cptr -> unit;
	val lock : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => lock_ self)
	val unlock_ : cptr -> unit
	    = _import "gtk_accel_group_unlock" : cptr -> unit;
	val unlock : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => unlock_ self)
	local open Signal
	      infixr -->
	in val accel_activate_sig : (unit -> int -> unit -> bool)
				    -> 'a t Signal.signal
	       = fn f => signal "accel-activate" false
			        (unit --> int --> unit --> return_bool) f
	   val accel_changed_sig : (int -> unit -> unit -> unit)
				   -> 'a t Signal.signal
	       = fn f => signal "accel-changed" false
			        (int --> unit --> unit --> return_void) f
	end
    end
    structure Adjustment :>
      sig
	type base
	type 'a adjustment_t
	type 'a t = 'a adjustment_t Object.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toAdjustment : 'a t -> base t
	val get_type : unit -> GType.t
	val new : real -> real -> real -> real -> real -> real -> base t
	val changed : 'a t -> unit
	val value_changed : 'a t -> unit
	val clamp_page : 'a t -> real -> real -> unit
	val get_value : 'a t -> real
	val set_value : 'a t -> real -> unit
	val changed_sig : (unit -> unit) -> 'a t Signal.signal
	val value_changed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a adjustment_t = unit
	type 'a t = 'a adjustment_t Object.t
	fun inherit w con = Object.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toAdjustment obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_adjustment_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : real * real * real * real * real * real -> cptr
	    = _import "gtk_adjustment_new"
		      : real * real * real * real * real * real -> cptr;
	val new : real -> real -> real -> real -> real -> real -> base t
	    = fn value => fn lower => fn upper => fn step_increment => 
	      fn page_increment => fn page_size =>
		 make (new_ (value, lower, upper, step_increment, 
			     page_increment, page_size))
	val changed_ : cptr -> unit
	    = _import "gtk_adjustment_changed" : cptr -> unit;
	val changed : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => changed_ self)
	val value_changed_ : cptr -> unit
	    = _import "gtk_adjustment_value_changed" : cptr -> unit;
	val value_changed : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => value_changed_ self)
	val clamp_page_ : cptr * real * real -> unit
	    = _import "gtk_adjustment_clamp_page" : cptr * real * real -> unit;
	val clamp_page : 'a t -> real -> real -> unit
	    = fn self => fn lower => fn upper =>
		 GObject.withPtr
		   (self, fn self => clamp_page_ (self, lower, upper))
	val get_value_ : cptr -> real
	    = _import "gtk_adjustment_get_value" : cptr -> real;
	val get_value : 'a t -> real
	    = fn self => GObject.withPtr (self, fn self => get_value_ self)
	val set_value_ : cptr * real -> unit
	    = _import "gtk_adjustment_set_value" : cptr * real -> unit;
	val set_value : 'a t -> real -> unit
	    = fn self => fn value =>
		 GObject.withPtr (self, fn self => set_value_ (self, value))
	local open Signal
	      infixr -->
	in val changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "changed" false (void --> return_void) f
	   val value_changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "value-changed" false (void --> return_void) f
	end
    end
    structure Clipboard :>
      sig
	type base
	type 'a clipboard_t
	type 'a t = 'a clipboard_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toClipboard : 'a t -> base t
	val get_type : unit -> GType.t
	val get_owner : 'a t -> base GObject.t
	val clear : 'a t -> unit
	val set_text : 'a t -> string -> int -> unit
	val set_text' : 'a t -> string -> unit
	val wait_for_text : 'a t -> string
	val wait_is_text_available : 'a t -> bool
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a clipboard_t = unit
	type 'a t = 'a clipboard_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toClipboard obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_clipboard_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val get_owner_ : cptr -> cptr
	    = _import "gtk_clipboard_get_owner" : cptr -> cptr;
	val get_owner : 'a t -> base GObject.t
	    = fn self => GObject.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_owner_ self))
	val clear_ : cptr -> unit
	    = _import "gtk_clipboard_clear" : cptr -> unit;
	val clear : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => clear_ self)
	val set_text_ : cptr * CString.cstring * int -> unit
	    = _import "gtk_clipboard_set_text"
		      : cptr * CString.cstring * int -> unit;
	val set_text : 'a t -> string -> int -> unit
	    = fn self => fn text => fn len =>
		 GObject.withPtr
		   (self, 
		    fn self => set_text_ (self, CString.fromString text, len))
	val set_text' : 'a t -> string -> unit
	    = fn self => fn text =>
		 GObject.withPtr
		   (self, 
		    fn self => set_text_ (self, CString.fromString text, ~1))
	val wait_for_text_ : cptr -> CString.t
	    = _import "gtk_clipboard_wait_for_text" : cptr -> CString.t;
	val wait_for_text : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = wait_for_text_ self
				       in CString.toString t end)
	val wait_is_text_available_ : cptr -> bool
	    = _import "gtk_clipboard_wait_is_text_available" : cptr -> bool;
	val wait_is_text_available : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => wait_is_text_available_ self)
    end
    structure RcStyle :>
      sig
	type base
	type 'a rcstyle_t
	type 'a t = 'a rcstyle_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toRcStyle : 'a t -> base t
	val get_type : unit -> GType.t
	val copy : 'a t -> base t
	val refe : 'a t -> unit
	val unref : 'a t -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a rcstyle_t = unit
	type 'a t = 'a rcstyle_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toRcStyle obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_rc_style_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val copy_ : cptr -> cptr = _import "gtk_rc_style_copy" : cptr -> cptr;
	val copy : 'a t -> base t
	    = fn self => make (GObject.withPtr (self, fn self => copy_ self))
	val ref_ : cptr -> unit = _import "gtk_rc_style_ref" : cptr -> unit;
	val refe : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => ref_ self)
	val unref_ : cptr -> unit
	    = _import "gtk_rc_style_unref" : cptr -> unit;
	val unref : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => unref_ self)
    end
    structure Requisition :>
      sig
	type t = GObject.cptr
	type base
	val alloc_GtkRequisition : unit -> t
	val get_type : unit -> GType.t
	val copy : t -> t
	val free : t -> unit
      end = struct
	type cptr = GObject.cptr
	type t = GObject.cptr
	type base = unit
	val alloc_GtkRequisition = _import "alloc_GtkRequisition" : unit -> t;
	val get_type_ : unit -> GType.t
	    = _import "gtk_requisition_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val copy_ : cptr -> cptr
	    = _import "gtk_requisition_copy" : cptr -> cptr;
	val copy : t -> t = fn self => copy_ self
	val free_ : cptr -> unit
	    = _import "gtk_requisition_free" : cptr -> unit;
	val free : t -> unit = fn self => free_ self
    end
    structure Widget :>
      sig
	type base
	type 'a widget_t
	type 'a t = 'a widget_t Object.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toWidget : 'a t -> base t
	type helptype
	val HELP_TOOLTIP : helptype
	val HELP_WHATS_THIS : helptype
	type flags
	val TOPLEVEL : flags
	val NO_WINDOW : flags
	val REALIZED : flags
	val MAPPED : flags
	val VISIBLE : flags
	val SENSITIVE : flags
	val PARENT_SENSITIVE : flags
	val CAN_FOCUS : flags
	val HAS_FOCUS : flags
	val CAN_DEFAULT : flags
	val HAS_DEFAULT : flags
	val HAS_GRAB : flags
	val RC_STYLE : flags
	val COMPOSITE_CHILD : flags
	val NO_REPARENT : flags
	val APP_PAINTABLE : flags
	val RECEIVES_DEFAULT : flags
	val DOUBLE_BUFFERED : flags
	val NO_SHOW_ALL : flags
	val drag_check_threshold : 'a t -> int -> int -> int -> int -> bool
	val drag_highlight : 'a t -> unit
	val drag_unhighlight : 'a t -> unit
	val drag_dest_unset : 'a t -> unit
	val drag_source_unset : 'a t -> unit
	val drag_source_set_icon_stock : 'a t -> string -> unit
	val grab_add : 'a t -> unit
	val grab_remove : 'a t -> unit
	val rc_get_style : 'a t -> base t
	val selection_remove_all : 'a t -> unit
	val get_type : unit -> GType.t
	val new : GType.t -> string -> base t
	val refe : 'a t -> base t
	val unref : 'a t -> unit
	val destroy : 'a t -> unit
	val destroyed : 'a t -> 'b t -> unit
	val set : 'a t -> string -> unit
	val unparent : 'a t -> unit
	val show : 'a t -> unit
	val show_now : 'a t -> unit
	val hide : 'a t -> unit
	val show_all : 'a t -> unit
	val hide_all : 'a t -> unit
	val set_no_show_all : 'a t -> bool -> unit
	val get_no_show_all : 'a t -> bool
	val map : 'a t -> unit
	val unmap : 'a t -> unit
	val realize : 'a t -> unit
	val unrealize : 'a t -> unit
	val queue_draw : 'a t -> unit
	val queue_draw_area : 'a t -> int -> int -> int -> int -> unit
	val queue_resize : 'a t -> unit
	val queue_resize_no_redraw : 'a t -> unit
	val get_child_requisition : 'a t -> Requisition.t -> unit
	val set_accel_path : 'a t -> string -> 'b AccelGroup.t -> unit
	val can_activate_accel : 'a t -> int -> bool
	val mnemonic_activate : 'a t -> bool -> bool
	val activate : 'a t -> bool
	val set_scroll_adjustments
	  : 'a t -> 'b Adjustment.t option -> 'c Adjustment.t option -> bool
	val set_scroll_adjustments' : 'a t -> bool
	val reparent : 'a t -> 'b t -> unit
	val freeze_child_notify : 'a t -> unit
	val child_notify : 'a t -> string -> unit
	val thaw_child_notify : 'a t -> unit
	val is_focus : 'a t -> bool
	val grab_focus : 'a t -> unit
	val grab_default : 'a t -> unit
	val set_name : 'a t -> string -> unit
	val get_name : 'a t -> string
	val set_state : 'a t -> statetype -> unit
	val set_sensitive : 'a t -> bool -> unit
	val set_app_paintable : 'a t -> bool -> unit
	val set_double_buffered : 'a t -> bool -> unit
	val set_redraw_on_allocate : 'a t -> bool -> unit
	val set_parent : 'a t -> 'b t -> unit
	val set_child_visible : 'a t -> bool -> unit
	val get_child_visible : 'a t -> bool
	val get_parent : 'a t -> base t
	val child_focus : 'a t -> directiontype -> bool
	val set_size_request : 'a t -> int -> int -> unit
	val get_size_request : 'a t -> int * int
	val set_events : 'a t -> int -> unit
	val add_events : 'a t -> int -> unit
	val get_toplevel : 'a t -> base t
	val get_ancestor : 'a t -> GType.t -> base t
	val has_screen : 'a t -> bool
	val get_settings : 'a t -> base Settings.t
	val get_events : 'a t -> int
	val get_pointer : 'a t -> int * int
	val is_ancestor : 'a t -> 'b t -> bool
	val translate_coordinates
	  : 'a t -> 'b t -> int -> int -> bool * int * int
	val hide_on_delete : 'a t -> bool
	val set_style : 'a t -> 'b t option -> unit
	val set_style' : 'a t -> unit
	val ensure_style : 'a t -> unit
	val get_style : 'a t -> base t
	val modify_style : 'a t -> 'b RcStyle.t -> unit
	val get_modifier_style : 'a t -> base RcStyle.t
	val set_composite_name : 'a t -> string -> unit
	val get_composite_name : 'a t -> string
	val reset_rc_styles : 'a t -> unit
	val push_composite_child : unit -> unit
	val pop_composite_child : unit -> unit
	val pop_colormap : unit -> unit
	val style_get_property : 'a t -> string -> GValue.GValue -> unit
	val style_get : 'a t -> string -> unit
	val get_default_style : unit -> base t
	val set_direction : 'a t -> text_direction -> unit
	val get_direction : 'a t -> text_direction
	val set_default_direction : text_direction -> unit
	val get_default_direction : unit -> text_direction
	val reset_shapes : 'a t -> unit
	val path : 'a t -> int * string * string
	val class_path : 'a t -> int * string * string
	val add_mnemonic_label : 'a t -> 'b t -> unit
	val remove_mnemonic_label : 'a t -> 'b t -> unit
	val show_sig : (unit -> unit) -> 'a t Signal.signal
	val hide_sig : (unit -> unit) -> 'a t Signal.signal
	val map_sig : (unit -> unit) -> 'a t Signal.signal
	val unmap_sig : (unit -> unit) -> 'a t Signal.signal
	val realize_sig : (unit -> unit) -> 'a t Signal.signal
	val unrealize_sig : (unit -> unit) -> 'a t Signal.signal
	val size_request_sig : (unit -> unit) -> 'a t Signal.signal
	val size_allocate_sig : (unit -> unit) -> 'a t Signal.signal
	val state_changed_sig : (unit -> unit) -> 'a t Signal.signal
	val parent_set_sig : (unit -> unit) -> 'a t Signal.signal
	val hierarchy_changed_sig : (unit -> unit) -> 'a t Signal.signal
	val style_set_sig : (unit -> unit) -> 'a t Signal.signal
	val direction_changed_sig : (unit -> unit) -> 'a t Signal.signal
	val grab_notify_sig : (bool -> unit) -> 'a t Signal.signal
	val child_notify_sig : (unit -> unit) -> 'a t Signal.signal
	val mnemonic_activate_sig : (bool -> bool) -> 'a t Signal.signal
	val grab_focus_sig : (unit -> unit) -> 'a t Signal.signal
	val focus_sig : (unit -> bool) -> 'a t Signal.signal
	val event_sig : (unit -> bool) -> 'a t Signal.signal
	val event_after_sig : (unit -> unit) -> 'a t Signal.signal
	val button_press_event_sig : (unit -> bool) -> 'a t Signal.signal
	val button_release_event_sig : (unit -> bool) -> 'a t Signal.signal
	val scroll_event_sig : (unit -> bool) -> 'a t Signal.signal
	val motion_notify_event_sig : (unit -> bool) -> 'a t Signal.signal
	val delete_event_sig : (unit -> bool) -> 'a t Signal.signal
	val destroy_event_sig : (unit -> bool) -> 'a t Signal.signal
	val expose_event_sig : (unit -> bool) -> 'a t Signal.signal
	val key_press_event_sig : (unit -> bool) -> 'a t Signal.signal
	val key_release_event_sig : (unit -> bool) -> 'a t Signal.signal
	val enter_notify_event_sig : (unit -> bool) -> 'a t Signal.signal
	val leave_notify_event_sig : (unit -> bool) -> 'a t Signal.signal
	val configure_event_sig : (unit -> bool) -> 'a t Signal.signal
	val focus_in_event_sig : (unit -> bool) -> 'a t Signal.signal
	val focus_out_event_sig : (unit -> bool) -> 'a t Signal.signal
	val map_event_sig : (unit -> bool) -> 'a t Signal.signal
	val unmap_event_sig : (unit -> bool) -> 'a t Signal.signal
	val property_notify_event_sig : (unit -> bool) -> 'a t Signal.signal
	val selection_clear_event_sig : (unit -> bool) -> 'a t Signal.signal
	val selection_request_event_sig : (unit -> bool) -> 'a t Signal.signal
	val selection_notify_event_sig : (unit -> bool) -> 'a t Signal.signal
	val selection_received_sig
	  : (unit -> int -> unit) -> 'a t Signal.signal
	val selection_get_sig : (unit -> int -> int -> unit)
				-> 'a t Signal.signal
	val proximity_in_event_sig : (unit -> bool) -> 'a t Signal.signal
	val proximity_out_event_sig : (unit -> bool) -> 'a t Signal.signal
	val drag_leave_sig : (unit -> int -> unit) -> 'a t Signal.signal
	val drag_begin_sig : (unit -> unit) -> 'a t Signal.signal
	val drag_end_sig : (unit -> unit) -> 'a t Signal.signal
	val drag_data_delete_sig : (unit -> unit) -> 'a t Signal.signal
	val drag_motion_sig : (unit -> int -> int -> int -> bool)
			      -> 'a t Signal.signal
	val drag_drop_sig : (unit -> int -> int -> int -> bool)
			    -> 'a t Signal.signal
	val drag_data_get_sig : (unit -> unit -> int -> int -> unit)
				-> 'a t Signal.signal
	val drag_data_received_sig
	  : (unit -> int -> int -> unit -> int -> int -> unit)
	    -> 'a t Signal.signal
	val visibility_notify_event_sig : (unit -> bool) -> 'a t Signal.signal
	val client_event_sig : (unit -> bool) -> 'a t Signal.signal
	val no_expose_event_sig : (unit -> bool) -> 'a t Signal.signal
	val window_state_event_sig : (unit -> bool) -> 'a t Signal.signal
	val popup_menu_sig : (unit -> bool) -> 'a t Signal.signal
	val show_help_sig : (unit -> bool) -> 'a t Signal.signal
	val accel_closures_changed_sig : (unit -> unit) -> 'a t Signal.signal
	val screen_changed_sig : (unit -> unit) -> 'a t Signal.signal
	val can_activate_accel_sig : (int -> bool) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a widget_t = unit
	type 'a t = 'a widget_t Object.t
	fun inherit w con = Object.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toWidget obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type helptype = int
	val get_helptype_ : int ref * int ref -> unit
	    = _import "mgtk_get_gtk_widget_helptype"
		      : int ref * int ref -> unit;
	val (HELP_TOOLTIP, HELP_WHATS_THIS)
	    = let val (x0, x1) = (ref 0, ref 0) in get_helptype_ (x0, x1)
						 ; (!x0, !x1)
						end
	type flags = int
	val get_flags_ : int ref * int ref * int ref * int ref * int ref 
		       * int ref * int ref * int ref * int ref * int ref 
		       * int ref * int ref * int ref * int ref * int ref 
		       * int ref * int ref * int ref * int ref
			 -> unit
	    = _import "mgtk_get_gtk_widget_flags"
		      : int ref * int ref * int ref * int ref * int ref 
		      * int ref * int ref * int ref * int ref * int ref 
		      * int ref * int ref * int ref * int ref * int ref 
		      * int ref * int ref * int ref * int ref
			-> unit;
	val (TOPLEVEL, NO_WINDOW, REALIZED, MAPPED, VISIBLE, SENSITIVE, 
	     PARENT_SENSITIVE, CAN_FOCUS, HAS_FOCUS, CAN_DEFAULT, HAS_DEFAULT, 
	     HAS_GRAB, RC_STYLE, COMPOSITE_CHILD, NO_REPARENT, APP_PAINTABLE, 
	     RECEIVES_DEFAULT, DOUBLE_BUFFERED, NO_SHOW_ALL)
	    = let val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, 
		       x13, x14, x15, x16, x17, x18)
		      = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
			 ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
			 ref 0, ref 0, ref 0, ref 0, ref 0)
	      in get_flags_ (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, 
			     x12, x13, x14, x15, x16, x17, x18)
	       ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6, !x7, !x8, !x9, !x10, 
		  !x11, !x12, !x13, !x14, !x15, !x16, !x17, !x18)
	      end
	val drag_check_threshold_ : cptr * int * int * int * int -> bool
	    = _import "gtk_drag_check_threshold"
		      : cptr * int * int * int * int -> bool;
	val drag_check_threshold : 'a t -> int -> int -> int -> int -> bool
	    = fn self => fn start_x => fn start_y => fn current_x => 
	      fn current_y =>
		 GObject.withPtr (self, 
				  fn self => drag_check_threshold_
					       (self, start_x, start_y, 
						current_x, current_y))
	val drag_highlight_ : cptr -> unit
	    = _import "gtk_drag_highlight" : cptr -> unit;
	val drag_highlight : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => drag_highlight_ self)
	val drag_unhighlight_ : cptr -> unit
	    = _import "gtk_drag_unhighlight" : cptr -> unit;
	val drag_unhighlight : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => drag_unhighlight_ self)
	val drag_dest_unset_ : cptr -> unit
	    = _import "gtk_drag_dest_unset" : cptr -> unit;
	val drag_dest_unset : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => drag_dest_unset_ self)
	val drag_source_unset_ : cptr -> unit
	    = _import "gtk_drag_source_unset" : cptr -> unit;
	val drag_source_unset : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => drag_source_unset_ self)
	val drag_source_set_icon_stock_ : cptr * CString.cstring -> unit
	    = _import "gtk_drag_source_set_icon_stock"
		      : cptr * CString.cstring -> unit;
	val drag_source_set_icon_stock : 'a t -> string -> unit
	    = fn self => fn stock_id =>
		 GObject.withPtr
		   (self, 
		    fn self => drag_source_set_icon_stock_
				 (self, CString.fromString stock_id))
	val grab_add_ : cptr -> unit = _import "gtk_grab_add" : cptr -> unit;
	val grab_add : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => grab_add_ self)
	val grab_remove_ : cptr -> unit
	    = _import "gtk_grab_remove" : cptr -> unit;
	val grab_remove : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => grab_remove_ self)
	val rc_get_style_ : cptr -> cptr
	    = _import "gtk_rc_get_style" : cptr -> cptr;
	val rc_get_style : 'a t -> base t
	    = fn self => make (GObject.withPtr
				 (self, fn self => rc_get_style_ self))
	val selection_remove_all_ : cptr -> unit
	    = _import "gtk_selection_remove_all" : cptr -> unit;
	val selection_remove_all : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => selection_remove_all_ self)
	val get_type_ : unit -> GType.t
	    = _import "gtk_widget_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : GType.t * CString.cstring -> cptr
	    = _import "gtk_widget_new" : GType.t * CString.cstring -> cptr;
	val new : GType.t -> string -> base t
	    = fn typ => fn first_property_name =>
		 make (new_ (typ, CString.fromString first_property_name))
	val ref_ : cptr -> cptr = _import "gtk_widget_ref" : cptr -> cptr;
	val refe : 'a t -> base t
	    = fn self => make (GObject.withPtr (self, fn self => ref_ self))
	val unref_ : cptr -> unit = _import "gtk_widget_unref" : cptr -> unit;
	val unref : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => unref_ self)
	val destroy_ : cptr -> unit
	    = _import "gtk_widget_destroy" : cptr -> unit;
	val destroy : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => destroy_ self)
	val destroyed_ : cptr * cptr -> unit
	    = _import "gtk_widget_destroyed" : cptr * cptr -> unit;
	val destroyed : 'a t -> 'b t -> unit
	    = fn self => fn widget_pointer =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (widget_pointer, 
				  fn widget_pointer =>
				     destroyed_ (self, widget_pointer)))
	val set_ : cptr * CString.cstring -> unit
	    = _import "gtk_widget_set" : cptr * CString.cstring -> unit;
	val set : 'a t -> string -> unit
	    = fn self => fn first_property_name =>
		 GObject.withPtr (self, 
				  fn self => set_ (self, 
						   CString.fromString
						     first_property_name))
	val unparent_ : cptr -> unit
	    = _import "gtk_widget_unparent" : cptr -> unit;
	val unparent : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => unparent_ self)
	val show_ : cptr -> unit = _import "gtk_widget_show" : cptr -> unit;
	val show : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => show_ self)
	val show_now_ : cptr -> unit
	    = _import "gtk_widget_show_now" : cptr -> unit;
	val show_now : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => show_now_ self)
	val hide_ : cptr -> unit = _import "gtk_widget_hide" : cptr -> unit;
	val hide : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => hide_ self)
	val show_all_ : cptr -> unit
	    = _import "gtk_widget_show_all" : cptr -> unit;
	val show_all : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => show_all_ self)
	val hide_all_ : cptr -> unit
	    = _import "gtk_widget_hide_all" : cptr -> unit;
	val hide_all : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => hide_all_ self)
	val set_no_show_all_ : cptr * bool -> unit
	    = _import "gtk_widget_set_no_show_all" : cptr * bool -> unit;
	val set_no_show_all : 'a t -> bool -> unit
	    = fn self => fn no_show_all =>
		 GObject.withPtr
		   (self, fn self => set_no_show_all_ (self, no_show_all))
	val get_no_show_all_ : cptr -> bool
	    = _import "gtk_widget_get_no_show_all" : cptr -> bool;
	val get_no_show_all : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_no_show_all_ self)
	val map_ : cptr -> unit = _import "gtk_widget_map" : cptr -> unit;
	val map : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => map_ self)
	val unmap_ : cptr -> unit = _import "gtk_widget_unmap" : cptr -> unit;
	val unmap : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => unmap_ self)
	val realize_ : cptr -> unit
	    = _import "gtk_widget_realize" : cptr -> unit;
	val realize : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => realize_ self)
	val unrealize_ : cptr -> unit
	    = _import "gtk_widget_unrealize" : cptr -> unit;
	val unrealize : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => unrealize_ self)
	val queue_draw_ : cptr -> unit
	    = _import "gtk_widget_queue_draw" : cptr -> unit;
	val queue_draw : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => queue_draw_ self)
	val queue_draw_area_ : cptr * int * int * int * int -> unit
	    = _import "gtk_widget_queue_draw_area"
		      : cptr * int * int * int * int -> unit;
	val queue_draw_area : 'a t -> int -> int -> int -> int -> unit
	    = fn self => fn x => fn y => fn width => fn height =>
		 GObject.withPtr (self, 
				  fn self => queue_draw_area_
					       (self, x, y, width, height))
	val queue_resize_ : cptr -> unit
	    = _import "gtk_widget_queue_resize" : cptr -> unit;
	val queue_resize : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => queue_resize_ self)
	val queue_resize_no_redraw_ : cptr -> unit
	    = _import "gtk_widget_queue_resize_no_redraw" : cptr -> unit;
	val queue_resize_no_redraw : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => queue_resize_no_redraw_ self)
	val get_child_requisition_ : cptr * cptr -> unit
	    = _import "gtk_widget_get_child_requisition" : cptr * cptr -> unit;
	val get_child_requisition : 'a t -> Requisition.t -> unit
	    = fn self => fn requisition =>
		 GObject.withPtr (self, 
				  fn self => get_child_requisition_
					       (self, requisition))
	val set_accel_path_ : cptr * CString.cstring * cptr -> unit
	    = _import "gtk_widget_set_accel_path"
		      : cptr * CString.cstring * cptr -> unit;
	val set_accel_path : 'a t -> string -> 'b AccelGroup.t -> unit
	    = fn self => fn accel_path => fn accel_group =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (accel_group, 
				  fn accel_group =>
				     set_accel_path_
				       (self, CString.fromString accel_path, 
					accel_group)))
	val can_activate_accel_ : cptr * int -> bool
	    = _import "gtk_widget_can_activate_accel" : cptr * int -> bool;
	val can_activate_accel : 'a t -> int -> bool
	    = fn self => fn signal_id =>
		 GObject.withPtr
		   (self, fn self => can_activate_accel_ (self, signal_id))
	val mnemonic_activate_ : cptr * bool -> bool
	    = _import "gtk_widget_mnemonic_activate" : cptr * bool -> bool;
	val mnemonic_activate : 'a t -> bool -> bool
	    = fn self => fn group_cycling =>
		 GObject.withPtr
		   (self, fn self => mnemonic_activate_ (self, group_cycling))
	val activate_ : cptr -> bool
	    = _import "gtk_widget_activate" : cptr -> bool;
	val activate : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => activate_ self)
	val set_scroll_adjustments_ : cptr * cptr * cptr -> bool
	    = _import "gtk_widget_set_scroll_adjustments"
		      : cptr * cptr * cptr -> bool;
	val set_scroll_adjustments
	  : 'a t -> 'b Adjustment.t option -> 'c Adjustment.t option -> bool
	    = fn self => fn hadjustment => fn vadjustment =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withOpt
				 (hadjustment, 
				  fn hadjustment =>
				     GObject.withOpt
				       (vadjustment, 
					fn vadjustment =>
					   set_scroll_adjustments_
					     (self, hadjustment, 
					      vadjustment))))
	val set_scroll_adjustments' : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => set_scroll_adjustments_
					 (self, GObject.null, GObject.null))
	val reparent_ : cptr * cptr -> unit
	    = _import "gtk_widget_reparent" : cptr * cptr -> unit;
	val reparent : 'a t -> 'b t -> unit
	    = fn self => fn new_parent =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (new_parent, 
				  fn new_parent =>
				     reparent_ (self, new_parent)))
	val freeze_child_notify_ : cptr -> unit
	    = _import "gtk_widget_freeze_child_notify" : cptr -> unit;
	val freeze_child_notify : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => freeze_child_notify_ self)
	val child_notify_ : cptr * CString.cstring -> unit
	    = _import "gtk_widget_child_notify"
		      : cptr * CString.cstring -> unit;
	val child_notify : 'a t -> string -> unit
	    = fn self => fn child_property =>
		 GObject.withPtr
		   (self, 
		    fn self => child_notify_
				 (self, CString.fromString child_property))
	val thaw_child_notify_ : cptr -> unit
	    = _import "gtk_widget_thaw_child_notify" : cptr -> unit;
	val thaw_child_notify : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => thaw_child_notify_ self)
	val is_focus_ : cptr -> bool
	    = _import "gtk_widget_is_focus" : cptr -> bool;
	val is_focus : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => is_focus_ self)
	val grab_focus_ : cptr -> unit
	    = _import "gtk_widget_grab_focus" : cptr -> unit;
	val grab_focus : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => grab_focus_ self)
	val grab_default_ : cptr -> unit
	    = _import "gtk_widget_grab_default" : cptr -> unit;
	val grab_default : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => grab_default_ self)
	val set_name_ : cptr * CString.cstring -> unit
	    = _import "gtk_widget_set_name" : cptr * CString.cstring -> unit;
	val set_name : 'a t -> string -> unit
	    = fn self => fn name =>
		 GObject.withPtr
		   (self, fn self => set_name_ (self, CString.fromString name))
	val get_name_ : cptr -> CString.t
	    = _import "gtk_widget_get_name" : cptr -> CString.t;
	val get_name : 'a t -> string
	    = fn self => GObject.withPtr (self, 
					  fn self => let val t = get_name_ self
						     in CString.toString t end)
	val set_state_ : cptr * int -> unit
	    = _import "gtk_widget_set_state" : cptr * int -> unit;
	val set_state : 'a t -> statetype -> unit
	    = fn self => fn state =>
		 GObject.withPtr (self, fn self => set_state_ (self, state))
	val set_sensitive_ : cptr * bool -> unit
	    = _import "gtk_widget_set_sensitive" : cptr * bool -> unit;
	val set_sensitive : 'a t -> bool -> unit
	    = fn self => fn sensitive =>
		 GObject.withPtr
		   (self, fn self => set_sensitive_ (self, sensitive))
	val set_app_paintable_ : cptr * bool -> unit
	    = _import "gtk_widget_set_app_paintable" : cptr * bool -> unit;
	val set_app_paintable : 'a t -> bool -> unit
	    = fn self => fn app_paintable =>
		 GObject.withPtr
		   (self, fn self => set_app_paintable_ (self, app_paintable))
	val set_double_buffered_ : cptr * bool -> unit
	    = _import "gtk_widget_set_double_buffered" : cptr * bool -> unit;
	val set_double_buffered : 'a t -> bool -> unit
	    = fn self => fn double_buffered =>
		 GObject.withPtr (self, 
				  fn self => set_double_buffered_
					       (self, double_buffered))
	val set_redraw_on_allocate_ : cptr * bool -> unit
	    = _import "gtk_widget_set_redraw_on_allocate"
		      : cptr * bool -> unit;
	val set_redraw_on_allocate : 'a t -> bool -> unit
	    = fn self => fn redraw_on_allocate =>
		 GObject.withPtr (self, 
				  fn self => set_redraw_on_allocate_
					       (self, redraw_on_allocate))
	val set_parent_ : cptr * cptr -> unit
	    = _import "gtk_widget_set_parent" : cptr * cptr -> unit;
	val set_parent : 'a t -> 'b t -> unit
	    = fn self => fn parent =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (parent, fn parent => set_parent_ (self, parent)))
	val set_child_visible_ : cptr * bool -> unit
	    = _import "gtk_widget_set_child_visible" : cptr * bool -> unit;
	val set_child_visible : 'a t -> bool -> unit
	    = fn self => fn is_visible =>
		 GObject.withPtr
		   (self, fn self => set_child_visible_ (self, is_visible))
	val get_child_visible_ : cptr -> bool
	    = _import "gtk_widget_get_child_visible" : cptr -> bool;
	val get_child_visible : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_child_visible_ self)
	val get_parent_ : cptr -> cptr
	    = _import "gtk_widget_get_parent" : cptr -> cptr;
	val get_parent : 'a t -> base t
	    = fn self => make (GObject.withPtr
				 (self, fn self => get_parent_ self))
	val child_focus_ : cptr * int -> bool
	    = _import "gtk_widget_child_focus" : cptr * int -> bool;
	val child_focus : 'a t -> directiontype -> bool
	    = fn self => fn direction =>
		 GObject.withPtr
		   (self, fn self => child_focus_ (self, direction))
	val set_size_request_ : cptr * int * int -> unit
	    = _import "gtk_widget_set_size_request" : cptr * int * int -> unit;
	val set_size_request : 'a t -> int -> int -> unit
	    = fn self => fn width => fn height =>
		 GObject.withPtr
		   (self, fn self => set_size_request_ (self, width, height))
	val get_size_request_ : cptr * int ref * int ref -> unit
	    = _import "gtk_widget_get_size_request"
		      : cptr * int ref * int ref -> unit;
	val get_size_request : 'a t -> int * int
	    = fn self => let val (width, height) = (ref 0, ref 0)
			     val ret = GObject.withPtr
					 (self, 
					  fn self => get_size_request_
						       (self, width, height))
			 in (!width, !height) end
	val set_events_ : cptr * int -> unit
	    = _import "gtk_widget_set_events" : cptr * int -> unit;
	val set_events : 'a t -> int -> unit
	    = fn self => fn events =>
		 GObject.withPtr (self, fn self => set_events_ (self, events))
	val add_events_ : cptr * int -> unit
	    = _import "gtk_widget_add_events" : cptr * int -> unit;
	val add_events : 'a t -> int -> unit
	    = fn self => fn events =>
		 GObject.withPtr (self, fn self => add_events_ (self, events))
	val get_toplevel_ : cptr -> cptr
	    = _import "gtk_widget_get_toplevel" : cptr -> cptr;
	val get_toplevel : 'a t -> base t
	    = fn self => make (GObject.withPtr
				 (self, fn self => get_toplevel_ self))
	val get_ancestor_ : cptr * GType.t -> cptr
	    = _import "gtk_widget_get_ancestor" : cptr * GType.t -> cptr;
	val get_ancestor : 'a t -> GType.t -> base t
	    = fn self => fn widget_type =>
		 make (GObject.withPtr
			 (self, fn self => get_ancestor_ (self, widget_type)))
	val has_screen_ : cptr -> bool
	    = _import "gtk_widget_has_screen" : cptr -> bool;
	val has_screen : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => has_screen_ self)
	val get_settings_ : cptr -> cptr
	    = _import "gtk_widget_get_settings" : cptr -> cptr;
	val get_settings : 'a t -> base Settings.t
	    = fn self => Settings.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_settings_ self))
	val get_events_ : cptr -> int
	    = _import "gtk_widget_get_events" : cptr -> int;
	val get_events : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_events_ self)
	val get_pointer_ : cptr * int ref * int ref -> unit
	    = _import "gtk_widget_get_pointer"
		      : cptr * int ref * int ref -> unit;
	val get_pointer : 'a t -> int * int
	    = fn self =>
		 let val (x, y) = (ref 0, ref 0)
		     val ret = GObject.withPtr
				 (self, fn self => get_pointer_ (self, x, y))
		 in (!x, !y) end
	val is_ancestor_ : cptr * cptr -> bool
	    = _import "gtk_widget_is_ancestor" : cptr * cptr -> bool;
	val is_ancestor : 'a t -> 'b t -> bool
	    = fn self => fn ancestor =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (ancestor, 
				  fn ancestor =>
				     is_ancestor_ (self, ancestor)))
	val translate_coordinates_
	  : cptr * cptr * int * int * int ref * int ref -> bool
	    = _import "gtk_widget_translate_coordinates"
		      : cptr * cptr * int * int * int ref * int ref -> bool;
	val translate_coordinates
	  : 'a t -> 'b t -> int -> int -> bool * int * int
	    = fn self => fn dest_widget => fn src_x => fn src_y =>
		 let val (dest_x, dest_y) = (ref 0, ref 0)
		     val ret
			 = GObject.withPtr
			     (self, 
			      fn self => GObject.withPtr
					   (dest_widget, 
					    fn dest_widget =>
					       translate_coordinates_
						 (self, dest_widget, src_x, 
						  src_y, dest_x, dest_y)))
		 in (ret, !dest_x, !dest_y) end
	val hide_on_delete_ : cptr -> bool
	    = _import "gtk_widget_hide_on_delete" : cptr -> bool;
	val hide_on_delete : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => hide_on_delete_ self)
	val set_style_ : cptr * cptr -> unit
	    = _import "gtk_widget_set_style" : cptr * cptr -> unit;
	val set_style : 'a t -> 'b t option -> unit
	    = fn self => fn style =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withOpt
				 (style, fn style => set_style_ (self, style)))
	val set_style' : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => set_style_ (self, GObject.null))
	val ensure_style_ : cptr -> unit
	    = _import "gtk_widget_ensure_style" : cptr -> unit;
	val ensure_style : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => ensure_style_ self)
	val get_style_ : cptr -> cptr
	    = _import "gtk_widget_get_style" : cptr -> cptr;
	val get_style : 'a t -> base t
	    = fn self => make (GObject.withPtr
				 (self, fn self => get_style_ self))
	val modify_style_ : cptr * cptr -> unit
	    = _import "gtk_widget_modify_style" : cptr * cptr -> unit;
	val modify_style : 'a t -> 'b RcStyle.t -> unit
	    = fn self => fn style =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (style, fn style => modify_style_ (self, style)))
	val get_modifier_style_ : cptr -> cptr
	    = _import "gtk_widget_get_modifier_style" : cptr -> cptr;
	val get_modifier_style : 'a t -> base RcStyle.t
	    = fn self =>
		 RcStyle.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_modifier_style_ self))
	val set_composite_name_ : cptr * CString.cstring -> unit
	    = _import "gtk_widget_set_composite_name"
		      : cptr * CString.cstring -> unit;
	val set_composite_name : 'a t -> string -> unit
	    = fn self => fn name =>
		 GObject.withPtr (self, 
				  fn self => set_composite_name_
					       (self, CString.fromString name))
	val get_composite_name_ : cptr -> CString.t
	    = _import "gtk_widget_get_composite_name" : cptr -> CString.t;
	val get_composite_name : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_composite_name_ self
				       in CString.toString t end)
	val reset_rc_styles_ : cptr -> unit
	    = _import "gtk_widget_reset_rc_styles" : cptr -> unit;
	val reset_rc_styles : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => reset_rc_styles_ self)
	val push_composite_child_ : unit -> unit
	    = _import "gtk_widget_push_composite_child" : unit -> unit;
	val push_composite_child : unit -> unit
	    = fn dummy => push_composite_child_ dummy
	val pop_composite_child_ : unit -> unit
	    = _import "gtk_widget_pop_composite_child" : unit -> unit;
	val pop_composite_child : unit -> unit
	    = fn dummy => pop_composite_child_ dummy
	val pop_colormap_ : unit -> unit
	    = _import "gtk_widget_pop_colormap" : unit -> unit;
	val pop_colormap : unit -> unit = fn dummy => pop_colormap_ dummy
	val style_get_property_
	  : cptr * CString.cstring * GValue.GValue -> unit
	    = _import "gtk_widget_style_get_property"
		      : cptr * CString.cstring * GValue.GValue -> unit;
	val style_get_property : 'a t -> string -> GValue.GValue -> unit
	    = fn self => fn property_name => fn value =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       style_get_property_
			 (self, CString.fromString property_name, value))
	val style_get_ : cptr * CString.cstring -> unit
	    = _import "gtk_widget_style_get" : cptr * CString.cstring -> unit;
	val style_get : 'a t -> string -> unit
	    = fn self => fn first_property_name =>
		 GObject.withPtr
		   (self, 
		    fn self => style_get_ (self, 
					   CString.fromString
					     first_property_name))
	val get_default_style_ : unit -> cptr
	    = _import "gtk_widget_get_default_style" : unit -> cptr;
	val get_default_style : unit -> base t
	    = fn dummy => make (get_default_style_ dummy)
	val set_direction_ : cptr * int -> unit
	    = _import "gtk_widget_set_direction" : cptr * int -> unit;
	val set_direction : 'a t -> text_direction -> unit
	    = fn self => fn dir =>
		 GObject.withPtr (self, fn self => set_direction_ (self, dir))
	val get_direction_ : cptr -> int
	    = _import "gtk_widget_get_direction" : cptr -> int;
	val get_direction : 'a t -> text_direction
	    = fn self => GObject.withPtr (self, fn self => get_direction_ self)
	val set_default_direction_ : int -> unit
	    = _import "gtk_widget_set_default_direction" : int -> unit;
	val set_default_direction : text_direction -> unit
	    = fn dir => set_default_direction_ dir
	val get_default_direction_ : unit -> int
	    = _import "gtk_widget_get_default_direction" : unit -> int;
	val get_default_direction : unit -> text_direction
	    = fn dummy => get_default_direction_ dummy
	val reset_shapes_ : cptr -> unit
	    = _import "gtk_widget_reset_shapes" : cptr -> unit;
	val reset_shapes : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => reset_shapes_ self)
	val path_ : cptr * int ref * CString.t ref * CString.t ref -> unit
	    = _import "gtk_widget_path"
		      : cptr * int ref * CString.t ref * CString.t ref -> unit;
	val path : 'a t -> int * string * string
	    = fn self =>
		 let val (path_length, path, path_reversed)
			 = (ref 0, ref CString.null, ref CString.null)
		     val ret = GObject.withPtr
				 (self, 
				  fn self => path_ (self, path_length, path, 
						    path_reversed))
		 in (!path_length, CString.toString (!path), 
		     CString.toString (!path_reversed))
		 end
	val class_path_
	  : cptr * int ref * CString.t ref * CString.t ref -> unit
	    = _import "gtk_widget_class_path"
		      : cptr * int ref * CString.t ref * CString.t ref -> unit;
	val class_path : 'a t -> int * string * string
	    = fn self => let val (path_length, path, path_reversed)
				 = (ref 0, ref CString.null, ref CString.null)
			     val ret = GObject.withPtr
					 (self, 
					  fn self => class_path_
						       (self, path_length, 
							path, path_reversed))
			 in (!path_length, CString.toString (!path), 
			     CString.toString (!path_reversed))
			 end
	val add_mnemonic_label_ : cptr * cptr -> unit
	    = _import "gtk_widget_add_mnemonic_label" : cptr * cptr -> unit;
	val add_mnemonic_label : 'a t -> 'b t -> unit
	    = fn self => fn label =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr (label, 
						fn label => add_mnemonic_label_
							      (self, label)))
	val remove_mnemonic_label_ : cptr * cptr -> unit
	    = _import "gtk_widget_remove_mnemonic_label" : cptr * cptr -> unit;
	val remove_mnemonic_label : 'a t -> 'b t -> unit
	    = fn self => fn label =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (label, 
				  fn label => remove_mnemonic_label_
						(self, label)))
	local open Signal
	      infixr -->
	in
	  val show_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "show" false (void --> return_void) f
	  val hide_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "hide" false (void --> return_void) f
	  val map_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "map" false (void --> return_void) f
	  val unmap_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "unmap" false (void --> return_void) f
	  val realize_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "realize" false (void --> return_void) f
	  val unrealize_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "unrealize" false (void --> return_void) f
	  val size_request_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "size-request" false (unit --> return_void) f
	  val size_allocate_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "size-allocate" false (unit --> return_void) f
	  val state_changed_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "state-changed" false (unit --> return_void) f
	  val parent_set_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "parent-set" false (unit --> return_void) f
	  val hierarchy_changed_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f =>
		   signal "hierarchy-changed" false (unit --> return_void) f
	  val style_set_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "style-set" false (unit --> return_void) f
	  val direction_changed_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f =>
		   signal "direction-changed" false (unit --> return_void) f
	  val grab_notify_sig : (bool -> unit) -> 'a t Signal.signal
	      = fn f => signal "grab-notify" false (bool --> return_void) f
	  val child_notify_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "child-notify" false (unit --> return_void) f
	  val mnemonic_activate_sig : (bool -> bool) -> 'a t Signal.signal
	      = fn f =>
		   signal "mnemonic-activate" false (bool --> return_bool) f
	  val grab_focus_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "grab-focus" false (void --> return_void) f
	  val focus_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "focus" false (unit --> return_bool) f
	  val event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "event" false (unit --> return_bool) f
	  val event_after_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "event-after" false (unit --> return_void) f
	  val button_press_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "button-press-event" false
			       (unit --> return_bool) f
	  val button_release_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "button-release-event" false
			       (unit --> return_bool) f
	  val scroll_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "scroll-event" false (unit --> return_bool) f
	  val motion_notify_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "motion-notify-event" false
			       (unit --> return_bool) f
	  val delete_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "delete-event" false (unit --> return_bool) f
	  val destroy_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "destroy-event" false (unit --> return_bool) f
	  val expose_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "expose-event" false (unit --> return_bool) f
	  val key_press_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "key-press-event" false (unit --> return_bool) f
	  val key_release_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f =>
		   signal "key-release-event" false (unit --> return_bool) f
	  val enter_notify_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "enter-notify-event" false
			       (unit --> return_bool) f
	  val leave_notify_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "leave-notify-event" false
			       (unit --> return_bool) f
	  val configure_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "configure-event" false (unit --> return_bool) f
	  val focus_in_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "focus-in-event" false (unit --> return_bool) f
	  val focus_out_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "focus-out-event" false (unit --> return_bool) f
	  val map_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "map-event" false (unit --> return_bool) f
	  val unmap_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "unmap-event" false (unit --> return_bool) f
	  val property_notify_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "property-notify-event" false
			       (unit --> return_bool) f
	  val selection_clear_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "selection-clear-event" false
			       (unit --> return_bool) f
	  val selection_request_event_sig
	    : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "selection-request-event" false
			       (unit --> return_bool) f
	  val selection_notify_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "selection-notify-event" false
			       (unit --> return_bool) f
	  val selection_received_sig
	    : (unit -> int -> unit) -> 'a t Signal.signal
	      = fn f => signal "selection-received" false
			       (unit --> int --> return_void) f
	  val selection_get_sig : (unit -> int -> int -> unit)
				  -> 'a t Signal.signal
	      = fn f => signal "selection-get" false
			       (unit --> int --> int --> return_void) f
	  val proximity_in_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "proximity-in-event" false
			       (unit --> return_bool) f
	  val proximity_out_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "proximity-out-event" false
			       (unit --> return_bool) f
	  val drag_leave_sig : (unit -> int -> unit) -> 'a t Signal.signal
	      = fn f => signal "drag-leave" false
			       (unit --> int --> return_void) f
	  val drag_begin_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "drag-begin" false (unit --> return_void) f
	  val drag_end_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "drag-end" false (unit --> return_void) f
	  val drag_data_delete_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f =>
		   signal "drag-data-delete" false (unit --> return_void) f
	  val drag_motion_sig : (unit -> int -> int -> int -> bool)
				-> 'a t Signal.signal
	      = fn f => signal "drag-motion" false
			       (unit --> int --> int --> int --> return_bool) f
	  val drag_drop_sig : (unit -> int -> int -> int -> bool)
			      -> 'a t Signal.signal
	      = fn f => signal "drag-drop" false
			       (unit --> int --> int --> int --> return_bool) f
	  val drag_data_get_sig : (unit -> unit -> int -> int -> unit)
				  -> 'a t Signal.signal
	      = fn f =>
		   signal "drag-data-get" false
			  (unit --> unit --> int --> int --> return_void) f
	  val drag_data_received_sig
	    : (unit -> int -> int -> unit -> int -> int -> unit)
	      -> 'a t Signal.signal
	      = fn f => signal 
(*  31*)"drag-data-received" false
(*  31*) (unit --> int --> int --> unit --> int --> int --> return_void) f
	  val visibility_notify_event_sig
	    : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "visibility-notify-event" false
			       (unit --> return_bool) f
	  val client_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "client-event" false (unit --> return_bool) f
	  val no_expose_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "no-expose-event" false (unit --> return_bool) f
	  val window_state_event_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "window-state-event" false
			       (unit --> return_bool) f
	  val popup_menu_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "popup-menu" false (void --> return_bool) f
	  val show_help_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "show-help" false (unit --> return_bool) f
	  val accel_closures_changed_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "accel-closures-changed" false
			       (void --> return_void) f
	  val screen_changed_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "screen-changed" false (unit --> return_void) f
	  val can_activate_accel_sig : (int -> bool) -> 'a t Signal.signal
	      = fn f =>
		   signal "can-activate-accel" false (int --> return_bool) f
	end
    end
    structure Style :>
      sig
	type base
	type 'a style_t
	type 'a t = 'a style_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toStyle : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val copy : 'a t -> base t
	val detach : 'a t -> unit
	val lookup_iconset : 'a t -> string -> IconSet.t
	val realize_sig : (unit -> unit) -> 'a t Signal.signal
	val unrealize_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a style_t = unit
	type 'a t = 'a style_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toStyle obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_style_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_style_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val copy_ : cptr -> cptr = _import "gtk_style_copy" : cptr -> cptr;
	val copy : 'a t -> base t
	    = fn self => make (GObject.withPtr (self, fn self => copy_ self))
	val detach_ : cptr -> unit = _import "gtk_style_detach" : cptr -> unit;
	val detach : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => detach_ self)
	val lookup_iconset_ : cptr * CString.cstring -> cptr
	    = _import "gtk_style_lookup_icon_set"
		      : cptr * CString.cstring -> cptr;
	val lookup_iconset : 'a t -> string -> IconSet.t
	    = fn self => fn stock_id =>
		 GObject.withPtr
		   (self, 
		    fn self => lookup_iconset_
				 (self, CString.fromString stock_id))
	local open Signal
	      infixr -->
	in val realize_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "realize" false (void --> return_void) f
	   val unrealize_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "unrealize" false (void --> return_void) f
	end
    end
    structure Demoted :>
      sig
	type base
	val accelerator_get_default_mod_mask : unit -> int
	val icon_size_register : string -> int -> int -> icon_size
	val icon_size_register_alias : string -> icon_size -> unit
	val icon_size_from_name : string -> icon_size
	val check_version : int -> int -> int -> string
	val disable_setlocale : unit -> unit
	val set_locale : unit -> string
	val events_pending : unit -> int
	val main : unit -> unit
	val main_level : unit -> int
	val main_quit : unit -> unit
	val main_iteration : unit -> bool
	val main_iteration_do : bool -> bool
	val main_iteration_do' : unit -> bool
	val rc_add_default_file : string -> unit
	val rc_get_style_by_paths
	  : 'a Settings.t -> string -> string -> GType.t -> base Style.t
	val rc_reparse_all_for_settings : 'a Settings.t -> bool -> bool
	val rc_reset_styles : 'a Settings.t -> unit
	val rc_parse : string -> unit
	val rc_parse_string : string -> unit
	val rc_reparse_all : unit -> bool
	val rc_find_module_in_path : string -> string
	val rc_get_theme_dir : unit -> string
	val rc_get_module_dir : unit -> string
	val rc_get_im_module_path : unit -> string
	val rc_get_im_module_file : unit -> string
	val tips_query_get_type : unit -> GType.t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	val accelerator_get_default_mod_mask_ : unit -> int
	    = _import "gtk_accelerator_get_default_mod_mask" : unit -> int;
	val accelerator_get_default_mod_mask : unit -> int
	    = fn dummy => accelerator_get_default_mod_mask_ dummy
	val icon_size_register_ : CString.cstring * int * int -> int
	    = _import "gtk_icon_size_register"
		      : CString.cstring * int * int -> int;
	val icon_size_register : string -> int -> int -> icon_size
	    = fn name => fn width => fn height =>
		 icon_size_register_ (CString.fromString name, width, height)
	val icon_size_register_alias_ : CString.cstring * int -> unit
	    = _import "gtk_icon_size_register_alias"
		      : CString.cstring * int -> unit;
	val icon_size_register_alias : string -> icon_size -> unit
	    = fn alias => fn target => icon_size_register_alias_
					 (CString.fromString alias, target)
	val icon_size_from_name_ : CString.cstring -> int
	    = _import "gtk_icon_size_from_name" : CString.cstring -> int;
	val icon_size_from_name : string -> icon_size
	    = fn name => icon_size_from_name_ (CString.fromString name)
	val check_version_ : int * int * int -> CString.t
	    = _import "gtk_check_version" : int * int * int -> CString.t;
	val check_version : int -> int -> int -> string
	    = fn required_major => fn required_minor => fn required_micro =>
		 let val t = check_version_ (required_major, required_minor, 
					     required_micro)
		 in CString.toString t end
	val disable_setlocale_ : unit -> unit
	    = _import "gtk_disable_setlocale" : unit -> unit;
	val disable_setlocale : unit -> unit
	    = fn dummy => disable_setlocale_ dummy
	val set_locale_ : unit -> CString.t
	    = _import "gtk_set_locale" : unit -> CString.t;
	val set_locale : unit -> string
	    = fn dummy => let val t = set_locale_ dummy
			  in CString.toString t end
	val events_pending_ : unit -> int
	    = _import "gtk_events_pending" : unit -> int;
	val events_pending : unit -> int = fn dummy => events_pending_ dummy
	val main_ : unit -> unit = _import "gtk_main" : unit -> unit;
	val main : unit -> unit = fn dummy => main_ dummy
	val main_level_ : unit -> int = _import "gtk_main_level" : unit -> int;
	val main_level : unit -> int = fn dummy => main_level_ dummy
	val main_quit_ : unit -> unit = _import "gtk_main_quit" : unit -> unit;
	val main_quit : unit -> unit = fn dummy => main_quit_ dummy
	val main_iteration_ : unit -> bool
	    = _import "gtk_main_iteration" : unit -> bool;
	val main_iteration : unit -> bool = fn dummy => main_iteration_ dummy
	val main_iteration_do_ : bool -> bool
	    = _import "gtk_main_iteration_do" : bool -> bool;
	val main_iteration_do : bool -> bool
	    = fn blocking => main_iteration_do_ blocking
	val main_iteration_do' : unit -> bool
	    = fn dummy => main_iteration_do_ true
	val rc_add_default_file_ : CString.cstring -> unit
	    = _import "gtk_rc_add_default_file" : CString.cstring -> unit;
	val rc_add_default_file : string -> unit
	    = fn filename => rc_add_default_file_ (CString.fromString filename)
	val rc_get_style_by_paths_
	  : cptr * CString.cstring * CString.cstring * GType.t -> cptr
	    = _import "gtk_rc_get_style_by_paths"
		      : cptr * CString.cstring * CString.cstring * GType.t
			-> cptr;
	val rc_get_style_by_paths
	  : 'a Settings.t -> string -> string -> GType.t -> base Style.t
	    = fn settings => fn widget_path => fn class_path => fn typ =>
		 Style.inherit
		   ()
		   (fn () =>
		       GObject.withPtr
			 (settings, 
			  fn settings =>
			     rc_get_style_by_paths_
			       (settings, CString.fromString widget_path, 
				CString.fromString class_path, typ)))
	val rc_reparse_all_for_settings_ : cptr * bool -> bool
	    = _import "gtk_rc_reparse_all_for_settings" : cptr * bool -> bool;
	val rc_reparse_all_for_settings : 'a Settings.t -> bool -> bool
	    = fn settings => fn force_load =>
		 GObject.withPtr (settings, 
				  fn settings => rc_reparse_all_for_settings_
						   (settings, force_load))
	val rc_reset_styles_ : cptr -> unit
	    = _import "gtk_rc_reset_styles" : cptr -> unit;
	val rc_reset_styles : 'a Settings.t -> unit
	    = fn settings =>
		 GObject.withPtr
		   (settings, fn settings => rc_reset_styles_ settings)
	val rc_parse_ : CString.cstring -> unit
	    = _import "gtk_rc_parse" : CString.cstring -> unit;
	val rc_parse : string -> unit
	    = fn filename => rc_parse_ (CString.fromString filename)
	val rc_parse_string_ : CString.cstring -> unit
	    = _import "gtk_rc_parse_string" : CString.cstring -> unit;
	val rc_parse_string : string -> unit
	    = fn rc_string => rc_parse_string_ (CString.fromString rc_string)
	val rc_reparse_all_ : unit -> bool
	    = _import "gtk_rc_reparse_all" : unit -> bool;
	val rc_reparse_all : unit -> bool = fn dummy => rc_reparse_all_ dummy
	val rc_find_module_in_path_ : CString.cstring -> CString.t
	    = _import "gtk_rc_find_module_in_path"
		      : CString.cstring -> CString.t;
	val rc_find_module_in_path : string -> string
	    = fn module_file => let val t = rc_find_module_in_path_
					      (CString.fromString module_file)
				in CString.toString t end
	val rc_get_theme_dir_ : unit -> CString.t
	    = _import "gtk_rc_get_theme_dir" : unit -> CString.t;
	val rc_get_theme_dir : unit -> string
	    = fn dummy => let val t = rc_get_theme_dir_ dummy
			  in CString.toString t end
	val rc_get_module_dir_ : unit -> CString.t
	    = _import "gtk_rc_get_module_dir" : unit -> CString.t;
	val rc_get_module_dir : unit -> string
	    = fn dummy => let val t = rc_get_module_dir_ dummy
			  in CString.toString t end
	val rc_get_im_module_path_ : unit -> CString.t
	    = _import "gtk_rc_get_im_module_path" : unit -> CString.t;
	val rc_get_im_module_path : unit -> string
	    = fn dummy => let val t = rc_get_im_module_path_ dummy
			  in CString.toString t end
	val rc_get_im_module_file_ : unit -> CString.t
	    = _import "gtk_rc_get_im_module_file" : unit -> CString.t;
	val rc_get_im_module_file : unit -> string
	    = fn dummy => let val t = rc_get_im_module_file_ dummy
			  in CString.toString t end
	val tips_query_get_type_ : unit -> GType.t
	    = _import "gtk_tips_query_get_type" : unit -> GType.t;
	val tips_query_get_type : unit -> GType.t
	    = fn dummy => tips_query_get_type_ dummy
    end
    structure Border :>
      sig
	type t = GObject.cptr
	type base
	val alloc_GtkBorder : unit -> t
	val get_type : unit -> GType.t
	val copy : t -> t
	val free : t -> unit
      end = struct
	type cptr = GObject.cptr
	type t = GObject.cptr
	type base = unit
	val alloc_GtkBorder = _import "alloc_GtkBorder" : unit -> t;
	val get_type_ : unit -> GType.t
	    = _import "gtk_border_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val copy_ : cptr -> cptr = _import "gtk_border_copy" : cptr -> cptr;
	val copy : t -> t = fn self => copy_ self
	val free_ : cptr -> unit = _import "gtk_border_free" : cptr -> unit;
	val free : t -> unit = fn self => free_ self
    end
    structure IconInfo :>
      sig
	type t = GObject.cptr
	type base
	val alloc_GtkIconInfo : unit -> t
	val get_type : unit -> GType.t
	val copy : t -> t
	val free : t -> unit
	val get_base_size : t -> int
	val get_filename : t -> string
	val set_raw_coordinates : t -> bool -> unit
	val get_display_name : t -> string
      end = struct
	type cptr = GObject.cptr
	type t = GObject.cptr
	type base = unit
	val alloc_GtkIconInfo = _import "alloc_GtkIconInfo" : unit -> t;
	val get_type_ : unit -> GType.t
	    = _import "gtk_icon_info_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val copy_ : cptr -> cptr = _import "gtk_icon_info_copy" : cptr -> cptr;
	val copy : t -> t = fn self => copy_ self
	val free_ : cptr -> unit = _import "gtk_icon_info_free" : cptr -> unit;
	val free : t -> unit = fn self => free_ self
	val get_base_size_ : cptr -> int
	    = _import "gtk_icon_info_get_base_size" : cptr -> int;
	val get_base_size : t -> int = fn self => get_base_size_ self
	val get_filename_ : cptr -> CString.t
	    = _import "gtk_icon_info_get_filename" : cptr -> CString.t;
	val get_filename : t -> string
	    = fn self => let val t = get_filename_ self
			 in CString.toString t end
	val set_raw_coordinates_ : cptr * bool -> unit
	    = _import "gtk_icon_info_set_raw_coordinates"
		      : cptr * bool -> unit;
	val set_raw_coordinates : t -> bool -> unit
	    = fn self => fn raw_coordinates =>
		 set_raw_coordinates_ (self, raw_coordinates)
	val get_display_name_ : cptr -> CString.t
	    = _import "gtk_icon_info_get_display_name" : cptr -> CString.t;
	val get_display_name : t -> string
	    = fn self => let val t = get_display_name_ self
			 in CString.toString t end
    end
    structure IconTheme :>
      sig
	type base
	type 'a icontheme_t
	type 'a t = 'a icontheme_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toIconTheme : 'a t -> base t
	type error
	val NOT_FOUND : error
	val FAILED : error
	val get_type : unit -> GType.t
	val new : unit -> base t
	val get_default : unit -> base t
	val set_search_path : 'a t -> string list -> int -> unit
	val append_search_path : 'a t -> string -> unit
	val prepend_search_path : 'a t -> string -> unit
	val set_custom_theme : 'a t -> string -> unit
	val has_icon : 'a t -> string -> bool
	val lookup_icon
	  : 'a t -> string -> int -> icon_lookup_flags list -> IconInfo.t
	val get_example_icon_name : 'a t -> string
	val rescan_if_needed : 'a t -> bool
	val changed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a icontheme_t = unit
	type 'a t = 'a icontheme_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toIconTheme obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type error = int
	val get_error_ : int ref * int ref -> unit
	    = _import "mgtk_get_gtk_icon_theme_error"
		      : int ref * int ref -> unit;
	val (NOT_FOUND, FAILED) = let val (x0, x1) = (ref 0, ref 0)
				  in get_error_ (x0, x1)
				   ; (!x0, !x1)
				  end
	val get_type_ : unit -> GType.t
	    = _import "gtk_icon_theme_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_icon_theme_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_default_ : unit -> cptr
	    = _import "gtk_icon_theme_get_default" : unit -> cptr;
	val get_default : unit -> base t
	    = fn dummy => make (get_default_ dummy)
	val set_search_path_ : cptr * CString.cstring array * int -> unit
	    = _import "gtk_icon_theme_set_search_path"
		      : cptr * CString.cstring array * int -> unit;
	val set_search_path : 'a t -> string list -> int -> unit
	    = fn self => fn path => fn n_elements =>
		 GObject.withPtr
		   (self, 
		    fn self => set_search_path_
				 (self, Array.fromList(List.map CString.fromString path), n_elements))
	val append_search_path_ : cptr * CString.cstring -> unit
	    = _import "gtk_icon_theme_append_search_path"
		      : cptr * CString.cstring -> unit;
	val append_search_path : 'a t -> string -> unit
	    = fn self => fn path =>
		 GObject.withPtr (self, 
				  fn self => append_search_path_
					       (self, CString.fromString path))
	val prepend_search_path_ : cptr * CString.cstring -> unit
	    = _import "gtk_icon_theme_prepend_search_path"
		      : cptr * CString.cstring -> unit;
	val prepend_search_path : 'a t -> string -> unit
	    = fn self => fn path =>
		 GObject.withPtr (self, 
				  fn self => prepend_search_path_
					       (self, CString.fromString path))
	val set_custom_theme_ : cptr * CString.cstring -> unit
	    = _import "gtk_icon_theme_set_custom_theme"
		      : cptr * CString.cstring -> unit;
	val set_custom_theme : 'a t -> string -> unit
	    = fn self => fn theme_name =>
		 GObject.withPtr
		   (self, 
		    fn self => set_custom_theme_
				 (self, CString.fromString theme_name))
	val has_icon_ : cptr * CString.cstring -> bool
	    = _import "gtk_icon_theme_has_icon"
		      : cptr * CString.cstring -> bool;
	val has_icon : 'a t -> string -> bool
	    = fn self => fn icon_name =>
		 GObject.withPtr
		   (self, 
		    fn self => has_icon_ (self, CString.fromString icon_name))
	val lookup_icon_ : cptr * CString.cstring * int * int -> cptr
	    = _import "gtk_icon_theme_lookup_icon"
		      : cptr * CString.cstring * int * int -> cptr;
	val lookup_icon
	  : 'a t -> string -> int -> icon_lookup_flags list -> IconInfo.t
	    = fn self => fn icon_name => fn size => fn flags =>
		 GObject.withPtr
		   (self, 
		    fn self => lookup_icon_ (self, 
					     CString.fromString icon_name, 
					     size, Flags.set flags))
	val get_example_icon_name_ : cptr -> CString.t
	    = _import "gtk_icon_theme_get_example_icon_name"
		      : cptr -> CString.t;
	val get_example_icon_name : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_example_icon_name_ self
				       in CString.toString t end)
	val rescan_if_needed_ : cptr -> bool
	    = _import "gtk_icon_theme_rescan_if_needed" : cptr -> bool;
	val rescan_if_needed : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => rescan_if_needed_ self)
	local open Signal
	      infixr -->
	in val changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "changed" false (void --> return_void) f
	end
    end
    structure TreeIter :>
      sig
	type t = GObject.cptr
	type base
	val alloc_GtkTreeIter : unit -> t
	val copy : t -> t
	val free : t -> unit
	val get_type : unit -> GType.t
      end = struct
	type cptr = GObject.cptr
	type t = GObject.cptr
	type base = unit
	val alloc_GtkTreeIter = _import "alloc_GtkTreeIter" : unit -> t;
	val copy_ : cptr -> cptr = _import "gtk_tree_iter_copy" : cptr -> cptr;
	val copy : t -> t = fn self => copy_ self
	val free_ : cptr -> unit = _import "gtk_tree_iter_free" : cptr -> unit;
	val free : t -> unit = fn self => free_ self
	val get_type_ : unit -> GType.t
	    = _import "gtk_tree_iter_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
    end
    structure TreePath :>
      sig
	type base
	type 'a treepath_t
	type 'a t = 'a treepath_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTreePath : 'a t -> base t
	val new : unit -> base t
	val new_from_string : string -> base t
	val new_from_indices : int -> base t
	val to_string : 'a t -> string
	val new_first : unit -> base t
	val append_index : 'a t -> int -> unit
	val prepend_index : 'a t -> int -> unit
	val get_depth : 'a t -> int
	val free : 'a t -> unit
	val copy : 'a t -> base t
	val compare : 'a t -> 'b t -> int
	val next : 'a t -> unit
	val prev : 'a t -> int
	val up : 'a t -> int
	val down : 'a t -> unit
	val is_ancestor : 'a t -> 'b t -> bool
	val is_descendant : 'a t -> 'b t -> bool
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a treepath_t = unit
	type 'a t = 'a treepath_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTreePath obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : unit -> cptr = _import "gtk_tree_path_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_from_string_ : CString.cstring -> cptr
	    = _import "gtk_tree_path_new_from_string"
		      : CString.cstring -> cptr;
	val new_from_string : string -> base t
	    = fn path => make (new_from_string_ (CString.fromString path))
	val new_from_indices_ : int -> cptr
	    = _import "gtk_tree_path_new_from_indices" : int -> cptr;
	val new_from_indices : int -> base t
	    = fn first_index => make (new_from_indices_ first_index)
	val to_string_ : cptr -> CString.t
	    = _import "gtk_tree_path_to_string" : cptr -> CString.t;
	val to_string : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = to_string_ self
				       in CString.toString t end)
	val new_first_ : unit -> cptr
	    = _import "gtk_tree_path_new_first" : unit -> cptr;
	val new_first : unit -> base t = fn dummy => make (new_first_ dummy)
	val append_index_ : cptr * int -> unit
	    = _import "gtk_tree_path_append_index" : cptr * int -> unit;
	val append_index : 'a t -> int -> unit
	    = fn self => fn index =>
		 GObject.withPtr (self, fn self => append_index_ (self, index))
	val prepend_index_ : cptr * int -> unit
	    = _import "gtk_tree_path_prepend_index" : cptr * int -> unit;
	val prepend_index : 'a t -> int -> unit
	    = fn self => fn index =>
		 GObject.withPtr
		   (self, fn self => prepend_index_ (self, index))
	val get_depth_ : cptr -> int
	    = _import "gtk_tree_path_get_depth" : cptr -> int;
	val get_depth : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_depth_ self)
	val free_ : cptr -> unit = _import "gtk_tree_path_free" : cptr -> unit;
	val free : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => free_ self)
	val copy_ : cptr -> cptr = _import "gtk_tree_path_copy" : cptr -> cptr;
	val copy : 'a t -> base t
	    = fn self => make (GObject.withPtr (self, fn self => copy_ self))
	val compare_ : cptr * cptr -> int
	    = _import "gtk_tree_path_compare" : cptr * cptr -> int;
	val compare : 'a t -> 'b t -> int
	    = fn self => fn b =>
		 GObject.withPtr (self, fn self => GObject.withPtr (b, fn b => compare_ (self, b)))
	val next_ : cptr -> unit = _import "gtk_tree_path_next" : cptr -> unit;
	val next : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => next_ self)
	val prev_ : cptr -> int = _import "gtk_tree_path_prev" : cptr -> int;
	val prev : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => prev_ self)
	val up_ : cptr -> int = _import "gtk_tree_path_up" : cptr -> int;
	val up : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => up_ self)
	val down_ : cptr -> unit = _import "gtk_tree_path_down" : cptr -> unit;
	val down : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => down_ self)
	val is_ancestor_ : cptr * cptr -> bool
	    = _import "gtk_tree_path_is_ancestor" : cptr * cptr -> bool;
	val is_ancestor : 'a t -> 'b t -> bool
	    = fn self => fn descendant =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (descendant, 
				  fn descendant =>
				     is_ancestor_ (self, descendant)))
	val is_descendant_ : cptr * cptr -> bool
	    = _import "gtk_tree_path_is_descendant" : cptr * cptr -> bool;
	val is_descendant : 'a t -> 'b t -> bool
	    = fn self => fn ancestor =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (ancestor, 
				  fn ancestor => is_descendant_
						   (self, ancestor)))
    end
    structure TreeModel :>
      sig
	type base
	type 'a treemodel_t
	type 'a t = 'a treemodel_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTreeModel : 'a t -> base t
	type flags
	val TREE_MODEL_ITERS_PERSIST : flags
	val TREE_MODEL_LIST_ONLY : flags
	val get_type : unit -> GType.t
	val get_flags : 'a t -> flags list
	val get_n_columns : 'a t -> int
	val get_columntype : 'a t -> int -> GType.t
	val getiter : 'a t -> 'b TreePath.t -> bool * TreeIter.t
	val getiter_from_string : 'a t -> string -> bool * TreeIter.t
	val get_string_fromiter : 'a t -> TreeIter.t -> string
	val getiter_first : 'a t -> bool * TreeIter.t
	val get_path : 'a t -> TreeIter.t -> base TreePath.t
	val get_value : 'a t -> TreeIter.t -> int -> GValue.GValue
			-> GValue.GValue
	val iter_next : 'a t -> TreeIter.t -> bool * TreeIter.t
	val iter_children : 'a t -> TreeIter.t option -> bool * TreeIter.t
	val iter_children' : 'a t -> bool * TreeIter.t
	val iter_has_child : 'a t -> TreeIter.t -> bool
	val iter_n_children : 'a t -> TreeIter.t option -> int
	val iter_n_children' : 'a t -> int
	val iter_nth_child : 'a t -> TreeIter.t option -> int
			     -> bool * TreeIter.t
	val iter_nth_child' : 'a t -> int -> bool * TreeIter.t
	val iter_parent : 'a t -> TreeIter.t -> bool * TreeIter.t
	val ref_node : 'a t -> TreeIter.t -> unit
	val unref_node : 'a t -> TreeIter.t -> unit
	val get : 'a t -> TreeIter.t -> unit
	val row_changed : 'a t -> 'b TreePath.t -> TreeIter.t -> unit
	val row_inserted : 'a t -> 'b TreePath.t -> TreeIter.t -> unit
	val row_has_child_toggled : 'a t -> 'b TreePath.t -> TreeIter.t -> unit
	val row_deleted : 'a t -> 'b TreePath.t -> unit
	val filter_get_type : unit -> GType.t
	val filter_new : 'a t -> 'b TreePath.t option -> base t
	val filter_new' : 'a t -> base t
	val sort_get_type : unit -> GType.t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a treemodel_t = unit
	type 'a t = 'a treemodel_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeModel obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type flags = int
	val get_flags_ : int ref * int ref -> unit
	    = _import "mgtk_get_gtk_treemodel_flags"
		      : int ref * int ref -> unit;
	val (TREE_MODEL_ITERS_PERSIST, TREE_MODEL_LIST_ONLY)
	    = let val (x0, x1) = (ref 0, ref 0) in get_flags_ (x0, x1)
						 ; (!x0, !x1)
						end
	val get_type_ : unit -> GType.t
	    = _import "gtk_tree_model_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val get_flags_ : cptr -> int
	    = _import "gtk_tree_model_get_flags" : cptr -> int;
	val get_flags : 'a t -> flags list
	    = fn self => Flags.get (GObject.withPtr
				      (self, fn self => get_flags_ self))
	val get_n_columns_ : cptr -> int
	    = _import "gtk_tree_model_get_n_columns" : cptr -> int;
	val get_n_columns : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_n_columns_ self)
	val get_columntype_ : cptr * int -> GType.t
	    = _import "gtk_tree_model_get_column_type" : cptr * int -> GType.t;
	val get_columntype : 'a t -> int -> GType.t
	    = fn self => fn index =>
		 GObject.withPtr
		   (self, fn self => get_columntype_ (self, index))
	val getiter_ : cptr * cptr * cptr -> bool
	    = _import "gtk_tree_model_get_iter" : cptr * cptr * cptr -> bool;
	val getiter : 'a t -> 'b TreePath.t -> bool * TreeIter.t
	    = fn self => fn path =>
		 let val iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self =>
				     GObject.withPtr
				       (path, 
					fn path =>
					   getiter_ (self, iter, path)))
		 in (ret, iter) end
	val getiter_from_string_ : cptr * cptr * CString.cstring -> bool
	    = _import "gtk_tree_model_get_iter_from_string"
		      : cptr * cptr * CString.cstring -> bool;
	val getiter_from_string : 'a t -> string -> bool * TreeIter.t
	    = fn self => fn path_string =>
		 let val iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => getiter_from_string_
					       (self, iter, 
						CString.fromString
						  path_string))
		 in (ret, iter) end
	val get_string_fromiter_ : cptr * cptr -> CString.t
	    = _import "gtk_tree_model_get_string_from_iter"
		      : cptr * cptr -> CString.t;
	val get_string_fromiter : 'a t -> TreeIter.t -> string
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, 
		    fn self => let val t = get_string_fromiter_ (self, iter)
			       in CString.toString t end)
	val getiter_first_ : cptr * cptr -> bool
	    = _import "gtk_tree_model_get_iter_first" : cptr * cptr -> bool;
	val getiter_first : 'a t -> bool * TreeIter.t
	    = fn self =>
		 let val iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, fn self => getiter_first_ (self, iter))
		 in (ret, iter) end
	val get_path_ : cptr * cptr -> cptr
	    = _import "gtk_tree_model_get_path" : cptr * cptr -> cptr;
	val get_path : 'a t -> TreeIter.t -> base TreePath.t
	    = fn self => fn iter =>
		 TreePath.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_path_ (self, iter)))
	val get_value_ : cptr * cptr * int * GValue.GValue -> unit
	    = _import "gtk_tree_model_get_value"
		      : cptr * cptr * int * GValue.GValue -> unit;
	val get_value : 'a t -> TreeIter.t -> int -> GValue.GValue
			-> GValue.GValue
	    = fn self => fn iter => fn column => fn value =>
		 let val value = value
		     val ret = GObject.withPtr
				 (self, 
				  fn self => get_value_
					       (self, iter, column, value))
		 in value end
	val iter_next_ : cptr * cptr -> bool
	    = _import "gtk_tree_model_iter_next" : cptr * cptr -> bool;
	val iter_next : 'a t -> TreeIter.t -> bool * TreeIter.t
	    = fn self => fn iter =>
		 let val iter = iter
		     val ret = GObject.withPtr
				 (self, fn self => iter_next_ (self, iter))
		 in (ret, iter) end
	val iter_children_ : cptr * cptr * cptr -> bool
	    = _import "gtk_tree_model_iter_children"
		      : cptr * cptr * cptr -> bool;
	val iter_children : 'a t -> TreeIter.t option -> bool * TreeIter.t
	    = fn self => fn parent =>
		 let val iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => iter_children_
					       (self, iter, 
						getOpt (parent, GObject.null)))
		 in (ret, iter) end
	val iter_children' : 'a t -> bool * TreeIter.t
	    = fn self => let val iter = TreeIter.alloc_GtkTreeIter ()
			     val ret = GObject.withPtr
					 (self, 
					  fn self => iter_children_
						       (self, iter, 
							GObject.null))
			 in (ret, iter) end
	val iter_has_child_ : cptr * cptr -> bool
	    = _import "gtk_tree_model_iter_has_child" : cptr * cptr -> bool;
	val iter_has_child : 'a t -> TreeIter.t -> bool
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, fn self => iter_has_child_ (self, iter))
	val iter_n_children_ : cptr * cptr -> int
	    = _import "gtk_tree_model_iter_n_children" : cptr * cptr -> int;
	val iter_n_children : 'a t -> TreeIter.t option -> int
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, 
		    fn self => iter_n_children_
				 (self, getOpt (iter, GObject.null)))
	val iter_n_children' : 'a t -> int
	    = fn self =>
		 GObject.withPtr
		   (self, fn self => iter_n_children_ (self, GObject.null))
	val iter_nth_child_ : cptr * cptr * cptr * int -> bool
	    = _import "gtk_tree_model_iter_nth_child"
		      : cptr * cptr * cptr * int -> bool;
	val iter_nth_child : 'a t -> TreeIter.t option -> int
			     -> bool * TreeIter.t
	    = fn self => fn parent => fn n =>
		 let val iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self =>
				     iter_nth_child_
				       (self, iter, 
					getOpt (parent, GObject.null), n))
		 in (ret, iter) end
	val iter_nth_child' : 'a t -> int -> bool * TreeIter.t
	    = fn self => fn n =>
		 let val iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => iter_nth_child_
					       (self, iter, GObject.null, n))
		 in (ret, iter) end
	val iter_parent_ : cptr * cptr * cptr -> bool
	    = _import "gtk_tree_model_iter_parent"
		      : cptr * cptr * cptr -> bool;
	val iter_parent : 'a t -> TreeIter.t -> bool * TreeIter.t
	    = fn self => fn child =>
		 let val iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => iter_parent_ (self, iter, child))
		 in (ret, iter) end
	val ref_node_ : cptr * cptr -> unit
	    = _import "gtk_tree_model_ref_node" : cptr * cptr -> unit;
	val ref_node : 'a t -> TreeIter.t -> unit
	    = fn self => fn iter =>
		 GObject.withPtr (self, fn self => ref_node_ (self, iter))
	val unref_node_ : cptr * cptr -> unit
	    = _import "gtk_tree_model_unref_node" : cptr * cptr -> unit;
	val unref_node : 'a t -> TreeIter.t -> unit
	    = fn self => fn iter =>
		 GObject.withPtr (self, fn self => unref_node_ (self, iter))
	val get_ : cptr * cptr -> unit
	    = _import "gtk_tree_model_get" : cptr * cptr -> unit;
	val get : 'a t -> TreeIter.t -> unit
	    = fn self => fn iter =>
		 GObject.withPtr (self, fn self => get_ (self, iter))
	val row_changed_ : cptr * cptr * cptr -> unit
	    = _import "gtk_tree_model_row_changed"
		      : cptr * cptr * cptr -> unit;
	val row_changed : 'a t -> 'b TreePath.t -> TreeIter.t -> unit
	    = fn self => fn path => fn iter =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (path, fn path => row_changed_ (self, path, iter)))
	val row_inserted_ : cptr * cptr * cptr -> unit
	    = _import "gtk_tree_model_row_inserted"
		      : cptr * cptr * cptr -> unit;
	val row_inserted : 'a t -> 'b TreePath.t -> TreeIter.t -> unit
	    = fn self => fn path => fn iter =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (path, fn path => row_inserted_ (self, path, iter)))
	val row_has_child_toggled_ : cptr * cptr * cptr -> unit
	    = _import "gtk_tree_model_row_has_child_toggled"
		      : cptr * cptr * cptr -> unit;
	val row_has_child_toggled : 'a t -> 'b TreePath.t -> TreeIter.t -> unit
	    = fn self => fn path => fn iter =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (path, 
				  fn path => row_has_child_toggled_
					       (self, path, iter)))
	val row_deleted_ : cptr * cptr -> unit
	    = _import "gtk_tree_model_row_deleted" : cptr * cptr -> unit;
	val row_deleted : 'a t -> 'b TreePath.t -> unit
	    = fn self => fn path =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (path, fn path => row_deleted_ (self, path)))
	val filter_get_type_ : unit -> GType.t
	    = _import "gtk_tree_model_filter_get_type" : unit -> GType.t;
	val filter_get_type : unit -> GType.t
	    = fn dummy => filter_get_type_ dummy
	val filter_new_ : cptr * cptr -> cptr
	    = _import "gtk_tree_model_filter_new" : cptr * cptr -> cptr;
	val filter_new : 'a t -> 'b TreePath.t option -> base t
	    = fn self => fn root =>
		 make (GObject.withPtr
			 (self, 
			  fn self =>
			     GObject.withOpt
			       (root, fn root => filter_new_ (self, root))))
	val filter_new' : 'a t -> base t
	    = fn self =>
		 make (GObject.withPtr
			 (self, fn self => filter_new_ (self, GObject.null)))
	val sort_get_type_ : unit -> GType.t
	    = _import "gtk_tree_model_sort_get_type" : unit -> GType.t;
	val sort_get_type : unit -> GType.t = fn dummy => sort_get_type_ dummy
    end
    structure SelectionData :>
      sig
	type t = GObject.cptr
	type base
	val alloc_GtkSelectionData : unit -> t
	val set_text : t -> string -> int -> bool
	val set_text' : t -> string -> bool
	val targets_include_text : t -> bool
	val get_type : unit -> GType.t
	val copy : t -> t
	val free : t -> unit
	val tree_set_row_drag_data
	  : t -> 'a TreeModel.t -> 'b TreePath.t -> bool
	val tree_get_row_drag_data
	  : t -> 'a TreeModel.t -> 'b TreePath.t -> bool
      end = struct
	type cptr = GObject.cptr
	type t = GObject.cptr
	type base = unit
	val alloc_GtkSelectionData
	    = _import "alloc_GtkSelectionData" : unit -> t;
	val set_text_ : cptr * CString.cstring * int -> bool
	    = _import "gtk_selection_data_set_text"
		      : cptr * CString.cstring * int -> bool;
	val set_text : t -> string -> int -> bool
	    = fn self => fn str => fn len =>
		 set_text_ (self, CString.fromString str, len)
	val set_text' : t -> string -> bool
	    = fn self => fn str => set_text_ (self, CString.fromString str, ~1)
	val targets_include_text_ : cptr -> bool
	    = _import "gtk_selection_data_targets_include_text" : cptr -> bool;
	val targets_include_text : t -> bool
	    = fn self => targets_include_text_ self
	val get_type_ : unit -> GType.t
	    = _import "gtk_selection_data_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val copy_ : cptr -> cptr
	    = _import "gtk_selection_data_copy" : cptr -> cptr;
	val copy : t -> t = fn self => copy_ self
	val free_ : cptr -> unit
	    = _import "gtk_selection_data_free" : cptr -> unit;
	val free : t -> unit = fn self => free_ self
	val tree_set_row_drag_data_ : cptr * cptr * cptr -> bool
	    = _import "gtk_tree_set_row_drag_data"
		      : cptr * cptr * cptr -> bool;
	val tree_set_row_drag_data
	  : t -> 'a TreeModel.t -> 'b TreePath.t -> bool
	    = fn self => fn tree_model => fn path =>
		 GObject.withPtr
		   (tree_model, 
		    fn tree_model =>
		       GObject.withPtr (path, 
					fn path => tree_set_row_drag_data_
						     (self, tree_model, path)))
	val tree_get_row_drag_data_ : cptr * cptr * cptr -> bool
	    = _import "gtk_tree_get_row_drag_data"
		      : cptr * cptr * cptr -> bool;
	val tree_get_row_drag_data
	  : t -> 'a TreeModel.t -> 'b TreePath.t -> bool
	    = fn self => fn tree_model => fn path =>
		 GObject.withPtr
		   (tree_model, 
		    fn tree_model =>
		       GObject.withPtr (path, 
					fn path => tree_get_row_drag_data_
						     (self, tree_model, path)))
    end
    structure TextAttributes :>
      sig
	type t = GObject.cptr
	type base
	val alloc_GtkTextAttributes : unit -> t
	val new : unit -> t
	val copy : t -> t
	val copy_values : t -> t -> unit
	val unref : t -> unit
	val refe : t -> unit
	val get_type : unit -> GType.t
      end = struct
	type cptr = GObject.cptr
	type t = GObject.cptr
	type base = unit
	val alloc_GtkTextAttributes
	    = _import "alloc_GtkTextAttributes" : unit -> t;
	val new_ : unit -> cptr
	    = _import "gtk_text_attributes_new" : unit -> cptr;
	val new : unit -> t = fn dummy => new_ dummy
	val copy_ : cptr -> cptr
	    = _import "gtk_text_attributes_copy" : cptr -> cptr;
	val copy : t -> t = fn self => copy_ self
	val copy_values_ : cptr * cptr -> unit
	    = _import "gtk_text_attributes_copy_values" : cptr * cptr -> unit;
	val copy_values : t -> t -> unit
	    = fn self => fn dest => copy_values_ (self, dest)
	val unref_ : cptr -> unit
	    = _import "gtk_text_attributes_unref" : cptr -> unit;
	val unref : t -> unit = fn self => unref_ self
	val ref_ : cptr -> unit
	    = _import "gtk_text_attributes_ref" : cptr -> unit;
	val refe : t -> unit = fn self => ref_ self
	val get_type_ : unit -> GType.t
	    = _import "gtk_text_attributes_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
    end
    structure TextChildAnchor :>
      sig
	type base
	type 'a textchildanchor_t
	type 'a t = 'a textchildanchor_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTextChildAnchor : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val get_deleted : 'a t -> bool
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a textchildanchor_t = unit
	type 'a t = 'a textchildanchor_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTextChildAnchor obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_text_child_anchor_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr
	    = _import "gtk_text_child_anchor_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_deleted_ : cptr -> bool
	    = _import "gtk_text_child_anchor_get_deleted" : cptr -> bool;
	val get_deleted : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_deleted_ self)
    end
    structure TextTag :>
      sig
	type base
	type 'a texttag_t
	type 'a t = 'a texttag_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTextTag : 'a t -> base t
	val get_type : unit -> GType.t
	val new : string -> base t
	val get_priority : 'a t -> int
	val set_priority : 'a t -> int -> unit
	val event_sig : (unit -> unit -> unit -> bool) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a texttag_t = unit
	type 'a t = 'a texttag_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTextTag obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_text_tag_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : CString.cstring -> cptr
	    = _import "gtk_text_tag_new" : CString.cstring -> cptr;
	val new : string -> base t
	    = fn name => make (new_ (CString.fromString name))
	val get_priority_ : cptr -> int
	    = _import "gtk_text_tag_get_priority" : cptr -> int;
	val get_priority : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_priority_ self)
	val set_priority_ : cptr * int -> unit
	    = _import "gtk_text_tag_set_priority" : cptr * int -> unit;
	val set_priority : 'a t -> int -> unit
	    = fn self => fn priority =>
		 GObject.withPtr
		   (self, fn self => set_priority_ (self, priority))
	local open Signal
	      infixr -->
	in val event_sig : (unit -> unit -> unit -> bool) -> 'a t Signal.signal
	       = fn f => signal "event" false
			        (unit --> unit --> unit --> return_bool) f
	end
    end
    structure TextIter :>
      sig
	type t = GObject.cptr
	type base
	val alloc_GtkTextIter : unit -> t
	val copy : t -> t
	val free : t -> unit
	val get_type : unit -> GType.t
	val get_offset : t -> int
	val get_line : t -> int
	val get_line_offset : t -> int
	val get_line_index : t -> int
	val get_visible_line_offset : t -> int
	val get_visible_line_index : t -> int
	val get_char : t -> char
	val get_slice : t -> t -> string
	val get_text : t -> t -> string
	val get_visible_slice : t -> t -> string
	val get_visible_text : t -> t -> string
	val get_child_anchor : t -> base TextChildAnchor.t
	val begins_tag : t -> 'a TextTag.t option -> bool
	val begins_tag' : t -> bool
	val ends_tag : t -> 'a TextTag.t option -> bool
	val ends_tag' : t -> bool
	val toggles_tag : t -> 'a TextTag.t option -> bool
	val toggles_tag' : t -> bool
	val has_tag : t -> 'a TextTag.t -> bool
	val editable : t -> bool -> bool
	val can_insert : t -> bool -> bool
	val starts_word : t -> bool
	val ends_word : t -> bool
	val inside_word : t -> bool
	val starts_sentence : t -> bool
	val ends_sentence : t -> bool
	val inside_sentence : t -> bool
	val starts_line : t -> bool
	val ends_line : t -> bool
	val is_cursor_position : t -> bool
	val get_chars_in_line : t -> int
	val get_bytes_in_line : t -> int
	val get_attributes : t -> TextAttributes.t -> bool
	val is_end : t -> bool
	val is_start : t -> bool
	val forward_char : t -> bool
	val backward_char : t -> bool
	val forward_chars : t -> int -> bool
	val backward_chars : t -> int -> bool
	val forward_line : t -> bool
	val backward_line : t -> bool
	val forward_lines : t -> int -> bool
	val backward_lines : t -> int -> bool
	val forward_word_end : t -> bool
	val backward_word_start : t -> bool
	val forward_word_ends : t -> int -> bool
	val backward_word_starts : t -> int -> bool
	val forward_visible_word_end : t -> bool
	val backward_visible_word_start : t -> bool
	val forward_visible_word_ends : t -> int -> bool
	val backward_visible_word_starts : t -> int -> bool
	val forward_sentence_end : t -> bool
	val backward_sentence_start : t -> bool
	val forward_sentence_ends : t -> int -> bool
	val backward_sentence_starts : t -> int -> bool
	val forward_cursor_position : t -> bool
	val backward_cursor_position : t -> bool
	val forward_cursor_positions : t -> int -> bool
	val backward_cursor_positions : t -> int -> bool
	val forward_visible_cursor_position : t -> bool
	val backward_visible_cursor_position : t -> bool
	val forward_visible_cursor_positions : t -> int -> bool
	val backward_visible_cursor_positions : t -> int -> bool
	val set_offset : t -> int -> unit
	val set_line : t -> int -> unit
	val set_line_offset : t -> int -> unit
	val set_line_index : t -> int -> unit
	val forward_to_end : t -> unit
	val forward_to_line_end : t -> bool
	val set_visible_line_offset : t -> int -> unit
	val set_visible_line_index : t -> int -> unit
	val forward_to_tag_toggle : t -> 'a TextTag.t option -> bool
	val forward_to_tag_toggle' : t -> bool
	val backward_to_tag_toggle : t -> 'a TextTag.t option -> bool
	val backward_to_tag_toggle' : t -> bool
	val forward_search
	  : t -> string -> text_search_flags list -> t option -> bool * t * t
	val forward_search'
	  : t -> string -> text_search_flags list -> bool * t * t
	val backward_search
	  : t -> string -> text_search_flags list -> t option -> bool * t * t
	val backward_search'
	  : t -> string -> text_search_flags list -> bool * t * t
	val equal : t -> t -> bool
	val compare : t -> t -> int
	val in_range : t -> t -> t -> bool
	val order : t -> t -> unit
      end = struct
	type cptr = GObject.cptr
	type t = GObject.cptr
	type base = unit
	val alloc_GtkTextIter = _import "alloc_GtkTextIter" : unit -> t;
	val copy_ : cptr -> cptr = _import "gtk_text_iter_copy" : cptr -> cptr;
	val copy : t -> t = fn self => copy_ self
	val free_ : cptr -> unit = _import "gtk_text_iter_free" : cptr -> unit;
	val free : t -> unit = fn self => free_ self
	val get_type_ : unit -> GType.t
	    = _import "gtk_text_iter_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val get_offset_ : cptr -> int
	    = _import "gtk_text_iter_get_offset" : cptr -> int;
	val get_offset : t -> int = fn self => get_offset_ self
	val get_line_ : cptr -> int
	    = _import "gtk_text_iter_get_line" : cptr -> int;
	val get_line : t -> int = fn self => get_line_ self
	val get_line_offset_ : cptr -> int
	    = _import "gtk_text_iter_get_line_offset" : cptr -> int;
	val get_line_offset : t -> int = fn self => get_line_offset_ self
	val get_line_index_ : cptr -> int
	    = _import "gtk_text_iter_get_line_index" : cptr -> int;
	val get_line_index : t -> int = fn self => get_line_index_ self
	val get_visible_line_offset_ : cptr -> int
	    = _import "gtk_text_iter_get_visible_line_offset" : cptr -> int;
	val get_visible_line_offset : t -> int
	    = fn self => get_visible_line_offset_ self
	val get_visible_line_index_ : cptr -> int
	    = _import "gtk_text_iter_get_visible_line_index" : cptr -> int;
	val get_visible_line_index : t -> int
	    = fn self => get_visible_line_index_ self
	val get_char_ : cptr -> char
	    = _import "gtk_text_iter_get_char" : cptr -> char;
	val get_char : t -> char = fn self => get_char_ self
	val get_slice_ : cptr * cptr -> CString.t
	    = _import "gtk_text_iter_get_slice" : cptr * cptr -> CString.t;
	val get_slice : t -> t -> string
	    = fn self => fn en => let val t = get_slice_ (self, en)
				  in CString.toString t end
	val get_text_ : cptr * cptr -> CString.t
	    = _import "gtk_text_iter_get_text" : cptr * cptr -> CString.t;
	val get_text : t -> t -> string
	    = fn self => fn en => let val t = get_text_ (self, en)
				  in CString.toString t end
	val get_visible_slice_ : cptr * cptr -> CString.t
	    = _import "gtk_text_iter_get_visible_slice"
		      : cptr * cptr -> CString.t;
	val get_visible_slice : t -> t -> string
	    = fn self => fn en => let val t = get_visible_slice_ (self, en)
				  in CString.toString t end
	val get_visible_text_ : cptr * cptr -> CString.t
	    = _import "gtk_text_iter_get_visible_text"
		      : cptr * cptr -> CString.t;
	val get_visible_text : t -> t -> string
	    = fn self => fn en => let val t = get_visible_text_ (self, en)
				  in CString.toString t end
	val get_child_anchor_ : cptr -> cptr
	    = _import "gtk_text_iter_get_child_anchor" : cptr -> cptr;
	val get_child_anchor : t -> base TextChildAnchor.t
	    = fn self => TextChildAnchor.inherit
			   () (fn () => get_child_anchor_ self)
	val begins_tag_ : cptr * cptr -> bool
	    = _import "gtk_text_iter_begins_tag" : cptr * cptr -> bool;
	val begins_tag : t -> 'a TextTag.t option -> bool
	    = fn self => fn tag =>
		 GObject.withOpt (tag, fn tag => begins_tag_ (self, tag))
	val begins_tag' : t -> bool
	    = fn self => begins_tag_ (self, GObject.null)
	val ends_tag_ : cptr * cptr -> bool
	    = _import "gtk_text_iter_ends_tag" : cptr * cptr -> bool;
	val ends_tag : t -> 'a TextTag.t option -> bool
	    = fn self => fn tag =>
		 GObject.withOpt (tag, fn tag => ends_tag_ (self, tag))
	val ends_tag' : t -> bool = fn self => ends_tag_ (self, GObject.null)
	val toggles_tag_ : cptr * cptr -> bool
	    = _import "gtk_text_iter_toggles_tag" : cptr * cptr -> bool;
	val toggles_tag : t -> 'a TextTag.t option -> bool
	    = fn self => fn tag =>
		 GObject.withOpt (tag, fn tag => toggles_tag_ (self, tag))
	val toggles_tag' : t -> bool
	    = fn self => toggles_tag_ (self, GObject.null)
	val has_tag_ : cptr * cptr -> bool
	    = _import "gtk_text_iter_has_tag" : cptr * cptr -> bool;
	val has_tag : t -> 'a TextTag.t -> bool
	    = fn self => fn tag =>
		 GObject.withPtr (tag, fn tag => has_tag_ (self, tag))
	val editable_ : cptr * bool -> bool
	    = _import "gtk_text_iter_editable" : cptr * bool -> bool;
	val editable : t -> bool -> bool = fn self => fn default_setting =>
					      editable_ (self, default_setting)
	val can_insert_ : cptr * bool -> bool
	    = _import "gtk_text_iter_can_insert" : cptr * bool -> bool;
	val can_insert : t -> bool -> bool
	    = fn self => fn default_editability =>
		 can_insert_ (self, default_editability)
	val starts_word_ : cptr -> bool
	    = _import "gtk_text_iter_starts_word" : cptr -> bool;
	val starts_word : t -> bool = fn self => starts_word_ self
	val ends_word_ : cptr -> bool
	    = _import "gtk_text_iter_ends_word" : cptr -> bool;
	val ends_word : t -> bool = fn self => ends_word_ self
	val inside_word_ : cptr -> bool
	    = _import "gtk_text_iter_inside_word" : cptr -> bool;
	val inside_word : t -> bool = fn self => inside_word_ self
	val starts_sentence_ : cptr -> bool
	    = _import "gtk_text_iter_starts_sentence" : cptr -> bool;
	val starts_sentence : t -> bool = fn self => starts_sentence_ self
	val ends_sentence_ : cptr -> bool
	    = _import "gtk_text_iter_ends_sentence" : cptr -> bool;
	val ends_sentence : t -> bool = fn self => ends_sentence_ self
	val inside_sentence_ : cptr -> bool
	    = _import "gtk_text_iter_inside_sentence" : cptr -> bool;
	val inside_sentence : t -> bool = fn self => inside_sentence_ self
	val starts_line_ : cptr -> bool
	    = _import "gtk_text_iter_starts_line" : cptr -> bool;
	val starts_line : t -> bool = fn self => starts_line_ self
	val ends_line_ : cptr -> bool
	    = _import "gtk_text_iter_ends_line" : cptr -> bool;
	val ends_line : t -> bool = fn self => ends_line_ self
	val is_cursor_position_ : cptr -> bool
	    = _import "gtk_text_iter_is_cursor_position" : cptr -> bool;
	val is_cursor_position : t -> bool
	    = fn self => is_cursor_position_ self
	val get_chars_in_line_ : cptr -> int
	    = _import "gtk_text_iter_get_chars_in_line" : cptr -> int;
	val get_chars_in_line : t -> int = fn self => get_chars_in_line_ self
	val get_bytes_in_line_ : cptr -> int
	    = _import "gtk_text_iter_get_bytes_in_line" : cptr -> int;
	val get_bytes_in_line : t -> int = fn self => get_bytes_in_line_ self
	val get_attributes_ : cptr * cptr -> bool
	    = _import "gtk_text_iter_get_attributes" : cptr * cptr -> bool;
	val get_attributes : t -> TextAttributes.t -> bool
	    = fn self => fn values => get_attributes_ (self, values)
	val is_end_ : cptr -> bool
	    = _import "gtk_text_iter_is_end" : cptr -> bool;
	val is_end : t -> bool = fn self => is_end_ self
	val is_start_ : cptr -> bool
	    = _import "gtk_text_iter_is_start" : cptr -> bool;
	val is_start : t -> bool = fn self => is_start_ self
	val forward_char_ : cptr -> bool
	    = _import "gtk_text_iter_forward_char" : cptr -> bool;
	val forward_char : t -> bool = fn self => forward_char_ self
	val backward_char_ : cptr -> bool
	    = _import "gtk_text_iter_backward_char" : cptr -> bool;
	val backward_char : t -> bool = fn self => backward_char_ self
	val forward_chars_ : cptr * int -> bool
	    = _import "gtk_text_iter_forward_chars" : cptr * int -> bool;
	val forward_chars : t -> int -> bool
	    = fn self => fn count => forward_chars_ (self, count)
	val backward_chars_ : cptr * int -> bool
	    = _import "gtk_text_iter_backward_chars" : cptr * int -> bool;
	val backward_chars : t -> int -> bool
	    = fn self => fn count => backward_chars_ (self, count)
	val forward_line_ : cptr -> bool
	    = _import "gtk_text_iter_forward_line" : cptr -> bool;
	val forward_line : t -> bool = fn self => forward_line_ self
	val backward_line_ : cptr -> bool
	    = _import "gtk_text_iter_backward_line" : cptr -> bool;
	val backward_line : t -> bool = fn self => backward_line_ self
	val forward_lines_ : cptr * int -> bool
	    = _import "gtk_text_iter_forward_lines" : cptr * int -> bool;
	val forward_lines : t -> int -> bool
	    = fn self => fn count => forward_lines_ (self, count)
	val backward_lines_ : cptr * int -> bool
	    = _import "gtk_text_iter_backward_lines" : cptr * int -> bool;
	val backward_lines : t -> int -> bool
	    = fn self => fn count => backward_lines_ (self, count)
	val forward_word_end_ : cptr -> bool
	    = _import "gtk_text_iter_forward_word_end" : cptr -> bool;
	val forward_word_end : t -> bool = fn self => forward_word_end_ self
	val backward_word_start_ : cptr -> bool
	    = _import "gtk_text_iter_backward_word_start" : cptr -> bool;
	val backward_word_start : t -> bool
	    = fn self => backward_word_start_ self
	val forward_word_ends_ : cptr * int -> bool
	    = _import "gtk_text_iter_forward_word_ends" : cptr * int -> bool;
	val forward_word_ends : t -> int -> bool
	    = fn self => fn count => forward_word_ends_ (self, count)
	val backward_word_starts_ : cptr * int -> bool
	    = _import "gtk_text_iter_backward_word_starts"
		      : cptr * int -> bool;
	val backward_word_starts : t -> int -> bool
	    = fn self => fn count => backward_word_starts_ (self, count)
	val forward_visible_word_end_ : cptr -> bool
	    = _import "gtk_text_iter_forward_visible_word_end" : cptr -> bool;
	val forward_visible_word_end : t -> bool
	    = fn self => forward_visible_word_end_ self
	val backward_visible_word_start_ : cptr -> bool
	    = _import "gtk_text_iter_backward_visible_word_start"
		      : cptr -> bool;
	val backward_visible_word_start : t -> bool
	    = fn self => backward_visible_word_start_ self
	val forward_visible_word_ends_ : cptr * int -> bool
	    = _import "gtk_text_iter_forward_visible_word_ends"
		      : cptr * int -> bool;
	val forward_visible_word_ends : t -> int -> bool
	    = fn self => fn count => forward_visible_word_ends_ (self, count)
	val backward_visible_word_starts_ : cptr * int -> bool
	    = _import "gtk_text_iter_backward_visible_word_starts"
		      : cptr * int -> bool;
	val backward_visible_word_starts : t -> int -> bool
	    = fn self => fn count =>
		 backward_visible_word_starts_ (self, count)
	val forward_sentence_end_ : cptr -> bool
	    = _import "gtk_text_iter_forward_sentence_end" : cptr -> bool;
	val forward_sentence_end : t -> bool
	    = fn self => forward_sentence_end_ self
	val backward_sentence_start_ : cptr -> bool
	    = _import "gtk_text_iter_backward_sentence_start" : cptr -> bool;
	val backward_sentence_start : t -> bool
	    = fn self => backward_sentence_start_ self
	val forward_sentence_ends_ : cptr * int -> bool
	    = _import "gtk_text_iter_forward_sentence_ends"
		      : cptr * int -> bool;
	val forward_sentence_ends : t -> int -> bool
	    = fn self => fn count => forward_sentence_ends_ (self, count)
	val backward_sentence_starts_ : cptr * int -> bool
	    = _import "gtk_text_iter_backward_sentence_starts"
		      : cptr * int -> bool;
	val backward_sentence_starts : t -> int -> bool
	    = fn self => fn count => backward_sentence_starts_ (self, count)
	val forward_cursor_position_ : cptr -> bool
	    = _import "gtk_text_iter_forward_cursor_position" : cptr -> bool;
	val forward_cursor_position : t -> bool
	    = fn self => forward_cursor_position_ self
	val backward_cursor_position_ : cptr -> bool
	    = _import "gtk_text_iter_backward_cursor_position" : cptr -> bool;
	val backward_cursor_position : t -> bool
	    = fn self => backward_cursor_position_ self
	val forward_cursor_positions_ : cptr * int -> bool
	    = _import "gtk_text_iter_forward_cursor_positions"
		      : cptr * int -> bool;
	val forward_cursor_positions : t -> int -> bool
	    = fn self => fn count => forward_cursor_positions_ (self, count)
	val backward_cursor_positions_ : cptr * int -> bool
	    = _import "gtk_text_iter_backward_cursor_positions"
		      : cptr * int -> bool;
	val backward_cursor_positions : t -> int -> bool
	    = fn self => fn count => backward_cursor_positions_ (self, count)
	val forward_visible_cursor_position_ : cptr -> bool
	    = _import "gtk_text_iter_forward_visible_cursor_position"
		      : cptr -> bool;
	val forward_visible_cursor_position : t -> bool
	    = fn self => forward_visible_cursor_position_ self
	val backward_visible_cursor_position_ : cptr -> bool
	    = _import "gtk_text_iter_backward_visible_cursor_position"
		      : cptr -> bool;
	val backward_visible_cursor_position : t -> bool
	    = fn self => backward_visible_cursor_position_ self
	val forward_visible_cursor_positions_ : cptr * int -> bool
	    = _import "gtk_text_iter_forward_visible_cursor_positions"
		      : cptr * int -> bool;
	val forward_visible_cursor_positions : t -> int -> bool
	    = fn self => fn count =>
		 forward_visible_cursor_positions_ (self, count)
	val backward_visible_cursor_positions_ : cptr * int -> bool
	    = _import "gtk_text_iter_backward_visible_cursor_positions"
		      : cptr * int -> bool;
	val backward_visible_cursor_positions : t -> int -> bool
	    = fn self => fn count =>
		 backward_visible_cursor_positions_ (self, count)
	val set_offset_ : cptr * int -> unit
	    = _import "gtk_text_iter_set_offset" : cptr * int -> unit;
	val set_offset : t -> int -> unit
	    = fn self => fn char_offset => set_offset_ (self, char_offset)
	val set_line_ : cptr * int -> unit
	    = _import "gtk_text_iter_set_line" : cptr * int -> unit;
	val set_line : t -> int -> unit
	    = fn self => fn line_number => set_line_ (self, line_number)
	val set_line_offset_ : cptr * int -> unit
	    = _import "gtk_text_iter_set_line_offset" : cptr * int -> unit;
	val set_line_offset : t -> int -> unit
	    = fn self => fn char_on_line =>
		 set_line_offset_ (self, char_on_line)
	val set_line_index_ : cptr * int -> unit
	    = _import "gtk_text_iter_set_line_index" : cptr * int -> unit;
	val set_line_index : t -> int -> unit
	    = fn self => fn byte_on_line =>
		 set_line_index_ (self, byte_on_line)
	val forward_to_end_ : cptr -> unit
	    = _import "gtk_text_iter_forward_to_end" : cptr -> unit;
	val forward_to_end : t -> unit = fn self => forward_to_end_ self
	val forward_to_line_end_ : cptr -> bool
	    = _import "gtk_text_iter_forward_to_line_end" : cptr -> bool;
	val forward_to_line_end : t -> bool
	    = fn self => forward_to_line_end_ self
	val set_visible_line_offset_ : cptr * int -> unit
	    = _import "gtk_text_iter_set_visible_line_offset"
		      : cptr * int -> unit;
	val set_visible_line_offset : t -> int -> unit
	    = fn self => fn char_on_line =>
		 set_visible_line_offset_ (self, char_on_line)
	val set_visible_line_index_ : cptr * int -> unit
	    = _import "gtk_text_iter_set_visible_line_index"
		      : cptr * int -> unit;
	val set_visible_line_index : t -> int -> unit
	    = fn self => fn byte_on_line =>
		 set_visible_line_index_ (self, byte_on_line)
	val forward_to_tag_toggle_ : cptr * cptr -> bool
	    = _import "gtk_text_iter_forward_to_tag_toggle"
		      : cptr * cptr -> bool;
	val forward_to_tag_toggle : t -> 'a TextTag.t option -> bool
	    = fn self => fn tag =>
		 GObject.withOpt
		   (tag, fn tag => forward_to_tag_toggle_ (self, tag))
	val forward_to_tag_toggle' : t -> bool
	    = fn self => forward_to_tag_toggle_ (self, GObject.null)
	val backward_to_tag_toggle_ : cptr * cptr -> bool
	    = _import "gtk_text_iter_backward_to_tag_toggle"
		      : cptr * cptr -> bool;
	val backward_to_tag_toggle : t -> 'a TextTag.t option -> bool
	    = fn self => fn tag =>
		 GObject.withOpt
		   (tag, fn tag => backward_to_tag_toggle_ (self, tag))
	val backward_to_tag_toggle' : t -> bool
	    = fn self => backward_to_tag_toggle_ (self, GObject.null)
	val forward_search_
	  : cptr * CString.cstring * int * cptr * cptr * cptr -> bool
	    = _import "gtk_text_iter_forward_search"
		      : cptr * CString.cstring * int * cptr * cptr * cptr
			-> bool;
	val forward_search
	  : t -> string -> text_search_flags list -> t option -> bool * t * t
	    = fn self => fn str => fn flags => fn limit =>
		 let val (match_start, match_end)
			 = (alloc_GtkTextIter (), alloc_GtkTextIter ())
		     val ret = forward_search_ (self, CString.fromString str, 
						Flags.set flags, match_start, 
						match_end, 
						getOpt (limit, GObject.null))
		 in (ret, match_start, match_end) end
	val forward_search'
	  : t -> string -> text_search_flags list -> bool * t * t
	    = fn self => fn str => fn flags =>
		 let val (match_start, match_end)
			 = (alloc_GtkTextIter (), alloc_GtkTextIter ())
		     val ret = forward_search_ (self, CString.fromString str, 
						Flags.set flags, match_start, 
						match_end, GObject.null)
		 in (ret, match_start, match_end) end
	val backward_search_
	  : cptr * CString.cstring * int * cptr * cptr * cptr -> bool
	    = _import "gtk_text_iter_backward_search"
		      : cptr * CString.cstring * int * cptr * cptr * cptr
			-> bool;
	val backward_search
	  : t -> string -> text_search_flags list -> t option -> bool * t * t
	    = fn self => fn str => fn flags => fn limit =>
		 let val (match_start, match_end)
			 = (alloc_GtkTextIter (), alloc_GtkTextIter ())
		     val ret = backward_search_ (self, CString.fromString str, 
						 Flags.set flags, match_start, 
						 match_end, 
						 getOpt (limit, GObject.null))
		 in (ret, match_start, match_end) end
	val backward_search'
	  : t -> string -> text_search_flags list -> bool * t * t
	    = fn self => fn str => fn flags =>
		 let val (match_start, match_end)
			 = (alloc_GtkTextIter (), alloc_GtkTextIter ())
		     val ret = backward_search_ (self, CString.fromString str, 
						 Flags.set flags, match_start, 
						 match_end, GObject.null)
		 in (ret, match_start, match_end) end
	val equal_ : cptr * cptr -> bool
	    = _import "gtk_text_iter_equal" : cptr * cptr -> bool;
	val equal : t -> t -> bool = fn self => fn rhs => equal_ (self, rhs)
	val compare_ : cptr * cptr -> int
	    = _import "gtk_text_iter_compare" : cptr * cptr -> int;
	val compare : t -> t -> int = fn self => fn rhs => compare_ (self, rhs)
	val in_range_ : cptr * cptr * cptr -> bool
	    = _import "gtk_text_iter_in_range" : cptr * cptr * cptr -> bool;
	val in_range : t -> t -> t -> bool
	    = fn self => fn start => fn en => in_range_ (self, start, en)
	val order_ : cptr * cptr -> unit
	    = _import "gtk_text_iter_order" : cptr * cptr -> unit;
	val order : t -> t -> unit
	    = fn self => fn second => order_ (self, second)
    end
    structure TreeRowReference :>
      sig
	type t = GObject.cptr
	type base
	val alloc_GtkTreeRowReference : unit -> t
	val get_type : unit -> GType.t
	val new : 'a TreeModel.t -> 'b TreePath.t -> t
	val new_proxy : 'a GObject.t -> 'b TreeModel.t -> 'c TreePath.t -> t
	val get_path : t -> base TreePath.t
	val valid : t -> bool
	val copy : t -> t
	val free : t -> unit
	val inserted : 'a GObject.t -> 'b TreePath.t -> unit
	val deleted : 'a GObject.t -> 'b TreePath.t -> unit
      end = struct
	type cptr = GObject.cptr
	type t = GObject.cptr
	type base = unit
	val alloc_GtkTreeRowReference
	    = _import "alloc_GtkTreeRowReference" : unit -> t;
	val get_type_ : unit -> GType.t
	    = _import "gtk_tree_row_reference_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr * cptr -> cptr
	    = _import "gtk_tree_row_reference_new" : cptr * cptr -> cptr;
	val new : 'a TreeModel.t -> 'b TreePath.t -> t
	    = fn model => fn path =>
		 GObject.withPtr
		   (model, 
		    fn model => GObject.withPtr
				  (path, fn path => new_ (model, path)))
	val new_proxy_ : cptr * cptr * cptr -> cptr
	    = _import "gtk_tree_row_reference_new_proxy"
		      : cptr * cptr * cptr -> cptr;
	val new_proxy : 'a GObject.t -> 'b TreeModel.t -> 'c TreePath.t -> t
	    = fn proxy => fn model => fn path =>
		 GObject.withPtr
		   (proxy, 
		    fn proxy =>
		       GObject.withPtr
			 (model, 
			  fn model =>
			     GObject.withPtr
			       (path, 
				fn path => new_proxy_ (proxy, model, path))))
	val get_path_ : cptr -> cptr
	    = _import "gtk_tree_row_reference_get_path" : cptr -> cptr;
	val get_path : t -> base TreePath.t
	    = fn self => TreePath.inherit () (fn () => get_path_ self)
	val valid_ : cptr -> bool
	    = _import "gtk_tree_row_reference_valid" : cptr -> bool;
	val valid : t -> bool = fn self => valid_ self
	val copy_ : cptr -> cptr
	    = _import "gtk_tree_row_reference_copy" : cptr -> cptr;
	val copy : t -> t = fn self => copy_ self
	val free_ : cptr -> unit
	    = _import "gtk_tree_row_reference_free" : cptr -> unit;
	val free : t -> unit = fn self => free_ self
	val inserted_ : cptr * cptr -> unit
	    = _import "gtk_tree_row_reference_inserted" : cptr * cptr -> unit;
	val inserted : 'a GObject.t -> 'b TreePath.t -> unit
	    = fn proxy => fn path =>
		 GObject.withPtr
		   (proxy, 
		    fn proxy => GObject.withPtr
				  (path, fn path => inserted_ (proxy, path)))
	val deleted_ : cptr * cptr -> unit
	    = _import "gtk_tree_row_reference_deleted" : cptr * cptr -> unit;
	val deleted : 'a GObject.t -> 'b TreePath.t -> unit
	    = fn proxy => fn path =>
		 GObject.withPtr
		   (proxy, 
		    fn proxy => GObject.withPtr
				  (path, fn path => deleted_ (proxy, path)))
    end
    structure CellEditable :>
      sig
	type base
	type 'a celleditable_t
	type 'a t = 'a celleditable_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toCellEditable : 'a t -> base t
	val get_type : unit -> GType.t
	val editing_done : 'a t -> unit
	val remove_widget : 'a t -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a celleditable_t = unit
	type 'a t = 'a celleditable_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toCellEditable obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_cell_editable_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val editing_done_ : cptr -> unit
	    = _import "gtk_cell_editable_editing_done" : cptr -> unit;
	val editing_done : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => editing_done_ self)
	val remove_widget_ : cptr -> unit
	    = _import "gtk_cell_editable_remove_widget" : cptr -> unit;
	val remove_widget : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => remove_widget_ self)
    end
    structure CellRenderer :>
      sig
	type base
	type 'a cellrenderer_t
	type 'a t = 'a cellrenderer_t Object.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toCellRenderer : 'a t -> base t
	type mode
	val CELL_RENDERER_MODE_INERT : mode
	val CELL_RENDERER_MODE_ACTIVATABLE : mode
	val CELL_RENDERER_MODE_EDITABLE : mode
	type state
	val CELL_RENDERER_SELECTED : state
	val CELL_RENDERER_PRELIT : state
	val CELL_RENDERER_INSENSITIVE : state
	val CELL_RENDERER_SORTED : state
	val CELL_RENDERER_FOCUSED : state
	val get_type : unit -> GType.t
	val set_fixed_size : 'a t -> int -> int -> unit
	val editing_canceled : 'a t -> unit
	val pixbuf_get_type : unit -> GType.t
	val text_get_type : unit -> GType.t
	val toggle_get_type : unit -> GType.t
	val editing_canceled_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a cellrenderer_t = unit
	type 'a t = 'a cellrenderer_t Object.t
	fun inherit w con = Object.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toCellRenderer obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type mode = int
	val get_mode_ : int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_cellrenderer_mode"
		      : int ref * int ref * int ref -> unit;
	val (CELL_RENDERER_MODE_INERT, CELL_RENDERER_MODE_ACTIVATABLE, 
	     CELL_RENDERER_MODE_EDITABLE)
	    = let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
	      in get_mode_ (x0, x1, x2)
	       ; (!x0, !x1, !x2)
	      end
	type state = int
	val get_state_
	  : int ref * int ref * int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_cellrenderer_state"
		      : int ref * int ref * int ref * int ref * int ref
			-> unit;
	val (CELL_RENDERER_SELECTED, CELL_RENDERER_PRELIT, 
	     CELL_RENDERER_INSENSITIVE, CELL_RENDERER_SORTED, 
	     CELL_RENDERER_FOCUSED)
	    = let val (x0, x1, x2, x3, x4)
		      = (ref 0, ref 0, ref 0, ref 0, ref 0)
	      in get_state_ (x0, x1, x2, x3, x4)
	       ; (!x0, !x1, !x2, !x3, !x4)
	      end
	val get_type_ : unit -> GType.t
	    = _import "gtk_cell_renderer_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_fixed_size_ : cptr * int * int -> unit
	    = _import "gtk_cell_renderer_set_fixed_size"
		      : cptr * int * int -> unit;
	val set_fixed_size : 'a t -> int -> int -> unit
	    = fn self => fn width => fn height =>
		 GObject.withPtr
		   (self, fn self => set_fixed_size_ (self, width, height))
	val editing_canceled_ : cptr -> unit
	    = _import "gtk_cell_renderer_editing_canceled" : cptr -> unit;
	val editing_canceled : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => editing_canceled_ self)
	val pixbuf_get_type_ : unit -> GType.t
	    = _import "gtk_cell_renderer_pixbuf_get_type" : unit -> GType.t;
	val pixbuf_get_type : unit -> GType.t
	    = fn dummy => pixbuf_get_type_ dummy
	val text_get_type_ : unit -> GType.t
	    = _import "gtk_cell_renderer_text_get_type" : unit -> GType.t;
	val text_get_type : unit -> GType.t = fn dummy => text_get_type_ dummy
	val toggle_get_type_ : unit -> GType.t
	    = _import "gtk_cell_renderer_toggle_get_type" : unit -> GType.t;
	val toggle_get_type : unit -> GType.t
	    = fn dummy => toggle_get_type_ dummy
	local open Signal
	      infixr -->
	in val editing_canceled_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f =>
		    signal "editing-canceled" false (void --> return_void) f
	end
    end
    structure CellLayout :>
      sig
	type base
	type 'a celllayout_t
	type 'a t = 'a celllayout_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toCellLayout : 'a t -> base t
	val get_type : unit -> GType.t
	val pack_start : 'a t -> 'b CellRenderer.t -> bool -> unit
	val pack_start' : 'a t -> 'b CellRenderer.t -> unit
	val pack_end : 'a t -> 'b CellRenderer.t -> bool -> unit
	val pack_end' : 'a t -> 'b CellRenderer.t -> unit
	val clear : 'a t -> unit
	val set_attributes : 'a t -> 'b CellRenderer.t -> unit
	val add_attribute : 'a t -> 'b CellRenderer.t -> string -> int -> unit
	val clear_attributes : 'a t -> 'b CellRenderer.t -> unit
	val reorder : 'a t -> 'b CellRenderer.t -> int -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a celllayout_t = unit
	type 'a t = 'a celllayout_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toCellLayout obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_cell_layout_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val pack_start_ : cptr * cptr * bool -> unit
	    = _import "gtk_cell_layout_pack_start"
		      : cptr * cptr * bool -> unit;
	val pack_start : 'a t -> 'b CellRenderer.t -> bool -> unit
	    = fn self => fn cell => fn expand =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (cell, fn cell => pack_start_ (self, cell, expand)))
	val pack_start' : 'a t -> 'b CellRenderer.t -> unit
	    = fn self => fn cell =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (cell, fn cell => pack_start_ (self, cell, true)))
	val pack_end_ : cptr * cptr * bool -> unit
	    = _import "gtk_cell_layout_pack_end" : cptr * cptr * bool -> unit;
	val pack_end : 'a t -> 'b CellRenderer.t -> bool -> unit
	    = fn self => fn cell => fn expand =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (cell, fn cell => pack_end_ (self, cell, expand)))
	val pack_end' : 'a t -> 'b CellRenderer.t -> unit
	    = fn self => fn cell =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (cell, fn cell => pack_end_ (self, cell, true)))
	val clear_ : cptr -> unit
	    = _import "gtk_cell_layout_clear" : cptr -> unit;
	val clear : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => clear_ self)
	val set_attributes_ : cptr * cptr -> unit
	    = _import "gtk_cell_layout_set_attributes" : cptr * cptr -> unit;
	val set_attributes : 'a t -> 'b CellRenderer.t -> unit
	    = fn self => fn cell =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (cell, fn cell => set_attributes_ (self, cell)))
	val add_attribute_ : cptr * cptr * CString.cstring * int -> unit
	    = _import "gtk_cell_layout_add_attribute"
		      : cptr * cptr * CString.cstring * int -> unit;
	val add_attribute : 'a t -> 'b CellRenderer.t -> string -> int -> unit
	    = fn self => fn cell => fn attribute => fn column =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (cell, 
				  fn cell => add_attribute_
					       (self, cell, 
						CString.fromString attribute, 
						column)))
	val clear_attributes_ : cptr * cptr -> unit
	    = _import "gtk_cell_layout_clear_attributes" : cptr * cptr -> unit;
	val clear_attributes : 'a t -> 'b CellRenderer.t -> unit
	    = fn self => fn cell =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (cell, fn cell => clear_attributes_ (self, cell)))
	val reorder_ : cptr * cptr * int -> unit
	    = _import "gtk_cell_layout_reorder" : cptr * cptr * int -> unit;
	val reorder : 'a t -> 'b CellRenderer.t -> int -> unit
	    = fn self => fn cell => fn position =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (cell, fn cell => reorder_ (self, cell, position)))
    end
    structure Editable :>
      sig
	type base
	type 'a editable_t
	type 'a t = 'a editable_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toEditable : 'a t -> base t
	val get_type : unit -> GType.t
	val select_region : 'a t -> int -> int -> unit
	val insert_text : 'a t -> string -> int -> int -> int
	val delete_text : 'a t -> int -> int -> unit
	val get_chars : 'a t -> int -> int -> string
	val cut_clipboard : 'a t -> unit
	val copy_clipboard : 'a t -> unit
	val paste_clipboard : 'a t -> unit
	val delete_selection : 'a t -> unit
	val set_position : 'a t -> int -> unit
	val get_position : 'a t -> int
	val set_editable : 'a t -> bool -> unit
	val get_editable : 'a t -> bool
	val changed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a editable_t = unit
	type 'a t = 'a editable_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toEditable obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_editable_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val select_region_ : cptr * int * int -> unit
	    = _import "gtk_editable_select_region" : cptr * int * int -> unit;
	val select_region : 'a t -> int -> int -> unit
	    = fn self => fn start => fn en =>
		 GObject.withPtr
		   (self, fn self => select_region_ (self, start, en))
	val insert_text_ : cptr * CString.cstring * int * int ref -> unit
	    = _import "gtk_editable_insert_text"
		      : cptr * CString.cstring * int * int ref -> unit;
	val insert_text : 'a t -> string -> int -> int -> int
	    = fn self => fn new_text => fn new_text_length => fn position =>
		 let val position = ref position
		     val ret = GObject.withPtr
				 (self, 
				  fn self =>
				     insert_text_
				       (self, CString.fromString new_text, 
					new_text_length, position))
		 in !position end
	val delete_text_ : cptr * int * int -> unit
	    = _import "gtk_editable_delete_text" : cptr * int * int -> unit;
	val delete_text : 'a t -> int -> int -> unit
	    = fn self => fn start_pos => fn end_pos =>
		 GObject.withPtr
		   (self, fn self => delete_text_ (self, start_pos, end_pos))
	val get_chars_ : cptr * int * int -> CString.t
	    = _import "gtk_editable_get_chars" : cptr * int * int -> CString.t;
	val get_chars : 'a t -> int -> int -> string
	    = fn self => fn start_pos => fn end_pos =>
		 GObject.withPtr
		   (self, 
		    fn self => let val t = get_chars_
					     (self, start_pos, end_pos)
			       in CString.toString t end)
	val cut_clipboard_ : cptr -> unit
	    = _import "gtk_editable_cut_clipboard" : cptr -> unit;
	val cut_clipboard : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => cut_clipboard_ self)
	val copy_clipboard_ : cptr -> unit
	    = _import "gtk_editable_copy_clipboard" : cptr -> unit;
	val copy_clipboard : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => copy_clipboard_ self)
	val paste_clipboard_ : cptr -> unit
	    = _import "gtk_editable_paste_clipboard" : cptr -> unit;
	val paste_clipboard : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => paste_clipboard_ self)
	val delete_selection_ : cptr -> unit
	    = _import "gtk_editable_delete_selection" : cptr -> unit;
	val delete_selection : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => delete_selection_ self)
	val set_position_ : cptr * int -> unit
	    = _import "gtk_editable_set_position" : cptr * int -> unit;
	val set_position : 'a t -> int -> unit
	    = fn self => fn position =>
		 GObject.withPtr
		   (self, fn self => set_position_ (self, position))
	val get_position_ : cptr -> int
	    = _import "gtk_editable_get_position" : cptr -> int;
	val get_position : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_position_ self)
	val set_editable_ : cptr * bool -> unit
	    = _import "gtk_editable_set_editable" : cptr * bool -> unit;
	val set_editable : 'a t -> bool -> unit
	    = fn self => fn is_editable =>
		 GObject.withPtr
		   (self, fn self => set_editable_ (self, is_editable))
	val get_editable_ : cptr -> bool
	    = _import "gtk_editable_get_editable" : cptr -> bool;
	val get_editable : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_editable_ self)
	local open Signal
	      infixr -->
	in val changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "changed" false (void --> return_void) f
	end
    end
    structure Container :>
      sig
	type base
	type 'a container_t
	type 'a t = 'a container_t Widget.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toContainer : 'a t -> base t
	val get_type : unit -> GType.t
	val set_border_width : 'a t -> int -> unit
	val get_border_width : 'a t -> int
	val add : 'a t -> 'b Widget.t -> unit
	val remove : 'a t -> 'b Widget.t -> unit
	val set_resize_mode : 'a t -> resize_mode -> unit
	val get_resize_mode : 'a t -> resize_mode
	val check_resize : 'a t -> unit
	val unset_focus_chain : 'a t -> unit
	val set_reallocate_redraws : 'a t -> bool -> unit
	val set_focus_child : 'a t -> 'b Widget.t -> unit
	val set_focus_vadjustment : 'a t -> 'b Adjustment.t -> unit
	val get_focus_vadjustment : 'a t -> base Adjustment.t
	val set_focus_hadjustment : 'a t -> 'b Adjustment.t -> unit
	val get_focus_hadjustment : 'a t -> base Adjustment.t
	val resize_children : 'a t -> unit
	val childtype : 'a t -> GType.t
	val add_with_properties : 'a t -> 'b Widget.t -> string -> unit
	val child_set : 'a t -> 'b Widget.t -> string -> unit
	val child_get : 'a t -> 'b Widget.t -> string -> unit
	val child_set_property
	  : 'a t -> 'b Widget.t -> string -> GValue.GValue -> unit
	val child_get_property
	  : 'a t -> 'b Widget.t -> string -> GValue.GValue -> unit
	val add_sig : (unit -> unit) -> 'a t Signal.signal
	val remove_sig : (unit -> unit) -> 'a t Signal.signal
	val check_resize_sig : (unit -> unit) -> 'a t Signal.signal
	val set_focus_child_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a container_t = unit
	type 'a t = 'a container_t Widget.t
	fun inherit w con = Widget.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toContainer obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_container_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_border_width_ : cptr * int -> unit
	    = _import "gtk_container_set_border_width" : cptr * int -> unit;
	val set_border_width : 'a t -> int -> unit
	    = fn self => fn border_width =>
		 GObject.withPtr
		   (self, fn self => set_border_width_ (self, border_width))
	val get_border_width_ : cptr -> int
	    = _import "gtk_container_get_border_width" : cptr -> int;
	val get_border_width : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_border_width_ self)
	val add_ : cptr * cptr -> unit
	    = _import "gtk_container_add" : cptr * cptr -> unit;
	val add : 'a t -> 'b Widget.t -> unit
	    = fn self => fn widget =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (widget, fn widget => add_ (self, widget)))
	val remove_ : cptr * cptr -> unit
	    = _import "gtk_container_remove" : cptr * cptr -> unit;
	val remove : 'a t -> 'b Widget.t -> unit
	    = fn self => fn widget =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (widget, fn widget => remove_ (self, widget)))
	val set_resize_mode_ : cptr * int -> unit
	    = _import "gtk_container_set_resize_mode" : cptr * int -> unit;
	val set_resize_mode : 'a t -> resize_mode -> unit
	    = fn self => fn resize_mode =>
		 GObject.withPtr
		   (self, fn self => set_resize_mode_ (self, resize_mode))
	val get_resize_mode_ : cptr -> int
	    = _import "gtk_container_get_resize_mode" : cptr -> int;
	val get_resize_mode : 'a t -> resize_mode
	    = fn self => GObject.withPtr
			   (self, fn self => get_resize_mode_ self)
	val check_resize_ : cptr -> unit
	    = _import "gtk_container_check_resize" : cptr -> unit;
	val check_resize : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => check_resize_ self)
	val unset_focus_chain_ : cptr -> unit
	    = _import "gtk_container_unset_focus_chain" : cptr -> unit;
	val unset_focus_chain : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => unset_focus_chain_ self)
	val set_reallocate_redraws_ : cptr * bool -> unit
	    = _import "gtk_container_set_reallocate_redraws"
		      : cptr * bool -> unit;
	val set_reallocate_redraws : 'a t -> bool -> unit
	    = fn self => fn needs_redraws =>
		 GObject.withPtr (self, 
				  fn self => set_reallocate_redraws_
					       (self, needs_redraws))
	val set_focus_child_ : cptr * cptr -> unit
	    = _import "gtk_container_set_focus_child" : cptr * cptr -> unit;
	val set_focus_child : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (child, fn child => set_focus_child_ (self, child)))
	val set_focus_vadjustment_ : cptr * cptr -> unit
	    = _import "gtk_container_set_focus_vadjustment"
		      : cptr * cptr -> unit;
	val set_focus_vadjustment : 'a t -> 'b Adjustment.t -> unit
	    = fn self => fn adjustment =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (adjustment, 
				  fn adjustment => set_focus_vadjustment_
						     (self, adjustment)))
	val get_focus_vadjustment_ : cptr -> cptr
	    = _import "gtk_container_get_focus_vadjustment" : cptr -> cptr;
	val get_focus_vadjustment : 'a t -> base Adjustment.t
	    = fn self =>
		 Adjustment.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_focus_vadjustment_ self))
	val set_focus_hadjustment_ : cptr * cptr -> unit
	    = _import "gtk_container_set_focus_hadjustment"
		      : cptr * cptr -> unit;
	val set_focus_hadjustment : 'a t -> 'b Adjustment.t -> unit
	    = fn self => fn adjustment =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (adjustment, 
				  fn adjustment => set_focus_hadjustment_
						     (self, adjustment)))
	val get_focus_hadjustment_ : cptr -> cptr
	    = _import "gtk_container_get_focus_hadjustment" : cptr -> cptr;
	val get_focus_hadjustment : 'a t -> base Adjustment.t
	    = fn self =>
		 Adjustment.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_focus_hadjustment_ self))
	val resize_children_ : cptr -> unit
	    = _import "gtk_container_resize_children" : cptr -> unit;
	val resize_children : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => resize_children_ self)
	val childtype_ : cptr -> GType.t
	    = _import "gtk_container_child_type" : cptr -> GType.t;
	val childtype : 'a t -> GType.t
	    = fn self => GObject.withPtr (self, fn self => childtype_ self)
	val add_with_properties_ : cptr * cptr * CString.cstring -> unit
	    = _import "gtk_container_add_with_properties"
		      : cptr * cptr * CString.cstring -> unit;
	val add_with_properties : 'a t -> 'b Widget.t -> string -> unit
	    = fn self => fn widget => fn first_prop_name =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (widget, 
				  fn widget => add_with_properties_
						 (self, widget, 
						  CString.fromString
						    first_prop_name)))
	val child_set_ : cptr * cptr * CString.cstring -> unit
	    = _import "gtk_container_child_set"
		      : cptr * cptr * CString.cstring -> unit;
	val child_set : 'a t -> 'b Widget.t -> string -> unit
	    = fn self => fn child => fn first_prop_name =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => child_set_ (self, child, 
							  CString.fromString
							    first_prop_name)))
	val child_get_ : cptr * cptr * CString.cstring -> unit
	    = _import "gtk_container_child_get"
		      : cptr * cptr * CString.cstring -> unit;
	val child_get : 'a t -> 'b Widget.t -> string -> unit
	    = fn self => fn child => fn first_prop_name =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => child_get_ (self, child, 
							  CString.fromString
							    first_prop_name)))
	val child_set_property_
	  : cptr * cptr * CString.cstring * GValue.GValue -> unit
	    = _import "gtk_container_child_set_property"
		      : cptr * cptr * CString.cstring * GValue.GValue -> unit;
	val child_set_property
	  : 'a t -> 'b Widget.t -> string -> GValue.GValue -> unit
	    = fn self => fn child => fn property_name => fn value =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => child_set_property_
						(self, child, 
						 CString.fromString
						   property_name, 
						 value)))
	val child_get_property_
	  : cptr * cptr * CString.cstring * GValue.GValue -> unit
	    = _import "gtk_container_child_get_property"
		      : cptr * cptr * CString.cstring * GValue.GValue -> unit;
	val child_get_property
	  : 'a t -> 'b Widget.t -> string -> GValue.GValue -> unit
	    = fn self => fn child => fn property_name => fn value =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => child_get_property_
						(self, child, 
						 CString.fromString
						   property_name, 
						 value)))
	local open Signal
	      infixr -->
	in val add_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "add" false (unit --> return_void) f
	   val remove_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "remove" false (unit --> return_void) f
	   val check_resize_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "check-resize" false (void --> return_void) f
	   val set_focus_child_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f =>
		    signal "set-focus-child" false (unit --> return_void) f
	end
    end
    structure Bin :>
      sig
	type base
	type 'a bin_t
	type 'a t = 'a bin_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toBin : 'a t -> base t
	val get_type : unit -> GType.t
	val get_child : 'a t -> base Widget.t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a bin_t = unit
	type 'a t = 'a bin_t Container.t
	fun inherit w con = Container.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toBin obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_bin_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val get_child_ : cptr -> cptr
	    = _import "gtk_bin_get_child" : cptr -> cptr;
	val get_child : 'a t -> base Widget.t
	    = fn self => Widget.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_child_ self))
    end
    structure Tooltips :>
      sig
	type base
	type 'a tooltips_t
	type 'a t = 'a tooltips_t Object.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTooltips : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val enable : 'a t -> unit
	val disable : 'a t -> unit
	val set_tip
	  : 'a t -> 'b Widget.t -> string option -> string option -> unit
	val set_tip' : 'a t -> 'b Widget.t -> unit
	val force_window : 'a t -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a tooltips_t = unit
	type 'a t = 'a tooltips_t Object.t
	fun inherit w con = Object.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTooltips obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_tooltips_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_tooltips_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val enable_ : cptr -> unit
	    = _import "gtk_tooltips_enable" : cptr -> unit;
	val enable : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => enable_ self)
	val disable_ : cptr -> unit
	    = _import "gtk_tooltips_disable" : cptr -> unit;
	val disable : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => disable_ self)
	val set_tip_ : cptr * cptr * CString.cstring * CString.cstring -> unit
	    = _import "gtk_tooltips_set_tip"
		      : cptr * cptr * CString.cstring * CString.cstring
			-> unit;
	val set_tip
	  : 'a t -> 'b Widget.t -> string option -> string option -> unit
	    = fn self => fn widget => fn tip_text => fn tip_private =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (widget, 
				  fn widget =>
				     set_tip_ (self, widget, 
					       CString.fromString
						 (getOpt (tip_text, "")), 
					       CString.fromString
						 (getOpt (tip_private, "")))))
	val set_tip' : 'a t -> 'b Widget.t -> unit
	    = fn self => fn widget =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (widget, 
				  fn widget =>
				     set_tip_ (self, widget, 
					       CString.fromString "", 
					       CString.fromString "")))
	val force_window_ : cptr -> unit
	    = _import "gtk_tooltips_force_window" : cptr -> unit;
	val force_window : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => force_window_ self)
    end
    structure Window :>
      sig
	type base
	type 'a window_t
	type 'a t = 'a window_t Bin.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toWindow : 'a t -> base t
	type position
	val WIN_POS_NONE : position
	val WIN_POS_CENTER : position
	val WIN_POS_MOUSE : position
	val WIN_POS_CENTER_ALWAYS : position
	val WIN_POS_CENTER_ON_PARENT : position
	type type_t
	val TOPLEVEL : type_t
	val POPUP : type_t
	val tooltips_get_info_from_tip_window
	  : 'a t -> 'b Tooltips.t -> 'c Widget.t -> bool
	val get_type : unit -> GType.t
	val new : type_t -> base t
	val set_title : 'a t -> string -> unit
	val get_title : 'a t -> string
	val set_wmclass : 'a t -> string -> string -> unit
	val set_role : 'a t -> string -> unit
	val get_role : 'a t -> string
	val add_accelgroup : 'a t -> 'b AccelGroup.t -> unit
	val remove_accelgroup : 'a t -> 'b AccelGroup.t -> unit
	val set_position : 'a t -> position -> unit
	val activate_focus : 'a t -> bool
	val set_focus : 'a t -> 'b Widget.t option -> unit
	val set_focus' : 'a t -> unit
	val get_focus : 'a t -> base Widget.t
	val set_default : 'a t -> 'b Widget.t option -> unit
	val set_default' : 'a t -> unit
	val activate_default : 'a t -> bool
	val set_transient_for : 'a t -> 'b t option -> unit
	val set_transient_for' : 'a t -> unit
	val get_transient_for : 'a t -> base t
	val set_skip_taskbar_hint : 'a t -> bool -> unit
	val get_skip_taskbar_hint : 'a t -> bool
	val set_skip_pager_hint : 'a t -> bool -> unit
	val get_skip_pager_hint : 'a t -> bool
	val set_accept_focus : 'a t -> bool -> unit
	val get_accept_focus : 'a t -> bool
	val set_destroy_with_parent : 'a t -> bool -> unit
	val get_destroy_with_parent : 'a t -> bool
	val set_resizable : 'a t -> bool -> unit
	val get_resizable : 'a t -> bool
	val is_active : 'a t -> bool
	val has_toplevel_focus : 'a t -> bool
	val set_has_frame : 'a t -> bool -> unit
	val get_has_frame : 'a t -> bool
	val set_frame_dimensions : 'a t -> int -> int -> int -> int -> unit
	val set_decorated : 'a t -> bool -> unit
	val get_decorated : 'a t -> bool
	val set_auto_startup_notification : bool -> unit
	val set_modal : 'a t -> bool -> unit
	val get_modal : 'a t -> bool
	val add_mnemonic : 'a t -> int -> 'b Widget.t -> unit
	val remove_mnemonic : 'a t -> int -> 'b Widget.t -> unit
	val present : 'a t -> unit
	val iconify : 'a t -> unit
	val deiconify : 'a t -> unit
	val stick : 'a t -> unit
	val unstick : 'a t -> unit
	val maximize : 'a t -> unit
	val unmaximize : 'a t -> unit
	val fullscreen : 'a t -> unit
	val unfullscreen : 'a t -> unit
	val set_keep_above : 'a t -> bool -> unit
	val set_keep_below : 'a t -> bool -> unit
	val begin_move_drag : 'a t -> int -> int -> int -> int -> unit
	val set_default_size : 'a t -> int -> int -> unit
	val get_default_size : 'a t -> int * int
	val resize : 'a t -> int -> int -> unit
	val get_size : 'a t -> int * int
	val move : 'a t -> int -> int -> unit
	val get_position : 'a t -> int * int
	val parse_geometry : 'a t -> string -> bool
	val reshow_with_initial_size : 'a t -> unit
	val group_get_type : unit -> GType.t
	val remove_embedded_xid : 'a t -> int -> unit
	val add_embedded_xid : 'a t -> int -> unit
	val set_focus_sig : (unit -> unit) -> 'a t Signal.signal
	val frame_event_sig : (unit -> bool) -> 'a t Signal.signal
	val activate_focus_sig : (unit -> unit) -> 'a t Signal.signal
	val activate_default_sig : (unit -> unit) -> 'a t Signal.signal
	val move_focus_sig : (unit -> unit) -> 'a t Signal.signal
	val keys_changed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a window_t = unit
	type 'a t = 'a window_t Bin.t
	fun inherit w con = Bin.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toWindow obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type position = int
	val get_position_
	  : int ref * int ref * int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_window_position"
		      : int ref * int ref * int ref * int ref * int ref
			-> unit;
	val (WIN_POS_NONE, WIN_POS_CENTER, WIN_POS_MOUSE, 
	     WIN_POS_CENTER_ALWAYS, WIN_POS_CENTER_ON_PARENT)
	    = let val (x0, x1, x2, x3, x4)
		      = (ref 0, ref 0, ref 0, ref 0, ref 0)
	      in get_position_ (x0, x1, x2, x3, x4)
	       ; (!x0, !x1, !x2, !x3, !x4)
	      end
	type type_t = int
	val get_type_t_ : int ref * int ref -> unit
	    = _import "mgtk_get_gtk_window_type" : int ref * int ref -> unit;
	val (TOPLEVEL, POPUP) = let val (x0, x1) = (ref 0, ref 0)
				in get_type_t_ (x0, x1)
				 ; (!x0, !x1)
				end
	val tooltips_get_info_from_tip_window_ : cptr * cptr * cptr -> bool
	    = _import "gtk_tooltips_get_info_from_tip_window"
		      : cptr * cptr * cptr -> bool;
	val tooltips_get_info_from_tip_window
	  : 'a t -> 'b Tooltips.t -> 'c Widget.t -> bool
	    = fn self => fn tooltips => fn current_widget =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (tooltips, 
			  fn tooltips =>
			     GObject.withPtr
			       (current_widget, 
				fn current_widget =>
				   tooltips_get_info_from_tip_window_
				     (self, tooltips, current_widget))))
	val get_type_ : unit -> GType.t
	    = _import "gtk_window_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : int -> cptr = _import "gtk_window_new" : int -> cptr;
	val new : type_t -> base t = fn typ => make (new_ typ)
	val set_title_ : cptr * CString.cstring -> unit
	    = _import "gtk_window_set_title" : cptr * CString.cstring -> unit;
	val set_title : 'a t -> string -> unit
	    = fn self => fn title =>
		 GObject.withPtr
		   (self, 
		    fn self => set_title_ (self, CString.fromString title))
	val get_title_ : cptr -> CString.t
	    = _import "gtk_window_get_title" : cptr -> CString.t;
	val get_title : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_title_ self
				       in CString.toString t end)
	val set_wmclass_ : cptr * CString.cstring * CString.cstring -> unit
	    = _import "gtk_window_set_wmclass"
		      : cptr * CString.cstring * CString.cstring -> unit;
	val set_wmclass : 'a t -> string -> string -> unit
	    = fn self => fn wmclass_name => fn wmclass_class =>
		 GObject.withPtr
		   (self, 
		    fn self => set_wmclass_
				 (self, CString.fromString wmclass_name, 
				  CString.fromString wmclass_class))
	val set_role_ : cptr * CString.cstring -> unit
	    = _import "gtk_window_set_role" : cptr * CString.cstring -> unit;
	val set_role : 'a t -> string -> unit
	    = fn self => fn role =>
		 GObject.withPtr
		   (self, fn self => set_role_ (self, CString.fromString role))
	val get_role_ : cptr -> CString.t
	    = _import "gtk_window_get_role" : cptr -> CString.t;
	val get_role : 'a t -> string
	    = fn self => GObject.withPtr (self, 
					  fn self => let val t = get_role_ self
						     in CString.toString t end)
	val add_accelgroup_ : cptr * cptr -> unit
	    = _import "gtk_window_add_accel_group" : cptr * cptr -> unit;
	val add_accelgroup : 'a t -> 'b AccelGroup.t -> unit
	    = fn self => fn accel_group =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (accel_group, 
				  fn accel_group =>
				     add_accelgroup_ (self, accel_group)))
	val remove_accelgroup_ : cptr * cptr -> unit
	    = _import "gtk_window_remove_accel_group" : cptr * cptr -> unit;
	val remove_accelgroup : 'a t -> 'b AccelGroup.t -> unit
	    = fn self => fn accel_group =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr (accel_group, 
						fn accel_group =>
						   remove_accelgroup_
						     (self, accel_group)))
	val set_position_ : cptr * int -> unit
	    = _import "gtk_window_set_position" : cptr * int -> unit;
	val set_position : 'a t -> position -> unit
	    = fn self => fn position =>
		 GObject.withPtr
		   (self, fn self => set_position_ (self, position))
	val activate_focus_ : cptr -> bool
	    = _import "gtk_window_activate_focus" : cptr -> bool;
	val activate_focus : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => activate_focus_ self)
	val set_focus_ : cptr * cptr -> unit
	    = _import "gtk_window_set_focus" : cptr * cptr -> unit;
	val set_focus : 'a t -> 'b Widget.t option -> unit
	    = fn self => fn focus =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withOpt
				 (focus, fn focus => set_focus_ (self, focus)))
	val set_focus' : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => set_focus_ (self, GObject.null))
	val get_focus_ : cptr -> cptr
	    = _import "gtk_window_get_focus" : cptr -> cptr;
	val get_focus : 'a t -> base Widget.t
	    = fn self => Widget.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_focus_ self))
	val set_default_ : cptr * cptr -> unit
	    = _import "gtk_window_set_default" : cptr * cptr -> unit;
	val set_default : 'a t -> 'b Widget.t option -> unit
	    = fn self => fn default_widget =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withOpt
				 (default_widget, 
				  fn default_widget =>
				     set_default_ (self, default_widget)))
	val set_default' : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => set_default_ (self, GObject.null))
	val activate_default_ : cptr -> bool
	    = _import "gtk_window_activate_default" : cptr -> bool;
	val activate_default : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => activate_default_ self)
	val set_transient_for_ : cptr * cptr -> unit
	    = _import "gtk_window_set_transient_for" : cptr * cptr -> unit;
	val set_transient_for : 'a t -> 'b t option -> unit
	    = fn self => fn parent =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withOpt (parent, 
						fn parent => set_transient_for_
							       (self, parent)))
	val set_transient_for' : 'a t -> unit
	    = fn self => GObject.withPtr (self, 
					  fn self => set_transient_for_
						       (self, GObject.null))
	val get_transient_for_ : cptr -> cptr
	    = _import "gtk_window_get_transient_for" : cptr -> cptr;
	val get_transient_for : 'a t -> base t
	    = fn self => make (GObject.withPtr
				 (self, fn self => get_transient_for_ self))
	val set_skip_taskbar_hint_ : cptr * bool -> unit
	    = _import "gtk_window_set_skip_taskbar_hint" : cptr * bool -> unit;
	val set_skip_taskbar_hint : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_skip_taskbar_hint_ (self, setting))
	val get_skip_taskbar_hint_ : cptr -> bool
	    = _import "gtk_window_get_skip_taskbar_hint" : cptr -> bool;
	val get_skip_taskbar_hint : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_skip_taskbar_hint_ self)
	val set_skip_pager_hint_ : cptr * bool -> unit
	    = _import "gtk_window_set_skip_pager_hint" : cptr * bool -> unit;
	val set_skip_pager_hint : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_skip_pager_hint_ (self, setting))
	val get_skip_pager_hint_ : cptr -> bool
	    = _import "gtk_window_get_skip_pager_hint" : cptr -> bool;
	val get_skip_pager_hint : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_skip_pager_hint_ self)
	val set_accept_focus_ : cptr * bool -> unit
	    = _import "gtk_window_set_accept_focus" : cptr * bool -> unit;
	val set_accept_focus : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_accept_focus_ (self, setting))
	val get_accept_focus_ : cptr -> bool
	    = _import "gtk_window_get_accept_focus" : cptr -> bool;
	val get_accept_focus : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_accept_focus_ self)
	val set_destroy_with_parent_ : cptr * bool -> unit
	    = _import "gtk_window_set_destroy_with_parent"
		      : cptr * bool -> unit;
	val set_destroy_with_parent : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_destroy_with_parent_ (self, setting))
	val get_destroy_with_parent_ : cptr -> bool
	    = _import "gtk_window_get_destroy_with_parent" : cptr -> bool;
	val get_destroy_with_parent : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_destroy_with_parent_ self)
	val set_resizable_ : cptr * bool -> unit
	    = _import "gtk_window_set_resizable" : cptr * bool -> unit;
	val set_resizable : 'a t -> bool -> unit
	    = fn self => fn resizable =>
		 GObject.withPtr
		   (self, fn self => set_resizable_ (self, resizable))
	val get_resizable_ : cptr -> bool
	    = _import "gtk_window_get_resizable" : cptr -> bool;
	val get_resizable : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_resizable_ self)
	val is_active_ : cptr -> bool
	    = _import "gtk_window_is_active" : cptr -> bool;
	val is_active : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => is_active_ self)
	val has_toplevel_focus_ : cptr -> bool
	    = _import "gtk_window_has_toplevel_focus" : cptr -> bool;
	val has_toplevel_focus : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => has_toplevel_focus_ self)
	val set_has_frame_ : cptr * bool -> unit
	    = _import "gtk_window_set_has_frame" : cptr * bool -> unit;
	val set_has_frame : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_has_frame_ (self, setting))
	val get_has_frame_ : cptr -> bool
	    = _import "gtk_window_get_has_frame" : cptr -> bool;
	val get_has_frame : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_has_frame_ self)
	val set_frame_dimensions_ : cptr * int * int * int * int -> unit
	    = _import "gtk_window_set_frame_dimensions"
		      : cptr * int * int * int * int -> unit;
	val set_frame_dimensions : 'a t -> int -> int -> int -> int -> unit
	    = fn self => fn left => fn top => fn right => fn bottom =>
		 GObject.withPtr
		   (self, 
		    fn self => set_frame_dimensions_
				 (self, left, top, right, bottom))
	val set_decorated_ : cptr * bool -> unit
	    = _import "gtk_window_set_decorated" : cptr * bool -> unit;
	val set_decorated : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_decorated_ (self, setting))
	val get_decorated_ : cptr -> bool
	    = _import "gtk_window_get_decorated" : cptr -> bool;
	val get_decorated : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_decorated_ self)
	val set_auto_startup_notification_ : bool -> unit
	    = _import "gtk_window_set_auto_startup_notification"
		      : bool -> unit;
	val set_auto_startup_notification : bool -> unit
	    = fn setting => set_auto_startup_notification_ setting
	val set_modal_ : cptr * bool -> unit
	    = _import "gtk_window_set_modal" : cptr * bool -> unit;
	val set_modal : 'a t -> bool -> unit
	    = fn self => fn modal =>
		 GObject.withPtr (self, fn self => set_modal_ (self, modal))
	val get_modal_ : cptr -> bool
	    = _import "gtk_window_get_modal" : cptr -> bool;
	val get_modal : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_modal_ self)
	val add_mnemonic_ : cptr * int * cptr -> unit
	    = _import "gtk_window_add_mnemonic" : cptr * int * cptr -> unit;
	val add_mnemonic : 'a t -> int -> 'b Widget.t -> unit
	    = fn self => fn keyval => fn target =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (target, 
				  fn target => add_mnemonic_
						 (self, keyval, target)))
	val remove_mnemonic_ : cptr * int * cptr -> unit
	    = _import "gtk_window_remove_mnemonic" : cptr * int * cptr -> unit;
	val remove_mnemonic : 'a t -> int -> 'b Widget.t -> unit
	    = fn self => fn keyval => fn target =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (target, 
				  fn target => remove_mnemonic_
						 (self, keyval, target)))
	val present_ : cptr -> unit
	    = _import "gtk_window_present" : cptr -> unit;
	val present : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => present_ self)
	val iconify_ : cptr -> unit
	    = _import "gtk_window_iconify" : cptr -> unit;
	val iconify : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => iconify_ self)
	val deiconify_ : cptr -> unit
	    = _import "gtk_window_deiconify" : cptr -> unit;
	val deiconify : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => deiconify_ self)
	val stick_ : cptr -> unit = _import "gtk_window_stick" : cptr -> unit;
	val stick : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => stick_ self)
	val unstick_ : cptr -> unit
	    = _import "gtk_window_unstick" : cptr -> unit;
	val unstick : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => unstick_ self)
	val maximize_ : cptr -> unit
	    = _import "gtk_window_maximize" : cptr -> unit;
	val maximize : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => maximize_ self)
	val unmaximize_ : cptr -> unit
	    = _import "gtk_window_unmaximize" : cptr -> unit;
	val unmaximize : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => unmaximize_ self)
	val fullscreen_ : cptr -> unit
	    = _import "gtk_window_fullscreen" : cptr -> unit;
	val fullscreen : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => fullscreen_ self)
	val unfullscreen_ : cptr -> unit
	    = _import "gtk_window_unfullscreen" : cptr -> unit;
	val unfullscreen : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => unfullscreen_ self)
	val set_keep_above_ : cptr * bool -> unit
	    = _import "gtk_window_set_keep_above" : cptr * bool -> unit;
	val set_keep_above : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_keep_above_ (self, setting))
	val set_keep_below_ : cptr * bool -> unit
	    = _import "gtk_window_set_keep_below" : cptr * bool -> unit;
	val set_keep_below : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_keep_below_ (self, setting))
	val begin_move_drag_ : cptr * int * int * int * int -> unit
	    = _import "gtk_window_begin_move_drag"
		      : cptr * int * int * int * int -> unit;
	val begin_move_drag : 'a t -> int -> int -> int -> int -> unit
	    = fn self => fn button => fn root_x => fn root_y => fn timestamp =>
		 GObject.withPtr
		   (self, 
		    fn self => begin_move_drag_
				 (self, button, root_x, root_y, timestamp))
	val set_default_size_ : cptr * int * int -> unit
	    = _import "gtk_window_set_default_size" : cptr * int * int -> unit;
	val set_default_size : 'a t -> int -> int -> unit
	    = fn self => fn width => fn height =>
		 GObject.withPtr
		   (self, fn self => set_default_size_ (self, width, height))
	val get_default_size_ : cptr * int ref * int ref -> unit
	    = _import "gtk_window_get_default_size"
		      : cptr * int ref * int ref -> unit;
	val get_default_size : 'a t -> int * int
	    = fn self => let val (width, height) = (ref 0, ref 0)
			     val ret = GObject.withPtr
					 (self, 
					  fn self => get_default_size_
						       (self, width, height))
			 in (!width, !height) end
	val resize_ : cptr * int * int -> unit
	    = _import "gtk_window_resize" : cptr * int * int -> unit;
	val resize : 'a t -> int -> int -> unit
	    = fn self => fn width => fn height =>
		 GObject.withPtr
		   (self, fn self => resize_ (self, width, height))
	val get_size_ : cptr * int ref * int ref -> unit
	    = _import "gtk_window_get_size" : cptr * int ref * int ref -> unit;
	val get_size : 'a t -> int * int
	    = fn self =>
		 let val (width, height) = (ref 0, ref 0)
		     val ret = GObject.withPtr
				 (self, 
				  fn self => get_size_ (self, width, height))
		 in (!width, !height) end
	val move_ : cptr * int * int -> unit
	    = _import "gtk_window_move" : cptr * int * int -> unit;
	val move : 'a t -> int -> int -> unit
	    = fn self => fn x => fn y =>
		 GObject.withPtr (self, fn self => move_ (self, x, y))
	val get_position_ : cptr * int ref * int ref -> unit
	    = _import "gtk_window_get_position"
		      : cptr * int ref * int ref -> unit;
	val get_position : 'a t -> int * int
	    = fn self => let val (root_x, root_y) = (ref 0, ref 0)
			     val ret = GObject.withPtr
					 (self, 
					  fn self => get_position_
						       (self, root_x, root_y))
			 in (!root_x, !root_y) end
	val parse_geometry_ : cptr * CString.cstring -> bool
	    = _import "gtk_window_parse_geometry"
		      : cptr * CString.cstring -> bool;
	val parse_geometry : 'a t -> string -> bool
	    = fn self => fn geometry =>
		 GObject.withPtr
		   (self, 
		    fn self => parse_geometry_
				 (self, CString.fromString geometry))
	val reshow_with_initial_size_ : cptr -> unit
	    = _import "gtk_window_reshow_with_initial_size" : cptr -> unit;
	val reshow_with_initial_size : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => reshow_with_initial_size_ self)
	val group_get_type_ : unit -> GType.t
	    = _import "gtk_window_group_get_type" : unit -> GType.t;
	val group_get_type : unit -> GType.t
	    = fn dummy => group_get_type_ dummy
	val remove_embedded_xid_ : cptr * int -> unit
	    = _import "gtk_window_remove_embedded_xid" : cptr * int -> unit;
	val remove_embedded_xid : 'a t -> int -> unit
	    = fn self => fn xid =>
		 GObject.withPtr
		   (self, fn self => remove_embedded_xid_ (self, xid))
	val add_embedded_xid_ : cptr * int -> unit
	    = _import "gtk_window_add_embedded_xid" : cptr * int -> unit;
	val add_embedded_xid : 'a t -> int -> unit
	    = fn self => fn xid =>
		 GObject.withPtr
		   (self, fn self => add_embedded_xid_ (self, xid))
	local open Signal
	      infixr -->
	in val set_focus_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "set-focus" false (unit --> return_void) f
	   val frame_event_sig : (unit -> bool) -> 'a t Signal.signal
	       = fn f => signal "frame-event" false (unit --> return_bool) f
	   val activate_focus_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "activate-focus" false (void --> return_void) f
	   val activate_default_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f =>
		    signal "activate-default" false (void --> return_void) f
	   val move_focus_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "move-focus" false (unit --> return_void) f
	   val keys_changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "keys-changed" false (void --> return_void) f
	end
    end
    structure FileChooser :>
      sig
	type base
	type 'a filechooser_t
	type 'a t = 'a filechooser_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toFileChooser : 'a t -> base t
	type action
	val ACTION_OPEN : action
	val ACTION_SAVE : action
	val ACTION_SELECT_FOLDER : action
	val ACTION_CREATE_FOLDER : action
	type error
	val ERROR_NONEXISTENT : error
	val ERROR_BAD_FILENAME : error
	val get_type : unit -> GType.t
	val set_action : 'a t -> action -> unit
	val get_action : 'a t -> action
	val set_local_only : 'a t -> bool -> unit
	val get_local_only : 'a t -> bool
	val set_select_multiple : 'a t -> bool -> unit
	val get_select_multiple : 'a t -> bool
	val set_current_name : 'a t -> string -> unit
	val get_filename : 'a t -> string
	val set_filename : 'a t -> string -> bool
	val select_filename : 'a t -> string -> bool
	val unselect_filename : 'a t -> string -> unit
	val select_all : 'a t -> unit
	val unselect_all : 'a t -> unit
	val set_current_folder : 'a t -> string -> bool
	val get_current_folder : 'a t -> string
	val get_uri : 'a t -> string
	val set_uri : 'a t -> string -> bool
	val select_uri : 'a t -> string -> bool
	val unselect_uri : 'a t -> string -> unit
	val set_current_folder_uri : 'a t -> string -> bool
	val get_current_folder_uri : 'a t -> string
	val set_preview_widget : 'a t -> 'b Widget.t -> unit
	val get_preview_widget : 'a t -> base Widget.t
	val set_preview_widget_active : 'a t -> bool -> unit
	val get_preview_widget_active : 'a t -> bool
	val set_use_preview_label : 'a t -> bool -> unit
	val get_use_preview_label : 'a t -> bool
	val get_preview_filename : 'a t -> string
	val get_preview_uri : 'a t -> string
	val set_extra_widget : 'a t -> 'b Widget.t -> unit
	val get_extra_widget : 'a t -> base Widget.t
	val dialog_get_type : unit -> GType.t
	val dialog_new_with_backend
	  : string -> 'a Window.t -> action -> string -> string -> base t
	val widget_get_type : unit -> GType.t
	val widget_new_with_backend : action -> string -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a filechooser_t = unit
	type 'a t = 'a filechooser_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toFileChooser obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type action = int
	val get_action_ : int ref * int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_file_chooser_action"
		      : int ref * int ref * int ref * int ref -> unit;
	val (ACTION_OPEN, ACTION_SAVE, ACTION_SELECT_FOLDER, 
	     ACTION_CREATE_FOLDER)
	    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	      in get_action_ (x0, x1, x2, x3)
	       ; (!x0, !x1, !x2, !x3)
	      end
	type error = int
	val get_error_ : int ref * int ref -> unit
	    = _import "mgtk_get_gtk_file_chooser_error"
		      : int ref * int ref -> unit;
	val (ERROR_NONEXISTENT, ERROR_BAD_FILENAME)
	    = let val (x0, x1) = (ref 0, ref 0) in get_error_ (x0, x1)
						 ; (!x0, !x1)
						end
	val get_type_ : unit -> GType.t
	    = _import "gtk_file_chooser_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_action_ : cptr * int -> unit
	    = _import "gtk_file_chooser_set_action" : cptr * int -> unit;
	val set_action : 'a t -> action -> unit
	    = fn self => fn action =>
		 GObject.withPtr (self, fn self => set_action_ (self, action))
	val get_action_ : cptr -> int
	    = _import "gtk_file_chooser_get_action" : cptr -> int;
	val get_action : 'a t -> action
	    = fn self => GObject.withPtr (self, fn self => get_action_ self)
	val set_local_only_ : cptr * bool -> unit
	    = _import "gtk_file_chooser_set_local_only" : cptr * bool -> unit;
	val set_local_only : 'a t -> bool -> unit
	    = fn self => fn local_only =>
		 GObject.withPtr
		   (self, fn self => set_local_only_ (self, local_only))
	val get_local_only_ : cptr -> bool
	    = _import "gtk_file_chooser_get_local_only" : cptr -> bool;
	val get_local_only : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_local_only_ self)
	val set_select_multiple_ : cptr * bool -> unit
	    = _import "gtk_file_chooser_set_select_multiple"
		      : cptr * bool -> unit;
	val set_select_multiple : 'a t -> bool -> unit
	    = fn self => fn select_multiple =>
		 GObject.withPtr (self, 
				  fn self => set_select_multiple_
					       (self, select_multiple))
	val get_select_multiple_ : cptr -> bool
	    = _import "gtk_file_chooser_get_select_multiple" : cptr -> bool;
	val get_select_multiple : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_select_multiple_ self)
	val set_current_name_ : cptr * CString.cstring -> unit
	    = _import "gtk_file_chooser_set_current_name"
		      : cptr * CString.cstring -> unit;
	val set_current_name : 'a t -> string -> unit
	    = fn self => fn name =>
		 GObject.withPtr (self, 
				  fn self => set_current_name_
					       (self, CString.fromString name))
	val get_filename_ : cptr -> CString.t
	    = _import "gtk_file_chooser_get_filename" : cptr -> CString.t;
	val get_filename : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_filename_ self
				       in CString.toString t end)
	val set_filename_ : cptr * CString.cstring -> bool
	    = _import "gtk_file_chooser_set_filename"
		      : cptr * CString.cstring -> bool;
	val set_filename : 'a t -> string -> bool
	    = fn self => fn filename =>
		 GObject.withPtr
		   (self, 
		    fn self => set_filename_
				 (self, CString.fromString filename))
	val select_filename_ : cptr * CString.cstring -> bool
	    = _import "gtk_file_chooser_select_filename"
		      : cptr * CString.cstring -> bool;
	val select_filename : 'a t -> string -> bool
	    = fn self => fn filename =>
		 GObject.withPtr
		   (self, 
		    fn self => select_filename_
				 (self, CString.fromString filename))
	val unselect_filename_ : cptr * CString.cstring -> unit
	    = _import "gtk_file_chooser_unselect_filename"
		      : cptr * CString.cstring -> unit;
	val unselect_filename : 'a t -> string -> unit
	    = fn self => fn filename =>
		 GObject.withPtr
		   (self, 
		    fn self => unselect_filename_
				 (self, CString.fromString filename))
	val select_all_ : cptr -> unit
	    = _import "gtk_file_chooser_select_all" : cptr -> unit;
	val select_all : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => select_all_ self)
	val unselect_all_ : cptr -> unit
	    = _import "gtk_file_chooser_unselect_all" : cptr -> unit;
	val unselect_all : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => unselect_all_ self)
	val set_current_folder_ : cptr * CString.cstring -> bool
	    = _import "gtk_file_chooser_set_current_folder"
		      : cptr * CString.cstring -> bool;
	val set_current_folder : 'a t -> string -> bool
	    = fn self => fn filename =>
		 GObject.withPtr
		   (self, 
		    fn self => set_current_folder_
				 (self, CString.fromString filename))
	val get_current_folder_ : cptr -> CString.t
	    = _import "gtk_file_chooser_get_current_folder"
		      : cptr -> CString.t;
	val get_current_folder : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_current_folder_ self
				       in CString.toString t end)
	val get_uri_ : cptr -> CString.t
	    = _import "gtk_file_chooser_get_uri" : cptr -> CString.t;
	val get_uri : 'a t -> string
	    = fn self => GObject.withPtr (self, 
					  fn self => let val t = get_uri_ self
						     in CString.toString t end)
	val set_uri_ : cptr * CString.cstring -> bool
	    = _import "gtk_file_chooser_set_uri"
		      : cptr * CString.cstring -> bool;
	val set_uri : 'a t -> string -> bool
	    = fn self => fn uri =>
		 GObject.withPtr
		   (self, fn self => set_uri_ (self, CString.fromString uri))
	val select_uri_ : cptr * CString.cstring -> bool
	    = _import "gtk_file_chooser_select_uri"
		      : cptr * CString.cstring -> bool;
	val select_uri : 'a t -> string -> bool
	    = fn self => fn uri =>
		 GObject.withPtr
		   (self, 
		    fn self => select_uri_ (self, CString.fromString uri))
	val unselect_uri_ : cptr * CString.cstring -> unit
	    = _import "gtk_file_chooser_unselect_uri"
		      : cptr * CString.cstring -> unit;
	val unselect_uri : 'a t -> string -> unit
	    = fn self => fn uri =>
		 GObject.withPtr
		   (self, 
		    fn self => unselect_uri_ (self, CString.fromString uri))
	val set_current_folder_uri_ : cptr * CString.cstring -> bool
	    = _import "gtk_file_chooser_set_current_folder_uri"
		      : cptr * CString.cstring -> bool;
	val set_current_folder_uri : 'a t -> string -> bool
	    = fn self => fn uri =>
		 GObject.withPtr (self, 
				  fn self => set_current_folder_uri_
					       (self, CString.fromString uri))
	val get_current_folder_uri_ : cptr -> CString.t
	    = _import "gtk_file_chooser_get_current_folder_uri"
		      : cptr -> CString.t;
	val get_current_folder_uri : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_current_folder_uri_ self
				       in CString.toString t end)
	val set_preview_widget_ : cptr * cptr -> unit
	    = _import "gtk_file_chooser_set_preview_widget"
		      : cptr * cptr -> unit;
	val set_preview_widget : 'a t -> 'b Widget.t -> unit
	    = fn self => fn preview_widget =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr (preview_widget, 
						fn preview_widget =>
						   set_preview_widget_
						     (self, preview_widget)))
	val get_preview_widget_ : cptr -> cptr
	    = _import "gtk_file_chooser_get_preview_widget" : cptr -> cptr;
	val get_preview_widget : 'a t -> base Widget.t
	    = fn self =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_preview_widget_ self))
	val set_preview_widget_active_ : cptr * bool -> unit
	    = _import "gtk_file_chooser_set_preview_widget_active"
		      : cptr * bool -> unit;
	val set_preview_widget_active : 'a t -> bool -> unit
	    = fn self => fn active =>
		 GObject.withPtr
		   (self, fn self => set_preview_widget_active_ (self, active))
	val get_preview_widget_active_ : cptr -> bool
	    = _import "gtk_file_chooser_get_preview_widget_active"
		      : cptr -> bool;
	val get_preview_widget_active : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_preview_widget_active_ self)
	val set_use_preview_label_ : cptr * bool -> unit
	    = _import "gtk_file_chooser_set_use_preview_label"
		      : cptr * bool -> unit;
	val set_use_preview_label : 'a t -> bool -> unit
	    = fn self => fn use_label =>
		 GObject.withPtr
		   (self, fn self => set_use_preview_label_ (self, use_label))
	val get_use_preview_label_ : cptr -> bool
	    = _import "gtk_file_chooser_get_use_preview_label" : cptr -> bool;
	val get_use_preview_label : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_use_preview_label_ self)
	val get_preview_filename_ : cptr -> CString.t
	    = _import "gtk_file_chooser_get_preview_filename"
		      : cptr -> CString.t;
	val get_preview_filename : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_preview_filename_ self
				       in CString.toString t end)
	val get_preview_uri_ : cptr -> CString.t
	    = _import "gtk_file_chooser_get_preview_uri" : cptr -> CString.t;
	val get_preview_uri : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_preview_uri_ self
				       in CString.toString t end)
	val set_extra_widget_ : cptr * cptr -> unit
	    = _import "gtk_file_chooser_set_extra_widget"
		      : cptr * cptr -> unit;
	val set_extra_widget : 'a t -> 'b Widget.t -> unit
	    = fn self => fn extra_widget =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr (extra_widget, 
						fn extra_widget =>
						   set_extra_widget_
						     (self, extra_widget)))
	val get_extra_widget_ : cptr -> cptr
	    = _import "gtk_file_chooser_get_extra_widget" : cptr -> cptr;
	val get_extra_widget : 'a t -> base Widget.t
	    = fn self =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_extra_widget_ self))
	val dialog_get_type_ : unit -> GType.t
	    = _import "gtk_file_chooser_dialog_get_type" : unit -> GType.t;
	val dialog_get_type : unit -> GType.t
	    = fn dummy => dialog_get_type_ dummy
	val dialog_new_with_backend_ : CString.cstring * cptr * int 
				     * CString.cstring * CString.cstring
				       -> cptr
	    = _import "gtk_file_chooser_dialog_new_with_backend"
		      : CString.cstring * cptr * int * CString.cstring 
		      * CString.cstring
			-> cptr;
	val dialog_new_with_backend
	  : string -> 'a Window.t -> action -> string -> string -> base t
	    = fn title => fn parent => fn action => fn backend => 
	      fn first_button_text =>
		 make (GObject.withPtr
			 (parent, 
			  fn parent =>
			     dialog_new_with_backend_
			       (CString.fromString title, parent, action, 
				CString.fromString backend, 
				CString.fromString first_button_text)))
	val widget_get_type_ : unit -> GType.t
	    = _import "gtk_file_chooser_widget_get_type" : unit -> GType.t;
	val widget_get_type : unit -> GType.t
	    = fn dummy => widget_get_type_ dummy
	val widget_new_with_backend_ : int * CString.cstring -> cptr
	    = _import "gtk_file_chooser_widget_new_with_backend"
		      : int * CString.cstring -> cptr;
	val widget_new_with_backend : action -> string -> base t
	    = fn action => fn backend =>
		 make (widget_new_with_backend_
			 (action, CString.fromString backend))
    end
    structure TreeDragDest :>
      sig
	type base
	type 'a treedragdest_t
	type 'a t = 'a treedragdest_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTreeDragDest : 'a t -> base t
	val get_type : unit -> GType.t
	val drag_data_received
	  : 'a t -> 'b TreePath.t -> SelectionData.t -> bool
	val row_drop_possible
	  : 'a t -> 'b TreePath.t -> SelectionData.t -> bool
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a treedragdest_t = unit
	type 'a t = 'a treedragdest_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeDragDest obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_tree_drag_dest_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val drag_data_received_ : cptr * cptr * cptr -> bool
	    = _import "gtk_tree_drag_dest_drag_data_received"
		      : cptr * cptr * cptr -> bool;
	val drag_data_received
	  : 'a t -> 'b TreePath.t -> SelectionData.t -> bool
	    = fn self => fn dest => fn selection_data =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (dest, 
				  fn dest => drag_data_received_
					       (self, dest, selection_data)))
	val row_drop_possible_ : cptr * cptr * cptr -> bool
	    = _import "gtk_tree_drag_dest_row_drop_possible"
		      : cptr * cptr * cptr -> bool;
	val row_drop_possible
	  : 'a t -> 'b TreePath.t -> SelectionData.t -> bool
	    = fn self => fn dest_path => fn selection_data =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (dest_path, 
				  fn dest_path => row_drop_possible_
						    (self, dest_path, 
						     selection_data)))
    end
    structure TreeDragSource :>
      sig
	type base
	type 'a treedragsource_t
	type 'a t = 'a treedragsource_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTreeDragSource : 'a t -> base t
	val get_type : unit -> GType.t
	val row_draggable : 'a t -> 'b TreePath.t -> bool
	val drag_data_delete : 'a t -> 'b TreePath.t -> bool
	val drag_data_get : 'a t -> 'b TreePath.t -> SelectionData.t -> bool
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a treedragsource_t = unit
	type 'a t = 'a treedragsource_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeDragSource obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_tree_drag_source_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val row_draggable_ : cptr * cptr -> bool
	    = _import "gtk_tree_drag_source_row_draggable"
		      : cptr * cptr -> bool;
	val row_draggable : 'a t -> 'b TreePath.t -> bool
	    = fn self => fn path =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (path, fn path => row_draggable_ (self, path)))
	val drag_data_delete_ : cptr * cptr -> bool
	    = _import "gtk_tree_drag_source_drag_data_delete"
		      : cptr * cptr -> bool;
	val drag_data_delete : 'a t -> 'b TreePath.t -> bool
	    = fn self => fn path =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (path, fn path => drag_data_delete_ (self, path)))
	val drag_data_get_ : cptr * cptr * cptr -> bool
	    = _import "gtk_tree_drag_source_drag_data_get"
		      : cptr * cptr * cptr -> bool;
	val drag_data_get : 'a t -> 'b TreePath.t -> SelectionData.t -> bool
	    = fn self => fn path => fn selection_data =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (path, 
				  fn path => drag_data_get_
					       (self, path, selection_data)))
    end
    structure TreeSortable :>
      sig
	type base
	type 'a treesortable_t
	type 'a t = 'a treesortable_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTreeSortable : 'a t -> base t
	val get_type : unit -> GType.t
	val sort_column_changed : 'a t -> unit
	val set_sort_column_id : 'a t -> int -> sorttype -> unit
	val has_default_sort_func : 'a t -> bool
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a treesortable_t = unit
	type 'a t = 'a treesortable_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeSortable obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_tree_sortable_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val sort_column_changed_ : cptr -> unit
	    = _import "gtk_tree_sortable_sort_column_changed" : cptr -> unit;
	val sort_column_changed : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => sort_column_changed_ self)
	val set_sort_column_id_ : cptr * int * int -> unit
	    = _import "gtk_tree_sortable_set_sort_column_id"
		      : cptr * int * int -> unit;
	val set_sort_column_id : 'a t -> int -> sorttype -> unit
	    = fn self => fn sort_column_id => fn order =>
		 GObject.withPtr (self, 
				  fn self => set_sort_column_id_
					       (self, sort_column_id, order))
	val has_default_sort_func_ : cptr -> bool
	    = _import "gtk_tree_sortable_has_default_sort_func" : cptr -> bool;
	val has_default_sort_func : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => has_default_sort_func_ self)
    end
    structure Misc :>
      sig
	type base
	type 'a misc_t
	type 'a t = 'a misc_t Widget.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toMisc : 'a t -> base t
	val get_type : unit -> GType.t
	val set_alignment : 'a t -> real -> real -> unit
	val set_padding : 'a t -> int -> int -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a misc_t = unit
	type 'a t = 'a misc_t Widget.t
	fun inherit w con = Widget.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toMisc obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_misc_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_alignment_ : cptr * real * real -> unit
	    = _import "gtk_misc_set_alignment" : cptr * real * real -> unit;
	val set_alignment : 'a t -> real -> real -> unit
	    = fn self => fn xalign => fn yalign =>
		 GObject.withPtr
		   (self, fn self => set_alignment_ (self, xalign, yalign))
	val set_padding_ : cptr * int * int -> unit
	    = _import "gtk_misc_set_padding" : cptr * int * int -> unit;
	val set_padding : 'a t -> int -> int -> unit
	    = fn self => fn xpad => fn ypad =>
		 GObject.withPtr
		   (self, fn self => set_padding_ (self, xpad, ypad))
    end
    structure Label :>
      sig
	type base
	type 'a label_t
	type 'a t = 'a label_t Misc.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toLabel : 'a t -> base t
	val get_type : unit -> GType.t
	val new : string -> base t
	val new_with_mnemonic : string -> base t
	val set_text : 'a t -> string -> unit
	val get_text : 'a t -> string
	val set_label : 'a t -> string -> unit
	val get_label : 'a t -> string
	val set_markup : 'a t -> string -> unit
	val set_use_markup : 'a t -> bool -> unit
	val get_use_markup : 'a t -> bool
	val set_use_underline : 'a t -> bool -> unit
	val get_use_underline : 'a t -> bool
	val set_markup_with_mnemonic : 'a t -> string -> unit
	val get_mnemonic_keyval : 'a t -> int
	val set_mnemonic_widget : 'a t -> 'b Widget.t -> unit
	val get_mnemonic_widget : 'a t -> base Widget.t
	val set_text_with_mnemonic : 'a t -> string -> unit
	val set_justify : 'a t -> justification -> unit
	val get_justify : 'a t -> justification
	val set_pattern : 'a t -> string -> unit
	val set_line_wrap : 'a t -> bool -> unit
	val get_line_wrap : 'a t -> bool
	val set_selectable : 'a t -> bool -> unit
	val get_selectable : 'a t -> bool
	val select_region : 'a t -> int -> int -> unit
	val move_cursor_sig : (unit -> int -> bool -> unit)
			      -> 'a t Signal.signal
	val copy_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	val populate_popup_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a label_t = unit
	type 'a t = 'a label_t Misc.t
	fun inherit w con = Misc.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toLabel obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_label_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : CString.cstring -> cptr
	    = _import "gtk_label_new" : CString.cstring -> cptr;
	val new : string -> base t
	    = fn str => make (new_ (CString.fromString str))
	val new_with_mnemonic_ : CString.cstring -> cptr
	    = _import "gtk_label_new_with_mnemonic" : CString.cstring -> cptr;
	val new_with_mnemonic : string -> base t
	    = fn str => make (new_with_mnemonic_ (CString.fromString str))
	val set_text_ : cptr * CString.cstring -> unit
	    = _import "gtk_label_set_text" : cptr * CString.cstring -> unit;
	val set_text : 'a t -> string -> unit
	    = fn self => fn str =>
		 GObject.withPtr
		   (self, fn self => set_text_ (self, CString.fromString str))
	val get_text_ : cptr -> CString.t
	    = _import "gtk_label_get_text" : cptr -> CString.t;
	val get_text : 'a t -> string
	    = fn self => GObject.withPtr (self, 
					  fn self => let val t = get_text_ self
						     in CString.toString t end)
	val set_label_ : cptr * CString.cstring -> unit
	    = _import "gtk_label_set_label" : cptr * CString.cstring -> unit;
	val set_label : 'a t -> string -> unit
	    = fn self => fn str =>
		 GObject.withPtr
		   (self, fn self => set_label_ (self, CString.fromString str))
	val get_label_ : cptr -> CString.t
	    = _import "gtk_label_get_label" : cptr -> CString.t;
	val get_label : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_label_ self
				       in CString.toString t end)
	val set_markup_ : cptr * CString.cstring -> unit
	    = _import "gtk_label_set_markup" : cptr * CString.cstring -> unit;
	val set_markup : 'a t -> string -> unit
	    = fn self => fn str =>
		 GObject.withPtr
		   (self, 
		    fn self => set_markup_ (self, CString.fromString str))
	val set_use_markup_ : cptr * bool -> unit
	    = _import "gtk_label_set_use_markup" : cptr * bool -> unit;
	val set_use_markup : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_use_markup_ (self, setting))
	val get_use_markup_ : cptr -> bool
	    = _import "gtk_label_get_use_markup" : cptr -> bool;
	val get_use_markup : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_use_markup_ self)
	val set_use_underline_ : cptr * bool -> unit
	    = _import "gtk_label_set_use_underline" : cptr * bool -> unit;
	val set_use_underline : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_use_underline_ (self, setting))
	val get_use_underline_ : cptr -> bool
	    = _import "gtk_label_get_use_underline" : cptr -> bool;
	val get_use_underline : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_use_underline_ self)
	val set_markup_with_mnemonic_ : cptr * CString.cstring -> unit
	    = _import "gtk_label_set_markup_with_mnemonic"
		      : cptr * CString.cstring -> unit;
	val set_markup_with_mnemonic : 'a t -> string -> unit
	    = fn self => fn str =>
		 GObject.withPtr (self, 
				  fn self => set_markup_with_mnemonic_
					       (self, CString.fromString str))
	val get_mnemonic_keyval_ : cptr -> int
	    = _import "gtk_label_get_mnemonic_keyval" : cptr -> int;
	val get_mnemonic_keyval : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_mnemonic_keyval_ self)
	val set_mnemonic_widget_ : cptr * cptr -> unit
	    = _import "gtk_label_set_mnemonic_widget" : cptr * cptr -> unit;
	val set_mnemonic_widget : 'a t -> 'b Widget.t -> unit
	    = fn self => fn widget =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (widget, 
				  fn widget => set_mnemonic_widget_
						 (self, widget)))
	val get_mnemonic_widget_ : cptr -> cptr
	    = _import "gtk_label_get_mnemonic_widget" : cptr -> cptr;
	val get_mnemonic_widget : 'a t -> base Widget.t
	    = fn self =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_mnemonic_widget_ self))
	val set_text_with_mnemonic_ : cptr * CString.cstring -> unit
	    = _import "gtk_label_set_text_with_mnemonic"
		      : cptr * CString.cstring -> unit;
	val set_text_with_mnemonic : 'a t -> string -> unit
	    = fn self => fn str =>
		 GObject.withPtr (self, 
				  fn self => set_text_with_mnemonic_
					       (self, CString.fromString str))
	val set_justify_ : cptr * int -> unit
	    = _import "gtk_label_set_justify" : cptr * int -> unit;
	val set_justify : 'a t -> justification -> unit
	    = fn self => fn jtype =>
		 GObject.withPtr (self, fn self => set_justify_ (self, jtype))
	val get_justify_ : cptr -> int
	    = _import "gtk_label_get_justify" : cptr -> int;
	val get_justify : 'a t -> justification
	    = fn self => GObject.withPtr (self, fn self => get_justify_ self)
	val set_pattern_ : cptr * CString.cstring -> unit
	    = _import "gtk_label_set_pattern" : cptr * CString.cstring -> unit;
	val set_pattern : 'a t -> string -> unit
	    = fn self => fn pattern =>
		 GObject.withPtr
		   (self, 
		    fn self => set_pattern_ (self, CString.fromString pattern))
	val set_line_wrap_ : cptr * bool -> unit
	    = _import "gtk_label_set_line_wrap" : cptr * bool -> unit;
	val set_line_wrap : 'a t -> bool -> unit
	    = fn self => fn wrap =>
		 GObject.withPtr (self, fn self => set_line_wrap_ (self, wrap))
	val get_line_wrap_ : cptr -> bool
	    = _import "gtk_label_get_line_wrap" : cptr -> bool;
	val get_line_wrap : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_line_wrap_ self)
	val set_selectable_ : cptr * bool -> unit
	    = _import "gtk_label_set_selectable" : cptr * bool -> unit;
	val set_selectable : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_selectable_ (self, setting))
	val get_selectable_ : cptr -> bool
	    = _import "gtk_label_get_selectable" : cptr -> bool;
	val get_selectable : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_selectable_ self)
	val select_region_ : cptr * int * int -> unit
	    = _import "gtk_label_select_region" : cptr * int * int -> unit;
	val select_region : 'a t -> int -> int -> unit
	    = fn self => fn start_offset => fn end_offset =>
		 GObject.withPtr
		   (self, 
		    fn self => select_region_ (self, start_offset, end_offset))
	local open Signal
	      infixr -->
	in val move_cursor_sig : (unit -> int -> bool -> unit)
				 -> 'a t Signal.signal
	       = fn f => signal "move-cursor" false
			        (unit --> int --> bool --> return_void) f
	   val copy_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "copy-clipboard" false (void --> return_void) f
	   val populate_popup_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "populate-popup" false (unit --> return_void) f
	end
    end
    structure AccelLabel :>
      sig
	type base
	type 'a accellabel_t
	type 'a t = 'a accellabel_t Label.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toAccelLabel : 'a t -> base t
	val get_type : unit -> GType.t
	val new : string -> base t
	val get_accel_widget : 'a t -> base Widget.t
	val get_accel_width : 'a t -> int
	val set_accel_widget : 'a t -> 'b Widget.t -> unit
	val refetch : 'a t -> bool
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a accellabel_t = unit
	type 'a t = 'a accellabel_t Label.t
	fun inherit w con = Label.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toAccelLabel obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_accel_label_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : CString.cstring -> cptr
	    = _import "gtk_accel_label_new" : CString.cstring -> cptr;
	val new : string -> base t
	    = fn string => make (new_ (CString.fromString string))
	val get_accel_widget_ : cptr -> cptr
	    = _import "gtk_accel_label_get_accel_widget" : cptr -> cptr;
	val get_accel_widget : 'a t -> base Widget.t
	    = fn self =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_accel_widget_ self))
	val get_accel_width_ : cptr -> int
	    = _import "gtk_accel_label_get_accel_width" : cptr -> int;
	val get_accel_width : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_accel_width_ self)
	val set_accel_widget_ : cptr * cptr -> unit
	    = _import "gtk_accel_label_set_accel_widget" : cptr * cptr -> unit;
	val set_accel_widget : 'a t -> 'b Widget.t -> unit
	    = fn self => fn accel_widget =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr (accel_widget, 
						fn accel_widget =>
						   set_accel_widget_
						     (self, accel_widget)))
	val refetch_ : cptr -> bool
	    = _import "gtk_accel_label_refetch" : cptr -> bool;
	val refetch : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => refetch_ self)
    end
    structure Action :>
      sig
	type base
	type 'a action_t
	type 'a t = 'a action_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toAction : 'a t -> base t
	val get_type : unit -> GType.t
	val get_name : 'a t -> string
	val is_sensitive : 'a t -> bool
	val get_sensitive : 'a t -> bool
	val is_visible : 'a t -> bool
	val get_visible : 'a t -> bool
	val activate : 'a t -> unit
	val create_icon : 'a t -> icon_size -> base Widget.t
	val create_menu_item : 'a t -> base Widget.t
	val create_toolitem : 'a t -> base Widget.t
	val connect_proxy : 'a t -> 'b Widget.t -> unit
	val disconnect_proxy : 'a t -> 'b Widget.t -> unit
	val connect_accelerator : 'a t -> unit
	val disconnect_accelerator : 'a t -> unit
	val block_activate_from : 'a t -> 'b Widget.t -> unit
	val unblock_activate_from : 'a t -> 'b Widget.t -> unit
	val set_accel_path : 'a t -> string -> unit
	val set_accelgroup : 'a t -> 'b AccelGroup.t -> unit
	val activate_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a action_t = unit
	type 'a t = 'a action_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toAction obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_action_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val get_name_ : cptr -> CString.t
	    = _import "gtk_action_get_name" : cptr -> CString.t;
	val get_name : 'a t -> string
	    = fn self => GObject.withPtr (self, 
					  fn self => let val t = get_name_ self
						     in CString.toString t end)
	val is_sensitive_ : cptr -> bool
	    = _import "gtk_action_is_sensitive" : cptr -> bool;
	val is_sensitive : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => is_sensitive_ self)
	val get_sensitive_ : cptr -> bool
	    = _import "gtk_action_get_sensitive" : cptr -> bool;
	val get_sensitive : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_sensitive_ self)
	val is_visible_ : cptr -> bool
	    = _import "gtk_action_is_visible" : cptr -> bool;
	val is_visible : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => is_visible_ self)
	val get_visible_ : cptr -> bool
	    = _import "gtk_action_get_visible" : cptr -> bool;
	val get_visible : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_visible_ self)
	val activate_ : cptr -> unit
	    = _import "gtk_action_activate" : cptr -> unit;
	val activate : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => activate_ self)
	val create_icon_ : cptr * int -> cptr
	    = _import "gtk_action_create_icon" : cptr * int -> cptr;
	val create_icon : 'a t -> icon_size -> base Widget.t
	    = fn self => fn icon_size =>
		 Widget.inherit
		   ()
		   (fn () =>
		       GObject.withPtr
			 (self, fn self => create_icon_ (self, icon_size)))
	val create_menu_item_ : cptr -> cptr
	    = _import "gtk_action_create_menu_item" : cptr -> cptr;
	val create_menu_item : 'a t -> base Widget.t
	    = fn self =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => create_menu_item_ self))
	val create_toolitem_ : cptr -> cptr
	    = _import "gtk_action_create_tool_item" : cptr -> cptr;
	val create_toolitem : 'a t -> base Widget.t
	    = fn self =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => create_toolitem_ self))
	val connect_proxy_ : cptr * cptr -> unit
	    = _import "gtk_action_connect_proxy" : cptr * cptr -> unit;
	val connect_proxy : 'a t -> 'b Widget.t -> unit
	    = fn self => fn proxy =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (proxy, fn proxy => connect_proxy_ (self, proxy)))
	val disconnect_proxy_ : cptr * cptr -> unit
	    = _import "gtk_action_disconnect_proxy" : cptr * cptr -> unit;
	val disconnect_proxy : 'a t -> 'b Widget.t -> unit
	    = fn self => fn proxy =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr (proxy, 
						fn proxy => disconnect_proxy_
							      (self, proxy)))
	val connect_accelerator_ : cptr -> unit
	    = _import "gtk_action_connect_accelerator" : cptr -> unit;
	val connect_accelerator : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => connect_accelerator_ self)
	val disconnect_accelerator_ : cptr -> unit
	    = _import "gtk_action_disconnect_accelerator" : cptr -> unit;
	val disconnect_accelerator : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => disconnect_accelerator_ self)
	val block_activate_from_ : cptr * cptr -> unit
	    = _import "gtk_action_block_activate_from" : cptr * cptr -> unit;
	val block_activate_from : 'a t -> 'b Widget.t -> unit
	    = fn self => fn proxy =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (proxy, 
				  fn proxy => block_activate_from_
						(self, proxy)))
	val unblock_activate_from_ : cptr * cptr -> unit
	    = _import "gtk_action_unblock_activate_from" : cptr * cptr -> unit;
	val unblock_activate_from : 'a t -> 'b Widget.t -> unit
	    = fn self => fn proxy =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (proxy, 
				  fn proxy => unblock_activate_from_
						(self, proxy)))
	val set_accel_path_ : cptr * CString.cstring -> unit
	    = _import "gtk_action_set_accel_path"
		      : cptr * CString.cstring -> unit;
	val set_accel_path : 'a t -> string -> unit
	    = fn self => fn accel_path =>
		 GObject.withPtr
		   (self, 
		    fn self => set_accel_path_
				 (self, CString.fromString accel_path))
	val set_accelgroup_ : cptr * cptr -> unit
	    = _import "gtk_action_set_accel_group" : cptr * cptr -> unit;
	val set_accelgroup : 'a t -> 'b AccelGroup.t -> unit
	    = fn self => fn accel_group =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (accel_group, 
				  fn accel_group =>
				     set_accelgroup_ (self, accel_group)))
	local open Signal
	      infixr -->
	in val activate_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "activate" false (void --> return_void) f
	end
    end
    structure ActionGroup :>
      sig
	type base
	type 'a actiongroup_t
	type 'a t = 'a actiongroup_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toActionGroup : 'a t -> base t
	val get_type : unit -> GType.t
	val new : string -> base t
	val get_name : 'a t -> string
	val get_sensitive : 'a t -> bool
	val set_sensitive : 'a t -> bool -> unit
	val get_visible : 'a t -> bool
	val set_visible : 'a t -> bool -> unit
	val get_action : 'a t -> string -> base Action.t
	val add_action : 'a t -> 'b Action.t -> unit
	val add_action_with_accel
	  : 'a t -> 'b Action.t -> string option -> unit
	val add_action_with_accel' : 'a t -> 'b Action.t -> unit
	val remove_action : 'a t -> 'b Action.t -> unit
	val set_translation_domain : 'a t -> string -> unit
	val connect_proxy_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	val disconnect_proxy_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	val pre_activate_sig : (unit -> unit) -> 'a t Signal.signal
	val post_activate_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a actiongroup_t = unit
	type 'a t = 'a actiongroup_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toActionGroup obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_action_group_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : CString.cstring -> cptr
	    = _import "gtk_action_group_new" : CString.cstring -> cptr;
	val new : string -> base t
	    = fn name => make (new_ (CString.fromString name))
	val get_name_ : cptr -> CString.t
	    = _import "gtk_action_group_get_name" : cptr -> CString.t;
	val get_name : 'a t -> string
	    = fn self => GObject.withPtr (self, 
					  fn self => let val t = get_name_ self
						     in CString.toString t end)
	val get_sensitive_ : cptr -> bool
	    = _import "gtk_action_group_get_sensitive" : cptr -> bool;
	val get_sensitive : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_sensitive_ self)
	val set_sensitive_ : cptr * bool -> unit
	    = _import "gtk_action_group_set_sensitive" : cptr * bool -> unit;
	val set_sensitive : 'a t -> bool -> unit
	    = fn self => fn sensitive =>
		 GObject.withPtr
		   (self, fn self => set_sensitive_ (self, sensitive))
	val get_visible_ : cptr -> bool
	    = _import "gtk_action_group_get_visible" : cptr -> bool;
	val get_visible : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_visible_ self)
	val set_visible_ : cptr * bool -> unit
	    = _import "gtk_action_group_set_visible" : cptr * bool -> unit;
	val set_visible : 'a t -> bool -> unit
	    = fn self => fn visible =>
		 GObject.withPtr
		   (self, fn self => set_visible_ (self, visible))
	val get_action_ : cptr * CString.cstring -> cptr
	    = _import "gtk_action_group_get_action"
		      : cptr * CString.cstring -> cptr;
	val get_action : 'a t -> string -> base Action.t
	    = fn self => fn action_name =>
		 Action.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self => get_action_ (self, 
							CString.fromString
							  action_name)))
	val add_action_ : cptr * cptr -> unit
	    = _import "gtk_action_group_add_action" : cptr * cptr -> unit;
	val add_action : 'a t -> 'b Action.t -> unit
	    = fn self => fn action =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (action, fn action => add_action_ (self, action)))
	val add_action_with_accel_ : cptr * cptr * CString.cstring -> unit
	    = _import "gtk_action_group_add_action_with_accel"
		      : cptr * cptr * CString.cstring -> unit;
	val add_action_with_accel
	  : 'a t -> 'b Action.t -> string option -> unit
	    = fn self => fn action => fn accelerator =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (action, 
				  fn action =>
				     add_action_with_accel_
				       (self, action, 
					CString.fromString
					  (getOpt (accelerator, "")))))
	val add_action_with_accel' : 'a t -> 'b Action.t -> unit
	    = fn self => fn action =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (action, 
				  fn action => add_action_with_accel_
						 (self, action, 
						  CString.fromString "")))
	val remove_action_ : cptr * cptr -> unit
	    = _import "gtk_action_group_remove_action" : cptr * cptr -> unit;
	val remove_action : 'a t -> 'b Action.t -> unit
	    = fn self => fn action =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (action, fn action => remove_action_ (self, action)))
	val set_translation_domain_ : cptr * CString.cstring -> unit
	    = _import "gtk_action_group_set_translation_domain"
		      : cptr * CString.cstring -> unit;
	val set_translation_domain : 'a t -> string -> unit
	    = fn self => fn domain =>
		 GObject.withPtr
		   (self, 
		    fn self => set_translation_domain_
				 (self, CString.fromString domain))
	local open Signal
	      infixr -->
	in val connect_proxy_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "connect-proxy" false
			        (unit --> unit --> return_void) f
	   val disconnect_proxy_sig
	     : (unit -> unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "disconnect-proxy" false
			        (unit --> unit --> return_void) f
	   val pre_activate_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "pre-activate" false (unit --> return_void) f
	   val post_activate_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "post-activate" false (unit --> return_void) f
	end
    end
    structure Alignment :>
      sig
	type base
	type 'a alignment_t
	type 'a t = 'a alignment_t Bin.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toAlignment : 'a t -> base t
	val get_type : unit -> GType.t
	val new : real -> real -> real -> real -> base t
	val set : 'a t -> real -> real -> real -> real -> unit
	val set_padding : 'a t -> int -> int -> int -> int -> unit
	val get_padding : 'a t -> int * int * int * int
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a alignment_t = unit
	type 'a t = 'a alignment_t Bin.t
	fun inherit w con = Bin.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toAlignment obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_alignment_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : real * real * real * real -> cptr
	    = _import "gtk_alignment_new" : real * real * real * real -> cptr;
	val new : real -> real -> real -> real -> base t
	    = fn xalign => fn yalign => fn xscale => fn yscale =>
		 make (new_ (xalign, yalign, xscale, yscale))
	val set_ : cptr * real * real * real * real -> unit
	    = _import "gtk_alignment_set"
		      : cptr * real * real * real * real -> unit;
	val set : 'a t -> real -> real -> real -> real -> unit
	    = fn self => fn xalign => fn yalign => fn xscale => fn yscale =>
		 GObject.withPtr (self, 
				  fn self => set_ (self, xalign, yalign, 
						   xscale, yscale))
	val set_padding_ : cptr * int * int * int * int -> unit
	    = _import "gtk_alignment_set_padding"
		      : cptr * int * int * int * int -> unit;
	val set_padding : 'a t -> int -> int -> int -> int -> unit
	    = fn self => fn padding_top => fn padding_bottom => 
	      fn padding_left => fn padding_right =>
		 GObject.withPtr
		   (self, 
		    fn self => set_padding_
				 (self, padding_top, padding_bottom, 
				  padding_left, padding_right))
	val get_padding_ : cptr * int ref * int ref * int ref * int ref -> unit
	    = _import "gtk_alignment_get_padding"
		      : cptr * int ref * int ref * int ref * int ref -> unit;
	val get_padding : 'a t -> int * int * int * int
	    = fn self =>
		 let val (padding_top, padding_bottom, padding_left, 
			  padding_right)
			 = (ref 0, ref 0, ref 0, ref 0)
		     val ret = GObject.withPtr
				 (self, 
				  fn self =>
				     get_padding_
				       (self, padding_top, padding_bottom, 
					padding_left, padding_right))
		 in (!padding_top, !padding_bottom, !padding_left, 
		     !padding_right)
		 end
    end
    structure Arrow :>
      sig
	type base
	type 'a arrow_t
	type 'a t = 'a arrow_t Misc.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toArrow : 'a t -> base t
	val get_type : unit -> GType.t
	val new : arrowtype -> shadowtype -> base t
	val set : 'a t -> arrowtype -> shadowtype -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a arrow_t = unit
	type 'a t = 'a arrow_t Misc.t
	fun inherit w con = Misc.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toArrow obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_arrow_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : int * int -> cptr
	    = _import "gtk_arrow_new" : int * int -> cptr;
	val new : arrowtype -> shadowtype -> base t
	    = fn arrow_type => fn shadow_type =>
		 make (new_ (arrow_type, shadow_type))
	val set_ : cptr * int * int -> unit
	    = _import "gtk_arrow_set" : cptr * int * int -> unit;
	val set : 'a t -> arrowtype -> shadowtype -> unit
	    = fn self => fn arrow_type => fn shadow_type =>
		 GObject.withPtr
		   (self, fn self => set_ (self, arrow_type, shadow_type))
    end
    structure Frame :>
      sig
	type base
	type 'a frame_t
	type 'a t = 'a frame_t Bin.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toFrame : 'a t -> base t
	val get_type : unit -> GType.t
	val new : string -> base t
	val set_label : 'a t -> string option -> unit
	val set_label' : 'a t -> unit
	val get_label : 'a t -> string
	val set_label_widget : 'a t -> 'b Widget.t -> unit
	val get_label_widget : 'a t -> base Widget.t
	val set_label_align : 'a t -> real -> real -> unit
	val set_shadowtype : 'a t -> shadowtype -> unit
	val get_shadowtype : 'a t -> shadowtype
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a frame_t = unit
	type 'a t = 'a frame_t Bin.t
	fun inherit w con = Bin.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toFrame obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_frame_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : CString.cstring -> cptr
	    = _import "gtk_frame_new" : CString.cstring -> cptr;
	val new : string -> base t
	    = fn label => make (new_ (CString.fromString label))
	val set_label_ : cptr * CString.cstring -> unit
	    = _import "gtk_frame_set_label" : cptr * CString.cstring -> unit;
	val set_label : 'a t -> string option -> unit
	    = fn self => fn label =>
		 GObject.withPtr
		   (self, 
		    fn self => set_label_ (self, 
					   CString.fromString
					     (getOpt (label, ""))))
	val set_label' : 'a t -> unit
	    = fn self =>
		 GObject.withPtr
		   (self, fn self => set_label_ (self, CString.fromString ""))
	val get_label_ : cptr -> CString.t
	    = _import "gtk_frame_get_label" : cptr -> CString.t;
	val get_label : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_label_ self
				       in CString.toString t end)
	val set_label_widget_ : cptr * cptr -> unit
	    = _import "gtk_frame_set_label_widget" : cptr * cptr -> unit;
	val set_label_widget : 'a t -> 'b Widget.t -> unit
	    = fn self => fn label_widget =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr (label_widget, 
						fn label_widget =>
						   set_label_widget_
						     (self, label_widget)))
	val get_label_widget_ : cptr -> cptr
	    = _import "gtk_frame_get_label_widget" : cptr -> cptr;
	val get_label_widget : 'a t -> base Widget.t
	    = fn self =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_label_widget_ self))
	val set_label_align_ : cptr * real * real -> unit
	    = _import "gtk_frame_set_label_align" : cptr * real * real -> unit;
	val set_label_align : 'a t -> real -> real -> unit
	    = fn self => fn xalign => fn yalign =>
		 GObject.withPtr
		   (self, fn self => set_label_align_ (self, xalign, yalign))
	val set_shadowtype_ : cptr * int -> unit
	    = _import "gtk_frame_set_shadow_type" : cptr * int -> unit;
	val set_shadowtype : 'a t -> shadowtype -> unit
	    = fn self => fn typ =>
		 GObject.withPtr (self, fn self => set_shadowtype_ (self, typ))
	val get_shadowtype_ : cptr -> int
	    = _import "gtk_frame_get_shadow_type" : cptr -> int;
	val get_shadowtype : 'a t -> shadowtype
	    = fn self => GObject.withPtr
			   (self, fn self => get_shadowtype_ self)
    end
    structure AspectFrame :>
      sig
	type base
	type 'a aspectframe_t
	type 'a t = 'a aspectframe_t Frame.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toAspectFrame : 'a t -> base t
	val get_type : unit -> GType.t
	val new : string -> real -> real -> real -> bool -> base t
	val set : 'a t -> real -> real -> real -> bool -> unit
	val set' : 'a t -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a aspectframe_t = unit
	type 'a t = 'a aspectframe_t Frame.t
	fun inherit w con = Frame.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toAspectFrame obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_aspect_frame_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : CString.cstring * real * real * real * bool -> cptr
	    = _import "gtk_aspect_frame_new"
		      : CString.cstring * real * real * real * bool -> cptr;
	val new : string -> real -> real -> real -> bool -> base t
	    = fn label => fn xalign => fn yalign => fn ratio => 
	      fn obey_child =>
		 make (new_ (CString.fromString label, xalign, yalign, ratio, 
			     obey_child))
	val set_ : cptr * real * real * real * bool -> unit
	    = _import "gtk_aspect_frame_set"
		      : cptr * real * real * real * bool -> unit;
	val set : 'a t -> real -> real -> real -> bool -> unit
	    = fn self => fn xalign => fn yalign => fn ratio => fn obey_child =>
		 GObject.withPtr (self, 
				  fn self => set_ (self, xalign, yalign, 
						   ratio, obey_child))
	val set' : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => set_ (self, 0.0, 0.0, 1.0, true))
    end
    structure Box :>
      sig
	type base
	type 'a box_t
	type 'a t = 'a box_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toBox : 'a t -> base t
	val get_type : unit -> GType.t
	val pack_start : 'a t -> 'b Widget.t -> bool -> bool -> int -> unit
	val pack_start' : 'a t -> 'b Widget.t -> unit
	val pack_end : 'a t -> 'b Widget.t -> bool -> bool -> int -> unit
	val pack_end' : 'a t -> 'b Widget.t -> unit
	val set_homogeneous : 'a t -> bool -> unit
	val get_homogeneous : 'a t -> bool
	val set_spacing : 'a t -> int -> unit
	val get_spacing : 'a t -> int
	val reorder_child : 'a t -> 'b Widget.t -> int -> unit
	val set_child_packing
	  : 'a t -> 'b Widget.t -> bool -> bool -> int -> packtype -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a box_t = unit
	type 'a t = 'a box_t Container.t
	fun inherit w con = Container.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toBox obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_box_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val pack_start_ : cptr * cptr * bool * bool * int -> unit
	    = _import "gtk_box_pack_start"
		      : cptr * cptr * bool * bool * int -> unit;
	val pack_start : 'a t -> 'b Widget.t -> bool -> bool -> int -> unit
	    = fn self => fn child => fn expand => fn fill => fn padding =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => pack_start_
						(self, child, expand, 
						 fill, padding)))
	val pack_start' : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => pack_start_
						(self, child, true, true, 0)))
	val pack_end_ : cptr * cptr * bool * bool * int -> unit
	    = _import "gtk_box_pack_end"
		      : cptr * cptr * bool * bool * int -> unit;
	val pack_end : 'a t -> 'b Widget.t -> bool -> bool -> int -> unit
	    = fn self => fn child => fn expand => fn fill => fn padding =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => pack_end_ (self, child, expand, 
							 fill, padding)))
	val pack_end' : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child =>
				     pack_end_ (self, child, true, true, 0)))
	val set_homogeneous_ : cptr * bool -> unit
	    = _import "gtk_box_set_homogeneous" : cptr * bool -> unit;
	val set_homogeneous : 'a t -> bool -> unit
	    = fn self => fn homogeneous =>
		 GObject.withPtr
		   (self, fn self => set_homogeneous_ (self, homogeneous))
	val get_homogeneous_ : cptr -> bool
	    = _import "gtk_box_get_homogeneous" : cptr -> bool;
	val get_homogeneous : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_homogeneous_ self)
	val set_spacing_ : cptr * int -> unit
	    = _import "gtk_box_set_spacing" : cptr * int -> unit;
	val set_spacing : 'a t -> int -> unit
	    = fn self => fn spacing =>
		 GObject.withPtr
		   (self, fn self => set_spacing_ (self, spacing))
	val get_spacing_ : cptr -> int
	    = _import "gtk_box_get_spacing" : cptr -> int;
	val get_spacing : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_spacing_ self)
	val reorder_child_ : cptr * cptr * int -> unit
	    = _import "gtk_box_reorder_child" : cptr * cptr * int -> unit;
	val reorder_child : 'a t -> 'b Widget.t -> int -> unit
	    = fn self => fn child => fn position =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => reorder_child_
						(self, child, position)))
	val set_child_packing_ : cptr * cptr * bool * bool * int * int -> unit
	    = _import "gtk_box_set_child_packing"
		      : cptr * cptr * bool * bool * int * int -> unit;
	val set_child_packing
	  : 'a t -> 'b Widget.t -> bool -> bool -> int -> packtype -> unit
	    = fn self => fn child => fn expand => fn fill => fn padding => 
	      fn pack_type =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => set_child_packing_
						(self, child, expand, fill, 
						 padding, pack_type)))
    end
    structure Button :>
      sig
	type base
	type 'a button_t
	type 'a t = 'a button_t Bin.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toButton : 'a t -> base t
	type box_style
	val BUTTONBOX_DEFAULT_STYLE : box_style
	val BUTTONBOX_SPREAD : box_style
	val BUTTONBOX_EDGE : box_style
	val BUTTONBOX_START : box_style
	val BUTTONBOX_END : box_style
	type action
	val IGNORED : action
	val SELECTS : action
	val DRAGS : action
	val EXPANDS : action
	val box_get_type : unit -> GType.t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val new_with_label : string -> base t
	val new_from_stock : string -> base t
	val new_with_mnemonic : string -> base t
	val pressed : 'a t -> unit
	val released : 'a t -> unit
	val clicked : 'a t -> unit
	val enter : 'a t -> unit
	val leave : 'a t -> unit
	val set_relief : 'a t -> relief_style -> unit
	val get_relief : 'a t -> relief_style
	val set_label : 'a t -> string -> unit
	val get_label : 'a t -> string
	val set_use_underline : 'a t -> bool -> unit
	val get_use_underline : 'a t -> bool
	val set_use_stock : 'a t -> bool -> unit
	val get_use_stock : 'a t -> bool
	val set_focus_on_click : 'a t -> bool -> unit
	val get_focus_on_click : 'a t -> bool
	val set_alignment : 'a t -> real -> real -> unit
	val get_alignment : 'a t -> real * real
	val clicked_sig : (unit -> unit) -> 'a t Signal.signal
	val activate_sig : (unit -> unit) -> 'a t Signal.signal
	val pressed_sig : (unit -> unit) -> 'a t Signal.signal
	val released_sig : (unit -> unit) -> 'a t Signal.signal
	val enter_sig : (unit -> unit) -> 'a t Signal.signal
	val leave_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a button_t = unit
	type 'a t = 'a button_t Bin.t
	fun inherit w con = Bin.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toButton obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type box_style = int
	val get_box_style_
	  : int ref * int ref * int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_button_box_style"
		      : int ref * int ref * int ref * int ref * int ref
			-> unit;
	val (BUTTONBOX_DEFAULT_STYLE, BUTTONBOX_SPREAD, BUTTONBOX_EDGE, 
	     BUTTONBOX_START, BUTTONBOX_END)
	    = let val (x0, x1, x2, x3, x4)
		      = (ref 0, ref 0, ref 0, ref 0, ref 0)
	      in get_box_style_ (x0, x1, x2, x3, x4)
	       ; (!x0, !x1, !x2, !x3, !x4)
	      end
	type action = int
	val get_action_ : int ref * int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_button_action"
		      : int ref * int ref * int ref * int ref -> unit;
	val (IGNORED, SELECTS, DRAGS, EXPANDS)
	    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	      in get_action_ (x0, x1, x2, x3)
	       ; (!x0, !x1, !x2, !x3)
	      end
	val box_get_type_ : unit -> GType.t
	    = _import "gtk_button_box_get_type" : unit -> GType.t;
	val box_get_type : unit -> GType.t = fn dummy => box_get_type_ dummy
	val get_type_ : unit -> GType.t
	    = _import "gtk_button_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_button_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_label_ : CString.cstring -> cptr
	    = _import "gtk_button_new_with_label" : CString.cstring -> cptr;
	val new_with_label : string -> base t
	    = fn label => make (new_with_label_ (CString.fromString label))
	val new_from_stock_ : CString.cstring -> cptr
	    = _import "gtk_button_new_from_stock" : CString.cstring -> cptr;
	val new_from_stock : string -> base t
	    = fn stock_id => make (new_from_stock_
				     (CString.fromString stock_id))
	val new_with_mnemonic_ : CString.cstring -> cptr
	    = _import "gtk_button_new_with_mnemonic" : CString.cstring -> cptr;
	val new_with_mnemonic : string -> base t
	    = fn label => make (new_with_mnemonic_ (CString.fromString label))
	val pressed_ : cptr -> unit
	    = _import "gtk_button_pressed" : cptr -> unit;
	val pressed : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => pressed_ self)
	val released_ : cptr -> unit
	    = _import "gtk_button_released" : cptr -> unit;
	val released : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => released_ self)
	val clicked_ : cptr -> unit
	    = _import "gtk_button_clicked" : cptr -> unit;
	val clicked : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => clicked_ self)
	val enter_ : cptr -> unit = _import "gtk_button_enter" : cptr -> unit;
	val enter : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => enter_ self)
	val leave_ : cptr -> unit = _import "gtk_button_leave" : cptr -> unit;
	val leave : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => leave_ self)
	val set_relief_ : cptr * int -> unit
	    = _import "gtk_button_set_relief" : cptr * int -> unit;
	val set_relief : 'a t -> relief_style -> unit
	    = fn self => fn newstyle =>
		 GObject.withPtr
		   (self, fn self => set_relief_ (self, newstyle))
	val get_relief_ : cptr -> int
	    = _import "gtk_button_get_relief" : cptr -> int;
	val get_relief : 'a t -> relief_style
	    = fn self => GObject.withPtr (self, fn self => get_relief_ self)
	val set_label_ : cptr * CString.cstring -> unit
	    = _import "gtk_button_set_label" : cptr * CString.cstring -> unit;
	val set_label : 'a t -> string -> unit
	    = fn self => fn label =>
		 GObject.withPtr
		   (self, 
		    fn self => set_label_ (self, CString.fromString label))
	val get_label_ : cptr -> CString.t
	    = _import "gtk_button_get_label" : cptr -> CString.t;
	val get_label : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_label_ self
				       in CString.toString t end)
	val set_use_underline_ : cptr * bool -> unit
	    = _import "gtk_button_set_use_underline" : cptr * bool -> unit;
	val set_use_underline : 'a t -> bool -> unit
	    = fn self => fn use_underline =>
		 GObject.withPtr
		   (self, fn self => set_use_underline_ (self, use_underline))
	val get_use_underline_ : cptr -> bool
	    = _import "gtk_button_get_use_underline" : cptr -> bool;
	val get_use_underline : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_use_underline_ self)
	val set_use_stock_ : cptr * bool -> unit
	    = _import "gtk_button_set_use_stock" : cptr * bool -> unit;
	val set_use_stock : 'a t -> bool -> unit
	    = fn self => fn use_stock =>
		 GObject.withPtr
		   (self, fn self => set_use_stock_ (self, use_stock))
	val get_use_stock_ : cptr -> bool
	    = _import "gtk_button_get_use_stock" : cptr -> bool;
	val get_use_stock : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_use_stock_ self)
	val set_focus_on_click_ : cptr * bool -> unit
	    = _import "gtk_button_set_focus_on_click" : cptr * bool -> unit;
	val set_focus_on_click : 'a t -> bool -> unit
	    = fn self => fn focus_on_click =>
		 GObject.withPtr (self, 
				  fn self => set_focus_on_click_
					       (self, focus_on_click))
	val get_focus_on_click_ : cptr -> bool
	    = _import "gtk_button_get_focus_on_click" : cptr -> bool;
	val get_focus_on_click : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_focus_on_click_ self)
	val set_alignment_ : cptr * real * real -> unit
	    = _import "gtk_button_set_alignment" : cptr * real * real -> unit;
	val set_alignment : 'a t -> real -> real -> unit
	    = fn self => fn xalign => fn yalign =>
		 GObject.withPtr
		   (self, fn self => set_alignment_ (self, xalign, yalign))
	val get_alignment_ : cptr * real ref * real ref -> unit
	    = _import "gtk_button_get_alignment"
		      : cptr * real ref * real ref -> unit;
	val get_alignment : 'a t -> real * real
	    = fn self => let val (xalign, yalign) = (ref 0.0, ref 0.0)
			     val ret = GObject.withPtr
					 (self, 
					  fn self => get_alignment_
						       (self, xalign, yalign))
			 in (!xalign, !yalign) end
	local open Signal
	      infixr -->
	in val clicked_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "clicked" false (void --> return_void) f
	   val activate_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "activate" false (void --> return_void) f
	   val pressed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "pressed" false (void --> return_void) f
	   val released_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "released" false (void --> return_void) f
	   val enter_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "enter" false (void --> return_void) f
	   val leave_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "leave" false (void --> return_void) f
	end
    end
    structure ButtonBox :>
      sig
	type base
	type 'a buttonbox_t
	type 'a t = 'a buttonbox_t Box.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toButtonBox : 'a t -> base t
	val get_child_secondary : 'a t -> 'b Widget.t -> bool
	val set_child_secondary : 'a t -> 'b Widget.t -> bool -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a buttonbox_t = unit
	type 'a t = 'a buttonbox_t Box.t
	fun inherit w con = Box.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toButtonBox obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_child_secondary_ : cptr * cptr -> bool
	    = _import "gtk_button_box_get_child_secondary"
		      : cptr * cptr -> bool;
	val get_child_secondary : 'a t -> 'b Widget.t -> bool
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => get_child_secondary_
						(self, child)))
	val set_child_secondary_ : cptr * cptr * bool -> unit
	    = _import "gtk_button_box_set_child_secondary"
		      : cptr * cptr * bool -> unit;
	val set_child_secondary : 'a t -> 'b Widget.t -> bool -> unit
	    = fn self => fn child => fn is_secondary =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => set_child_secondary_
						(self, child, is_secondary)))
    end
    structure Calendar :>
      sig
	type base
	type 'a calendar_t
	type 'a t = 'a calendar_t Widget.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toCalendar : 'a t -> base t
	type display_options
	val SHOW_HEADING : display_options
	val SHOW_DAY_NAMES : display_options
	val NO_MONTH_CHANGE : display_options
	val SHOW_WEEK_NUMBERS : display_options
	val WEEK_START_MONDAY : display_options
	val get_type : unit -> GType.t
	val new : unit -> base t
	val select_month : 'a t -> int -> int -> bool
	val select_day : 'a t -> int -> unit
	val mark_day : 'a t -> int -> bool
	val unmark_day : 'a t -> int -> bool
	val clear_marks : 'a t -> unit
	val set_display_options : 'a t -> display_options list -> unit
	val get_display_options : 'a t -> display_options list
	val display_options : 'a t -> display_options list -> unit
	val freeze : 'a t -> unit
	val thaw : 'a t -> unit
	val month_changed_sig : (unit -> unit) -> 'a t Signal.signal
	val day_selected_sig : (unit -> unit) -> 'a t Signal.signal
	val day_selected_double_click_sig
	  : (unit -> unit) -> 'a t Signal.signal
	val prev_month_sig : (unit -> unit) -> 'a t Signal.signal
	val next_month_sig : (unit -> unit) -> 'a t Signal.signal
	val prev_year_sig : (unit -> unit) -> 'a t Signal.signal
	val next_year_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a calendar_t = unit
	type 'a t = 'a calendar_t Widget.t
	fun inherit w con = Widget.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toCalendar obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type display_options = int
	val get_display_options_
	  : int ref * int ref * int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_calendar_display_options"
		      : int ref * int ref * int ref * int ref * int ref
			-> unit;
	val (SHOW_HEADING, SHOW_DAY_NAMES, NO_MONTH_CHANGE, SHOW_WEEK_NUMBERS, 
	     WEEK_START_MONDAY)
	    = let val (x0, x1, x2, x3, x4)
		      = (ref 0, ref 0, ref 0, ref 0, ref 0)
	      in get_display_options_ (x0, x1, x2, x3, x4)
	       ; (!x0, !x1, !x2, !x3, !x4)
	      end
	val get_type_ : unit -> GType.t
	    = _import "gtk_calendar_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_calendar_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val select_month_ : cptr * int * int -> bool
	    = _import "gtk_calendar_select_month" : cptr * int * int -> bool;
	val select_month : 'a t -> int -> int -> bool
	    = fn self => fn month => fn year =>
		 GObject.withPtr
		   (self, fn self => select_month_ (self, month, year))
	val select_day_ : cptr * int -> unit
	    = _import "gtk_calendar_select_day" : cptr * int -> unit;
	val select_day : 'a t -> int -> unit
	    = fn self => fn day =>
		 GObject.withPtr (self, fn self => select_day_ (self, day))
	val mark_day_ : cptr * int -> bool
	    = _import "gtk_calendar_mark_day" : cptr * int -> bool;
	val mark_day : 'a t -> int -> bool
	    = fn self => fn day =>
		 GObject.withPtr (self, fn self => mark_day_ (self, day))
	val unmark_day_ : cptr * int -> bool
	    = _import "gtk_calendar_unmark_day" : cptr * int -> bool;
	val unmark_day : 'a t -> int -> bool
	    = fn self => fn day =>
		 GObject.withPtr (self, fn self => unmark_day_ (self, day))
	val clear_marks_ : cptr -> unit
	    = _import "gtk_calendar_clear_marks" : cptr -> unit;
	val clear_marks : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => clear_marks_ self)
	val set_display_options_ : cptr * int -> unit
	    = _import "gtk_calendar_set_display_options" : cptr * int -> unit;
	val set_display_options : 'a t -> display_options list -> unit
	    = fn self => fn flags =>
		 GObject.withPtr (self, 
				  fn self => set_display_options_
					       (self, Flags.set flags))
	val get_display_options_ : cptr -> int
	    = _import "gtk_calendar_get_display_options" : cptr -> int;
	val get_display_options : 'a t -> display_options list
	    = fn self =>
		 Flags.get (GObject.withPtr
			      (self, fn self => get_display_options_ self))
	val display_options_ : cptr * int -> unit
	    = _import "gtk_calendar_display_options" : cptr * int -> unit;
	val display_options : 'a t -> display_options list -> unit
	    = fn self => fn flags =>
		 GObject.withPtr
		   (self, fn self => display_options_ (self, Flags.set flags))
	val freeze_ : cptr -> unit
	    = _import "gtk_calendar_freeze" : cptr -> unit;
	val freeze : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => freeze_ self)
	val thaw_ : cptr -> unit = _import "gtk_calendar_thaw" : cptr -> unit;
	val thaw : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => thaw_ self)
	local open Signal
	      infixr -->
	in val month_changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "month-changed" false (void --> return_void) f
	   val day_selected_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "day-selected" false (void --> return_void) f
	   val day_selected_double_click_sig
	     : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "day-selected-double-click" false
			        (void --> return_void) f
	   val prev_month_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "prev-month" false (void --> return_void) f
	   val next_month_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "next-month" false (void --> return_void) f
	   val prev_year_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "prev-year" false (void --> return_void) f
	   val next_year_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "next-year" false (void --> return_void) f
	end
    end
    structure CellRendererPixbuf :>
      sig
	type base
	type 'a cellrendererpixbuf_t
	type 'a t = 'a cellrendererpixbuf_t CellRenderer.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toCellRendererPixbuf : 'a t -> base t
	val new : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a cellrendererpixbuf_t = unit
	type 'a t = 'a cellrendererpixbuf_t CellRenderer.t
	fun inherit w con = CellRenderer.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toCellRendererPixbuf obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : unit -> cptr
	    = _import "gtk_cell_renderer_pixbuf_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure CellRendererText :>
      sig
	type base
	type 'a cellrenderertext_t
	type 'a t = 'a cellrenderertext_t CellRenderer.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toCellRendererText : 'a t -> base t
	val new : unit -> base t
	val set_fixed_height_from_font : 'a t -> int -> unit
	val edited_sig : (char -> char -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a cellrenderertext_t = unit
	type 'a t = 'a cellrenderertext_t CellRenderer.t
	fun inherit w con = CellRenderer.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toCellRendererText obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : unit -> cptr
	    = _import "gtk_cell_renderer_text_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val set_fixed_height_from_font_ : cptr * int -> unit
	    = _import "gtk_cell_renderer_text_set_fixed_height_from_font"
		      : cptr * int -> unit;
	val set_fixed_height_from_font : 'a t -> int -> unit
	    = fn self => fn number_of_rows =>
		 GObject.withPtr (self, 
				  fn self => set_fixed_height_from_font_
					       (self, number_of_rows))
	local open Signal
	      infixr -->
	in val edited_sig : (char -> char -> unit) -> 'a t Signal.signal
	       = fn f =>
		    signal "edited" false (char --> char --> return_void) f
	end
    end
    structure CellRendererToggle :>
      sig
	type base
	type 'a cellrenderertoggle_t
	type 'a t = 'a cellrenderertoggle_t CellRenderer.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toCellRendererToggle : 'a t -> base t
	val new : unit -> base t
	val get_radio : 'a t -> bool
	val set_radio : 'a t -> bool -> unit
	val get_active : 'a t -> bool
	val set_active : 'a t -> bool -> unit
	val toggled_sig : (char -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a cellrenderertoggle_t = unit
	type 'a t = 'a cellrenderertoggle_t CellRenderer.t
	fun inherit w con = CellRenderer.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toCellRendererToggle obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : unit -> cptr
	    = _import "gtk_cell_renderer_toggle_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_radio_ : cptr -> bool
	    = _import "gtk_cell_renderer_toggle_get_radio" : cptr -> bool;
	val get_radio : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_radio_ self)
	val set_radio_ : cptr * bool -> unit
	    = _import "gtk_cell_renderer_toggle_set_radio"
		      : cptr * bool -> unit;
	val set_radio : 'a t -> bool -> unit
	    = fn self => fn radio =>
		 GObject.withPtr (self, fn self => set_radio_ (self, radio))
	val get_active_ : cptr -> bool
	    = _import "gtk_cell_renderer_toggle_get_active" : cptr -> bool;
	val get_active : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_active_ self)
	val set_active_ : cptr * bool -> unit
	    = _import "gtk_cell_renderer_toggle_set_active"
		      : cptr * bool -> unit;
	val set_active : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr (self, fn self => set_active_ (self, setting))
	local open Signal
	      infixr -->
	in val toggled_sig : (char -> unit) -> 'a t Signal.signal
	       = fn f => signal "toggled" false (char --> return_void) f
	end
    end
    structure ToggleButton :>
      sig
	type base
	type 'a togglebutton_t
	type 'a t = 'a togglebutton_t Button.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toToggleButton : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val new_with_label : string -> base t
	val new_with_mnemonic : string -> base t
	val set_mode : 'a t -> bool -> unit
	val get_mode : 'a t -> bool
	val set_active : 'a t -> bool -> unit
	val get_active : 'a t -> bool
	val toggled : 'a t -> unit
	val set_inconsistent : 'a t -> bool -> unit
	val get_inconsistent : 'a t -> bool
	val toggled_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a togglebutton_t = unit
	type 'a t = 'a togglebutton_t Button.t
	fun inherit w con = Button.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toToggleButton obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_toggle_button_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr
	    = _import "gtk_toggle_button_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_label_ : CString.cstring -> cptr
	    = _import "gtk_toggle_button_new_with_label"
		      : CString.cstring -> cptr;
	val new_with_label : string -> base t
	    = fn label => make (new_with_label_ (CString.fromString label))
	val new_with_mnemonic_ : CString.cstring -> cptr
	    = _import "gtk_toggle_button_new_with_mnemonic"
		      : CString.cstring -> cptr;
	val new_with_mnemonic : string -> base t
	    = fn label => make (new_with_mnemonic_ (CString.fromString label))
	val set_mode_ : cptr * bool -> unit
	    = _import "gtk_toggle_button_set_mode" : cptr * bool -> unit;
	val set_mode : 'a t -> bool -> unit
	    = fn self => fn draw_indicator =>
		 GObject.withPtr
		   (self, fn self => set_mode_ (self, draw_indicator))
	val get_mode_ : cptr -> bool
	    = _import "gtk_toggle_button_get_mode" : cptr -> bool;
	val get_mode : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_mode_ self)
	val set_active_ : cptr * bool -> unit
	    = _import "gtk_toggle_button_set_active" : cptr * bool -> unit;
	val set_active : 'a t -> bool -> unit
	    = fn self => fn is_active =>
		 GObject.withPtr
		   (self, fn self => set_active_ (self, is_active))
	val get_active_ : cptr -> bool
	    = _import "gtk_toggle_button_get_active" : cptr -> bool;
	val get_active : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_active_ self)
	val toggled_ : cptr -> unit
	    = _import "gtk_toggle_button_toggled" : cptr -> unit;
	val toggled : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => toggled_ self)
	val set_inconsistent_ : cptr * bool -> unit
	    = _import "gtk_toggle_button_set_inconsistent"
		      : cptr * bool -> unit;
	val set_inconsistent : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_inconsistent_ (self, setting))
	val get_inconsistent_ : cptr -> bool
	    = _import "gtk_toggle_button_get_inconsistent" : cptr -> bool;
	val get_inconsistent : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_inconsistent_ self)
	local open Signal
	      infixr -->
	in val toggled_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "toggled" false (void --> return_void) f
	end
    end
    structure CheckButton :>
      sig
	type base
	type 'a checkbutton_t
	type 'a t = 'a checkbutton_t ToggleButton.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toCheckButton : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val new_with_label : string -> base t
	val new_with_mnemonic : string -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a checkbutton_t = unit
	type 'a t = 'a checkbutton_t ToggleButton.t
	fun inherit w con = ToggleButton.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toCheckButton obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_check_button_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr
	    = _import "gtk_check_button_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_label_ : CString.cstring -> cptr
	    = _import "gtk_check_button_new_with_label"
		      : CString.cstring -> cptr;
	val new_with_label : string -> base t
	    = fn label => make (new_with_label_ (CString.fromString label))
	val new_with_mnemonic_ : CString.cstring -> cptr
	    = _import "gtk_check_button_new_with_mnemonic"
		      : CString.cstring -> cptr;
	val new_with_mnemonic : string -> base t
	    = fn label => make (new_with_mnemonic_ (CString.fromString label))
    end
    structure Item :>
      sig
	type base
	type 'a item_t
	type 'a t = 'a item_t Bin.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toItem : 'a t -> base t
	val get_type : unit -> GType.t
	val select : 'a t -> unit
	val deselect : 'a t -> unit
	val toggle : 'a t -> unit
	val factory_get_type : unit -> GType.t
	val factory_path_from_widget : 'a Widget.t -> string
	val factory_popup_data_from_widget : 'a Widget.t -> cptr
	val select_sig : (unit -> unit) -> 'a t Signal.signal
	val deselect_sig : (unit -> unit) -> 'a t Signal.signal
	val toggle_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a item_t = unit
	type 'a t = 'a item_t Bin.t
	fun inherit w con = Bin.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toItem obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_item_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val select_ : cptr -> unit = _import "gtk_item_select" : cptr -> unit;
	val select : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => select_ self)
	val deselect_ : cptr -> unit
	    = _import "gtk_item_deselect" : cptr -> unit;
	val deselect : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => deselect_ self)
	val toggle_ : cptr -> unit = _import "gtk_item_toggle" : cptr -> unit;
	val toggle : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => toggle_ self)
	val factory_get_type_ : unit -> GType.t
	    = _import "gtk_item_factory_get_type" : unit -> GType.t;
	val factory_get_type : unit -> GType.t
	    = fn dummy => factory_get_type_ dummy
	val factory_path_from_widget_ : cptr -> CString.t
	    = _import "gtk_item_factory_path_from_widget" : cptr -> CString.t;
	val factory_path_from_widget : 'a Widget.t -> string
	    = fn widget =>
		 GObject.withPtr
		   (widget, 
		    fn widget => let val t = factory_path_from_widget_ widget
				 in CString.toString t end)
	val factory_popup_data_from_widget_ : cptr -> cptr
	    = _import "gtk_item_factory_popup_data_from_widget" : cptr -> cptr;
	val factory_popup_data_from_widget : 'a Widget.t -> cptr
	    = fn widget =>
		 GObject.withPtr
		   (widget, 
		    fn widget => factory_popup_data_from_widget_ widget)
	local open Signal
	      infixr -->
	in val select_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "select" false (void --> return_void) f
	   val deselect_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "deselect" false (void --> return_void) f
	   val toggle_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "toggle" false (void --> return_void) f
	end
    end
    structure MenuItem :>
      sig
	type base
	type 'a menuitem_t
	type 'a t = 'a menuitem_t Item.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toMenuItem : 'a t -> base t
	val new : unit -> base t
	val new_with_label : string -> base t
	val new_with_mnemonic : string -> base t
	val set_submenu : 'a t -> 'b Widget.t -> unit
	val get_submenu : 'a t -> base Widget.t
	val remove_submenu : 'a t -> unit
	val select : 'a t -> unit
	val deselect : 'a t -> unit
	val activate : 'a t -> unit
	val toggle_size_allocate : 'a t -> int -> unit
	val set_right_justified : 'a t -> bool -> unit
	val get_right_justified : 'a t -> bool
	val set_accel_path : 'a t -> string -> unit
	val activate_sig : (unit -> unit) -> 'a t Signal.signal
	val activate_item_sig : (unit -> unit) -> 'a t Signal.signal
	val toggle_size_request_sig : (int -> unit) -> 'a t Signal.signal
	val toggle_size_allocate_sig : (int -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a menuitem_t = unit
	type 'a t = 'a menuitem_t Item.t
	fun inherit w con = Item.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toMenuItem obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : unit -> cptr = _import "gtk_menu_item_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_label_ : CString.cstring -> cptr
	    = _import "gtk_menu_item_new_with_label" : CString.cstring -> cptr;
	val new_with_label : string -> base t
	    = fn label => make (new_with_label_ (CString.fromString label))
	val new_with_mnemonic_ : CString.cstring -> cptr
	    = _import "gtk_menu_item_new_with_mnemonic"
		      : CString.cstring -> cptr;
	val new_with_mnemonic : string -> base t
	    = fn label => make (new_with_mnemonic_ (CString.fromString label))
	val set_submenu_ : cptr * cptr -> unit
	    = _import "gtk_menu_item_set_submenu" : cptr * cptr -> unit;
	val set_submenu : 'a t -> 'b Widget.t -> unit
	    = fn self => fn submenu =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (submenu, 
				  fn submenu => set_submenu_ (self, submenu)))
	val get_submenu_ : cptr -> cptr
	    = _import "gtk_menu_item_get_submenu" : cptr -> cptr;
	val get_submenu : 'a t -> base Widget.t
	    = fn self => Widget.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_submenu_ self))
	val remove_submenu_ : cptr -> unit
	    = _import "gtk_menu_item_remove_submenu" : cptr -> unit;
	val remove_submenu : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => remove_submenu_ self)
	val select_ : cptr -> unit
	    = _import "gtk_menu_item_select" : cptr -> unit;
	val select : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => select_ self)
	val deselect_ : cptr -> unit
	    = _import "gtk_menu_item_deselect" : cptr -> unit;
	val deselect : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => deselect_ self)
	val activate_ : cptr -> unit
	    = _import "gtk_menu_item_activate" : cptr -> unit;
	val activate : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => activate_ self)
	val toggle_size_allocate_ : cptr * int -> unit
	    = _import "gtk_menu_item_toggle_size_allocate"
		      : cptr * int -> unit;
	val toggle_size_allocate : 'a t -> int -> unit
	    = fn self => fn allocation =>
		 GObject.withPtr
		   (self, fn self => toggle_size_allocate_ (self, allocation))
	val set_right_justified_ : cptr * bool -> unit
	    = _import "gtk_menu_item_set_right_justified"
		      : cptr * bool -> unit;
	val set_right_justified : 'a t -> bool -> unit
	    = fn self => fn right_justified =>
		 GObject.withPtr (self, 
				  fn self => set_right_justified_
					       (self, right_justified))
	val get_right_justified_ : cptr -> bool
	    = _import "gtk_menu_item_get_right_justified" : cptr -> bool;
	val get_right_justified : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_right_justified_ self)
	val set_accel_path_ : cptr * CString.cstring -> unit
	    = _import "gtk_menu_item_set_accel_path"
		      : cptr * CString.cstring -> unit;
	val set_accel_path : 'a t -> string -> unit
	    = fn self => fn accel_path =>
		 GObject.withPtr
		   (self, 
		    fn self => set_accel_path_
				 (self, CString.fromString accel_path))
	local open Signal
	      infixr -->
	in val activate_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "activate" false (void --> return_void) f
	   val activate_item_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "activate-item" false (void --> return_void) f
	   val toggle_size_request_sig : (int -> unit) -> 'a t Signal.signal
	       = fn f => signal "toggle-size-request" false
			        (int --> return_void) f
	   val toggle_size_allocate_sig : (int -> unit) -> 'a t Signal.signal
	       = fn f => signal "toggle-size-allocate" false
			        (int --> return_void) f
	end
    end
    structure CheckMenuItem :>
      sig
	type base
	type 'a checkmenuitem_t
	type 'a t = 'a checkmenuitem_t MenuItem.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toCheckMenuItem : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val new_with_label : string -> base t
	val new_with_mnemonic : string -> base t
	val set_active : 'a t -> bool -> unit
	val get_active : 'a t -> bool
	val toggled : 'a t -> unit
	val set_inconsistent : 'a t -> bool -> unit
	val get_inconsistent : 'a t -> bool
	val set_draw_as_radio : 'a t -> bool -> unit
	val get_draw_as_radio : 'a t -> bool
	val toggled_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a checkmenuitem_t = unit
	type 'a t = 'a checkmenuitem_t MenuItem.t
	fun inherit w con = MenuItem.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toCheckMenuItem obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_check_menu_item_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr
	    = _import "gtk_check_menu_item_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_label_ : CString.cstring -> cptr
	    = _import "gtk_check_menu_item_new_with_label"
		      : CString.cstring -> cptr;
	val new_with_label : string -> base t
	    = fn label => make (new_with_label_ (CString.fromString label))
	val new_with_mnemonic_ : CString.cstring -> cptr
	    = _import "gtk_check_menu_item_new_with_mnemonic"
		      : CString.cstring -> cptr;
	val new_with_mnemonic : string -> base t
	    = fn label => make (new_with_mnemonic_ (CString.fromString label))
	val set_active_ : cptr * bool -> unit
	    = _import "gtk_check_menu_item_set_active" : cptr * bool -> unit;
	val set_active : 'a t -> bool -> unit
	    = fn self => fn is_active =>
		 GObject.withPtr
		   (self, fn self => set_active_ (self, is_active))
	val get_active_ : cptr -> bool
	    = _import "gtk_check_menu_item_get_active" : cptr -> bool;
	val get_active : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_active_ self)
	val toggled_ : cptr -> unit
	    = _import "gtk_check_menu_item_toggled" : cptr -> unit;
	val toggled : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => toggled_ self)
	val set_inconsistent_ : cptr * bool -> unit
	    = _import "gtk_check_menu_item_set_inconsistent"
		      : cptr * bool -> unit;
	val set_inconsistent : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_inconsistent_ (self, setting))
	val get_inconsistent_ : cptr -> bool
	    = _import "gtk_check_menu_item_get_inconsistent" : cptr -> bool;
	val get_inconsistent : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_inconsistent_ self)
	val set_draw_as_radio_ : cptr * bool -> unit
	    = _import "gtk_check_menu_item_set_draw_as_radio"
		      : cptr * bool -> unit;
	val set_draw_as_radio : 'a t -> bool -> unit
	    = fn self => fn draw_as_radio =>
		 GObject.withPtr
		   (self, fn self => set_draw_as_radio_ (self, draw_as_radio))
	val get_draw_as_radio_ : cptr -> bool
	    = _import "gtk_check_menu_item_get_draw_as_radio" : cptr -> bool;
	val get_draw_as_radio : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_draw_as_radio_ self)
	local open Signal
	      infixr -->
	in val toggled_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "toggled" false (void --> return_void) f
	end
    end
    structure ColorButton :>
      sig
	type base
	type 'a colorbutton_t
	type 'a t = 'a colorbutton_t Button.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toColorButton : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val set_alpha : 'a t -> int -> unit
	val get_alpha : 'a t -> int
	val set_use_alpha : 'a t -> bool -> unit
	val get_use_alpha : 'a t -> bool
	val set_title : 'a t -> string -> unit
	val get_title : 'a t -> string
	val color_set_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a colorbutton_t = unit
	type 'a t = 'a colorbutton_t Button.t
	fun inherit w con = Button.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toColorButton obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_color_button_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr
	    = _import "gtk_color_button_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val set_alpha_ : cptr * int -> unit
	    = _import "gtk_color_button_set_alpha" : cptr * int -> unit;
	val set_alpha : 'a t -> int -> unit
	    = fn self => fn alpha =>
		 GObject.withPtr (self, fn self => set_alpha_ (self, alpha))
	val get_alpha_ : cptr -> int
	    = _import "gtk_color_button_get_alpha" : cptr -> int;
	val get_alpha : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_alpha_ self)
	val set_use_alpha_ : cptr * bool -> unit
	    = _import "gtk_color_button_set_use_alpha" : cptr * bool -> unit;
	val set_use_alpha : 'a t -> bool -> unit
	    = fn self => fn use_alpha =>
		 GObject.withPtr
		   (self, fn self => set_use_alpha_ (self, use_alpha))
	val get_use_alpha_ : cptr -> bool
	    = _import "gtk_color_button_get_use_alpha" : cptr -> bool;
	val get_use_alpha : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_use_alpha_ self)
	val set_title_ : cptr * CString.cstring -> unit
	    = _import "gtk_color_button_set_title"
		      : cptr * CString.cstring -> unit;
	val set_title : 'a t -> string -> unit
	    = fn self => fn title =>
		 GObject.withPtr
		   (self, 
		    fn self => set_title_ (self, CString.fromString title))
	val get_title_ : cptr -> CString.t
	    = _import "gtk_color_button_get_title" : cptr -> CString.t;
	val get_title : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_title_ self
				       in CString.toString t end)
	local open Signal
	      infixr -->
	in val color_set_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "color-set" false (void --> return_void) f
	end
    end
    structure VBox :>
      sig
	type base
	type 'a vbox_t
	type 'a t = 'a vbox_t Box.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toVBox : 'a t -> base t
	val get_type : unit -> GType.t
	val new : bool -> int -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a vbox_t = unit
	type 'a t = 'a vbox_t Box.t
	fun inherit w con = Box.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toVBox obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_vbox_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : bool * int -> cptr
	    = _import "gtk_vbox_new" : bool * int -> cptr;
	val new : bool -> int -> base t = fn homogeneous => fn spacing =>
					     make (new_ (homogeneous, spacing))
    end
    structure ColorSelection :>
      sig
	type base
	type 'a colorselection_t
	type 'a t = 'a colorselection_t VBox.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toColorSelection : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val get_has_opacity_control : 'a t -> bool
	val set_has_opacity_control : 'a t -> bool -> unit
	val get_has_palette : 'a t -> bool
	val set_has_palette : 'a t -> bool -> unit
	val set_current_alpha : 'a t -> int -> unit
	val get_current_alpha : 'a t -> int
	val set_previous_alpha : 'a t -> int -> unit
	val get_previous_alpha : 'a t -> int
	val is_adjusting : 'a t -> bool
	val dialog_get_type : unit -> GType.t
	val color_changed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a colorselection_t = unit
	type 'a t = 'a colorselection_t VBox.t
	fun inherit w con = VBox.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toColorSelection obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_color_selection_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr
	    = _import "gtk_color_selection_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_has_opacity_control_ : cptr -> bool
	    = _import "gtk_color_selection_get_has_opacity_control"
		      : cptr -> bool;
	val get_has_opacity_control : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_has_opacity_control_ self)
	val set_has_opacity_control_ : cptr * bool -> unit
	    = _import "gtk_color_selection_set_has_opacity_control"
		      : cptr * bool -> unit;
	val set_has_opacity_control : 'a t -> bool -> unit
	    = fn self => fn has_opacity =>
		 GObject.withPtr (self, 
				  fn self => set_has_opacity_control_
					       (self, has_opacity))
	val get_has_palette_ : cptr -> bool
	    = _import "gtk_color_selection_get_has_palette" : cptr -> bool;
	val get_has_palette : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_has_palette_ self)
	val set_has_palette_ : cptr * bool -> unit
	    = _import "gtk_color_selection_set_has_palette"
		      : cptr * bool -> unit;
	val set_has_palette : 'a t -> bool -> unit
	    = fn self => fn has_palette =>
		 GObject.withPtr
		   (self, fn self => set_has_palette_ (self, has_palette))
	val set_current_alpha_ : cptr * int -> unit
	    = _import "gtk_color_selection_set_current_alpha"
		      : cptr * int -> unit;
	val set_current_alpha : 'a t -> int -> unit
	    = fn self => fn alpha =>
		 GObject.withPtr
		   (self, fn self => set_current_alpha_ (self, alpha))
	val get_current_alpha_ : cptr -> int
	    = _import "gtk_color_selection_get_current_alpha" : cptr -> int;
	val get_current_alpha : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_current_alpha_ self)
	val set_previous_alpha_ : cptr * int -> unit
	    = _import "gtk_color_selection_set_previous_alpha"
		      : cptr * int -> unit;
	val set_previous_alpha : 'a t -> int -> unit
	    = fn self => fn alpha =>
		 GObject.withPtr
		   (self, fn self => set_previous_alpha_ (self, alpha))
	val get_previous_alpha_ : cptr -> int
	    = _import "gtk_color_selection_get_previous_alpha" : cptr -> int;
	val get_previous_alpha : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_previous_alpha_ self)
	val is_adjusting_ : cptr -> bool
	    = _import "gtk_color_selection_is_adjusting" : cptr -> bool;
	val is_adjusting : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => is_adjusting_ self)
	val dialog_get_type_ : unit -> GType.t
	    = _import "gtk_color_selection_dialog_get_type" : unit -> GType.t;
	val dialog_get_type : unit -> GType.t
	    = fn dummy => dialog_get_type_ dummy
	local open Signal
	      infixr -->
	in val color_changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "color-changed" false (void --> return_void) f
	end
    end
    structure Dialog :>
      sig
	type base
	type 'a dialog_t
	type 'a t = 'a dialog_t Window.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toDialog : 'a t -> base t
	type flags
	val MODAL : flags
	val DESTROY_WITH_PARENT : flags
	val NO_SEPARATOR : flags
	val get_type : unit -> GType.t
	val new : unit -> base t
	val new_with_buttons : string option -> 'a Window.t option 
			    -> flags list -> string option
			       -> base t
	val new_with_buttons' : unit -> base t
	val add_action_widget : 'a t -> 'b Widget.t -> int -> unit
	val add_button : 'a t -> string -> int -> base Widget.t
	val add_buttons : 'a t -> string -> unit
	val set_response_sensitive : 'a t -> int -> bool -> unit
	val set_default_response : 'a t -> int -> unit
	val set_has_separator : 'a t -> bool -> unit
	val get_has_separator : 'a t -> bool
	val response : 'a t -> int -> unit
	val run : 'a t -> int
	val response_sig : (int -> unit) -> 'a t Signal.signal
	val close_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a dialog_t = unit
	type 'a t = 'a dialog_t Window.t
	fun inherit w con = Window.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toDialog obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type flags = int
	val get_flags_ : int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_dialog_flags"
		      : int ref * int ref * int ref -> unit;
	val (MODAL, DESTROY_WITH_PARENT, NO_SEPARATOR)
	    = let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
	      in get_flags_ (x0, x1, x2)
	       ; (!x0, !x1, !x2)
	      end
	val get_type_ : unit -> GType.t
	    = _import "gtk_dialog_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_dialog_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_buttons_
	  : CString.cstring * cptr * int * CString.cstring -> cptr
	    = _import "gtk_dialog_new_with_buttons"
		      : CString.cstring * cptr * int * CString.cstring -> cptr;
	val new_with_buttons : string option -> 'a Window.t option 
			    -> flags list -> string option
			       -> base t
	    = fn title => fn parent => fn flags => fn first_button_text =>
		 make (GObject.withOpt
			 (parent, 
			  fn parent => new_with_buttons_
					 (CString.fromString
					    (getOpt (title, "")), 
					  parent, Flags.set flags, 
					  CString.fromString
					    (getOpt (first_button_text, "")))))
	val new_with_buttons' : unit -> base t
	    = fn dummy => make (new_with_buttons_ (CString.fromString "", 
						   GObject.null, 0, 
						   CString.fromString ""))
	val add_action_widget_ : cptr * cptr * int -> unit
	    = _import "gtk_dialog_add_action_widget"
		      : cptr * cptr * int -> unit;
	val add_action_widget : 'a t -> 'b Widget.t -> int -> unit
	    = fn self => fn child => fn response_id =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => add_action_widget_
						(self, child, response_id)))
	val add_button_ : cptr * CString.cstring * int -> cptr
	    = _import "gtk_dialog_add_button"
		      : cptr * CString.cstring * int -> cptr;
	val add_button : 'a t -> string -> int -> base Widget.t
	    = fn self => fn button_text => fn response_id =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self => add_button_ (self, 
							CString.fromString
							  button_text, 
							response_id)))
	val add_buttons_ : cptr * CString.cstring -> unit
	    = _import "gtk_dialog_add_buttons"
		      : cptr * CString.cstring -> unit;
	val add_buttons : 'a t -> string -> unit
	    = fn self => fn first_button_text =>
		 GObject.withPtr
		   (self, 
		    fn self => add_buttons_
				 (self, CString.fromString first_button_text))
	val set_response_sensitive_ : cptr * int * bool -> unit
	    = _import "gtk_dialog_set_response_sensitive"
		      : cptr * int * bool -> unit;
	val set_response_sensitive : 'a t -> int -> bool -> unit
	    = fn self => fn response_id => fn setting =>
		 GObject.withPtr (self, 
				  fn self => set_response_sensitive_
					       (self, response_id, setting))
	val set_default_response_ : cptr * int -> unit
	    = _import "gtk_dialog_set_default_response" : cptr * int -> unit;
	val set_default_response : 'a t -> int -> unit
	    = fn self => fn response_id =>
		 GObject.withPtr
		   (self, fn self => set_default_response_ (self, response_id))
	val set_has_separator_ : cptr * bool -> unit
	    = _import "gtk_dialog_set_has_separator" : cptr * bool -> unit;
	val set_has_separator : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_has_separator_ (self, setting))
	val get_has_separator_ : cptr -> bool
	    = _import "gtk_dialog_get_has_separator" : cptr -> bool;
	val get_has_separator : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_has_separator_ self)
	val response_ : cptr * int -> unit
	    = _import "gtk_dialog_response" : cptr * int -> unit;
	val response : 'a t -> int -> unit
	    = fn self => fn response_id =>
		 GObject.withPtr
		   (self, fn self => response_ (self, response_id))
	val run_ : cptr -> int = _import "gtk_dialog_run" : cptr -> int;
	val run : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => run_ self)
	local open Signal
	      infixr -->
	in val response_sig : (int -> unit) -> 'a t Signal.signal
	       = fn f => signal "response" false (int --> return_void) f
	   val close_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "close" false (void --> return_void) f
	end
    end
    structure ColorSelectionDialog :>
      sig
	type base
	type 'a colorselectiondialog_t
	type 'a t = 'a colorselectiondialog_t Dialog.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toColorSelectionDialog : 'a t -> base t
	val new : string -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a colorselectiondialog_t = unit
	type 'a t = 'a colorselectiondialog_t Dialog.t
	fun inherit w con = Dialog.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toColorSelectionDialog obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : CString.cstring -> cptr
	    = _import "gtk_color_selection_dialog_new"
		      : CString.cstring -> cptr;
	val new : string -> base t
	    = fn title => make (new_ (CString.fromString title))
    end
    structure HBox :>
      sig
	type base
	type 'a hbox_t
	type 'a t = 'a hbox_t Box.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toHBox : 'a t -> base t
	val get_type : unit -> GType.t
	val new : bool -> int -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a hbox_t = unit
	type 'a t = 'a hbox_t Box.t
	fun inherit w con = Box.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toHBox obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_hbox_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : bool * int -> cptr
	    = _import "gtk_hbox_new" : bool * int -> cptr;
	val new : bool -> int -> base t = fn homogeneous => fn spacing =>
					     make (new_ (homogeneous, spacing))
    end
    structure Combo :>
      sig
	type base
	type 'a combo_t
	type 'a t = 'a combo_t HBox.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toCombo : 'a t -> base t
	val get_type : unit -> GType.t
	val set_value_in_list : 'a t -> bool -> bool -> unit
	val set_use_arrows : 'a t -> bool -> unit
	val set_use_arrows_always : 'a t -> bool -> unit
	val set_case_sensitive : 'a t -> bool -> unit
	val set_item_string : 'a t -> 'b Item.t -> string -> unit
	val disable_activate : 'a t -> unit
	val box_get_type : unit -> GType.t
	val box_new_text : unit -> base t
	val box_entry_get_type : unit -> GType.t
	val box_entry_new_with_model : 'a TreeModel.t -> int -> base t
	val box_entry_new_text : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a combo_t = unit
	type 'a t = 'a combo_t HBox.t
	fun inherit w con = HBox.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toCombo obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_combo_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_value_in_list_ : cptr * bool * bool -> unit
	    = _import "gtk_combo_set_value_in_list"
		      : cptr * bool * bool -> unit;
	val set_value_in_list : 'a t -> bool -> bool -> unit
	    = fn self => fn valu => fn ok_if_empty =>
		 GObject.withPtr (self, 
				  fn self => set_value_in_list_
					       (self, valu, ok_if_empty))
	val set_use_arrows_ : cptr * bool -> unit
	    = _import "gtk_combo_set_use_arrows" : cptr * bool -> unit;
	val set_use_arrows : 'a t -> bool -> unit
	    = fn self => fn valu =>
		 GObject.withPtr
		   (self, fn self => set_use_arrows_ (self, valu))
	val set_use_arrows_always_ : cptr * bool -> unit
	    = _import "gtk_combo_set_use_arrows_always" : cptr * bool -> unit;
	val set_use_arrows_always : 'a t -> bool -> unit
	    = fn self => fn valu =>
		 GObject.withPtr
		   (self, fn self => set_use_arrows_always_ (self, valu))
	val set_case_sensitive_ : cptr * bool -> unit
	    = _import "gtk_combo_set_case_sensitive" : cptr * bool -> unit;
	val set_case_sensitive : 'a t -> bool -> unit
	    = fn self => fn valu =>
		 GObject.withPtr
		   (self, fn self => set_case_sensitive_ (self, valu))
	val set_item_string_ : cptr * cptr * CString.cstring -> unit
	    = _import "gtk_combo_set_item_string"
		      : cptr * cptr * CString.cstring -> unit;
	val set_item_string : 'a t -> 'b Item.t -> string -> unit
	    = fn self => fn item => fn item_value =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (item, 
				  fn item => set_item_string_
					       (self, item, 
						CString.fromString
						  item_value)))
	val disable_activate_ : cptr -> unit
	    = _import "gtk_combo_disable_activate" : cptr -> unit;
	val disable_activate : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => disable_activate_ self)
	val box_get_type_ : unit -> GType.t
	    = _import "gtk_combo_box_get_type" : unit -> GType.t;
	val box_get_type : unit -> GType.t = fn dummy => box_get_type_ dummy
	val box_new_text_ : unit -> cptr
	    = _import "gtk_combo_box_new_text" : unit -> cptr;
	val box_new_text : unit -> base t
	    = fn dummy => make (box_new_text_ dummy)
	val box_entry_get_type_ : unit -> GType.t
	    = _import "gtk_combo_box_entry_get_type" : unit -> GType.t;
	val box_entry_get_type : unit -> GType.t
	    = fn dummy => box_entry_get_type_ dummy
	val box_entry_new_with_model_ : cptr * int -> cptr
	    = _import "gtk_combo_box_entry_new_with_model"
		      : cptr * int -> cptr;
	val box_entry_new_with_model : 'a TreeModel.t -> int -> base t
	    = fn model => fn text_column =>
		 make (GObject.withPtr (model, 
					fn model => box_entry_new_with_model_
						      (model, text_column)))
	val box_entry_new_text_ : unit -> cptr
	    = _import "gtk_combo_box_entry_new_text" : unit -> cptr;
	val box_entry_new_text : unit -> base t
	    = fn dummy => make (box_entry_new_text_ dummy)
    end
    structure ComboBox :>
      sig
	type base
	type 'a combobox_t
	type 'a t = 'a combobox_t Bin.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toComboBox : 'a t -> base t
	val asCellLayout : 'a t -> base CellLayout.t
	val new : unit -> base t
	val new_with_model : 'a TreeModel.t -> base t
	val set_wrap_width : 'a t -> int -> unit
	val set_row_span_column : 'a t -> int -> unit
	val set_column_span_column : 'a t -> int -> unit
	val get_active : 'a t -> int
	val set_active : 'a t -> int -> unit
	val get_activeiter : 'a t -> TreeIter.t -> bool
	val set_activeiter : 'a t -> TreeIter.t -> unit
	val set_model : 'a t -> 'b TreeModel.t -> unit
	val get_model : 'a t -> base TreeModel.t
	val append_text : 'a t -> string -> unit
	val insert_text : 'a t -> int -> string -> unit
	val prepend_text : 'a t -> string -> unit
	val remove_text : 'a t -> int -> unit
	val popup : 'a t -> unit
	val popdown : 'a t -> unit
	val changed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a combobox_t = unit
	type 'a t = 'a combobox_t Bin.t
	fun inherit w con = Bin.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toComboBox obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asCellLayout obj
	  = CellLayout.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : unit -> cptr = _import "gtk_combo_box_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_model_ : cptr -> cptr
	    = _import "gtk_combo_box_new_with_model" : cptr -> cptr;
	val new_with_model : 'a TreeModel.t -> base t
	    = fn model => make (GObject.withPtr
				  (model, fn model => new_with_model_ model))
	val set_wrap_width_ : cptr * int -> unit
	    = _import "gtk_combo_box_set_wrap_width" : cptr * int -> unit;
	val set_wrap_width : 'a t -> int -> unit
	    = fn self => fn width =>
		 GObject.withPtr
		   (self, fn self => set_wrap_width_ (self, width))
	val set_row_span_column_ : cptr * int -> unit
	    = _import "gtk_combo_box_set_row_span_column" : cptr * int -> unit;
	val set_row_span_column : 'a t -> int -> unit
	    = fn self => fn row_span =>
		 GObject.withPtr
		   (self, fn self => set_row_span_column_ (self, row_span))
	val set_column_span_column_ : cptr * int -> unit
	    = _import "gtk_combo_box_set_column_span_column"
		      : cptr * int -> unit;
	val set_column_span_column : 'a t -> int -> unit
	    = fn self => fn column_span =>
		 GObject.withPtr (self, 
				  fn self => set_column_span_column_
					       (self, column_span))
	val get_active_ : cptr -> int
	    = _import "gtk_combo_box_get_active" : cptr -> int;
	val get_active : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_active_ self)
	val set_active_ : cptr * int -> unit
	    = _import "gtk_combo_box_set_active" : cptr * int -> unit;
	val set_active : 'a t -> int -> unit
	    = fn self => fn index =>
		 GObject.withPtr (self, fn self => set_active_ (self, index))
	val get_activeiter_ : cptr * cptr -> bool
	    = _import "gtk_combo_box_get_active_iter" : cptr * cptr -> bool;
	val get_activeiter : 'a t -> TreeIter.t -> bool
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, fn self => get_activeiter_ (self, iter))
	val set_activeiter_ : cptr * cptr -> unit
	    = _import "gtk_combo_box_set_active_iter" : cptr * cptr -> unit;
	val set_activeiter : 'a t -> TreeIter.t -> unit
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, fn self => set_activeiter_ (self, iter))
	val set_model_ : cptr * cptr -> unit
	    = _import "gtk_combo_box_set_model" : cptr * cptr -> unit;
	val set_model : 'a t -> 'b TreeModel.t -> unit
	    = fn self => fn model =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (model, fn model => set_model_ (self, model)))
	val get_model_ : cptr -> cptr
	    = _import "gtk_combo_box_get_model" : cptr -> cptr;
	val get_model : 'a t -> base TreeModel.t
	    = fn self => TreeModel.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_model_ self))
	val append_text_ : cptr * CString.cstring -> unit
	    = _import "gtk_combo_box_append_text"
		      : cptr * CString.cstring -> unit;
	val append_text : 'a t -> string -> unit
	    = fn self => fn text =>
		 GObject.withPtr
		   (self, 
		    fn self => append_text_ (self, CString.fromString text))
	val insert_text_ : cptr * int * CString.cstring -> unit
	    = _import "gtk_combo_box_insert_text"
		      : cptr * int * CString.cstring -> unit;
	val insert_text : 'a t -> int -> string -> unit
	    = fn self => fn position => fn text =>
		 GObject.withPtr
		   (self, 
		    fn self => insert_text_ (self, position, 
					     CString.fromString text))
	val prepend_text_ : cptr * CString.cstring -> unit
	    = _import "gtk_combo_box_prepend_text"
		      : cptr * CString.cstring -> unit;
	val prepend_text : 'a t -> string -> unit
	    = fn self => fn text =>
		 GObject.withPtr
		   (self, 
		    fn self => prepend_text_ (self, CString.fromString text))
	val remove_text_ : cptr * int -> unit
	    = _import "gtk_combo_box_remove_text" : cptr * int -> unit;
	val remove_text : 'a t -> int -> unit
	    = fn self => fn position =>
		 GObject.withPtr
		   (self, fn self => remove_text_ (self, position))
	val popup_ : cptr -> unit
	    = _import "gtk_combo_box_popup" : cptr -> unit;
	val popup : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => popup_ self)
	val popdown_ : cptr -> unit
	    = _import "gtk_combo_box_popdown" : cptr -> unit;
	val popdown : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => popdown_ self)
	local open Signal
	      infixr -->
	in val changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "changed" false (void --> return_void) f
	end
    end
    structure ComboBoxEntry :>
      sig
	type base
	type 'a comboboxentry_t
	type 'a t = 'a comboboxentry_t ComboBox.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toComboBoxEntry : 'a t -> base t
	val asCellLayout : 'a t -> base CellLayout.t
	val new : unit -> base t
	val set_text_column : 'a t -> int -> unit
	val get_text_column : 'a t -> int
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a comboboxentry_t = unit
	type 'a t = 'a comboboxentry_t ComboBox.t
	fun inherit w con = ComboBox.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toComboBoxEntry obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asCellLayout obj
	  = CellLayout.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : unit -> cptr
	    = _import "gtk_combo_box_entry_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val set_text_column_ : cptr * int -> unit
	    = _import "gtk_combo_box_entry_set_text_column"
		      : cptr * int -> unit;
	val set_text_column : 'a t -> int -> unit
	    = fn self => fn text_column =>
		 GObject.withPtr
		   (self, fn self => set_text_column_ (self, text_column))
	val get_text_column_ : cptr -> int
	    = _import "gtk_combo_box_entry_get_text_column" : cptr -> int;
	val get_text_column : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_text_column_ self)
    end
    structure DrawingArea :>
      sig
	type base
	type 'a drawingarea_t
	type 'a t = 'a drawingarea_t Widget.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toDrawingArea : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a drawingarea_t = unit
	type 'a t = 'a drawingarea_t Widget.t
	fun inherit w con = Widget.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toDrawingArea obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_drawing_area_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr
	    = _import "gtk_drawing_area_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure Curve :>
      sig
	type base
	type 'a curve_t
	type 'a t = 'a curve_t DrawingArea.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toCurve : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val reset : 'a t -> unit
	val set_gamma : 'a t -> real -> unit
	val set_range : 'a t -> real -> real -> real -> real -> unit
	val set_curvetype : 'a t -> curvetype -> unit
	val curve_type_changed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a curve_t = unit
	type 'a t = 'a curve_t DrawingArea.t
	fun inherit w con = DrawingArea.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toCurve obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_curve_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_curve_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val reset_ : cptr -> unit = _import "gtk_curve_reset" : cptr -> unit;
	val reset : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => reset_ self)
	val set_gamma_ : cptr * real -> unit
	    = _import "gtk_curve_set_gamma" : cptr * real -> unit;
	val set_gamma : 'a t -> real -> unit
	    = fn self => fn gamma =>
		 GObject.withPtr (self, fn self => set_gamma_ (self, gamma))
	val set_range_ : cptr * real * real * real * real -> unit
	    = _import "gtk_curve_set_range"
		      : cptr * real * real * real * real -> unit;
	val set_range : 'a t -> real -> real -> real -> real -> unit
	    = fn self => fn min_x => fn max_x => fn min_y => fn max_y =>
		 GObject.withPtr
		   (self, 
		    fn self => set_range_ (self, min_x, max_x, min_y, max_y))
	val set_curvetype_ : cptr * int -> unit
	    = _import "gtk_curve_set_curve_type" : cptr * int -> unit;
	val set_curvetype : 'a t -> curvetype -> unit
	    = fn self => fn typ =>
		 GObject.withPtr (self, fn self => set_curvetype_ (self, typ))
	local open Signal
	      infixr -->
	in val curve_type_changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "curve-type-changed" false
			        (void --> return_void) f
	end
    end
    structure Entry :>
      sig
	type base
	type 'a entry_t
	type 'a t = 'a entry_t Widget.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toEntry : 'a t -> base t
	val asEditable : 'a t -> base Editable.t
	val asCellEditable : 'a t -> base CellEditable.t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val set_visibility : 'a t -> bool -> unit
	val get_visibility : 'a t -> bool
	val set_invisible_char : 'a t -> char -> unit
	val get_invisible_char : 'a t -> char
	val set_has_frame : 'a t -> bool -> unit
	val get_has_frame : 'a t -> bool
	val set_max_length : 'a t -> int -> unit
	val get_max_length : 'a t -> int
	val set_activates_default : 'a t -> bool -> unit
	val get_activates_default : 'a t -> bool
	val set_width_chars : 'a t -> int -> unit
	val get_width_chars : 'a t -> int
	val set_text : 'a t -> string -> unit
	val get_text : 'a t -> string
	val set_alignment : 'a t -> real -> unit
	val get_alignment : 'a t -> real
	val set_position : 'a t -> int -> unit
	val select_region : 'a t -> int -> int -> unit
	val set_editable : 'a t -> bool -> unit
	val completion_get_type : unit -> GType.t
	val move_cursor_sig : (unit -> int -> bool -> unit)
			      -> 'a t Signal.signal
	val activate_sig : (unit -> unit) -> 'a t Signal.signal
	val insert_at_cursor_sig : (char -> unit) -> 'a t Signal.signal
	val delete_from_cursor_sig
	  : (unit -> int -> unit) -> 'a t Signal.signal
	val cut_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	val copy_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	val paste_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	val toggle_overwrite_sig : (unit -> unit) -> 'a t Signal.signal
	val populate_popup_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a entry_t = unit
	type 'a t = 'a entry_t Widget.t
	fun inherit w con = Widget.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toEntry obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asEditable obj
	  = Editable.inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asCellEditable obj
	  = CellEditable.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_entry_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_entry_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val set_visibility_ : cptr * bool -> unit
	    = _import "gtk_entry_set_visibility" : cptr * bool -> unit;
	val set_visibility : 'a t -> bool -> unit
	    = fn self => fn visible =>
		 GObject.withPtr
		   (self, fn self => set_visibility_ (self, visible))
	val get_visibility_ : cptr -> bool
	    = _import "gtk_entry_get_visibility" : cptr -> bool;
	val get_visibility : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_visibility_ self)
	val set_invisible_char_ : cptr * char -> unit
	    = _import "gtk_entry_set_invisible_char" : cptr * char -> unit;
	val set_invisible_char : 'a t -> char -> unit
	    = fn self => fn ch =>
		 GObject.withPtr
		   (self, fn self => set_invisible_char_ (self, ch))
	val get_invisible_char_ : cptr -> char
	    = _import "gtk_entry_get_invisible_char" : cptr -> char;
	val get_invisible_char : 'a t -> char
	    = fn self => GObject.withPtr
			   (self, fn self => get_invisible_char_ self)
	val set_has_frame_ : cptr * bool -> unit
	    = _import "gtk_entry_set_has_frame" : cptr * bool -> unit;
	val set_has_frame : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_has_frame_ (self, setting))
	val get_has_frame_ : cptr -> bool
	    = _import "gtk_entry_get_has_frame" : cptr -> bool;
	val get_has_frame : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_has_frame_ self)
	val set_max_length_ : cptr * int -> unit
	    = _import "gtk_entry_set_max_length" : cptr * int -> unit;
	val set_max_length : 'a t -> int -> unit
	    = fn self => fn max =>
		 GObject.withPtr (self, fn self => set_max_length_ (self, max))
	val get_max_length_ : cptr -> int
	    = _import "gtk_entry_get_max_length" : cptr -> int;
	val get_max_length : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_max_length_ self)
	val set_activates_default_ : cptr * bool -> unit
	    = _import "gtk_entry_set_activates_default" : cptr * bool -> unit;
	val set_activates_default : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_activates_default_ (self, setting))
	val get_activates_default_ : cptr -> bool
	    = _import "gtk_entry_get_activates_default" : cptr -> bool;
	val get_activates_default : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_activates_default_ self)
	val set_width_chars_ : cptr * int -> unit
	    = _import "gtk_entry_set_width_chars" : cptr * int -> unit;
	val set_width_chars : 'a t -> int -> unit
	    = fn self => fn n_chars =>
		 GObject.withPtr
		   (self, fn self => set_width_chars_ (self, n_chars))
	val get_width_chars_ : cptr -> int
	    = _import "gtk_entry_get_width_chars" : cptr -> int;
	val get_width_chars : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_width_chars_ self)
	val set_text_ : cptr * CString.cstring -> unit
	    = _import "gtk_entry_set_text" : cptr * CString.cstring -> unit;
	val set_text : 'a t -> string -> unit
	    = fn self => fn text =>
		 GObject.withPtr
		   (self, fn self => set_text_ (self, CString.fromString text))
	val get_text_ : cptr -> CString.t
	    = _import "gtk_entry_get_text" : cptr -> CString.t;
	val get_text : 'a t -> string
	    = fn self => GObject.withPtr (self, 
					  fn self => let val t = get_text_ self
						     in CString.toString t end)
	val set_alignment_ : cptr * real -> unit
	    = _import "gtk_entry_set_alignment" : cptr * real -> unit;
	val set_alignment : 'a t -> real -> unit
	    = fn self => fn xalign =>
		 GObject.withPtr
		   (self, fn self => set_alignment_ (self, xalign))
	val get_alignment_ : cptr -> real
	    = _import "gtk_entry_get_alignment" : cptr -> real;
	val get_alignment : 'a t -> real
	    = fn self => GObject.withPtr (self, fn self => get_alignment_ self)
	val set_position_ : cptr * int -> unit
	    = _import "gtk_entry_set_position" : cptr * int -> unit;
	val set_position : 'a t -> int -> unit
	    = fn self => fn position =>
		 GObject.withPtr
		   (self, fn self => set_position_ (self, position))
	val select_region_ : cptr * int * int -> unit
	    = _import "gtk_entry_select_region" : cptr * int * int -> unit;
	val select_region : 'a t -> int -> int -> unit
	    = fn self => fn start => fn en =>
		 GObject.withPtr
		   (self, fn self => select_region_ (self, start, en))
	val set_editable_ : cptr * bool -> unit
	    = _import "gtk_entry_set_editable" : cptr * bool -> unit;
	val set_editable : 'a t -> bool -> unit
	    = fn self => fn editable =>
		 GObject.withPtr
		   (self, fn self => set_editable_ (self, editable))
	val completion_get_type_ : unit -> GType.t
	    = _import "gtk_entry_completion_get_type" : unit -> GType.t;
	val completion_get_type : unit -> GType.t
	    = fn dummy => completion_get_type_ dummy
	local open Signal
	      infixr -->
	in
	  val move_cursor_sig : (unit -> int -> bool -> unit)
				-> 'a t Signal.signal
	      = fn f => signal "move-cursor" false
			       (unit --> int --> bool --> return_void) f
	  val activate_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "activate" false (void --> return_void) f
	  val insert_at_cursor_sig : (char -> unit) -> 'a t Signal.signal
	      = fn f =>
		   signal "insert-at-cursor" false (char --> return_void) f
	  val delete_from_cursor_sig
	    : (unit -> int -> unit) -> 'a t Signal.signal
	      = fn f => signal "delete-from-cursor" false
			       (unit --> int --> return_void) f
	  val cut_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "cut-clipboard" false (void --> return_void) f
	  val copy_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "copy-clipboard" false (void --> return_void) f
	  val paste_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "paste-clipboard" false (void --> return_void) f
	  val toggle_overwrite_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f =>
		   signal "toggle-overwrite" false (void --> return_void) f
	  val populate_popup_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "populate-popup" false (unit --> return_void) f
	end
    end
    structure EntryCompletion :>
      sig
	type base
	type 'a entrycompletion_t
	type 'a t = 'a entrycompletion_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toEntryCompletion : 'a t -> base t
	val asCellLayout : 'a t -> base CellLayout.t
	val new : unit -> base t
	val get_entry : 'a t -> base Widget.t
	val set_model : 'a t -> 'b TreeModel.t -> unit
	val get_model : 'a t -> base TreeModel.t
	val set_minimum_key_length : 'a t -> int -> unit
	val get_minimum_key_length : 'a t -> int
	val complete : 'a t -> unit
	val insert_action_text : 'a t -> int -> string -> unit
	val insert_action_markup : 'a t -> int -> string -> unit
	val delete_action : 'a t -> int -> unit
	val set_text_column : 'a t -> int -> unit
	val match_selected_sig : (unit -> unit -> bool) -> 'a t Signal.signal
	val action_activated_sig : (int -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a entrycompletion_t = unit
	type 'a t = 'a entrycompletion_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toEntryCompletion obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asCellLayout obj
	  = CellLayout.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : unit -> cptr
	    = _import "gtk_entry_completion_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_entry_ : cptr -> cptr
	    = _import "gtk_entry_completion_get_entry" : cptr -> cptr;
	val get_entry : 'a t -> base Widget.t
	    = fn self => Widget.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_entry_ self))
	val set_model_ : cptr * cptr -> unit
	    = _import "gtk_entry_completion_set_model" : cptr * cptr -> unit;
	val set_model : 'a t -> 'b TreeModel.t -> unit
	    = fn self => fn model =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (model, fn model => set_model_ (self, model)))
	val get_model_ : cptr -> cptr
	    = _import "gtk_entry_completion_get_model" : cptr -> cptr;
	val get_model : 'a t -> base TreeModel.t
	    = fn self => TreeModel.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_model_ self))
	val set_minimum_key_length_ : cptr * int -> unit
	    = _import "gtk_entry_completion_set_minimum_key_length"
		      : cptr * int -> unit;
	val set_minimum_key_length : 'a t -> int -> unit
	    = fn self => fn length =>
		 GObject.withPtr
		   (self, fn self => set_minimum_key_length_ (self, length))
	val get_minimum_key_length_ : cptr -> int
	    = _import "gtk_entry_completion_get_minimum_key_length"
		      : cptr -> int;
	val get_minimum_key_length : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_minimum_key_length_ self)
	val complete_ : cptr -> unit
	    = _import "gtk_entry_completion_complete" : cptr -> unit;
	val complete : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => complete_ self)
	val insert_action_text_ : cptr * int * CString.cstring -> unit
	    = _import "gtk_entry_completion_insert_action_text"
		      : cptr * int * CString.cstring -> unit;
	val insert_action_text : 'a t -> int -> string -> unit
	    = fn self => fn index => fn text =>
		 GObject.withPtr
		   (self, 
		    fn self => insert_action_text_
				 (self, index, CString.fromString text))
	val insert_action_markup_ : cptr * int * CString.cstring -> unit
	    = _import "gtk_entry_completion_insert_action_markup"
		      : cptr * int * CString.cstring -> unit;
	val insert_action_markup : 'a t -> int -> string -> unit
	    = fn self => fn index => fn markup =>
		 GObject.withPtr
		   (self, 
		    fn self => insert_action_markup_
				 (self, index, CString.fromString markup))
	val delete_action_ : cptr * int -> unit
	    = _import "gtk_entry_completion_delete_action"
		      : cptr * int -> unit;
	val delete_action : 'a t -> int -> unit
	    = fn self => fn index =>
		 GObject.withPtr
		   (self, fn self => delete_action_ (self, index))
	val set_text_column_ : cptr * int -> unit
	    = _import "gtk_entry_completion_set_text_column"
		      : cptr * int -> unit;
	val set_text_column : 'a t -> int -> unit
	    = fn self => fn column =>
		 GObject.withPtr
		   (self, fn self => set_text_column_ (self, column))
	local open Signal
	      infixr -->
	in
	  val match_selected_sig : (unit -> unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "match-selected" false
			       (unit --> unit --> return_bool) f
	  val action_activated_sig : (int -> unit) -> 'a t Signal.signal
	      = fn f => signal "action-activated" false (int --> return_void) f
	end
    end
    structure EventBox :>
      sig
	type base
	type 'a eventbox_t
	type 'a t = 'a eventbox_t Bin.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toEventBox : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val get_visible_window : 'a t -> bool
	val set_visible_window : 'a t -> bool -> unit
	val get_above_child : 'a t -> bool
	val set_above_child : 'a t -> bool -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a eventbox_t = unit
	type 'a t = 'a eventbox_t Bin.t
	fun inherit w con = Bin.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toEventBox obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_event_box_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_event_box_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_visible_window_ : cptr -> bool
	    = _import "gtk_event_box_get_visible_window" : cptr -> bool;
	val get_visible_window : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_visible_window_ self)
	val set_visible_window_ : cptr * bool -> unit
	    = _import "gtk_event_box_set_visible_window" : cptr * bool -> unit;
	val set_visible_window : 'a t -> bool -> unit
	    = fn self => fn visible_window =>
		 GObject.withPtr (self, 
				  fn self => set_visible_window_
					       (self, visible_window))
	val get_above_child_ : cptr -> bool
	    = _import "gtk_event_box_get_above_child" : cptr -> bool;
	val get_above_child : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_above_child_ self)
	val set_above_child_ : cptr * bool -> unit
	    = _import "gtk_event_box_set_above_child" : cptr * bool -> unit;
	val set_above_child : 'a t -> bool -> unit
	    = fn self => fn above_child =>
		 GObject.withPtr
		   (self, fn self => set_above_child_ (self, above_child))
    end
    structure Expander :>
      sig
	type base
	type 'a expander_t
	type 'a t = 'a expander_t Bin.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toExpander : 'a t -> base t
	type style
	val COLLAPSED : style
	val SEMI_COLLAPSED : style
	val SEMI_EXPANDED : style
	val EXPANDED : style
	val get_type : unit -> GType.t
	val new : string -> base t
	val new_with_mnemonic : string option -> base t
	val new_with_mnemonic' : unit -> base t
	val set_expanded : 'a t -> bool -> unit
	val get_expanded : 'a t -> bool
	val set_spacing : 'a t -> int -> unit
	val get_spacing : 'a t -> int
	val set_label : 'a t -> string option -> unit
	val set_label' : 'a t -> unit
	val get_label : 'a t -> string
	val set_use_underline : 'a t -> bool -> unit
	val get_use_underline : 'a t -> bool
	val set_use_markup : 'a t -> bool -> unit
	val get_use_markup : 'a t -> bool
	val set_label_widget : 'a t -> 'b Widget.t option -> unit
	val set_label_widget' : 'a t -> unit
	val get_label_widget : 'a t -> base Widget.t
	val activate_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a expander_t = unit
	type 'a t = 'a expander_t Bin.t
	fun inherit w con = Bin.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toExpander obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type style = int
	val get_style_ : int ref * int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_expander_style"
		      : int ref * int ref * int ref * int ref -> unit;
	val (COLLAPSED, SEMI_COLLAPSED, SEMI_EXPANDED, EXPANDED)
	    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	      in get_style_ (x0, x1, x2, x3)
	       ; (!x0, !x1, !x2, !x3)
	      end
	val get_type_ : unit -> GType.t
	    = _import "gtk_expander_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : CString.cstring -> cptr
	    = _import "gtk_expander_new" : CString.cstring -> cptr;
	val new : string -> base t
	    = fn label => make (new_ (CString.fromString label))
	val new_with_mnemonic_ : CString.cstring -> cptr
	    = _import "gtk_expander_new_with_mnemonic"
		      : CString.cstring -> cptr;
	val new_with_mnemonic : string option -> base t
	    = fn label => make (new_with_mnemonic_
				  (CString.fromString (getOpt (label, ""))))
	val new_with_mnemonic' : unit -> base t
	    = fn dummy => make (new_with_mnemonic_ (CString.fromString ""))
	val set_expanded_ : cptr * bool -> unit
	    = _import "gtk_expander_set_expanded" : cptr * bool -> unit;
	val set_expanded : 'a t -> bool -> unit
	    = fn self => fn expanded =>
		 GObject.withPtr
		   (self, fn self => set_expanded_ (self, expanded))
	val get_expanded_ : cptr -> bool
	    = _import "gtk_expander_get_expanded" : cptr -> bool;
	val get_expanded : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_expanded_ self)
	val set_spacing_ : cptr * int -> unit
	    = _import "gtk_expander_set_spacing" : cptr * int -> unit;
	val set_spacing : 'a t -> int -> unit
	    = fn self => fn spacing =>
		 GObject.withPtr
		   (self, fn self => set_spacing_ (self, spacing))
	val get_spacing_ : cptr -> int
	    = _import "gtk_expander_get_spacing" : cptr -> int;
	val get_spacing : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_spacing_ self)
	val set_label_ : cptr * CString.cstring -> unit
	    = _import "gtk_expander_set_label"
		      : cptr * CString.cstring -> unit;
	val set_label : 'a t -> string option -> unit
	    = fn self => fn label =>
		 GObject.withPtr
		   (self, 
		    fn self => set_label_ (self, 
					   CString.fromString
					     (getOpt (label, ""))))
	val set_label' : 'a t -> unit
	    = fn self =>
		 GObject.withPtr
		   (self, fn self => set_label_ (self, CString.fromString ""))
	val get_label_ : cptr -> CString.t
	    = _import "gtk_expander_get_label" : cptr -> CString.t;
	val get_label : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_label_ self
				       in CString.toString t end)
	val set_use_underline_ : cptr * bool -> unit
	    = _import "gtk_expander_set_use_underline" : cptr * bool -> unit;
	val set_use_underline : 'a t -> bool -> unit
	    = fn self => fn use_underline =>
		 GObject.withPtr
		   (self, fn self => set_use_underline_ (self, use_underline))
	val get_use_underline_ : cptr -> bool
	    = _import "gtk_expander_get_use_underline" : cptr -> bool;
	val get_use_underline : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_use_underline_ self)
	val set_use_markup_ : cptr * bool -> unit
	    = _import "gtk_expander_set_use_markup" : cptr * bool -> unit;
	val set_use_markup : 'a t -> bool -> unit
	    = fn self => fn use_markup =>
		 GObject.withPtr
		   (self, fn self => set_use_markup_ (self, use_markup))
	val get_use_markup_ : cptr -> bool
	    = _import "gtk_expander_get_use_markup" : cptr -> bool;
	val get_use_markup : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_use_markup_ self)
	val set_label_widget_ : cptr * cptr -> unit
	    = _import "gtk_expander_set_label_widget" : cptr * cptr -> unit;
	val set_label_widget : 'a t -> 'b Widget.t option -> unit
	    = fn self => fn label_widget =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withOpt (label_widget, 
						fn label_widget =>
						   set_label_widget_
						     (self, label_widget)))
	val set_label_widget' : 'a t -> unit
	    = fn self =>
		 GObject.withPtr
		   (self, fn self => set_label_widget_ (self, GObject.null))
	val get_label_widget_ : cptr -> cptr
	    = _import "gtk_expander_get_label_widget" : cptr -> cptr;
	val get_label_widget : 'a t -> base Widget.t
	    = fn self =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_label_widget_ self))
	local open Signal
	      infixr -->
	in val activate_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "activate" false (void --> return_void) f
	end
    end
    structure FileChooserDialog :>
      sig
	type base
	type 'a filechooserdialog_t
	type 'a t = 'a filechooserdialog_t Dialog.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toFileChooserDialog : 'a t -> base t
	val asFileChooser : 'a t -> base FileChooser.t
	val new : string -> 'a Window.t -> FileChooser.action -> string -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a filechooserdialog_t = unit
	type 'a t = 'a filechooserdialog_t Dialog.t
	fun inherit w con = Dialog.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toFileChooserDialog obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asFileChooser obj
	  = FileChooser.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : CString.cstring * cptr * FileChooser.action * CString.cstring -> cptr
	    = _import "gtk_file_chooser_dialog_new"
		      : CString.cstring * cptr * FileChooser.action * CString.cstring -> cptr;
	val new : string -> 'a Window.t -> FileChooser.action -> string -> base t
	    = fn title => fn parent => fn action => fn first_button_text =>
		 make (GObject.withPtr
			 (parent, 
			  fn parent =>
			     new_ (CString.fromString title, parent, action, 
				   CString.fromString first_button_text)))
    end
    structure FileChooserWidget :>
      sig
	type base
	type 'a filechooserwidget_t
	type 'a t = 'a filechooserwidget_t VBox.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toFileChooserWidget : 'a t -> base t
	val asFileChooser : 'a t -> base FileChooser.t
	val new : FileChooser.action -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a filechooserwidget_t = unit
	type 'a t = 'a filechooserwidget_t VBox.t
	fun inherit w con = VBox.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toFileChooserWidget obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asFileChooser obj
	  = FileChooser.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : FileChooser.action -> cptr
	    = _import "gtk_file_chooser_widget_new" : FileChooser.action -> cptr;
	val new : FileChooser.action -> base t = fn action => make (new_ action)
    end
    structure FileFilter :>
      sig
	type base
	type 'a filefilter_t
	type 'a t = 'a filefilter_t Object.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toFileFilter : 'a t -> base t
	type flags
	val FILENAME : flags
	val URI : flags
	val DISPLAY_NAME : flags
	val MIME_TYPE : flags
	val get_type : unit -> GType.t
	val new : unit -> base t
	val set_name : 'a t -> string -> unit
	val get_name : 'a t -> string
	val add_mime_type : 'a t -> string -> unit
	val add_pattern : 'a t -> string -> unit
	val get_needed : 'a t -> flags list
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a filefilter_t = unit
	type 'a t = 'a filefilter_t Object.t
	fun inherit w con = Object.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toFileFilter obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type flags = int
	val get_flags_ : int ref * int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_file_filter_flags"
		      : int ref * int ref * int ref * int ref -> unit;
	val (FILENAME, URI, DISPLAY_NAME, MIME_TYPE)
	    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	      in get_flags_ (x0, x1, x2, x3)
	       ; (!x0, !x1, !x2, !x3)
	      end
	val get_type_ : unit -> GType.t
	    = _import "gtk_file_filter_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_file_filter_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val set_name_ : cptr * CString.cstring -> unit
	    = _import "gtk_file_filter_set_name"
		      : cptr * CString.cstring -> unit;
	val set_name : 'a t -> string -> unit
	    = fn self => fn name =>
		 GObject.withPtr
		   (self, fn self => set_name_ (self, CString.fromString name))
	val get_name_ : cptr -> CString.t
	    = _import "gtk_file_filter_get_name" : cptr -> CString.t;
	val get_name : 'a t -> string
	    = fn self => GObject.withPtr (self, 
					  fn self => let val t = get_name_ self
						     in CString.toString t end)
	val add_mime_type_ : cptr * CString.cstring -> unit
	    = _import "gtk_file_filter_add_mime_type"
		      : cptr * CString.cstring -> unit;
	val add_mime_type : 'a t -> string -> unit
	    = fn self => fn mime_type =>
		 GObject.withPtr
		   (self, 
		    fn self => add_mime_type_
				 (self, CString.fromString mime_type))
	val add_pattern_ : cptr * CString.cstring -> unit
	    = _import "gtk_file_filter_add_pattern"
		      : cptr * CString.cstring -> unit;
	val add_pattern : 'a t -> string -> unit
	    = fn self => fn pattern =>
		 GObject.withPtr
		   (self, 
		    fn self => add_pattern_ (self, CString.fromString pattern))
	val get_needed_ : cptr -> int
	    = _import "gtk_file_filter_get_needed" : cptr -> int;
	val get_needed : 'a t -> flags list
	    = fn self => Flags.get (GObject.withPtr
				      (self, fn self => get_needed_ self))
    end
    structure FileSelection :>
      sig
	type base
	type 'a fileselection_t
	type 'a t = 'a fileselection_t Dialog.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toFileSelection : 'a t -> base t
	val get_type : unit -> GType.t
	val new : string -> base t
	val set_filename : 'a t -> string -> unit
	val get_filename : 'a t -> string
	val complete : 'a t -> string -> unit
	val show_fileop_buttons : 'a t -> unit
	val hide_fileop_buttons : 'a t -> unit
	val set_select_multiple : 'a t -> bool -> unit
	val get_select_multiple : 'a t -> bool
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a fileselection_t = unit
	type 'a t = 'a fileselection_t Dialog.t
	fun inherit w con = Dialog.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toFileSelection obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_file_selection_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : CString.cstring -> cptr
	    = _import "gtk_file_selection_new" : CString.cstring -> cptr;
	val new : string -> base t
	    = fn title => make (new_ (CString.fromString title))
	val set_filename_ : cptr * CString.cstring -> unit
	    = _import "gtk_file_selection_set_filename"
		      : cptr * CString.cstring -> unit;
	val set_filename : 'a t -> string -> unit
	    = fn self => fn filename =>
		 GObject.withPtr
		   (self, 
		    fn self => set_filename_
				 (self, CString.fromString filename))
	val get_filename_ : cptr -> CString.t
	    = _import "gtk_file_selection_get_filename" : cptr -> CString.t;
	val get_filename : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_filename_ self
				       in CString.toString t end)
	val complete_ : cptr * CString.cstring -> unit
	    = _import "gtk_file_selection_complete"
		      : cptr * CString.cstring -> unit;
	val complete : 'a t -> string -> unit
	    = fn self => fn pattern =>
		 GObject.withPtr
		   (self, 
		    fn self => complete_ (self, CString.fromString pattern))
	val show_fileop_buttons_ : cptr -> unit
	    = _import "gtk_file_selection_show_fileop_buttons" : cptr -> unit;
	val show_fileop_buttons : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => show_fileop_buttons_ self)
	val hide_fileop_buttons_ : cptr -> unit
	    = _import "gtk_file_selection_hide_fileop_buttons" : cptr -> unit;
	val hide_fileop_buttons : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => hide_fileop_buttons_ self)
	val set_select_multiple_ : cptr * bool -> unit
	    = _import "gtk_file_selection_set_select_multiple"
		      : cptr * bool -> unit;
	val set_select_multiple : 'a t -> bool -> unit
	    = fn self => fn select_multiple =>
		 GObject.withPtr (self, 
				  fn self => set_select_multiple_
					       (self, select_multiple))
	val get_select_multiple_ : cptr -> bool
	    = _import "gtk_file_selection_get_select_multiple" : cptr -> bool;
	val get_select_multiple : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_select_multiple_ self)
    end
    structure Fixed :>
      sig
	type base
	type 'a fixed_t
	type 'a t = 'a fixed_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toFixed : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val put : 'a t -> 'b Widget.t -> int -> int -> unit
	val move : 'a t -> 'b Widget.t -> int -> int -> unit
	val set_has_window : 'a t -> bool -> unit
	val get_has_window : 'a t -> bool
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a fixed_t = unit
	type 'a t = 'a fixed_t Container.t
	fun inherit w con = Container.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toFixed obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_fixed_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_fixed_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val put_ : cptr * cptr * int * int -> unit
	    = _import "gtk_fixed_put" : cptr * cptr * int * int -> unit;
	val put : 'a t -> 'b Widget.t -> int -> int -> unit
	    = fn self => fn widget => fn x => fn y =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (widget, fn widget => put_ (self, widget, x, y)))
	val move_ : cptr * cptr * int * int -> unit
	    = _import "gtk_fixed_move" : cptr * cptr * int * int -> unit;
	val move : 'a t -> 'b Widget.t -> int -> int -> unit
	    = fn self => fn widget => fn x => fn y =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (widget, fn widget => move_ (self, widget, x, y)))
	val set_has_window_ : cptr * bool -> unit
	    = _import "gtk_fixed_set_has_window" : cptr * bool -> unit;
	val set_has_window : 'a t -> bool -> unit
	    = fn self => fn has_window =>
		 GObject.withPtr
		   (self, fn self => set_has_window_ (self, has_window))
	val get_has_window_ : cptr -> bool
	    = _import "gtk_fixed_get_has_window" : cptr -> bool;
	val get_has_window : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_has_window_ self)
    end
    structure FontButton :>
      sig
	type base
	type 'a fontbutton_t
	type 'a t = 'a fontbutton_t Button.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toFontButton : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val new_with_font : string -> base t
	val get_title : 'a t -> string
	val set_title : 'a t -> string -> unit
	val get_use_font : 'a t -> bool
	val set_use_font : 'a t -> bool -> unit
	val get_use_size : 'a t -> bool
	val set_use_size : 'a t -> bool -> unit
	val get_font_name : 'a t -> string
	val set_font_name : 'a t -> string -> bool
	val get_show_style : 'a t -> bool
	val set_show_style : 'a t -> bool -> unit
	val get_show_size : 'a t -> bool
	val set_show_size : 'a t -> bool -> unit
	val font_set_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a fontbutton_t = unit
	type 'a t = 'a fontbutton_t Button.t
	fun inherit w con = Button.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toFontButton obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_font_button_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_font_button_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_font_ : CString.cstring -> cptr
	    = _import "gtk_font_button_new_with_font"
		      : CString.cstring -> cptr;
	val new_with_font : string -> base t
	    = fn fontname => make (new_with_font_
				     (CString.fromString fontname))
	val get_title_ : cptr -> CString.t
	    = _import "gtk_font_button_get_title" : cptr -> CString.t;
	val get_title : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_title_ self
				       in CString.toString t end)
	val set_title_ : cptr * CString.cstring -> unit
	    = _import "gtk_font_button_set_title"
		      : cptr * CString.cstring -> unit;
	val set_title : 'a t -> string -> unit
	    = fn self => fn title =>
		 GObject.withPtr
		   (self, 
		    fn self => set_title_ (self, CString.fromString title))
	val get_use_font_ : cptr -> bool
	    = _import "gtk_font_button_get_use_font" : cptr -> bool;
	val get_use_font : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_use_font_ self)
	val set_use_font_ : cptr * bool -> unit
	    = _import "gtk_font_button_set_use_font" : cptr * bool -> unit;
	val set_use_font : 'a t -> bool -> unit
	    = fn self => fn use_font =>
		 GObject.withPtr
		   (self, fn self => set_use_font_ (self, use_font))
	val get_use_size_ : cptr -> bool
	    = _import "gtk_font_button_get_use_size" : cptr -> bool;
	val get_use_size : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_use_size_ self)
	val set_use_size_ : cptr * bool -> unit
	    = _import "gtk_font_button_set_use_size" : cptr * bool -> unit;
	val set_use_size : 'a t -> bool -> unit
	    = fn self => fn use_size =>
		 GObject.withPtr
		   (self, fn self => set_use_size_ (self, use_size))
	val get_font_name_ : cptr -> CString.t
	    = _import "gtk_font_button_get_font_name" : cptr -> CString.t;
	val get_font_name : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_font_name_ self
				       in CString.toString t end)
	val set_font_name_ : cptr * CString.cstring -> bool
	    = _import "gtk_font_button_set_font_name"
		      : cptr * CString.cstring -> bool;
	val set_font_name : 'a t -> string -> bool
	    = fn self => fn fontname =>
		 GObject.withPtr
		   (self, 
		    fn self => set_font_name_
				 (self, CString.fromString fontname))
	val get_show_style_ : cptr -> bool
	    = _import "gtk_font_button_get_show_style" : cptr -> bool;
	val get_show_style : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_show_style_ self)
	val set_show_style_ : cptr * bool -> unit
	    = _import "gtk_font_button_set_show_style" : cptr * bool -> unit;
	val set_show_style : 'a t -> bool -> unit
	    = fn self => fn show_style =>
		 GObject.withPtr
		   (self, fn self => set_show_style_ (self, show_style))
	val get_show_size_ : cptr -> bool
	    = _import "gtk_font_button_get_show_size" : cptr -> bool;
	val get_show_size : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_show_size_ self)
	val set_show_size_ : cptr * bool -> unit
	    = _import "gtk_font_button_set_show_size" : cptr * bool -> unit;
	val set_show_size : 'a t -> bool -> unit
	    = fn self => fn show_size =>
		 GObject.withPtr
		   (self, fn self => set_show_size_ (self, show_size))
	local open Signal
	      infixr -->
	in val font_set_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "font-set" false (void --> return_void) f
	end
    end
    structure FontSelection :>
      sig
	type base
	type 'a fontselection_t
	type 'a t = 'a fontselection_t VBox.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toFontSelection : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val get_font_name : 'a t -> string
	val set_font_name : 'a t -> string -> bool
	val get_preview_text : 'a t -> string
	val set_preview_text : 'a t -> string -> unit
	val dialog_get_type : unit -> GType.t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a fontselection_t = unit
	type 'a t = 'a fontselection_t VBox.t
	fun inherit w con = VBox.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toFontSelection obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_font_selection_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr
	    = _import "gtk_font_selection_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_font_name_ : cptr -> CString.t
	    = _import "gtk_font_selection_get_font_name" : cptr -> CString.t;
	val get_font_name : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_font_name_ self
				       in CString.toString t end)
	val set_font_name_ : cptr * CString.cstring -> bool
	    = _import "gtk_font_selection_set_font_name"
		      : cptr * CString.cstring -> bool;
	val set_font_name : 'a t -> string -> bool
	    = fn self => fn fontname =>
		 GObject.withPtr
		   (self, 
		    fn self => set_font_name_
				 (self, CString.fromString fontname))
	val get_preview_text_ : cptr -> CString.t
	    = _import "gtk_font_selection_get_preview_text"
		      : cptr -> CString.t;
	val get_preview_text : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_preview_text_ self
				       in CString.toString t end)
	val set_preview_text_ : cptr * CString.cstring -> unit
	    = _import "gtk_font_selection_set_preview_text"
		      : cptr * CString.cstring -> unit;
	val set_preview_text : 'a t -> string -> unit
	    = fn self => fn text =>
		 GObject.withPtr (self, 
				  fn self => set_preview_text_
					       (self, CString.fromString text))
	val dialog_get_type_ : unit -> GType.t
	    = _import "gtk_font_selection_dialog_get_type" : unit -> GType.t;
	val dialog_get_type : unit -> GType.t
	    = fn dummy => dialog_get_type_ dummy
    end
    structure FontSelectionDialog :>
      sig
	type base
	type 'a fontselectiondialog_t
	type 'a t = 'a fontselectiondialog_t Dialog.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toFontSelectionDialog : 'a t -> base t
	val new : string -> base t
	val get_font_name : 'a t -> string
	val set_font_name : 'a t -> string -> bool
	val get_preview_text : 'a t -> string
	val set_preview_text : 'a t -> string -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a fontselectiondialog_t = unit
	type 'a t = 'a fontselectiondialog_t Dialog.t
	fun inherit w con = Dialog.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toFontSelectionDialog obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : CString.cstring -> cptr
	    = _import "gtk_font_selection_dialog_new"
		      : CString.cstring -> cptr;
	val new : string -> base t
	    = fn title => make (new_ (CString.fromString title))
	val get_font_name_ : cptr -> CString.t
	    = _import "gtk_font_selection_dialog_get_font_name"
		      : cptr -> CString.t;
	val get_font_name : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_font_name_ self
				       in CString.toString t end)
	val set_font_name_ : cptr * CString.cstring -> bool
	    = _import "gtk_font_selection_dialog_set_font_name"
		      : cptr * CString.cstring -> bool;
	val set_font_name : 'a t -> string -> bool
	    = fn self => fn fontname =>
		 GObject.withPtr
		   (self, 
		    fn self => set_font_name_
				 (self, CString.fromString fontname))
	val get_preview_text_ : cptr -> CString.t
	    = _import "gtk_font_selection_dialog_get_preview_text"
		      : cptr -> CString.t;
	val get_preview_text : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_preview_text_ self
				       in CString.toString t end)
	val set_preview_text_ : cptr * CString.cstring -> unit
	    = _import "gtk_font_selection_dialog_set_preview_text"
		      : cptr * CString.cstring -> unit;
	val set_preview_text : 'a t -> string -> unit
	    = fn self => fn text =>
		 GObject.withPtr (self, 
				  fn self => set_preview_text_
					       (self, CString.fromString text))
    end
    structure GammaCurve :>
      sig
	type base
	type 'a gammacurve_t
	type 'a t = 'a gammacurve_t VBox.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toGammaCurve : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a gammacurve_t = unit
	type 'a t = 'a gammacurve_t VBox.t
	fun inherit w con = VBox.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toGammaCurve obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_gamma_curve_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_gamma_curve_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure HandleBox :>
      sig
	type base
	type 'a handlebox_t
	type 'a t = 'a handlebox_t Bin.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toHandleBox : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val set_shadowtype : 'a t -> shadowtype -> unit
	val get_shadowtype : 'a t -> shadowtype
	val set_handle_position : 'a t -> positiontype -> unit
	val get_handle_position : 'a t -> positiontype
	val set_snap_edge : 'a t -> positiontype -> unit
	val get_snap_edge : 'a t -> positiontype
	val child_attached_sig : (unit -> unit) -> 'a t Signal.signal
	val child_detached_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a handlebox_t = unit
	type 'a t = 'a handlebox_t Bin.t
	fun inherit w con = Bin.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toHandleBox obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_handle_box_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_handle_box_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val set_shadowtype_ : cptr * int -> unit
	    = _import "gtk_handle_box_set_shadow_type" : cptr * int -> unit;
	val set_shadowtype : 'a t -> shadowtype -> unit
	    = fn self => fn typ =>
		 GObject.withPtr (self, fn self => set_shadowtype_ (self, typ))
	val get_shadowtype_ : cptr -> int
	    = _import "gtk_handle_box_get_shadow_type" : cptr -> int;
	val get_shadowtype : 'a t -> shadowtype
	    = fn self => GObject.withPtr
			   (self, fn self => get_shadowtype_ self)
	val set_handle_position_ : cptr * int -> unit
	    = _import "gtk_handle_box_set_handle_position"
		      : cptr * int -> unit;
	val set_handle_position : 'a t -> positiontype -> unit
	    = fn self => fn position =>
		 GObject.withPtr
		   (self, fn self => set_handle_position_ (self, position))
	val get_handle_position_ : cptr -> int
	    = _import "gtk_handle_box_get_handle_position" : cptr -> int;
	val get_handle_position : 'a t -> positiontype
	    = fn self => GObject.withPtr
			   (self, fn self => get_handle_position_ self)
	val set_snap_edge_ : cptr * int -> unit
	    = _import "gtk_handle_box_set_snap_edge" : cptr * int -> unit;
	val set_snap_edge : 'a t -> positiontype -> unit
	    = fn self => fn edge =>
		 GObject.withPtr (self, fn self => set_snap_edge_ (self, edge))
	val get_snap_edge_ : cptr -> int
	    = _import "gtk_handle_box_get_snap_edge" : cptr -> int;
	val get_snap_edge : 'a t -> positiontype
	    = fn self => GObject.withPtr (self, fn self => get_snap_edge_ self)
	local open Signal
	      infixr -->
	in val child_attached_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "child-attached" false (unit --> return_void) f
	   val child_detached_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "child-detached" false (unit --> return_void) f
	end
    end
    structure HButtonBox :>
      sig
	type base
	type 'a hbuttonbox_t
	type 'a t = 'a hbuttonbox_t ButtonBox.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toHButtonBox : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a hbuttonbox_t = unit
	type 'a t = 'a hbuttonbox_t ButtonBox.t
	fun inherit w con = ButtonBox.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toHButtonBox obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_hbutton_box_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_hbutton_box_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure Paned :>
      sig
	type base
	type 'a paned_t
	type 'a t = 'a paned_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toPaned : 'a t -> base t
	val get_type : unit -> GType.t
	val add1 : 'a t -> 'b Widget.t -> unit
	val add2 : 'a t -> 'b Widget.t -> unit
	val pack1 : 'a t -> 'b Widget.t -> bool -> bool -> unit
	val pack1' : 'a t -> 'b Widget.t -> unit
	val pack2 : 'a t -> 'b Widget.t -> bool -> bool -> unit
	val pack2' : 'a t -> 'b Widget.t -> unit
	val get_position : 'a t -> int
	val set_position : 'a t -> int -> unit
	val get_child1 : 'a t -> base Widget.t
	val get_child2 : 'a t -> base Widget.t
	val compute_position : 'a t -> int -> int -> int -> unit
	val cycle_child_focus_sig : (bool -> bool) -> 'a t Signal.signal
	val toggle_handle_focus_sig : (unit -> bool) -> 'a t Signal.signal
	val move_handle_sig : (unit -> bool) -> 'a t Signal.signal
	val cycle_handle_focus_sig : (bool -> bool) -> 'a t Signal.signal
	val accept_position_sig : (unit -> bool) -> 'a t Signal.signal
	val cancel_position_sig : (unit -> bool) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a paned_t = unit
	type 'a t = 'a paned_t Container.t
	fun inherit w con = Container.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toPaned obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_paned_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val add1_ : cptr * cptr -> unit
	    = _import "gtk_paned_add1" : cptr * cptr -> unit;
	val add1 : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, fn child => add1_ (self, child)))
	val add2_ : cptr * cptr -> unit
	    = _import "gtk_paned_add2" : cptr * cptr -> unit;
	val add2 : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, fn child => add2_ (self, child)))
	val pack1_ : cptr * cptr * bool * bool -> unit
	    = _import "gtk_paned_pack1" : cptr * cptr * bool * bool -> unit;
	val pack1 : 'a t -> 'b Widget.t -> bool -> bool -> unit
	    = fn self => fn child => fn resize => fn shrink =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child =>
				     pack1_ (self, child, resize, shrink)))
	val pack1' : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => pack1_ (self, child, 
						      false, true)))
	val pack2_ : cptr * cptr * bool * bool -> unit
	    = _import "gtk_paned_pack2" : cptr * cptr * bool * bool -> unit;
	val pack2 : 'a t -> 'b Widget.t -> bool -> bool -> unit
	    = fn self => fn child => fn resize => fn shrink =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child =>
				     pack2_ (self, child, resize, shrink)))
	val pack2' : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (child, fn child => pack2_ (self, child, true, true)))
	val get_position_ : cptr -> int
	    = _import "gtk_paned_get_position" : cptr -> int;
	val get_position : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_position_ self)
	val set_position_ : cptr * int -> unit
	    = _import "gtk_paned_set_position" : cptr * int -> unit;
	val set_position : 'a t -> int -> unit
	    = fn self => fn position =>
		 GObject.withPtr
		   (self, fn self => set_position_ (self, position))
	val get_child1_ : cptr -> cptr
	    = _import "gtk_paned_get_child1" : cptr -> cptr;
	val get_child1 : 'a t -> base Widget.t
	    = fn self => Widget.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_child1_ self))
	val get_child2_ : cptr -> cptr
	    = _import "gtk_paned_get_child2" : cptr -> cptr;
	val get_child2 : 'a t -> base Widget.t
	    = fn self => Widget.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_child2_ self))
	val compute_position_ : cptr * int * int * int -> unit
	    = _import "gtk_paned_compute_position"
		      : cptr * int * int * int -> unit;
	val compute_position : 'a t -> int -> int -> int -> unit
	    = fn self => fn allocation => fn child1_req => fn child2_req =>
		 GObject.withPtr
		   (self, 
		    fn self => compute_position_
				 (self, allocation, child1_req, child2_req))
	local open Signal
	      infixr -->
	in
	  val cycle_child_focus_sig : (bool -> bool) -> 'a t Signal.signal
	      = fn f =>
		   signal "cycle-child-focus" false (bool --> return_bool) f
	  val toggle_handle_focus_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "toggle-handle-focus" false
			       (void --> return_bool) f
	  val move_handle_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "move-handle" false (unit --> return_bool) f
	  val cycle_handle_focus_sig : (bool -> bool) -> 'a t Signal.signal
	      = fn f => signal "cycle-handle-focus" false
			       (bool --> return_bool) f
	  val accept_position_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "accept-position" false (void --> return_bool) f
	  val cancel_position_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "cancel-position" false (void --> return_bool) f
	end
    end
    structure HPaned :>
      sig
	type base
	type 'a hpaned_t
	type 'a t = 'a hpaned_t Paned.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toHPaned : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a hpaned_t = unit
	type 'a t = 'a hpaned_t Paned.t
	fun inherit w con = Paned.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toHPaned obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_hpaned_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_hpaned_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure Ruler :>
      sig
	type base
	type 'a ruler_t
	type 'a t = 'a ruler_t Widget.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toRuler : 'a t -> base t
	val get_type : unit -> GType.t
	val set_metric : 'a t -> metrictype -> unit
	val set_range : 'a t -> real -> real -> real -> real -> unit
	val draw_ticks : 'a t -> unit
	val draw_pos : 'a t -> unit
	val get_metric : 'a t -> metrictype
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a ruler_t = unit
	type 'a t = 'a ruler_t Widget.t
	fun inherit w con = Widget.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toRuler obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_ruler_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_metric_ : cptr * int -> unit
	    = _import "gtk_ruler_set_metric" : cptr * int -> unit;
	val set_metric : 'a t -> metrictype -> unit
	    = fn self => fn metric =>
		 GObject.withPtr (self, fn self => set_metric_ (self, metric))
	val set_range_ : cptr * real * real * real * real -> unit
	    = _import "gtk_ruler_set_range"
		      : cptr * real * real * real * real -> unit;
	val set_range : 'a t -> real -> real -> real -> real -> unit
	    = fn self => fn lower => fn upper => fn position => fn max_size =>
		 GObject.withPtr (self, 
				  fn self => set_range_ (self, lower, upper, 
							 position, max_size))
	val draw_ticks_ : cptr -> unit
	    = _import "gtk_ruler_draw_ticks" : cptr -> unit;
	val draw_ticks : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => draw_ticks_ self)
	val draw_pos_ : cptr -> unit
	    = _import "gtk_ruler_draw_pos" : cptr -> unit;
	val draw_pos : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => draw_pos_ self)
	val get_metric_ : cptr -> int
	    = _import "gtk_ruler_get_metric" : cptr -> int;
	val get_metric : 'a t -> metrictype
	    = fn self => GObject.withPtr (self, fn self => get_metric_ self)
    end
    structure HRuler :>
      sig
	type base
	type 'a hruler_t
	type 'a t = 'a hruler_t Ruler.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toHRuler : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a hruler_t = unit
	type 'a t = 'a hruler_t Ruler.t
	fun inherit w con = Ruler.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toHRuler obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_hruler_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_hruler_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure Range :>
      sig
	type base
	type 'a range_t
	type 'a t = 'a range_t Widget.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toRange : 'a t -> base t
	val get_type : unit -> GType.t
	val set_update_policy : 'a t -> updatetype -> unit
	val get_update_policy : 'a t -> updatetype
	val set_adjustment : 'a t -> 'b Adjustment.t -> unit
	val get_adjustment : 'a t -> base Adjustment.t
	val set_inverted : 'a t -> bool -> unit
	val get_inverted : 'a t -> bool
	val set_increments : 'a t -> real -> real -> unit
	val set_range : 'a t -> real -> real -> unit
	val set_value : 'a t -> real -> unit
	val get_value : 'a t -> real
	val value_changed_sig : (unit -> unit) -> 'a t Signal.signal
	val adjust_bounds_sig : (real -> unit) -> 'a t Signal.signal
	val move_slider_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a range_t = unit
	type 'a t = 'a range_t Widget.t
	fun inherit w con = Widget.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toRange obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_range_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_update_policy_ : cptr * int -> unit
	    = _import "gtk_range_set_update_policy" : cptr * int -> unit;
	val set_update_policy : 'a t -> updatetype -> unit
	    = fn self => fn policy =>
		 GObject.withPtr
		   (self, fn self => set_update_policy_ (self, policy))
	val get_update_policy_ : cptr -> int
	    = _import "gtk_range_get_update_policy" : cptr -> int;
	val get_update_policy : 'a t -> updatetype
	    = fn self => GObject.withPtr
			   (self, fn self => get_update_policy_ self)
	val set_adjustment_ : cptr * cptr -> unit
	    = _import "gtk_range_set_adjustment" : cptr * cptr -> unit;
	val set_adjustment : 'a t -> 'b Adjustment.t -> unit
	    = fn self => fn adjustment =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (adjustment, 
				  fn adjustment =>
				     set_adjustment_ (self, adjustment)))
	val get_adjustment_ : cptr -> cptr
	    = _import "gtk_range_get_adjustment" : cptr -> cptr;
	val get_adjustment : 'a t -> base Adjustment.t
	    = fn self => Adjustment.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_adjustment_ self))
	val set_inverted_ : cptr * bool -> unit
	    = _import "gtk_range_set_inverted" : cptr * bool -> unit;
	val set_inverted : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_inverted_ (self, setting))
	val get_inverted_ : cptr -> bool
	    = _import "gtk_range_get_inverted" : cptr -> bool;
	val get_inverted : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_inverted_ self)
	val set_increments_ : cptr * real * real -> unit
	    = _import "gtk_range_set_increments" : cptr * real * real -> unit;
	val set_increments : 'a t -> real -> real -> unit
	    = fn self => fn step => fn page =>
		 GObject.withPtr
		   (self, fn self => set_increments_ (self, step, page))
	val set_range_ : cptr * real * real -> unit
	    = _import "gtk_range_set_range" : cptr * real * real -> unit;
	val set_range : 'a t -> real -> real -> unit
	    = fn self => fn min => fn max =>
		 GObject.withPtr (self, fn self => set_range_ (self, min, max))
	val set_value_ : cptr * real -> unit
	    = _import "gtk_range_set_value" : cptr * real -> unit;
	val set_value : 'a t -> real -> unit
	    = fn self => fn value =>
		 GObject.withPtr (self, fn self => set_value_ (self, value))
	val get_value_ : cptr -> real
	    = _import "gtk_range_get_value" : cptr -> real;
	val get_value : 'a t -> real
	    = fn self => GObject.withPtr (self, fn self => get_value_ self)
	local open Signal
	      infixr -->
	in val value_changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "value-changed" false (void --> return_void) f
	   val adjust_bounds_sig : (real -> unit) -> 'a t Signal.signal
	       = fn f => signal "adjust-bounds" false (real --> return_void) f
	   val move_slider_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "move-slider" false (unit --> return_void) f
	end
    end
    structure Scale :>
      sig
	type base
	type 'a scale_t
	type 'a t = 'a scale_t Range.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toScale : 'a t -> base t
	val get_type : unit -> GType.t
	val set_digits : 'a t -> int -> unit
	val get_digits : 'a t -> int
	val set_draw_value : 'a t -> bool -> unit
	val get_draw_value : 'a t -> bool
	val set_value_pos : 'a t -> positiontype -> unit
	val get_value_pos : 'a t -> positiontype
	val get_layout_offsets : 'a t -> int * int
	val format_value_sig : (real -> char) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a scale_t = unit
	type 'a t = 'a scale_t Range.t
	fun inherit w con = Range.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toScale obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_scale_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_digits_ : cptr * int -> unit
	    = _import "gtk_scale_set_digits" : cptr * int -> unit;
	val set_digits : 'a t -> int -> unit
	    = fn self => fn digits =>
		 GObject.withPtr (self, fn self => set_digits_ (self, digits))
	val get_digits_ : cptr -> int
	    = _import "gtk_scale_get_digits" : cptr -> int;
	val get_digits : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_digits_ self)
	val set_draw_value_ : cptr * bool -> unit
	    = _import "gtk_scale_set_draw_value" : cptr * bool -> unit;
	val set_draw_value : 'a t -> bool -> unit
	    = fn self => fn draw_value =>
		 GObject.withPtr
		   (self, fn self => set_draw_value_ (self, draw_value))
	val get_draw_value_ : cptr -> bool
	    = _import "gtk_scale_get_draw_value" : cptr -> bool;
	val get_draw_value : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_draw_value_ self)
	val set_value_pos_ : cptr * int -> unit
	    = _import "gtk_scale_set_value_pos" : cptr * int -> unit;
	val set_value_pos : 'a t -> positiontype -> unit
	    = fn self => fn pos =>
		 GObject.withPtr (self, fn self => set_value_pos_ (self, pos))
	val get_value_pos_ : cptr -> int
	    = _import "gtk_scale_get_value_pos" : cptr -> int;
	val get_value_pos : 'a t -> positiontype
	    = fn self => GObject.withPtr (self, fn self => get_value_pos_ self)
	val get_layout_offsets_ : cptr * int ref * int ref -> unit
	    = _import "gtk_scale_get_layout_offsets"
		      : cptr * int ref * int ref -> unit;
	val get_layout_offsets : 'a t -> int * int
	    = fn self => let val (x, y) = (ref 0, ref 0)
			     val ret = GObject.withPtr
					 (self, 
					  fn self => get_layout_offsets_
						       (self, x, y))
			 in (!x, !y) end
	local open Signal
	      infixr -->
	in val format_value_sig : (real -> char) -> 'a t Signal.signal
	       = fn f => signal "format-value" false (real --> return_char) f
	end
    end
    structure HScale :>
      sig
	type base
	type 'a hscale_t
	type 'a t = 'a hscale_t Scale.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toHScale : 'a t -> base t
	val get_type : unit -> GType.t
	val new : 'a Adjustment.t -> base t
	val new_with_range : real -> real -> real -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a hscale_t = unit
	type 'a t = 'a hscale_t Scale.t
	fun inherit w con = Scale.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toHScale obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_hscale_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr -> cptr = _import "gtk_hscale_new" : cptr -> cptr;
	val new : 'a Adjustment.t -> base t
	    = fn adjustment =>
		 make (GObject.withPtr
			 (adjustment, fn adjustment => new_ adjustment))
	val new_with_range_ : real * real * real -> cptr
	    = _import "gtk_hscale_new_with_range" : real * real * real -> cptr;
	val new_with_range : real -> real -> real -> base t
	    = fn min => fn max => fn step =>
		 make (new_with_range_ (min, max, step))
    end
    structure Scrollbar :>
      sig
	type base
	type 'a scrollbar_t
	type 'a t = 'a scrollbar_t Range.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toScrollbar : 'a t -> base t
	val get_type : unit -> GType.t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a scrollbar_t = unit
	type 'a t = 'a scrollbar_t Range.t
	fun inherit w con = Range.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toScrollbar obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_scrollbar_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
    end
    structure HScrollbar :>
      sig
	type base
	type 'a hscrollbar_t
	type 'a t = 'a hscrollbar_t Scrollbar.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toHScrollbar : 'a t -> base t
	val get_type : unit -> GType.t
	val new : 'a Adjustment.t -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a hscrollbar_t = unit
	type 'a t = 'a hscrollbar_t Scrollbar.t
	fun inherit w con = Scrollbar.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toHScrollbar obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_hscrollbar_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr -> cptr = _import "gtk_hscrollbar_new" : cptr -> cptr;
	val new : 'a Adjustment.t -> base t
	    = fn adjustment =>
		 make (GObject.withPtr
			 (adjustment, fn adjustment => new_ adjustment))
    end
    structure Separator :>
      sig
	type base
	type 'a separator_t
	type 'a t = 'a separator_t Widget.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toSeparator : 'a t -> base t
	val get_type : unit -> GType.t
	val menu_item_get_type : unit -> GType.t
	val toolitem_get_type : unit -> GType.t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a separator_t = unit
	type 'a t = 'a separator_t Widget.t
	fun inherit w con = Widget.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toSeparator obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_separator_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val menu_item_get_type_ : unit -> GType.t
	    = _import "gtk_separator_menu_item_get_type" : unit -> GType.t;
	val menu_item_get_type : unit -> GType.t
	    = fn dummy => menu_item_get_type_ dummy
	val toolitem_get_type_ : unit -> GType.t
	    = _import "gtk_separator_tool_item_get_type" : unit -> GType.t;
	val toolitem_get_type : unit -> GType.t
	    = fn dummy => toolitem_get_type_ dummy
    end
    structure HSeparator :>
      sig
	type base
	type 'a hseparator_t
	type 'a t = 'a hseparator_t Separator.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toHSeparator : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a hseparator_t = unit
	type 'a t = 'a hseparator_t Separator.t
	fun inherit w con = Separator.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toHSeparator obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_hseparator_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_hseparator_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure IconFactory :>
      sig
	type base
	type 'a iconfactory_t
	type 'a t = 'a iconfactory_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toIconFactory : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val add : 'a t -> string -> IconSet.t -> unit
	val lookup : 'a t -> string -> IconSet.t
	val add_default : 'a t -> unit
	val remove_default : 'a t -> unit
	val lookup_default : string -> IconSet.t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a iconfactory_t = unit
	type 'a t = 'a iconfactory_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toIconFactory obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_icon_factory_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr
	    = _import "gtk_icon_factory_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val add_ : cptr * CString.cstring * cptr -> unit
	    = _import "gtk_icon_factory_add"
		      : cptr * CString.cstring * cptr -> unit;
	val add : 'a t -> string -> IconSet.t -> unit
	    = fn self => fn stock_id => fn icon_set =>
		 GObject.withPtr
		   (self, 
		    fn self => add_ (self, CString.fromString stock_id, 
				     icon_set))
	val lookup_ : cptr * CString.cstring -> cptr
	    = _import "gtk_icon_factory_lookup"
		      : cptr * CString.cstring -> cptr;
	val lookup : 'a t -> string -> IconSet.t
	    = fn self => fn stock_id =>
		 GObject.withPtr
		   (self, 
		    fn self => lookup_ (self, CString.fromString stock_id))
	val add_default_ : cptr -> unit
	    = _import "gtk_icon_factory_add_default" : cptr -> unit;
	val add_default : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => add_default_ self)
	val remove_default_ : cptr -> unit
	    = _import "gtk_icon_factory_remove_default" : cptr -> unit;
	val remove_default : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => remove_default_ self)
	val lookup_default_ : CString.cstring -> cptr
	    = _import "gtk_icon_factory_lookup_default"
		      : CString.cstring -> cptr;
	val lookup_default : string -> IconSet.t
	    = fn stock_id => lookup_default_ (CString.fromString stock_id)
    end
    structure Image :>
      sig
	type base
	type 'a image_t
	type 'a t = 'a image_t Misc.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toImage : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val new_from_file : string -> base t
	val new_from_stock : string -> icon_size -> base t
	val new_from_iconset : IconSet.t -> icon_size -> base t
	val set_from_file : 'a t -> string option -> unit
	val set_from_file' : 'a t -> unit
	val set_from_stock : 'a t -> string -> icon_size -> unit
	val set_from_iconset : 'a t -> IconSet.t -> icon_size -> unit
	val get_storagetype : 'a t -> imagetype
	val menu_item_get_type : unit -> GType.t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a image_t = unit
	type 'a t = 'a image_t Misc.t
	fun inherit w con = Misc.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toImage obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_image_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_image_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_from_file_ : CString.cstring -> cptr
	    = _import "gtk_image_new_from_file" : CString.cstring -> cptr;
	val new_from_file : string -> base t
	    = fn filename => make (new_from_file_
				     (CString.fromString filename))
	val new_from_stock_ : CString.cstring * int -> cptr
	    = _import "gtk_image_new_from_stock"
		      : CString.cstring * int -> cptr;
	val new_from_stock : string -> icon_size -> base t
	    = fn stock_id => fn size =>
		 make (new_from_stock_ (CString.fromString stock_id, size))
	val new_from_iconset_ : cptr * int -> cptr
	    = _import "gtk_image_new_from_icon_set" : cptr * int -> cptr;
	val new_from_iconset : IconSet.t -> icon_size -> base t
	    = fn icon_set => fn size =>
		 make (new_from_iconset_ (icon_set, size))
	val set_from_file_ : cptr * CString.cstring -> unit
	    = _import "gtk_image_set_from_file"
		      : cptr * CString.cstring -> unit;
	val set_from_file : 'a t -> string option -> unit
	    = fn self => fn filename =>
		 GObject.withPtr
		   (self, 
		    fn self => set_from_file_ (self, 
					       CString.fromString
						 (getOpt (filename, ""))))
	val set_from_file' : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => set_from_file_
					 (self, CString.fromString ""))
	val set_from_stock_ : cptr * CString.cstring * int -> unit
	    = _import "gtk_image_set_from_stock"
		      : cptr * CString.cstring * int -> unit;
	val set_from_stock : 'a t -> string -> icon_size -> unit
	    = fn self => fn stock_id => fn size =>
		 GObject.withPtr
		   (self, 
		    fn self => set_from_stock_
				 (self, CString.fromString stock_id, size))
	val set_from_iconset_ : cptr * cptr * int -> unit
	    = _import "gtk_image_set_from_icon_set"
		      : cptr * cptr * int -> unit;
	val set_from_iconset : 'a t -> IconSet.t -> icon_size -> unit
	    = fn self => fn icon_set => fn size =>
		 GObject.withPtr
		   (self, fn self => set_from_iconset_ (self, icon_set, size))
	val get_storagetype_ : cptr -> int
	    = _import "gtk_image_get_storage_type" : cptr -> int;
	val get_storagetype : 'a t -> imagetype
	    = fn self => GObject.withPtr
			   (self, fn self => get_storagetype_ self)
	val menu_item_get_type_ : unit -> GType.t
	    = _import "gtk_image_menu_item_get_type" : unit -> GType.t;
	val menu_item_get_type : unit -> GType.t
	    = fn dummy => menu_item_get_type_ dummy
    end
    structure ImageMenuItem :>
      sig
	type base
	type 'a imagemenuitem_t
	type 'a t = 'a imagemenuitem_t MenuItem.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toImageMenuItem : 'a t -> base t
	val new : unit -> base t
	val new_with_label : string -> base t
	val new_with_mnemonic : string -> base t
	val new_from_stock : string -> 'a AccelGroup.t -> base t
	val set_image : 'a t -> 'b Widget.t option -> unit
	val set_image' : 'a t -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a imagemenuitem_t = unit
	type 'a t = 'a imagemenuitem_t MenuItem.t
	fun inherit w con = MenuItem.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toImageMenuItem obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : unit -> cptr
	    = _import "gtk_image_menu_item_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_label_ : CString.cstring -> cptr
	    = _import "gtk_image_menu_item_new_with_label"
		      : CString.cstring -> cptr;
	val new_with_label : string -> base t
	    = fn label => make (new_with_label_ (CString.fromString label))
	val new_with_mnemonic_ : CString.cstring -> cptr
	    = _import "gtk_image_menu_item_new_with_mnemonic"
		      : CString.cstring -> cptr;
	val new_with_mnemonic : string -> base t
	    = fn label => make (new_with_mnemonic_ (CString.fromString label))
	val new_from_stock_ : CString.cstring * cptr -> cptr
	    = _import "gtk_image_menu_item_new_from_stock"
		      : CString.cstring * cptr -> cptr;
	val new_from_stock : string -> 'a AccelGroup.t -> base t
	    = fn stock_id => fn accel_group =>
		 make (GObject.withPtr
			 (accel_group, 
			  fn accel_group =>
			     new_from_stock_
			       (CString.fromString stock_id, accel_group)))
	val set_image_ : cptr * cptr -> unit
	    = _import "gtk_image_menu_item_set_image" : cptr * cptr -> unit;
	val set_image : 'a t -> 'b Widget.t option -> unit
	    = fn self => fn image =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withOpt
				 (image, fn image => set_image_ (self, image)))
	val set_image' : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => set_image_ (self, GObject.null))
    end
    structure IMContext :>
      sig
	type base
	type 'a imcontext_t
	type 'a t = 'a imcontext_t Object.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toIMContext : 'a t -> base t
	val get_type : unit -> GType.t
	val focus_in : 'a t -> unit
	val focus_out : 'a t -> unit
	val reset : 'a t -> unit
	val set_use_preedit : 'a t -> bool -> unit
	val set_surrounding : 'a t -> string -> int -> int -> unit
	val delete_surrounding : 'a t -> int -> int -> bool
	val simple_get_type : unit -> GType.t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a imcontext_t = unit
	type 'a t = 'a imcontext_t Object.t
	fun inherit w con = Object.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toIMContext obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_im_context_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val focus_in_ : cptr -> unit
	    = _import "gtk_im_context_focus_in" : cptr -> unit;
	val focus_in : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => focus_in_ self)
	val focus_out_ : cptr -> unit
	    = _import "gtk_im_context_focus_out" : cptr -> unit;
	val focus_out : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => focus_out_ self)
	val reset_ : cptr -> unit
	    = _import "gtk_im_context_reset" : cptr -> unit;
	val reset : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => reset_ self)
	val set_use_preedit_ : cptr * bool -> unit
	    = _import "gtk_im_context_set_use_preedit" : cptr * bool -> unit;
	val set_use_preedit : 'a t -> bool -> unit
	    = fn self => fn use_preedit =>
		 GObject.withPtr
		   (self, fn self => set_use_preedit_ (self, use_preedit))
	val set_surrounding_ : cptr * CString.cstring * int * int -> unit
	    = _import "gtk_im_context_set_surrounding"
		      : cptr * CString.cstring * int * int -> unit;
	val set_surrounding : 'a t -> string -> int -> int -> unit
	    = fn self => fn text => fn len => fn cursor_index =>
		 GObject.withPtr (self, 
				  fn self => set_surrounding_
					       (self, CString.fromString text, 
						len, cursor_index))
	val delete_surrounding_ : cptr * int * int -> bool
	    = _import "gtk_im_context_delete_surrounding"
		      : cptr * int * int -> bool;
	val delete_surrounding : 'a t -> int -> int -> bool
	    = fn self => fn offset => fn n_chars =>
		 GObject.withPtr (self, 
				  fn self => delete_surrounding_
					       (self, offset, n_chars))
	val simple_get_type_ : unit -> GType.t
	    = _import "gtk_im_context_simple_get_type" : unit -> GType.t;
	val simple_get_type : unit -> GType.t
	    = fn dummy => simple_get_type_ dummy
    end
    structure IMContextSimple :>
      sig
	type base
	type 'a imcontextsimple_t
	type 'a t = 'a imcontextsimple_t IMContext.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toIMContextSimple : 'a t -> base t
	val new : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a imcontextsimple_t = unit
	type 'a t = 'a imcontextsimple_t IMContext.t
	fun inherit w con = IMContext.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toIMContextSimple obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : unit -> cptr
	    = _import "gtk_im_context_simple_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure MenuShell :>
      sig
	type base
	type 'a menushell_t
	type 'a t = 'a menushell_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toMenuShell : 'a t -> base t
	val get_type : unit -> GType.t
	val append : 'a t -> 'b Widget.t -> unit
	val prepend : 'a t -> 'b Widget.t -> unit
	val insert : 'a t -> 'b Widget.t -> int -> unit
	val deactivate : 'a t -> unit
	val select_item : 'a t -> 'b Widget.t -> unit
	val deselect : 'a t -> unit
	val activate_item : 'a t -> 'b Widget.t -> bool -> unit
	val select_first : 'a t -> bool -> unit
	val cancel : 'a t -> unit
	val deactivate_sig : (unit -> unit) -> 'a t Signal.signal
	val selection_done_sig : (unit -> unit) -> 'a t Signal.signal
	val move_current_sig : (unit -> unit) -> 'a t Signal.signal
	val activate_current_sig : (bool -> unit) -> 'a t Signal.signal
	val cancel_sig : (unit -> unit) -> 'a t Signal.signal
	val cycle_focus_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a menushell_t = unit
	type 'a t = 'a menushell_t Container.t
	fun inherit w con = Container.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toMenuShell obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_menu_shell_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val append_ : cptr * cptr -> unit
	    = _import "gtk_menu_shell_append" : cptr * cptr -> unit;
	val append : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, fn child => append_ (self, child)))
	val prepend_ : cptr * cptr -> unit
	    = _import "gtk_menu_shell_prepend" : cptr * cptr -> unit;
	val prepend : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, fn child => prepend_ (self, child)))
	val insert_ : cptr * cptr * int -> unit
	    = _import "gtk_menu_shell_insert" : cptr * cptr * int -> unit;
	val insert : 'a t -> 'b Widget.t -> int -> unit
	    = fn self => fn child => fn position =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (child, fn child => insert_ (self, child, position)))
	val deactivate_ : cptr -> unit
	    = _import "gtk_menu_shell_deactivate" : cptr -> unit;
	val deactivate : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => deactivate_ self)
	val select_item_ : cptr * cptr -> unit
	    = _import "gtk_menu_shell_select_item" : cptr * cptr -> unit;
	val select_item : 'a t -> 'b Widget.t -> unit
	    = fn self => fn menu_item =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (menu_item, 
				  fn menu_item =>
				     select_item_ (self, menu_item)))
	val deselect_ : cptr -> unit
	    = _import "gtk_menu_shell_deselect" : cptr -> unit;
	val deselect : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => deselect_ self)
	val activate_item_ : cptr * cptr * bool -> unit
	    = _import "gtk_menu_shell_activate_item"
		      : cptr * cptr * bool -> unit;
	val activate_item : 'a t -> 'b Widget.t -> bool -> unit
	    = fn self => fn menu_item => fn force_deactivate =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (menu_item, 
				  fn menu_item =>
				     activate_item_ (self, menu_item, 
						     force_deactivate)))
	val select_first_ : cptr * bool -> unit
	    = _import "gtk_menu_shell_select_first" : cptr * bool -> unit;
	val select_first : 'a t -> bool -> unit
	    = fn self => fn search_sensitive =>
		 GObject.withPtr
		   (self, fn self => select_first_ (self, search_sensitive))
	val cancel_ : cptr -> unit
	    = _import "gtk_menu_shell_cancel" : cptr -> unit;
	val cancel : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => cancel_ self)
	local open Signal
	      infixr -->
	in val deactivate_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "deactivate" false (void --> return_void) f
	   val selection_done_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "selection-done" false (void --> return_void) f
	   val move_current_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "move-current" false (unit --> return_void) f
	   val activate_current_sig : (bool -> unit) -> 'a t Signal.signal
	       = fn f =>
		    signal "activate-current" false (bool --> return_void) f
	   val cancel_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "cancel" false (void --> return_void) f
	   val cycle_focus_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "cycle-focus" false (unit --> return_void) f
	end
    end
    structure IMMulticontext :>
      sig
	type base
	type 'a immulticontext_t
	type 'a t = 'a immulticontext_t IMContext.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toIMMulticontext : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val append_menuitems : 'a t -> 'b MenuShell.t -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a immulticontext_t = unit
	type 'a t = 'a immulticontext_t IMContext.t
	fun inherit w con = IMContext.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toIMMulticontext obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_im_multicontext_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr
	    = _import "gtk_im_multicontext_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val append_menuitems_ : cptr * cptr -> unit
	    = _import "gtk_im_multicontext_append_menuitems"
		      : cptr * cptr -> unit;
	val append_menuitems : 'a t -> 'b MenuShell.t -> unit
	    = fn self => fn menushell =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (menushell, 
				  fn menushell => append_menuitems_
						    (self, menushell)))
    end
    structure InputDialog :>
      sig
	type base
	type 'a inputdialog_t
	type 'a t = 'a inputdialog_t Dialog.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toInputDialog : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val enable_device_sig : (unit -> unit) -> 'a t Signal.signal
	val disable_device_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a inputdialog_t = unit
	type 'a t = 'a inputdialog_t Dialog.t
	fun inherit w con = Dialog.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toInputDialog obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_input_dialog_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr
	    = _import "gtk_input_dialog_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	local open Signal
	      infixr -->
	in val enable_device_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "enable-device" false (unit --> return_void) f
	   val disable_device_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "disable-device" false (unit --> return_void) f
	end
    end
    structure Invisible :>
      sig
	type base
	type 'a invisible_t
	type 'a t = 'a invisible_t Widget.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toInvisible : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a invisible_t = unit
	type 'a t = 'a invisible_t Widget.t
	fun inherit w con = Widget.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toInvisible obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_invisible_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_invisible_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure ItemFactory :>
      sig
	type base
	type 'a itemfactory_t
	type 'a t = 'a itemfactory_t Object.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toItemFactory : 'a t -> base t
	val get_item : 'a t -> string -> base Widget.t
	val get_widget : 'a t -> string -> base Widget.t
	val get_widget_by_action : 'a t -> int -> base Widget.t
	val get_item_by_action : 'a t -> int -> base Widget.t
	val delete_item : 'a t -> string -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a itemfactory_t = unit
	type 'a t = 'a itemfactory_t Object.t
	fun inherit w con = Object.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toItemFactory obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_item_ : cptr * CString.cstring -> cptr
	    = _import "gtk_item_factory_get_item"
		      : cptr * CString.cstring -> cptr;
	val get_item : 'a t -> string -> base Widget.t
	    = fn self => fn path =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self =>
				   get_item_ (self, CString.fromString path)))
	val get_widget_ : cptr * CString.cstring -> cptr
	    = _import "gtk_item_factory_get_widget"
		      : cptr * CString.cstring -> cptr;
	val get_widget : 'a t -> string -> base Widget.t
	    = fn self => fn path =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self => get_widget_
					     (self, CString.fromString path)))
	val get_widget_by_action_ : cptr * int -> cptr
	    = _import "gtk_item_factory_get_widget_by_action"
		      : cptr * int -> cptr;
	val get_widget_by_action : 'a t -> int -> base Widget.t
	    = fn self => fn action =>
		 Widget.inherit ()
			        (fn () => GObject.withPtr
					    (self, 
					     fn self => get_widget_by_action_
							  (self, action)))
	val get_item_by_action_ : cptr * int -> cptr
	    = _import "gtk_item_factory_get_item_by_action"
		      : cptr * int -> cptr;
	val get_item_by_action : 'a t -> int -> base Widget.t
	    = fn self => fn action =>
		 Widget.inherit ()
			        (fn () => GObject.withPtr
					    (self, 
					     fn self => get_item_by_action_
							  (self, action)))
	val delete_item_ : cptr * CString.cstring -> unit
	    = _import "gtk_item_factory_delete_item"
		      : cptr * CString.cstring -> unit;
	val delete_item : 'a t -> string -> unit
	    = fn self => fn path =>
		 GObject.withPtr
		   (self, 
		    fn self => delete_item_ (self, CString.fromString path))
    end
    structure Layout :>
      sig
	type base
	type 'a layout_t
	type 'a t = 'a layout_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toLayout : 'a t -> base t
	val get_type : unit -> GType.t
	val new : 'a Adjustment.t -> 'b Adjustment.t -> base t
	val put : 'a t -> 'b Widget.t -> int -> int -> unit
	val move : 'a t -> 'b Widget.t -> int -> int -> unit
	val set_size : 'a t -> int -> int -> unit
	val set_hadjustment : 'a t -> 'b Adjustment.t option -> unit
	val set_hadjustment' : 'a t -> unit
	val set_vadjustment : 'a t -> 'b Adjustment.t option -> unit
	val set_vadjustment' : 'a t -> unit
	val set_scroll_adjustments_sig
	  : (unit -> unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a layout_t = unit
	type 'a t = 'a layout_t Container.t
	fun inherit w con = Container.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toLayout obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_layout_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr * cptr -> cptr
	    = _import "gtk_layout_new" : cptr * cptr -> cptr;
	val new : 'a Adjustment.t -> 'b Adjustment.t -> base t
	    = fn hadjustment => fn vadjustment =>
		 make (GObject.withPtr
			 (hadjustment, 
			  fn hadjustment =>
			     GObject.withPtr
			       (vadjustment, 
				fn vadjustment =>
				   new_ (hadjustment, vadjustment))))
	val put_ : cptr * cptr * int * int -> unit
	    = _import "gtk_layout_put" : cptr * cptr * int * int -> unit;
	val put : 'a t -> 'b Widget.t -> int -> int -> unit
	    = fn self => fn child_widget => fn x => fn y =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child_widget, 
				  fn child_widget =>
				     put_ (self, child_widget, x, y)))
	val move_ : cptr * cptr * int * int -> unit
	    = _import "gtk_layout_move" : cptr * cptr * int * int -> unit;
	val move : 'a t -> 'b Widget.t -> int -> int -> unit
	    = fn self => fn child_widget => fn x => fn y =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child_widget, 
				  fn child_widget =>
				     move_ (self, child_widget, x, y)))
	val set_size_ : cptr * int * int -> unit
	    = _import "gtk_layout_set_size" : cptr * int * int -> unit;
	val set_size : 'a t -> int -> int -> unit
	    = fn self => fn width => fn height =>
		 GObject.withPtr
		   (self, fn self => set_size_ (self, width, height))
	val set_hadjustment_ : cptr * cptr -> unit
	    = _import "gtk_layout_set_hadjustment" : cptr * cptr -> unit;
	val set_hadjustment : 'a t -> 'b Adjustment.t option -> unit
	    = fn self => fn adjustment =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withOpt
				 (adjustment, 
				  fn adjustment => set_hadjustment_
						     (self, adjustment)))
	val set_hadjustment' : 'a t -> unit
	    = fn self =>
		 GObject.withPtr
		   (self, fn self => set_hadjustment_ (self, GObject.null))
	val set_vadjustment_ : cptr * cptr -> unit
	    = _import "gtk_layout_set_vadjustment" : cptr * cptr -> unit;
	val set_vadjustment : 'a t -> 'b Adjustment.t option -> unit
	    = fn self => fn adjustment =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withOpt
				 (adjustment, 
				  fn adjustment => set_vadjustment_
						     (self, adjustment)))
	val set_vadjustment' : 'a t -> unit
	    = fn self =>
		 GObject.withPtr
		   (self, fn self => set_vadjustment_ (self, GObject.null))
	local open Signal
	      infixr -->
	in val set_scroll_adjustments_sig
	     : (unit -> unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "set-scroll-adjustments" false
			        (unit --> unit --> return_void) f
	end
    end
    structure List :>
      sig
	type base
	type 'a list_t
	type 'a t = 'a list_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toList : 'a t -> base t
	val clear_items : 'a t -> int -> int -> unit
	val select_item : 'a t -> int -> unit
	val unselect_item : 'a t -> int -> unit
	val select_child : 'a t -> 'b Widget.t -> unit
	val unselect_child : 'a t -> 'b Widget.t -> unit
	val child_position : 'a t -> 'b Widget.t -> int
	val item_get_type : unit -> GType.t
	val store_get_type : unit -> GType.t
	val selection_changed_sig : (unit -> unit) -> 'a t Signal.signal
	val select_child_sig : (unit -> unit) -> 'a t Signal.signal
	val unselect_child_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a list_t = unit
	type 'a t = 'a list_t Container.t
	fun inherit w con = Container.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toList obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val clear_items_ : cptr * int * int -> unit
	    = _import "gtk_list_clear_items" : cptr * int * int -> unit;
	val clear_items : 'a t -> int -> int -> unit
	    = fn self => fn start => fn en =>
		 GObject.withPtr
		   (self, fn self => clear_items_ (self, start, en))
	val select_item_ : cptr * int -> unit
	    = _import "gtk_list_select_item" : cptr * int -> unit;
	val select_item : 'a t -> int -> unit
	    = fn self => fn item =>
		 GObject.withPtr (self, fn self => select_item_ (self, item))
	val unselect_item_ : cptr * int -> unit
	    = _import "gtk_list_unselect_item" : cptr * int -> unit;
	val unselect_item : 'a t -> int -> unit
	    = fn self => fn item =>
		 GObject.withPtr (self, fn self => unselect_item_ (self, item))
	val select_child_ : cptr * cptr -> unit
	    = _import "gtk_list_select_child" : cptr * cptr -> unit;
	val select_child : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (child, fn child => select_child_ (self, child)))
	val unselect_child_ : cptr * cptr -> unit
	    = _import "gtk_list_unselect_child" : cptr * cptr -> unit;
	val unselect_child : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (child, fn child => unselect_child_ (self, child)))
	val child_position_ : cptr * cptr -> int
	    = _import "gtk_list_child_position" : cptr * cptr -> int;
	val child_position : 'a t -> 'b Widget.t -> int
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (child, fn child => child_position_ (self, child)))
	val item_get_type_ : unit -> GType.t
	    = _import "gtk_list_item_get_type" : unit -> GType.t;
	val item_get_type : unit -> GType.t = fn dummy => item_get_type_ dummy
	val store_get_type_ : unit -> GType.t
	    = _import "gtk_list_store_get_type" : unit -> GType.t;
	val store_get_type : unit -> GType.t
	    = fn dummy => store_get_type_ dummy
	local open Signal
	      infixr -->
	in val selection_changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f =>
		    signal "selection-changed" false (void --> return_void) f
	   val select_child_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "select-child" false (unit --> return_void) f
	   val unselect_child_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "unselect-child" false (unit --> return_void) f
	end
    end
    structure ListItem :>
      sig
	type base
	type 'a listitem_t
	type 'a t = 'a listitem_t Item.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toListItem : 'a t -> base t
	val select : 'a t -> unit
	val deselect : 'a t -> unit
	val select_all_sig : (unit -> unit) -> 'a t Signal.signal
	val unselect_all_sig : (unit -> unit) -> 'a t Signal.signal
	val toggle_focus_row_sig : (unit -> unit) -> 'a t Signal.signal
	val undo_selection_sig : (unit -> unit) -> 'a t Signal.signal
	val start_selection_sig : (unit -> unit) -> 'a t Signal.signal
	val end_selection_sig : (unit -> unit) -> 'a t Signal.signal
	val toggle_add_mode_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a listitem_t = unit
	type 'a t = 'a listitem_t Item.t
	fun inherit w con = Item.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toListItem obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val select_ : cptr -> unit
	    = _import "gtk_list_item_select" : cptr -> unit;
	val select : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => select_ self)
	val deselect_ : cptr -> unit
	    = _import "gtk_list_item_deselect" : cptr -> unit;
	val deselect : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => deselect_ self)
	local open Signal
	      infixr -->
	in
	  val select_all_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "select-all" false (void --> return_void) f
	  val unselect_all_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "unselect-all" false (void --> return_void) f
	  val toggle_focus_row_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f =>
		   signal "toggle-focus-row" false (void --> return_void) f
	  val undo_selection_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "undo-selection" false (void --> return_void) f
	  val start_selection_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "start-selection" false (void --> return_void) f
	  val end_selection_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "end-selection" false (void --> return_void) f
	  val toggle_add_mode_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "toggle-add-mode" false (void --> return_void) f
	end
    end
    structure ListStore :>
      sig
	type base
	type 'a liststore_t
	type 'a t = 'a liststore_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toListStore : 'a t -> base t
	val asTreeModel : 'a t -> base TreeModel.t
	val asTreeDragSource : 'a t -> base TreeDragSource.t
	val asTreeDragDest : 'a t -> base TreeDragDest.t
	val asTreeSortable : 'a t -> base TreeSortable.t
	val new : int -> base t
	val newv : int -> GType.t list -> base t
	val set_column_types : 'a t -> int -> GType.t list -> unit
	val set_value : 'a t -> TreeIter.t -> int -> GValue.GValue -> unit
	val set : 'a t -> TreeIter.t -> unit
	val remove : 'a t -> TreeIter.t -> bool * TreeIter.t
	val insert : 'a t -> int -> TreeIter.t
	val insert_before : 'a t -> TreeIter.t -> TreeIter.t
	val insert_after : 'a t -> TreeIter.t -> TreeIter.t
	val prepend : 'a t -> TreeIter.t
	val append : 'a t -> TreeIter.t
	val clear : 'a t -> unit
	val storeiter_is_valid : 'a t -> TreeIter.t -> bool
	val reorder : 'a t -> int list -> unit
	val swap : 'a t -> TreeIter.t -> TreeIter.t -> unit
	val move_after : 'a t -> TreeIter.t -> TreeIter.t option -> unit
	val move_after' : 'a t -> TreeIter.t -> unit
	val move_before : 'a t -> TreeIter.t -> TreeIter.t option -> unit
	val move_before' : 'a t -> TreeIter.t -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a liststore_t = unit
	type 'a t = 'a liststore_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toListStore obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asTreeModel obj
	  = TreeModel.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asTreeDragSource obj
	  = TreeDragSource.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asTreeDragDest obj
	  = TreeDragDest.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asTreeSortable obj
	  = TreeSortable.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : int -> cptr = _import "gtk_list_store_new" : int -> cptr;
	val new : int -> base t = fn n_columns => make (new_ n_columns)
	val newv_ : int * GType.t array -> cptr
	    = _import "gtk_list_store_newv" : int * GType.t array -> cptr;
	val newv : int -> GType.t list -> base t
	    = fn n_columns => fn types =>
		 make (newv_ (n_columns, Array.fromList types))
	val set_column_types_ : cptr * int * GType.t array -> unit
	    = _import "gtk_list_store_set_column_types"
		      : cptr * int * GType.t array -> unit;
	val set_column_types : 'a t -> int -> GType.t list -> unit
	    = fn self => fn n_columns => fn types =>
		 GObject.withPtr (self, 
				  fn self => set_column_types_
					       (self, n_columns, 
						Array.fromList types))
	val set_value_ : cptr * cptr * int * GValue.GValue -> unit
	    = _import "gtk_list_store_set_value"
		      : cptr * cptr * int * GValue.GValue -> unit;
	val set_value : 'a t -> TreeIter.t -> int -> GValue.GValue -> unit
	    = fn self => fn iter => fn column => fn value =>
		 GObject.withPtr
		   (self, fn self => set_value_ (self, iter, column, value))
	val set_ : cptr * cptr -> unit
	    = _import "gtk_list_store_set" : cptr * cptr -> unit;
	val set : 'a t -> TreeIter.t -> unit
	    = fn self => fn iter =>
		 GObject.withPtr (self, fn self => set_ (self, iter))
	val remove_ : cptr * cptr -> bool
	    = _import "gtk_list_store_remove" : cptr * cptr -> bool;
	val remove : 'a t -> TreeIter.t -> bool * TreeIter.t
	    = fn self => fn iter =>
		 let val iter = iter
		     val ret = GObject.withPtr
				 (self, fn self => remove_ (self, iter))
		 in (ret, iter) end
	val insert_ : cptr * cptr * int -> unit
	    = _import "gtk_list_store_insert" : cptr * cptr * int -> unit;
	val insert : 'a t -> int -> TreeIter.t
	    = fn self => fn position =>
		 let val iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => insert_ (self, iter, position))
		 in iter end
	val insert_before_ : cptr * cptr * cptr -> unit
	    = _import "gtk_list_store_insert_before"
		      : cptr * cptr * cptr -> unit;
	val insert_before : 'a t -> TreeIter.t -> TreeIter.t
	    = fn self => fn sibling =>
		 let val iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => insert_before_
					       (self, iter, sibling))
		 in iter end
	val insert_after_ : cptr * cptr * cptr -> unit
	    = _import "gtk_list_store_insert_after"
		      : cptr * cptr * cptr -> unit;
	val insert_after : 'a t -> TreeIter.t -> TreeIter.t
	    = fn self => fn sibling =>
		 let val iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => insert_after_
					       (self, iter, sibling))
		 in iter end
	val prepend_ : cptr * cptr -> unit
	    = _import "gtk_list_store_prepend" : cptr * cptr -> unit;
	val prepend : 'a t -> TreeIter.t
	    = fn self =>
		 let val iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, fn self => prepend_ (self, iter))
		 in iter end
	val append_ : cptr * cptr -> unit
	    = _import "gtk_list_store_append" : cptr * cptr -> unit;
	val append : 'a t -> TreeIter.t
	    = fn self =>
		 let val iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, fn self => append_ (self, iter))
		 in iter end
	val clear_ : cptr -> unit
	    = _import "gtk_list_store_clear" : cptr -> unit;
	val clear : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => clear_ self)
	val storeiter_is_valid_ : cptr * cptr -> bool
	    = _import "gtk_list_store_iter_is_valid" : cptr * cptr -> bool;
	val storeiter_is_valid : 'a t -> TreeIter.t -> bool
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, fn self => storeiter_is_valid_ (self, iter))
	val reorder_ : cptr * int array -> unit
	    = _import "gtk_list_store_reorder" : cptr * int array -> unit;
	val reorder : 'a t -> int list -> unit
	    = fn self => fn new_order =>
		 GObject.withPtr
		   (self, fn self => reorder_ (self, Array.fromList new_order))
	val swap_ : cptr * cptr * cptr -> unit
	    = _import "gtk_list_store_swap" : cptr * cptr * cptr -> unit;
	val swap : 'a t -> TreeIter.t -> TreeIter.t -> unit
	    = fn self => fn a => fn b =>
		 GObject.withPtr (self, fn self => swap_ (self, a, b))
	val move_after_ : cptr * cptr * cptr -> unit
	    = _import "gtk_list_store_move_after" : cptr * cptr * cptr -> unit;
	val move_after : 'a t -> TreeIter.t -> TreeIter.t option -> unit
	    = fn self => fn iter => fn position =>
		 GObject.withPtr
		   (self, 
		    fn self => move_after_ (self, iter, 
					    getOpt (position, GObject.null)))
	val move_after' : 'a t -> TreeIter.t -> unit
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, fn self => move_after_ (self, iter, GObject.null))
	val move_before_ : cptr * cptr * cptr -> unit
	    = _import "gtk_list_store_move_before"
		      : cptr * cptr * cptr -> unit;
	val move_before : 'a t -> TreeIter.t -> TreeIter.t option -> unit
	    = fn self => fn iter => fn position =>
		 GObject.withPtr
		   (self, 
		    fn self => move_before_ (self, iter, 
					     getOpt (position, GObject.null)))
	val move_before' : 'a t -> TreeIter.t -> unit
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, fn self => move_before_ (self, iter, GObject.null))
    end
    structure Menu :>
      sig
	type base
	type 'a menu_t
	type 'a t = 'a menu_t MenuShell.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toMenu : 'a t -> base t
	type directiontype
	val DIR_PARENT : directiontype
	val DIR_CHILD : directiontype
	val DIR_NEXT : directiontype
	val DIR_PREV : directiontype
	val get_type : unit -> GType.t
	val new : unit -> base t
	val reposition : 'a t -> unit
	val popdown : 'a t -> unit
	val get_active : 'a t -> base Widget.t
	val set_active : 'a t -> int -> unit
	val set_accelgroup : 'a t -> 'b AccelGroup.t -> unit
	val get_accelgroup : 'a t -> base AccelGroup.t
	val set_accel_path : 'a t -> string -> unit
	val detach : 'a t -> unit
	val get_attach_widget : 'a t -> base Widget.t
	val set_tearoff_state : 'a t -> bool -> unit
	val get_tearoff_state : 'a t -> bool
	val set_title : 'a t -> string -> unit
	val get_title : 'a t -> string
	val reorder_child : 'a t -> 'b Widget.t -> int -> unit
	val attach : 'a t -> 'b Widget.t -> int -> int -> int -> int -> unit
	val set_monitor : 'a t -> int -> unit
	val bar_get_type : unit -> GType.t
	val item_get_type : unit -> GType.t
	val move_scroll_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a menu_t = unit
	type 'a t = 'a menu_t MenuShell.t
	fun inherit w con = MenuShell.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toMenu obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type directiontype = int
	val get_directiontype_ : int ref * int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_menu_directiontype"
		      : int ref * int ref * int ref * int ref -> unit;
	val (DIR_PARENT, DIR_CHILD, DIR_NEXT, DIR_PREV)
	    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	      in get_directiontype_ (x0, x1, x2, x3)
	       ; (!x0, !x1, !x2, !x3)
	      end
	val get_type_ : unit -> GType.t
	    = _import "gtk_menu_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_menu_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val reposition_ : cptr -> unit
	    = _import "gtk_menu_reposition" : cptr -> unit;
	val reposition : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => reposition_ self)
	val popdown_ : cptr -> unit
	    = _import "gtk_menu_popdown" : cptr -> unit;
	val popdown : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => popdown_ self)
	val get_active_ : cptr -> cptr
	    = _import "gtk_menu_get_active" : cptr -> cptr;
	val get_active : 'a t -> base Widget.t
	    = fn self => Widget.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_active_ self))
	val set_active_ : cptr * int -> unit
	    = _import "gtk_menu_set_active" : cptr * int -> unit;
	val set_active : 'a t -> int -> unit
	    = fn self => fn index =>
		 GObject.withPtr (self, fn self => set_active_ (self, index))
	val set_accelgroup_ : cptr * cptr -> unit
	    = _import "gtk_menu_set_accel_group" : cptr * cptr -> unit;
	val set_accelgroup : 'a t -> 'b AccelGroup.t -> unit
	    = fn self => fn accel_group =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (accel_group, 
				  fn accel_group =>
				     set_accelgroup_ (self, accel_group)))
	val get_accelgroup_ : cptr -> cptr
	    = _import "gtk_menu_get_accel_group" : cptr -> cptr;
	val get_accelgroup : 'a t -> base AccelGroup.t
	    = fn self => AccelGroup.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_accelgroup_ self))
	val set_accel_path_ : cptr * CString.cstring -> unit
	    = _import "gtk_menu_set_accel_path"
		      : cptr * CString.cstring -> unit;
	val set_accel_path : 'a t -> string -> unit
	    = fn self => fn accel_path =>
		 GObject.withPtr
		   (self, 
		    fn self => set_accel_path_
				 (self, CString.fromString accel_path))
	val detach_ : cptr -> unit = _import "gtk_menu_detach" : cptr -> unit;
	val detach : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => detach_ self)
	val get_attach_widget_ : cptr -> cptr
	    = _import "gtk_menu_get_attach_widget" : cptr -> cptr;
	val get_attach_widget : 'a t -> base Widget.t
	    = fn self =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_attach_widget_ self))
	val set_tearoff_state_ : cptr * bool -> unit
	    = _import "gtk_menu_set_tearoff_state" : cptr * bool -> unit;
	val set_tearoff_state : 'a t -> bool -> unit
	    = fn self => fn torn_off =>
		 GObject.withPtr
		   (self, fn self => set_tearoff_state_ (self, torn_off))
	val get_tearoff_state_ : cptr -> bool
	    = _import "gtk_menu_get_tearoff_state" : cptr -> bool;
	val get_tearoff_state : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_tearoff_state_ self)
	val set_title_ : cptr * CString.cstring -> unit
	    = _import "gtk_menu_set_title" : cptr * CString.cstring -> unit;
	val set_title : 'a t -> string -> unit
	    = fn self => fn title =>
		 GObject.withPtr
		   (self, 
		    fn self => set_title_ (self, CString.fromString title))
	val get_title_ : cptr -> CString.t
	    = _import "gtk_menu_get_title" : cptr -> CString.t;
	val get_title : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_title_ self
				       in CString.toString t end)
	val reorder_child_ : cptr * cptr * int -> unit
	    = _import "gtk_menu_reorder_child" : cptr * cptr * int -> unit;
	val reorder_child : 'a t -> 'b Widget.t -> int -> unit
	    = fn self => fn child => fn position =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => reorder_child_
						(self, child, position)))
	val attach_ : cptr * cptr * int * int * int * int -> unit
	    = _import "gtk_menu_attach"
		      : cptr * cptr * int * int * int * int -> unit;
	val attach : 'a t -> 'b Widget.t -> int -> int -> int -> int -> unit
	    = fn self => fn child => fn left_attach => fn right_attach => 
	      fn top_attach => fn bottom_attach =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child =>
				     attach_ (self, child, left_attach, 
					      right_attach, top_attach, 
					      bottom_attach)))
	val set_monitor_ : cptr * int -> unit
	    = _import "gtk_menu_set_monitor" : cptr * int -> unit;
	val set_monitor : 'a t -> int -> unit
	    = fn self => fn monitor_num =>
		 GObject.withPtr
		   (self, fn self => set_monitor_ (self, monitor_num))
	val bar_get_type_ : unit -> GType.t
	    = _import "gtk_menu_bar_get_type" : unit -> GType.t;
	val bar_get_type : unit -> GType.t = fn dummy => bar_get_type_ dummy
	val item_get_type_ : unit -> GType.t
	    = _import "gtk_menu_item_get_type" : unit -> GType.t;
	val item_get_type : unit -> GType.t = fn dummy => item_get_type_ dummy
	local open Signal
	      infixr -->
	in val move_scroll_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "move-scroll" false (unit --> return_void) f
	end
    end
    structure MenuBar :>
      sig
	type base
	type 'a menubar_t
	type 'a t = 'a menubar_t MenuShell.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toMenuBar : 'a t -> base t
	val new : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a menubar_t = unit
	type 'a t = 'a menubar_t MenuShell.t
	fun inherit w con = MenuShell.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toMenuBar obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : unit -> cptr = _import "gtk_menu_bar_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure MessageDialog :>
      sig
	type base
	type 'a messagedialog_t
	type 'a t = 'a messagedialog_t Dialog.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toMessageDialog : 'a t -> base t
	val get_type : unit -> GType.t
	val set_markup : 'a t -> string -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a messagedialog_t = unit
	type 'a t = 'a messagedialog_t Dialog.t
	fun inherit w con = Dialog.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toMessageDialog obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_message_dialog_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_markup_ : cptr * CString.cstring -> unit
	    = _import "gtk_message_dialog_set_markup"
		      : cptr * CString.cstring -> unit;
	val set_markup : 'a t -> string -> unit
	    = fn self => fn str =>
		 GObject.withPtr
		   (self, 
		    fn self => set_markup_ (self, CString.fromString str))
    end
    structure Notebook :>
      sig
	type base
	type 'a notebook_t
	type 'a t = 'a notebook_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toNotebook : 'a t -> base t
	type tab
	val TAB_FIRST : tab
	val TAB_LAST : tab
	val get_type : unit -> GType.t
	val new : unit -> base t
	val append_page : 'a t -> 'b Widget.t -> 'c Widget.t option -> int
	val append_page' : 'a t -> 'b Widget.t -> int
	val append_page_menu : 'a t -> 'b Widget.t -> 'c Widget.t option 
			    -> 'd Widget.t option
			       -> int
	val append_page_menu' : 'a t -> 'b Widget.t -> int
	val prepend_page : 'a t -> 'b Widget.t -> 'c Widget.t option -> int
	val prepend_page' : 'a t -> 'b Widget.t -> int
	val prepend_page_menu : 'a t -> 'b Widget.t -> 'c Widget.t option 
			     -> 'd Widget.t option
				-> int
	val prepend_page_menu' : 'a t -> 'b Widget.t -> int
	val insert_page
	  : 'a t -> 'b Widget.t -> 'c Widget.t option -> int -> int
	val insert_page' : 'a t -> 'b Widget.t -> int
	val insert_page_menu : 'a t -> 'b Widget.t -> 'c Widget.t option 
			    -> 'd Widget.t option -> int
			       -> int
	val insert_page_menu' : 'a t -> 'b Widget.t -> int
	val remove_page : 'a t -> int -> unit
	val get_current_page : 'a t -> int
	val get_nth_page : 'a t -> int -> base Widget.t
	val get_n_pages : 'a t -> int
	val page_num : 'a t -> 'b Widget.t -> int
	val set_current_page : 'a t -> int -> unit
	val next_page : 'a t -> unit
	val prev_page : 'a t -> unit
	val set_show_border : 'a t -> bool -> unit
	val get_show_border : 'a t -> bool
	val set_show_tabs : 'a t -> bool -> unit
	val get_show_tabs : 'a t -> bool
	val set_tab_pos : 'a t -> positiontype -> unit
	val get_tab_pos : 'a t -> positiontype
	val set_scrollable : 'a t -> bool -> unit
	val get_scrollable : 'a t -> bool
	val popup_enable : 'a t -> unit
	val popup_disable : 'a t -> unit
	val get_tab_label : 'a t -> 'b Widget.t -> base Widget.t
	val set_tab_label : 'a t -> 'b Widget.t -> 'c Widget.t option -> unit
	val set_tab_label' : 'a t -> 'b Widget.t -> unit
	val set_tab_label_text : 'a t -> 'b Widget.t -> string -> unit
	val get_tab_label_text : 'a t -> 'b Widget.t -> string
	val get_menu_label : 'a t -> 'b Widget.t -> base Widget.t
	val set_menu_label : 'a t -> 'b Widget.t -> 'c Widget.t option -> unit
	val set_menu_label' : 'a t -> 'b Widget.t -> unit
	val set_menu_label_text : 'a t -> 'b Widget.t -> string -> unit
	val get_menu_label_text : 'a t -> 'b Widget.t -> string
	val set_tab_label_packing
	  : 'a t -> 'b Widget.t -> bool -> bool -> packtype -> unit
	val reorder_child : 'a t -> 'b Widget.t -> int -> unit
	val move_focus_out_sig : (unit -> unit) -> 'a t Signal.signal
	val switch_page_sig : (unit -> int -> unit) -> 'a t Signal.signal
	val focus_tab_sig : (unit -> bool) -> 'a t Signal.signal
	val select_page_sig : (bool -> bool) -> 'a t Signal.signal
	val change_current_page_sig : (int -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a notebook_t = unit
	type 'a t = 'a notebook_t Container.t
	fun inherit w con = Container.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toNotebook obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type tab = int
	val get_tab_ : int ref * int ref -> unit
	    = _import "mgtk_get_gtk_notebook_tab" : int ref * int ref -> unit;
	val (TAB_FIRST, TAB_LAST)
	    = let val (x0, x1) = (ref 0, ref 0) in get_tab_ (x0, x1)
						 ; (!x0, !x1)
						end
	val get_type_ : unit -> GType.t
	    = _import "gtk_notebook_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_notebook_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val append_page_ : cptr * cptr * cptr -> int
	    = _import "gtk_notebook_append_page" : cptr * cptr * cptr -> int;
	val append_page : 'a t -> 'b Widget.t -> 'c Widget.t option -> int
	    = fn self => fn child => fn tab_label =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => GObject.withOpt
						(tab_label, 
						 fn tab_label =>
						    append_page_ (self, child, 
								  tab_label))))
	val append_page' : 'a t -> 'b Widget.t -> int
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => append_page_ (self, child, 
							    GObject.null)))
	val append_page_menu_ : cptr * cptr * cptr * cptr -> int
	    = _import "gtk_notebook_append_page_menu"
		      : cptr * cptr * cptr * cptr -> int;
	val append_page_menu : 'a t -> 'b Widget.t -> 'c Widget.t option 
			    -> 'd Widget.t option
			       -> int
	    = fn self => fn child => fn tab_label => fn menu_label =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child =>
				     GObject.withOpt
				       (tab_label, 
					fn tab_label =>
					   GObject.withOpt
					     (menu_label, 
					      fn menu_label =>
						 append_page_menu_
						   (self, child, tab_label, 
						    menu_label)))))
	val append_page_menu' : 'a t -> 'b Widget.t -> int
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => append_page_menu_
						(self, child, GObject.null, 
						 GObject.null)))
	val prepend_page_ : cptr * cptr * cptr -> int
	    = _import "gtk_notebook_prepend_page" : cptr * cptr * cptr -> int;
	val prepend_page : 'a t -> 'b Widget.t -> 'c Widget.t option -> int
	    = fn self => fn child => fn tab_label =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => GObject.withOpt
						(tab_label, 
						 fn tab_label =>
						    prepend_page_
						      (self, child, 
						       tab_label))))
	val prepend_page' : 'a t -> 'b Widget.t -> int
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => prepend_page_ (self, child, 
							     GObject.null)))
	val prepend_page_menu_ : cptr * cptr * cptr * cptr -> int
	    = _import "gtk_notebook_prepend_page_menu"
		      : cptr * cptr * cptr * cptr -> int;
	val prepend_page_menu : 'a t -> 'b Widget.t -> 'c Widget.t option 
			     -> 'd Widget.t option
				-> int
	    = fn self => fn child => fn tab_label => fn menu_label =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child =>
				     GObject.withOpt
				       (tab_label, 
					fn tab_label =>
					   GObject.withOpt
					     (menu_label, 
					      fn menu_label =>
						 prepend_page_menu_
						   (self, child, tab_label, 
						    menu_label)))))
	val prepend_page_menu' : 'a t -> 'b Widget.t -> int
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => prepend_page_menu_
						(self, child, GObject.null, 
						 GObject.null)))
	val insert_page_ : cptr * cptr * cptr * int -> int
	    = _import "gtk_notebook_insert_page"
		      : cptr * cptr * cptr * int -> int;
	val insert_page
	  : 'a t -> 'b Widget.t -> 'c Widget.t option -> int -> int
	    = fn self => fn child => fn tab_label => fn position =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => GObject.withOpt
						(tab_label, 
						 fn tab_label =>
						    insert_page_
						      (self, child, tab_label, 
						       position))))
	val insert_page' : 'a t -> 'b Widget.t -> int
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => insert_page_ (self, child, 
							    GObject.null, ~1)))
	val insert_page_menu_ : cptr * cptr * cptr * cptr * int -> int
	    = _import "gtk_notebook_insert_page_menu"
		      : cptr * cptr * cptr * cptr * int -> int;
	val insert_page_menu : 'a t -> 'b Widget.t -> 'c Widget.t option 
			    -> 'd Widget.t option -> int
			       -> int
	    = fn self => fn child => fn tab_label => fn menu_label => 
	      fn position =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child =>
				     GObject.withOpt
				       (tab_label, 
					fn tab_label =>
					   GObject.withOpt
					     (menu_label, 
					      fn menu_label =>
						 insert_page_menu_
						   (self, child, tab_label, 
						    menu_label, position)))))
	val insert_page_menu' : 'a t -> 'b Widget.t -> int
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => insert_page_menu_
						(self, child, GObject.null, 
						 GObject.null, ~1)))
	val remove_page_ : cptr * int -> unit
	    = _import "gtk_notebook_remove_page" : cptr * int -> unit;
	val remove_page : 'a t -> int -> unit
	    = fn self => fn page_num =>
		 GObject.withPtr
		   (self, fn self => remove_page_ (self, page_num))
	val get_current_page_ : cptr -> int
	    = _import "gtk_notebook_get_current_page" : cptr -> int;
	val get_current_page : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_current_page_ self)
	val get_nth_page_ : cptr * int -> cptr
	    = _import "gtk_notebook_get_nth_page" : cptr * int -> cptr;
	val get_nth_page : 'a t -> int -> base Widget.t
	    = fn self => fn page_num =>
		 Widget.inherit
		   ()
		   (fn () =>
		       GObject.withPtr
			 (self, fn self => get_nth_page_ (self, page_num)))
	val get_n_pages_ : cptr -> int
	    = _import "gtk_notebook_get_n_pages" : cptr -> int;
	val get_n_pages : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_n_pages_ self)
	val page_num_ : cptr * cptr -> int
	    = _import "gtk_notebook_page_num" : cptr * cptr -> int;
	val page_num : 'a t -> 'b Widget.t -> int
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, fn child => page_num_ (self, child)))
	val set_current_page_ : cptr * int -> unit
	    = _import "gtk_notebook_set_current_page" : cptr * int -> unit;
	val set_current_page : 'a t -> int -> unit
	    = fn self => fn page_num =>
		 GObject.withPtr
		   (self, fn self => set_current_page_ (self, page_num))
	val next_page_ : cptr -> unit
	    = _import "gtk_notebook_next_page" : cptr -> unit;
	val next_page : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => next_page_ self)
	val prev_page_ : cptr -> unit
	    = _import "gtk_notebook_prev_page" : cptr -> unit;
	val prev_page : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => prev_page_ self)
	val set_show_border_ : cptr * bool -> unit
	    = _import "gtk_notebook_set_show_border" : cptr * bool -> unit;
	val set_show_border : 'a t -> bool -> unit
	    = fn self => fn show_border =>
		 GObject.withPtr
		   (self, fn self => set_show_border_ (self, show_border))
	val get_show_border_ : cptr -> bool
	    = _import "gtk_notebook_get_show_border" : cptr -> bool;
	val get_show_border : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_show_border_ self)
	val set_show_tabs_ : cptr * bool -> unit
	    = _import "gtk_notebook_set_show_tabs" : cptr * bool -> unit;
	val set_show_tabs : 'a t -> bool -> unit
	    = fn self => fn show_tabs =>
		 GObject.withPtr
		   (self, fn self => set_show_tabs_ (self, show_tabs))
	val get_show_tabs_ : cptr -> bool
	    = _import "gtk_notebook_get_show_tabs" : cptr -> bool;
	val get_show_tabs : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_show_tabs_ self)
	val set_tab_pos_ : cptr * int -> unit
	    = _import "gtk_notebook_set_tab_pos" : cptr * int -> unit;
	val set_tab_pos : 'a t -> positiontype -> unit
	    = fn self => fn pos =>
		 GObject.withPtr (self, fn self => set_tab_pos_ (self, pos))
	val get_tab_pos_ : cptr -> int
	    = _import "gtk_notebook_get_tab_pos" : cptr -> int;
	val get_tab_pos : 'a t -> positiontype
	    = fn self => GObject.withPtr (self, fn self => get_tab_pos_ self)
	val set_scrollable_ : cptr * bool -> unit
	    = _import "gtk_notebook_set_scrollable" : cptr * bool -> unit;
	val set_scrollable : 'a t -> bool -> unit
	    = fn self => fn scrollable =>
		 GObject.withPtr
		   (self, fn self => set_scrollable_ (self, scrollable))
	val get_scrollable_ : cptr -> bool
	    = _import "gtk_notebook_get_scrollable" : cptr -> bool;
	val get_scrollable : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_scrollable_ self)
	val popup_enable_ : cptr -> unit
	    = _import "gtk_notebook_popup_enable" : cptr -> unit;
	val popup_enable : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => popup_enable_ self)
	val popup_disable_ : cptr -> unit
	    = _import "gtk_notebook_popup_disable" : cptr -> unit;
	val popup_disable : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => popup_disable_ self)
	val get_tab_label_ : cptr * cptr -> cptr
	    = _import "gtk_notebook_get_tab_label" : cptr * cptr -> cptr;
	val get_tab_label : 'a t -> 'b Widget.t -> base Widget.t
	    = fn self => fn child =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self => GObject.withPtr
					     (child, 
					      fn child => get_tab_label_
							    (self, child))))
	val set_tab_label_ : cptr * cptr * cptr -> unit
	    = _import "gtk_notebook_set_tab_label"
		      : cptr * cptr * cptr -> unit;
	val set_tab_label : 'a t -> 'b Widget.t -> 'c Widget.t option -> unit
	    = fn self => fn child => fn tab_label =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => GObject.withOpt
						(tab_label, 
						 fn tab_label =>
						    set_tab_label_
						      (self, child, 
						       tab_label))))
	val set_tab_label' : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => set_tab_label_
						(self, child, GObject.null)))
	val set_tab_label_text_ : cptr * cptr * CString.cstring -> unit
	    = _import "gtk_notebook_set_tab_label_text"
		      : cptr * cptr * CString.cstring -> unit;
	val set_tab_label_text : 'a t -> 'b Widget.t -> string -> unit
	    = fn self => fn child => fn tab_text =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => set_tab_label_text_
						(self, child, 
						 CString.fromString tab_text)))
	val get_tab_label_text_ : cptr * cptr -> CString.t
	    = _import "gtk_notebook_get_tab_label_text"
		      : cptr * cptr -> CString.t;
	val get_tab_label_text : 'a t -> 'b Widget.t -> string
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => let val t = get_tab_label_text_
							    (self, child)
					      in CString.toString t end))
	val get_menu_label_ : cptr * cptr -> cptr
	    = _import "gtk_notebook_get_menu_label" : cptr * cptr -> cptr;
	val get_menu_label : 'a t -> 'b Widget.t -> base Widget.t
	    = fn self => fn child =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self => GObject.withPtr
					     (child, 
					      fn child => get_menu_label_
							    (self, child))))
	val set_menu_label_ : cptr * cptr * cptr -> unit
	    = _import "gtk_notebook_set_menu_label"
		      : cptr * cptr * cptr -> unit;
	val set_menu_label : 'a t -> 'b Widget.t -> 'c Widget.t option -> unit
	    = fn self => fn child => fn menu_label =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => GObject.withOpt
						(menu_label, 
						 fn menu_label =>
						    set_menu_label_
						      (self, child, 
						       menu_label))))
	val set_menu_label' : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => set_menu_label_
						(self, child, GObject.null)))
	val set_menu_label_text_ : cptr * cptr * CString.cstring -> unit
	    = _import "gtk_notebook_set_menu_label_text"
		      : cptr * cptr * CString.cstring -> unit;
	val set_menu_label_text : 'a t -> 'b Widget.t -> string -> unit
	    = fn self => fn child => fn menu_text =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => set_menu_label_text_
						(self, child, 
						 CString.fromString
						   menu_text)))
	val get_menu_label_text_ : cptr * cptr -> CString.t
	    = _import "gtk_notebook_get_menu_label_text"
		      : cptr * cptr -> CString.t;
	val get_menu_label_text : 'a t -> 'b Widget.t -> string
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => let val t = get_menu_label_text_
							    (self, child)
					      in CString.toString t end))
	val set_tab_label_packing_ : cptr * cptr * bool * bool * int -> unit
	    = _import "gtk_notebook_set_tab_label_packing"
		      : cptr * cptr * bool * bool * int -> unit;
	val set_tab_label_packing
	  : 'a t -> 'b Widget.t -> bool -> bool -> packtype -> unit
	    = fn self => fn child => fn expand => fn fill => fn pack_type =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr (child, 
						fn child =>
						   set_tab_label_packing_
						     (self, child, expand, 
						      fill, pack_type)))
	val reorder_child_ : cptr * cptr * int -> unit
	    = _import "gtk_notebook_reorder_child" : cptr * cptr * int -> unit;
	val reorder_child : 'a t -> 'b Widget.t -> int -> unit
	    = fn self => fn child => fn position =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => reorder_child_
						(self, child, position)))
	local open Signal
	      infixr -->
	in val move_focus_out_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "move-focus-out" false (unit --> return_void) f
	   val switch_page_sig : (unit -> int -> unit) -> 'a t Signal.signal
	       = fn f => signal "switch-page" false
			        (unit --> int --> return_void) f
	   val focus_tab_sig : (unit -> bool) -> 'a t Signal.signal
	       = fn f => signal "focus-tab" false (unit --> return_bool) f
	   val select_page_sig : (bool -> bool) -> 'a t Signal.signal
	       = fn f => signal "select-page" false (bool --> return_bool) f
	   val change_current_page_sig : (int -> unit) -> 'a t Signal.signal
	       = fn f => signal "change-current-page" false
			        (int --> return_void) f
	end
    end
    structure OldEditable :>
      sig
	type base
	type 'a oldeditable_t
	type 'a t = 'a oldeditable_t Widget.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toOldEditable : 'a t -> base t
	val asEditable : 'a t -> base Editable.t
	val get_type : unit -> GType.t
	val changed : 'a t -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a oldeditable_t = unit
	type 'a t = 'a oldeditable_t Widget.t
	fun inherit w con = Widget.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toOldEditable obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asEditable obj
	  = Editable.inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_old_editable_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val changed_ : cptr -> unit
	    = _import "gtk_old_editable_changed" : cptr -> unit;
	val changed : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => changed_ self)
    end
    structure OptionMenu :>
      sig
	type base
	type 'a optionmenu_t
	type 'a t = 'a optionmenu_t Button.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toOptionMenu : 'a t -> base t
	val get_type : unit -> GType.t
	val get_menu : 'a t -> base Widget.t
	val set_menu : 'a t -> 'b Widget.t -> unit
	val remove_menu : 'a t -> unit
	val get_history : 'a t -> int
	val set_history : 'a t -> int -> unit
	val changed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a optionmenu_t = unit
	type 'a t = 'a optionmenu_t Button.t
	fun inherit w con = Button.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toOptionMenu obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_option_menu_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val get_menu_ : cptr -> cptr
	    = _import "gtk_option_menu_get_menu" : cptr -> cptr;
	val get_menu : 'a t -> base Widget.t
	    = fn self => Widget.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_menu_ self))
	val set_menu_ : cptr * cptr -> unit
	    = _import "gtk_option_menu_set_menu" : cptr * cptr -> unit;
	val set_menu : 'a t -> 'b Widget.t -> unit
	    = fn self => fn menu =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (menu, fn menu => set_menu_ (self, menu)))
	val remove_menu_ : cptr -> unit
	    = _import "gtk_option_menu_remove_menu" : cptr -> unit;
	val remove_menu : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => remove_menu_ self)
	val get_history_ : cptr -> int
	    = _import "gtk_option_menu_get_history" : cptr -> int;
	val get_history : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_history_ self)
	val set_history_ : cptr * int -> unit
	    = _import "gtk_option_menu_set_history" : cptr * int -> unit;
	val set_history : 'a t -> int -> unit
	    = fn self => fn index =>
		 GObject.withPtr (self, fn self => set_history_ (self, index))
	local open Signal
	      infixr -->
	in val changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "changed" false (void --> return_void) f
	end
    end
    structure Pixmap :>
      sig
	type base
	type 'a pixmap_t
	type 'a t = 'a pixmap_t Misc.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toPixmap : 'a t -> base t
	val get_type : unit -> GType.t
	val set_build_insensitive : 'a t -> bool -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a pixmap_t = unit
	type 'a t = 'a pixmap_t Misc.t
	fun inherit w con = Misc.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toPixmap obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_pixmap_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_build_insensitive_ : cptr * bool -> unit
	    = _import "gtk_pixmap_set_build_insensitive" : cptr * bool -> unit;
	val set_build_insensitive : 'a t -> bool -> unit
	    = fn self => fn build =>
		 GObject.withPtr
		   (self, fn self => set_build_insensitive_ (self, build))
    end
    structure Plug :>
      sig
	type base
	type 'a plug_t
	type 'a t = 'a plug_t Window.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toPlug : 'a t -> base t
	val get_type : unit -> GType.t
	val embedded_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a plug_t = unit
	type 'a t = 'a plug_t Window.t
	fun inherit w con = Window.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toPlug obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_plug_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	local open Signal
	      infixr -->
	in val embedded_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "embedded" false (void --> return_void) f
	end
    end
    structure Preview :>
      sig
	type base
	type 'a preview_t
	type 'a t = 'a preview_t Widget.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toPreview : 'a t -> base t
	val get_type : unit -> GType.t
	val size : 'a t -> int -> int -> unit
	val set_expand : 'a t -> bool -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a preview_t = unit
	type 'a t = 'a preview_t Widget.t
	fun inherit w con = Widget.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toPreview obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_preview_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val size_ : cptr * int * int -> unit
	    = _import "gtk_preview_size" : cptr * int * int -> unit;
	val size : 'a t -> int -> int -> unit
	    = fn self => fn width => fn height =>
		 GObject.withPtr (self, fn self => size_ (self, width, height))
	val set_expand_ : cptr * bool -> unit
	    = _import "gtk_preview_set_expand" : cptr * bool -> unit;
	val set_expand : 'a t -> bool -> unit
	    = fn self => fn expand =>
		 GObject.withPtr (self, fn self => set_expand_ (self, expand))
    end
    structure Progress :>
      sig
	type base
	type 'a progress_t
	type 'a t = 'a progress_t Widget.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toProgress : 'a t -> base t
	val get_type : unit -> GType.t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a progress_t = unit
	type 'a t = 'a progress_t Widget.t
	fun inherit w con = Widget.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toProgress obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_progress_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
    end
    structure ProgressBar :>
      sig
	type base
	type 'a progressbar_t
	type 'a t = 'a progressbar_t Progress.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toProgressBar : 'a t -> base t
	type orientation
	val PROGRESS_LEFT_TO_RIGHT : orientation
	val PROGRESS_RIGHT_TO_LEFT : orientation
	val PROGRESS_BOTTOM_TO_TOP : orientation
	val PROGRESS_TOP_TO_BOTTOM : orientation
	type style
	val PROGRESS_CONTINUOUS : style
	val PROGRESS_DISCRETE : style
	val get_type : unit -> GType.t
	val new : unit -> base t
	val pulse : 'a t -> unit
	val set_text : 'a t -> string -> unit
	val set_fraction : 'a t -> real -> unit
	val set_pulse_step : 'a t -> real -> unit
	val set_orientation : 'a t -> orientation -> unit
	val get_text : 'a t -> string
	val get_fraction : 'a t -> real
	val get_pulse_step : 'a t -> real
	val get_orientation : 'a t -> orientation
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a progressbar_t = unit
	type 'a t = 'a progressbar_t Progress.t
	fun inherit w con = Progress.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toProgressBar obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type orientation = int
	val get_orientation_ : int ref * int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_progressbar_orientation"
		      : int ref * int ref * int ref * int ref -> unit;
	val (PROGRESS_LEFT_TO_RIGHT, PROGRESS_RIGHT_TO_LEFT, 
	     PROGRESS_BOTTOM_TO_TOP, PROGRESS_TOP_TO_BOTTOM)
	    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	      in get_orientation_ (x0, x1, x2, x3)
	       ; (!x0, !x1, !x2, !x3)
	      end
	type style = int
	val get_style_ : int ref * int ref -> unit
	    = _import "mgtk_get_gtk_progressbar_style"
		      : int ref * int ref -> unit;
	val (PROGRESS_CONTINUOUS, PROGRESS_DISCRETE)
	    = let val (x0, x1) = (ref 0, ref 0) in get_style_ (x0, x1)
						 ; (!x0, !x1)
						end
	val get_type_ : unit -> GType.t
	    = _import "gtk_progress_bar_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr
	    = _import "gtk_progress_bar_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val pulse_ : cptr -> unit
	    = _import "gtk_progress_bar_pulse" : cptr -> unit;
	val pulse : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => pulse_ self)
	val set_text_ : cptr * CString.cstring -> unit
	    = _import "gtk_progress_bar_set_text"
		      : cptr * CString.cstring -> unit;
	val set_text : 'a t -> string -> unit
	    = fn self => fn text =>
		 GObject.withPtr
		   (self, fn self => set_text_ (self, CString.fromString text))
	val set_fraction_ : cptr * real -> unit
	    = _import "gtk_progress_bar_set_fraction" : cptr * real -> unit;
	val set_fraction : 'a t -> real -> unit
	    = fn self => fn fraction =>
		 GObject.withPtr
		   (self, fn self => set_fraction_ (self, fraction))
	val set_pulse_step_ : cptr * real -> unit
	    = _import "gtk_progress_bar_set_pulse_step" : cptr * real -> unit;
	val set_pulse_step : 'a t -> real -> unit
	    = fn self => fn fraction =>
		 GObject.withPtr
		   (self, fn self => set_pulse_step_ (self, fraction))
	val set_orientation_ : cptr * int -> unit
	    = _import "gtk_progress_bar_set_orientation" : cptr * int -> unit;
	val set_orientation : 'a t -> orientation -> unit
	    = fn self => fn orientation =>
		 GObject.withPtr
		   (self, fn self => set_orientation_ (self, orientation))
	val get_text_ : cptr -> CString.t
	    = _import "gtk_progress_bar_get_text" : cptr -> CString.t;
	val get_text : 'a t -> string
	    = fn self => GObject.withPtr (self, 
					  fn self => let val t = get_text_ self
						     in CString.toString t end)
	val get_fraction_ : cptr -> real
	    = _import "gtk_progress_bar_get_fraction" : cptr -> real;
	val get_fraction : 'a t -> real
	    = fn self => GObject.withPtr (self, fn self => get_fraction_ self)
	val get_pulse_step_ : cptr -> real
	    = _import "gtk_progress_bar_get_pulse_step" : cptr -> real;
	val get_pulse_step : 'a t -> real
	    = fn self => GObject.withPtr
			   (self, fn self => get_pulse_step_ self)
	val get_orientation_ : cptr -> int
	    = _import "gtk_progress_bar_get_orientation" : cptr -> int;
	val get_orientation : 'a t -> orientation
	    = fn self => GObject.withPtr
			   (self, fn self => get_orientation_ self)
    end
    structure ToggleAction :>
      sig
	type base
	type 'a toggleaction_t
	type 'a t = 'a toggleaction_t Action.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toToggleAction : 'a t -> base t
	val get_type : unit -> GType.t
	val toggled : 'a t -> unit
	val set_active : 'a t -> bool -> unit
	val get_active : 'a t -> bool
	val set_draw_as_radio : 'a t -> bool -> unit
	val get_draw_as_radio : 'a t -> bool
	val toggled_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a toggleaction_t = unit
	type 'a t = 'a toggleaction_t Action.t
	fun inherit w con = Action.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toToggleAction obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_toggle_action_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val toggled_ : cptr -> unit
	    = _import "gtk_toggle_action_toggled" : cptr -> unit;
	val toggled : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => toggled_ self)
	val set_active_ : cptr * bool -> unit
	    = _import "gtk_toggle_action_set_active" : cptr * bool -> unit;
	val set_active : 'a t -> bool -> unit
	    = fn self => fn is_active =>
		 GObject.withPtr
		   (self, fn self => set_active_ (self, is_active))
	val get_active_ : cptr -> bool
	    = _import "gtk_toggle_action_get_active" : cptr -> bool;
	val get_active : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_active_ self)
	val set_draw_as_radio_ : cptr * bool -> unit
	    = _import "gtk_toggle_action_set_draw_as_radio"
		      : cptr * bool -> unit;
	val set_draw_as_radio : 'a t -> bool -> unit
	    = fn self => fn draw_as_radio =>
		 GObject.withPtr
		   (self, fn self => set_draw_as_radio_ (self, draw_as_radio))
	val get_draw_as_radio_ : cptr -> bool
	    = _import "gtk_toggle_action_get_draw_as_radio" : cptr -> bool;
	val get_draw_as_radio : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_draw_as_radio_ self)
	local open Signal
	      infixr -->
	in val toggled_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "toggled" false (void --> return_void) f
	end
    end
    structure RadioAction :>
      sig
	type base
	type 'a radioaction_t
	type 'a t = 'a radioaction_t ToggleAction.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toRadioAction : 'a t -> base t
	val get_type : unit -> GType.t
	val get_current_value : 'a t -> int
	val changed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a radioaction_t = unit
	type 'a t = 'a radioaction_t ToggleAction.t
	fun inherit w con = ToggleAction.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toRadioAction obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_radio_action_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val get_current_value_ : cptr -> int
	    = _import "gtk_radio_action_get_current_value" : cptr -> int;
	val get_current_value : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_current_value_ self)
	local open Signal
	      infixr -->
	in val changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "changed" false (unit --> return_void) f
	end
    end
    structure RadioButton :>
      sig
	type base
	type 'a radiobutton_t
	type 'a t = 'a radiobutton_t CheckButton.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toRadioButton : 'a t -> base t
	val get_type : unit -> GType.t
	val new_from_widget : 'a t -> base t
	val new_with_label_from_widget : 'a t -> string -> base t
	val new_with_mnemonic_from_widget : 'a t -> string -> base t
	val group_changed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a radiobutton_t = unit
	type 'a t = 'a radiobutton_t CheckButton.t
	fun inherit w con = CheckButton.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toRadioButton obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_radio_button_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_from_widget_ : cptr -> cptr
	    = _import "gtk_radio_button_new_from_widget" : cptr -> cptr;
	val new_from_widget : 'a t -> base t
	    = fn group => make (GObject.withPtr
				  (group, fn group => new_from_widget_ group))
	val new_with_label_from_widget_ : cptr * CString.cstring -> cptr
	    = _import "gtk_radio_button_new_with_label_from_widget"
		      : cptr * CString.cstring -> cptr;
	val new_with_label_from_widget : 'a t -> string -> base t
	    = fn group => fn label =>
		 make (GObject.withPtr
			 (group, 
			  fn group => new_with_label_from_widget_
					(group, CString.fromString label)))
	val new_with_mnemonic_from_widget_ : cptr * CString.cstring -> cptr
	    = _import "gtk_radio_button_new_with_mnemonic_from_widget"
		      : cptr * CString.cstring -> cptr;
	val new_with_mnemonic_from_widget : 'a t -> string -> base t
	    = fn group => fn label =>
		 make (GObject.withPtr
			 (group, 
			  fn group => new_with_mnemonic_from_widget_
					(group, CString.fromString label)))
	local open Signal
	      infixr -->
	in val group_changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "group-changed" false (void --> return_void) f
	end
    end
    structure RadioMenuItem :>
      sig
	type base
	type 'a radiomenuitem_t
	type 'a t = 'a radiomenuitem_t CheckMenuItem.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toRadioMenuItem : 'a t -> base t
	val get_type : unit -> GType.t
	val new_from_widget : 'a t -> base Widget.t
	val new_with_mnemonic_from_widget : 'a t -> string -> base Widget.t
	val new_with_label_from_widget : 'a t -> string -> base Widget.t
	val group_changed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a radiomenuitem_t = unit
	type 'a t = 'a radiomenuitem_t CheckMenuItem.t
	fun inherit w con = CheckMenuItem.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toRadioMenuItem obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_radio_menu_item_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_from_widget_ : cptr -> cptr
	    = _import "gtk_radio_menu_item_new_from_widget" : cptr -> cptr;
	val new_from_widget : 'a t -> base Widget.t
	    = fn self =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => new_from_widget_ self))
	val new_with_mnemonic_from_widget_ : cptr * CString.cstring -> cptr
	    = _import "gtk_radio_menu_item_new_with_mnemonic_from_widget"
		      : cptr * CString.cstring -> cptr;
	val new_with_mnemonic_from_widget : 'a t -> string -> base Widget.t
	    = fn self => fn label =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self => new_with_mnemonic_from_widget_
					     (self, CString.fromString label)))
	val new_with_label_from_widget_ : cptr * CString.cstring -> cptr
	    = _import "gtk_radio_menu_item_new_with_label_from_widget"
		      : cptr * CString.cstring -> cptr;
	val new_with_label_from_widget : 'a t -> string -> base Widget.t
	    = fn self => fn label =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self => new_with_label_from_widget_
					     (self, CString.fromString label)))
	local open Signal
	      infixr -->
	in val group_changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "group-changed" false (void --> return_void) f
	end
    end
    structure ToolItem :>
      sig
	type base
	type 'a toolitem_t
	type 'a t = 'a toolitem_t Bin.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toToolItem : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val set_homogeneous : 'a t -> bool -> unit
	val get_homogeneous : 'a t -> bool
	val set_expand : 'a t -> bool -> unit
	val get_expand : 'a t -> bool
	val set_tooltip
	  : 'a t -> 'b Tooltips.t -> string option -> string option -> unit
	val set_tooltip' : 'a t -> 'b Tooltips.t -> unit
	val set_use_drag_window : 'a t -> bool -> unit
	val get_use_drag_window : 'a t -> bool
	val set_visible_horizontal : 'a t -> bool -> unit
	val get_visible_horizontal : 'a t -> bool
	val set_visible_vertical : 'a t -> bool -> unit
	val get_visible_vertical : 'a t -> bool
	val set_is_important : 'a t -> bool -> unit
	val get_is_important : 'a t -> bool
	val get_icon_size : 'a t -> icon_size
	val get_orientation : 'a t -> orientation
	val get_relief_style : 'a t -> relief_style
	val retrieve_proxy_menu_item : 'a t -> base Widget.t
	val set_proxy_menu_item : 'a t -> string -> 'b Widget.t option -> unit
	val set_proxy_menu_item' : 'a t -> string -> unit
	val get_proxy_menu_item : 'a t -> string -> base Widget.t
	val create_menu_proxy_sig : (unit -> bool) -> 'a t Signal.signal
	val toolbar_reconfigured_sig : (unit -> unit) -> 'a t Signal.signal
	val set_tooltip_sig : (unit -> char -> char -> bool)
			      -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a toolitem_t = unit
	type 'a t = 'a toolitem_t Bin.t
	fun inherit w con = Bin.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toToolItem obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_tool_item_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_tool_item_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val set_homogeneous_ : cptr * bool -> unit
	    = _import "gtk_tool_item_set_homogeneous" : cptr * bool -> unit;
	val set_homogeneous : 'a t -> bool -> unit
	    = fn self => fn homogeneous =>
		 GObject.withPtr
		   (self, fn self => set_homogeneous_ (self, homogeneous))
	val get_homogeneous_ : cptr -> bool
	    = _import "gtk_tool_item_get_homogeneous" : cptr -> bool;
	val get_homogeneous : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_homogeneous_ self)
	val set_expand_ : cptr * bool -> unit
	    = _import "gtk_tool_item_set_expand" : cptr * bool -> unit;
	val set_expand : 'a t -> bool -> unit
	    = fn self => fn expand =>
		 GObject.withPtr (self, fn self => set_expand_ (self, expand))
	val get_expand_ : cptr -> bool
	    = _import "gtk_tool_item_get_expand" : cptr -> bool;
	val get_expand : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_expand_ self)
	val set_tooltip_
	  : cptr * cptr * CString.cstring * CString.cstring -> unit
	    = _import "gtk_tool_item_set_tooltip"
		      : cptr * cptr * CString.cstring * CString.cstring
			-> unit;
	val set_tooltip
	  : 'a t -> 'b Tooltips.t -> string option -> string option -> unit
	    = fn self => fn tooltips => fn tip_text => fn tip_private =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (tooltips, 
				  fn tooltips =>
				     set_tooltip_
				       (self, tooltips, 
					CString.fromString
					  (getOpt (tip_text, "")), 
					CString.fromString
					  (getOpt (tip_private, "")))))
	val set_tooltip' : 'a t -> 'b Tooltips.t -> unit
	    = fn self => fn tooltips =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (tooltips, 
				  fn tooltips =>
				     set_tooltip_ (self, tooltips, 
						   CString.fromString "", 
						   CString.fromString "")))
	val set_use_drag_window_ : cptr * bool -> unit
	    = _import "gtk_tool_item_set_use_drag_window"
		      : cptr * bool -> unit;
	val set_use_drag_window : 'a t -> bool -> unit
	    = fn self => fn use_drag_window =>
		 GObject.withPtr (self, 
				  fn self => set_use_drag_window_
					       (self, use_drag_window))
	val get_use_drag_window_ : cptr -> bool
	    = _import "gtk_tool_item_get_use_drag_window" : cptr -> bool;
	val get_use_drag_window : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_use_drag_window_ self)
	val set_visible_horizontal_ : cptr * bool -> unit
	    = _import "gtk_tool_item_set_visible_horizontal"
		      : cptr * bool -> unit;
	val set_visible_horizontal : 'a t -> bool -> unit
	    = fn self => fn visible_horizontal =>
		 GObject.withPtr (self, 
				  fn self => set_visible_horizontal_
					       (self, visible_horizontal))
	val get_visible_horizontal_ : cptr -> bool
	    = _import "gtk_tool_item_get_visible_horizontal" : cptr -> bool;
	val get_visible_horizontal : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_visible_horizontal_ self)
	val set_visible_vertical_ : cptr * bool -> unit
	    = _import "gtk_tool_item_set_visible_vertical"
		      : cptr * bool -> unit;
	val set_visible_vertical : 'a t -> bool -> unit
	    = fn self => fn visible_vertical =>
		 GObject.withPtr (self, 
				  fn self => set_visible_vertical_
					       (self, visible_vertical))
	val get_visible_vertical_ : cptr -> bool
	    = _import "gtk_tool_item_get_visible_vertical" : cptr -> bool;
	val get_visible_vertical : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_visible_vertical_ self)
	val set_is_important_ : cptr * bool -> unit
	    = _import "gtk_tool_item_set_is_important" : cptr * bool -> unit;
	val set_is_important : 'a t -> bool -> unit
	    = fn self => fn is_important =>
		 GObject.withPtr
		   (self, fn self => set_is_important_ (self, is_important))
	val get_is_important_ : cptr -> bool
	    = _import "gtk_tool_item_get_is_important" : cptr -> bool;
	val get_is_important : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_is_important_ self)
	val get_icon_size_ : cptr -> int
	    = _import "gtk_tool_item_get_icon_size" : cptr -> int;
	val get_icon_size : 'a t -> icon_size
	    = fn self => GObject.withPtr (self, fn self => get_icon_size_ self)
	val get_orientation_ : cptr -> int
	    = _import "gtk_tool_item_get_orientation" : cptr -> int;
	val get_orientation : 'a t -> orientation
	    = fn self => GObject.withPtr
			   (self, fn self => get_orientation_ self)
	val get_relief_style_ : cptr -> int
	    = _import "gtk_tool_item_get_relief_style" : cptr -> int;
	val get_relief_style : 'a t -> relief_style
	    = fn self => GObject.withPtr
			   (self, fn self => get_relief_style_ self)
	val retrieve_proxy_menu_item_ : cptr -> cptr
	    = _import "gtk_tool_item_retrieve_proxy_menu_item" : cptr -> cptr;
	val retrieve_proxy_menu_item : 'a t -> base Widget.t
	    = fn self =>
		 Widget.inherit
		   ()
		   (fn () =>
		       GObject.withPtr
			 (self, fn self => retrieve_proxy_menu_item_ self))
	val set_proxy_menu_item_ : cptr * CString.cstring * cptr -> unit
	    = _import "gtk_tool_item_set_proxy_menu_item"
		      : cptr * CString.cstring * cptr -> unit;
	val set_proxy_menu_item : 'a t -> string -> 'b Widget.t option -> unit
	    = fn self => fn menu_item_id => fn menu_item =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withOpt
				 (menu_item, 
				  fn menu_item =>
				     set_proxy_menu_item_
				       (self, CString.fromString menu_item_id, 
					menu_item)))
	val set_proxy_menu_item' : 'a t -> string -> unit
	    = fn self => fn menu_item_id =>
		 GObject.withPtr
		   (self, 
		    fn self => set_proxy_menu_item_
				 (self, CString.fromString menu_item_id, 
				  GObject.null))
	val get_proxy_menu_item_ : cptr * CString.cstring -> cptr
	    = _import "gtk_tool_item_get_proxy_menu_item"
		      : cptr * CString.cstring -> cptr;
	val get_proxy_menu_item : 'a t -> string -> base Widget.t
	    = fn self => fn menu_item_id =>
		 Widget.inherit ()
			        (fn () => GObject.withPtr
					    (self, 
					     fn self => get_proxy_menu_item_
							  (self, 
							   CString.fromString
							     menu_item_id)))
	local open Signal
	      infixr -->
	in val create_menu_proxy_sig : (unit -> bool) -> 'a t Signal.signal
	       = fn f =>
		    signal "create-menu-proxy" false (void --> return_bool) f
	   val toolbar_reconfigured_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "toolbar-reconfigured" false
			        (void --> return_void) f
	   val set_tooltip_sig : (unit -> char -> char -> bool)
				 -> 'a t Signal.signal
	       = fn f => signal "set-tooltip" false
			        (unit --> char --> char --> return_bool) f
	end
    end
    structure ToolButton :>
      sig
	type base
	type 'a toolbutton_t
	type 'a t = 'a toolbutton_t ToolItem.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toToolButton : 'a t -> base t
	val get_type : unit -> GType.t
	val new : 'a Widget.t option -> string option -> base t
	val new' : unit -> base t
	val new_from_stock : string -> base t
	val set_label : 'a t -> string option -> unit
	val set_label' : 'a t -> unit
	val get_label : 'a t -> string
	val set_use_underline : 'a t -> bool -> unit
	val get_use_underline : 'a t -> bool
	val set_stock_id : 'a t -> string option -> unit
	val set_stock_id' : 'a t -> unit
	val get_stock_id : 'a t -> string
	val set_icon_widget : 'a t -> 'b Widget.t option -> unit
	val set_icon_widget' : 'a t -> unit
	val get_icon_widget : 'a t -> base Widget.t
	val set_label_widget : 'a t -> 'b Widget.t option -> unit
	val set_label_widget' : 'a t -> unit
	val get_label_widget : 'a t -> base Widget.t
	val clicked_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a toolbutton_t = unit
	type 'a t = 'a toolbutton_t ToolItem.t
	fun inherit w con = ToolItem.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toToolButton obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_tool_button_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr * CString.cstring -> cptr
	    = _import "gtk_tool_button_new" : cptr * CString.cstring -> cptr;
	val new : 'a Widget.t option -> string option -> base t
	    = fn icon_widget => fn label =>
		 make (GObject.withOpt (icon_widget, 
					fn icon_widget =>
					   new_ (icon_widget, 
						 CString.fromString
						   (getOpt (label, "")))))
	val new' : unit -> base t
	    = fn dummy => make (new_ (GObject.null, CString.fromString ""))
	val new_from_stock_ : CString.cstring -> cptr
	    = _import "gtk_tool_button_new_from_stock"
		      : CString.cstring -> cptr;
	val new_from_stock : string -> base t
	    = fn stock_id => make (new_from_stock_
				     (CString.fromString stock_id))
	val set_label_ : cptr * CString.cstring -> unit
	    = _import "gtk_tool_button_set_label"
		      : cptr * CString.cstring -> unit;
	val set_label : 'a t -> string option -> unit
	    = fn self => fn label =>
		 GObject.withPtr
		   (self, 
		    fn self => set_label_ (self, 
					   CString.fromString
					     (getOpt (label, ""))))
	val set_label' : 'a t -> unit
	    = fn self =>
		 GObject.withPtr
		   (self, fn self => set_label_ (self, CString.fromString ""))
	val get_label_ : cptr -> CString.t
	    = _import "gtk_tool_button_get_label" : cptr -> CString.t;
	val get_label : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_label_ self
				       in CString.toString t end)
	val set_use_underline_ : cptr * bool -> unit
	    = _import "gtk_tool_button_set_use_underline"
		      : cptr * bool -> unit;
	val set_use_underline : 'a t -> bool -> unit
	    = fn self => fn use_underline =>
		 GObject.withPtr
		   (self, fn self => set_use_underline_ (self, use_underline))
	val get_use_underline_ : cptr -> bool
	    = _import "gtk_tool_button_get_use_underline" : cptr -> bool;
	val get_use_underline : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_use_underline_ self)
	val set_stock_id_ : cptr * CString.cstring -> unit
	    = _import "gtk_tool_button_set_stock_id"
		      : cptr * CString.cstring -> unit;
	val set_stock_id : 'a t -> string option -> unit
	    = fn self => fn stock_id =>
		 GObject.withPtr
		   (self, 
		    fn self => set_stock_id_ (self, 
					      CString.fromString
						(getOpt (stock_id, ""))))
	val set_stock_id' : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => set_stock_id_
					 (self, CString.fromString ""))
	val get_stock_id_ : cptr -> CString.t
	    = _import "gtk_tool_button_get_stock_id" : cptr -> CString.t;
	val get_stock_id : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_stock_id_ self
				       in CString.toString t end)
	val set_icon_widget_ : cptr * cptr -> unit
	    = _import "gtk_tool_button_set_icon_widget" : cptr * cptr -> unit;
	val set_icon_widget : 'a t -> 'b Widget.t option -> unit
	    = fn self => fn icon_widget =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withOpt (icon_widget, 
						fn icon_widget =>
						   set_icon_widget_
						     (self, icon_widget)))
	val set_icon_widget' : 'a t -> unit
	    = fn self =>
		 GObject.withPtr
		   (self, fn self => set_icon_widget_ (self, GObject.null))
	val get_icon_widget_ : cptr -> cptr
	    = _import "gtk_tool_button_get_icon_widget" : cptr -> cptr;
	val get_icon_widget : 'a t -> base Widget.t
	    = fn self =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_icon_widget_ self))
	val set_label_widget_ : cptr * cptr -> unit
	    = _import "gtk_tool_button_set_label_widget" : cptr * cptr -> unit;
	val set_label_widget : 'a t -> 'b Widget.t option -> unit
	    = fn self => fn label_widget =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withOpt (label_widget, 
						fn label_widget =>
						   set_label_widget_
						     (self, label_widget)))
	val set_label_widget' : 'a t -> unit
	    = fn self =>
		 GObject.withPtr
		   (self, fn self => set_label_widget_ (self, GObject.null))
	val get_label_widget_ : cptr -> cptr
	    = _import "gtk_tool_button_get_label_widget" : cptr -> cptr;
	val get_label_widget : 'a t -> base Widget.t
	    = fn self =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_label_widget_ self))
	local open Signal
	      infixr -->
	in val clicked_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "clicked" false (void --> return_void) f
	end
    end
    structure ToggleToolButton :>
      sig
	type base
	type 'a toggletoolbutton_t
	type 'a t = 'a toggletoolbutton_t ToolButton.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toToggleToolButton : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val new_from_stock : string -> base t
	val set_active : 'a t -> bool -> unit
	val get_active : 'a t -> bool
	val toggled_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a toggletoolbutton_t = unit
	type 'a t = 'a toggletoolbutton_t ToolButton.t
	fun inherit w con = ToolButton.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toToggleToolButton obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_toggle_tool_button_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr
	    = _import "gtk_toggle_tool_button_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_from_stock_ : CString.cstring -> cptr
	    = _import "gtk_toggle_tool_button_new_from_stock"
		      : CString.cstring -> cptr;
	val new_from_stock : string -> base t
	    = fn stock_id => make (new_from_stock_
				     (CString.fromString stock_id))
	val set_active_ : cptr * bool -> unit
	    = _import "gtk_toggle_tool_button_set_active"
		      : cptr * bool -> unit;
	val set_active : 'a t -> bool -> unit
	    = fn self => fn is_active =>
		 GObject.withPtr
		   (self, fn self => set_active_ (self, is_active))
	val get_active_ : cptr -> bool
	    = _import "gtk_toggle_tool_button_get_active" : cptr -> bool;
	val get_active : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_active_ self)
	local open Signal
	      infixr -->
	in val toggled_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "toggled" false (void --> return_void) f
	end
    end
    structure RadioToolButton :>
      sig
	type base
	type 'a radiotoolbutton_t
	type 'a t = 'a radiotoolbutton_t ToggleToolButton.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toRadioToolButton : 'a t -> base t
	val get_type : unit -> GType.t
	val new_from_widget : 'a t -> base t
	val new_with_stock_from_widget : 'a t -> string -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a radiotoolbutton_t = unit
	type 'a t = 'a radiotoolbutton_t ToggleToolButton.t
	fun inherit w con = ToggleToolButton.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toRadioToolButton obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_radio_tool_button_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_from_widget_ : cptr -> cptr
	    = _import "gtk_radio_tool_button_new_from_widget" : cptr -> cptr;
	val new_from_widget : 'a t -> base t
	    = fn group => make (GObject.withPtr
				  (group, fn group => new_from_widget_ group))
	val new_with_stock_from_widget_ : cptr * CString.cstring -> cptr
	    = _import "gtk_radio_tool_button_new_with_stock_from_widget"
		      : cptr * CString.cstring -> cptr;
	val new_with_stock_from_widget : 'a t -> string -> base t
	    = fn group => fn stock_id =>
		 make (GObject.withPtr
			 (group, 
			  fn group => new_with_stock_from_widget_
					(group, CString.fromString stock_id)))
    end
    structure ScrolledWindow :>
      sig
	type base
	type 'a scrolledwindow_t
	type 'a t = 'a scrolledwindow_t Bin.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toScrolledWindow : 'a t -> base t
	val get_type : unit -> GType.t
	val new : 'a Adjustment.t -> 'b Adjustment.t -> base t
	val set_hadjustment : 'a t -> 'b Adjustment.t -> unit
	val set_vadjustment : 'a t -> 'b Adjustment.t -> unit
	val get_hadjustment : 'a t -> base Adjustment.t
	val get_vadjustment : 'a t -> base Adjustment.t
	val set_policy : 'a t -> policytype -> policytype -> unit
	val get_policy : 'a t -> policytype * policytype
	val set_placement : 'a t -> cornertype -> unit
	val get_placement : 'a t -> cornertype
	val set_shadowtype : 'a t -> shadowtype -> unit
	val get_shadowtype : 'a t -> shadowtype
	val add_with_viewport : 'a t -> 'b Widget.t -> unit
	val scroll_child_sig : (unit -> bool -> unit) -> 'a t Signal.signal
	val move_focus_out_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a scrolledwindow_t = unit
	type 'a t = 'a scrolledwindow_t Bin.t
	fun inherit w con = Bin.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toScrolledWindow obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_scrolled_window_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr * cptr -> cptr
	    = _import "gtk_scrolled_window_new" : cptr * cptr -> cptr;
	val new : 'a Adjustment.t -> 'b Adjustment.t -> base t
	    = fn hadjustment => fn vadjustment =>
		 make (GObject.withPtr
			 (hadjustment, 
			  fn hadjustment =>
			     GObject.withPtr
			       (vadjustment, 
				fn vadjustment =>
				   new_ (hadjustment, vadjustment))))
	val set_hadjustment_ : cptr * cptr -> unit
	    = _import "gtk_scrolled_window_set_hadjustment"
		      : cptr * cptr -> unit;
	val set_hadjustment : 'a t -> 'b Adjustment.t -> unit
	    = fn self => fn hadjustment =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr (hadjustment, 
						fn hadjustment =>
						   set_hadjustment_
						     (self, hadjustment)))
	val set_vadjustment_ : cptr * cptr -> unit
	    = _import "gtk_scrolled_window_set_vadjustment"
		      : cptr * cptr -> unit;
	val set_vadjustment : 'a t -> 'b Adjustment.t -> unit
	    = fn self => fn hadjustment =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr (hadjustment, 
						fn hadjustment =>
						   set_vadjustment_
						     (self, hadjustment)))
	val get_hadjustment_ : cptr -> cptr
	    = _import "gtk_scrolled_window_get_hadjustment" : cptr -> cptr;
	val get_hadjustment : 'a t -> base Adjustment.t
	    = fn self =>
		 Adjustment.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_hadjustment_ self))
	val get_vadjustment_ : cptr -> cptr
	    = _import "gtk_scrolled_window_get_vadjustment" : cptr -> cptr;
	val get_vadjustment : 'a t -> base Adjustment.t
	    = fn self =>
		 Adjustment.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_vadjustment_ self))
	val set_policy_ : cptr * int * int -> unit
	    = _import "gtk_scrolled_window_set_policy"
		      : cptr * int * int -> unit;
	val set_policy : 'a t -> policytype -> policytype -> unit
	    = fn self => fn hscrollbar_policy => fn vscrollbar_policy =>
		 GObject.withPtr
		   (self, 
		    fn self => set_policy_ (self, hscrollbar_policy, 
					    vscrollbar_policy))
	val get_policy_ : cptr * int * int -> unit
	    = _import "gtk_scrolled_window_get_policy"
		      : cptr * int * int -> unit;
	val get_policy : 'a t -> policytype * policytype
	    = fn self =>
		 let val (hscrollbar_policy, vscrollbar_policy) = (0, 0)
		     val ret = GObject.withPtr
				 (self, 
				  fn self => get_policy_
					       (self, hscrollbar_policy, 
						vscrollbar_policy))
		 in (hscrollbar_policy, vscrollbar_policy) end
	val set_placement_ : cptr * int -> unit
	    = _import "gtk_scrolled_window_set_placement" : cptr * int -> unit;
	val set_placement : 'a t -> cornertype -> unit
	    = fn self => fn window_placement =>
		 GObject.withPtr
		   (self, fn self => set_placement_ (self, window_placement))
	val get_placement_ : cptr -> int
	    = _import "gtk_scrolled_window_get_placement" : cptr -> int;
	val get_placement : 'a t -> cornertype
	    = fn self => GObject.withPtr (self, fn self => get_placement_ self)
	val set_shadowtype_ : cptr * int -> unit
	    = _import "gtk_scrolled_window_set_shadow_type"
		      : cptr * int -> unit;
	val set_shadowtype : 'a t -> shadowtype -> unit
	    = fn self => fn typ =>
		 GObject.withPtr (self, fn self => set_shadowtype_ (self, typ))
	val get_shadowtype_ : cptr -> int
	    = _import "gtk_scrolled_window_get_shadow_type" : cptr -> int;
	val get_shadowtype : 'a t -> shadowtype
	    = fn self => GObject.withPtr
			   (self, fn self => get_shadowtype_ self)
	val add_with_viewport_ : cptr * cptr -> unit
	    = _import "gtk_scrolled_window_add_with_viewport"
		      : cptr * cptr -> unit;
	val add_with_viewport : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr (child, 
						fn child => add_with_viewport_
							      (self, child)))
	local open Signal
	      infixr -->
	in val scroll_child_sig : (unit -> bool -> unit) -> 'a t Signal.signal
	       = fn f => signal "scroll-child" false
			        (unit --> bool --> return_void) f
	   val move_focus_out_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "move-focus-out" false (unit --> return_void) f
	end
    end
    structure SeparatorMenuItem :>
      sig
	type base
	type 'a separatormenuitem_t
	type 'a t = 'a separatormenuitem_t MenuItem.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toSeparatorMenuItem : 'a t -> base t
	val new : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a separatormenuitem_t = unit
	type 'a t = 'a separatormenuitem_t MenuItem.t
	fun inherit w con = MenuItem.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toSeparatorMenuItem obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : unit -> cptr
	    = _import "gtk_separator_menu_item_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure SeparatorToolItem :>
      sig
	type base
	type 'a separatortoolitem_t
	type 'a t = 'a separatortoolitem_t ToolItem.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toSeparatorToolItem : 'a t -> base t
	val new : unit -> base t
	val get_draw : 'a t -> bool
	val set_draw : 'a t -> bool -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a separatortoolitem_t = unit
	type 'a t = 'a separatortoolitem_t ToolItem.t
	fun inherit w con = ToolItem.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toSeparatorToolItem obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : unit -> cptr
	    = _import "gtk_separator_tool_item_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_draw_ : cptr -> bool
	    = _import "gtk_separator_tool_item_get_draw" : cptr -> bool;
	val get_draw : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_draw_ self)
	val set_draw_ : cptr * bool -> unit
	    = _import "gtk_separator_tool_item_set_draw" : cptr * bool -> unit;
	val set_draw : 'a t -> bool -> unit
	    = fn self => fn draw =>
		 GObject.withPtr (self, fn self => set_draw_ (self, draw))
    end
    structure SizeGroup :>
      sig
	type base
	type 'a sizegroup_t
	type 'a t = 'a sizegroup_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toSizeGroup : 'a t -> base t
	type mode
	val NON : mode
	val HORIZONTAL : mode
	val VERTICAL : mode
	val BOTH : mode
	val get_type : unit -> GType.t
	val new : mode -> base t
	val set_mode : 'a t -> mode -> unit
	val get_mode : 'a t -> mode
	val add_widget : 'a t -> 'b Widget.t -> unit
	val remove_widget : 'a t -> 'b Widget.t -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a sizegroup_t = unit
	type 'a t = 'a sizegroup_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toSizeGroup obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type mode = int
	val get_mode_ : int ref * int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_size_group_mode"
		      : int ref * int ref * int ref * int ref -> unit;
	val (NON, HORIZONTAL, VERTICAL, BOTH)
	    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	      in get_mode_ (x0, x1, x2, x3)
	       ; (!x0, !x1, !x2, !x3)
	      end
	val get_type_ : unit -> GType.t
	    = _import "gtk_size_group_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : int -> cptr = _import "gtk_size_group_new" : int -> cptr;
	val new : mode -> base t = fn mode => make (new_ mode)
	val set_mode_ : cptr * int -> unit
	    = _import "gtk_size_group_set_mode" : cptr * int -> unit;
	val set_mode : 'a t -> mode -> unit
	    = fn self => fn mode =>
		 GObject.withPtr (self, fn self => set_mode_ (self, mode))
	val get_mode_ : cptr -> int
	    = _import "gtk_size_group_get_mode" : cptr -> int;
	val get_mode : 'a t -> mode
	    = fn self => GObject.withPtr (self, fn self => get_mode_ self)
	val add_widget_ : cptr * cptr -> unit
	    = _import "gtk_size_group_add_widget" : cptr * cptr -> unit;
	val add_widget : 'a t -> 'b Widget.t -> unit
	    = fn self => fn widget =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (widget, fn widget => add_widget_ (self, widget)))
	val remove_widget_ : cptr * cptr -> unit
	    = _import "gtk_size_group_remove_widget" : cptr * cptr -> unit;
	val remove_widget : 'a t -> 'b Widget.t -> unit
	    = fn self => fn widget =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (widget, fn widget => remove_widget_ (self, widget)))
    end
    structure Socket :>
      sig
	type base
	type 'a socket_t
	type 'a t = 'a socket_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toSocket : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val plug_added_sig : (unit -> unit) -> 'a t Signal.signal
	val plug_removed_sig : (unit -> bool) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a socket_t = unit
	type 'a t = 'a socket_t Container.t
	fun inherit w con = Container.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toSocket obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_socket_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_socket_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	local open Signal
	      infixr -->
	in val plug_added_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "plug-added" false (void --> return_void) f
	   val plug_removed_sig : (unit -> bool) -> 'a t Signal.signal
	       = fn f => signal "plug-removed" false (void --> return_bool) f
	end
    end
    structure SpinButton :>
      sig
	type base
	type 'a spinbutton_t
	type 'a t = 'a spinbutton_t Entry.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toSpinButton : 'a t -> base t
	type update_policy
	val UPDATE_ALWAYS : update_policy
	val UPDATE_IF_VALID : update_policy
	val get_type : unit -> GType.t
	val configure : 'a t -> 'b Adjustment.t option -> real -> int -> unit
	val configure' : 'a t -> real -> int -> unit
	val new : 'a Adjustment.t -> real -> int -> base t
	val new_with_range : real -> real -> real -> base t
	val set_adjustment : 'a t -> 'b Adjustment.t -> unit
	val get_adjustment : 'a t -> base Adjustment.t
	val set_digits : 'a t -> int -> unit
	val get_digits : 'a t -> int
	val set_increments : 'a t -> real -> real -> unit
	val set_range : 'a t -> real -> real -> unit
	val get_value : 'a t -> real
	val get_value_as_int : 'a t -> int
	val set_value : 'a t -> real -> unit
	val set_update_policy : 'a t -> update_policy -> unit
	val get_update_policy : 'a t -> int
	val set_numeric : 'a t -> bool -> unit
	val get_numeric : 'a t -> bool
	val spin : 'a t -> spintype -> real -> unit
	val set_wrap : 'a t -> bool -> unit
	val get_wrap : 'a t -> bool
	val set_snap_to_ticks : 'a t -> bool -> unit
	val get_snap_to_ticks : 'a t -> bool
	val update : 'a t -> unit
	val input_sig : (real -> int) -> 'a t Signal.signal
	val output_sig : (unit -> bool) -> 'a t Signal.signal
	val value_changed_sig : (unit -> unit) -> 'a t Signal.signal
	val change_value_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a spinbutton_t = unit
	type 'a t = 'a spinbutton_t Entry.t
	fun inherit w con = Entry.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toSpinButton obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type update_policy = int
	val get_update_policy_ : int ref * int ref -> unit
	    = _import "mgtk_get_gtk_spin_button_update_policy"
		      : int ref * int ref -> unit;
	val (UPDATE_ALWAYS, UPDATE_IF_VALID)
	    = let val (x0, x1) = (ref 0, ref 0) in get_update_policy_ (x0, x1)
						 ; (!x0, !x1)
						end
	val get_type_ : unit -> GType.t
	    = _import "gtk_spin_button_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val configure_ : cptr * cptr * real * int -> unit
	    = _import "gtk_spin_button_configure"
		      : cptr * cptr * real * int -> unit;
	val configure : 'a t -> 'b Adjustment.t option -> real -> int -> unit
	    = fn self => fn adjustment => fn climb_rate => fn digits =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withOpt
				 (adjustment, 
				  fn adjustment => configure_
						     (self, adjustment, 
						      climb_rate, digits)))
	val configure' : 'a t -> real -> int -> unit
	    = fn self => fn climb_rate => fn digits =>
		 GObject.withPtr (self, 
				  fn self => configure_ (self, GObject.null, 
							 climb_rate, digits))
	val new_ : cptr * real * int -> cptr
	    = _import "gtk_spin_button_new" : cptr * real * int -> cptr;
	val new : 'a Adjustment.t -> real -> int -> base t
	    = fn adjustment => fn climb_rate => fn digits =>
		 make (GObject.withPtr
			 (adjustment, 
			  fn adjustment =>
			     new_ (adjustment, climb_rate, digits)))
	val new_with_range_ : real * real * real -> cptr
	    = _import "gtk_spin_button_new_with_range"
		      : real * real * real -> cptr;
	val new_with_range : real -> real -> real -> base t
	    = fn min => fn max => fn step =>
		 make (new_with_range_ (min, max, step))
	val set_adjustment_ : cptr * cptr -> unit
	    = _import "gtk_spin_button_set_adjustment" : cptr * cptr -> unit;
	val set_adjustment : 'a t -> 'b Adjustment.t -> unit
	    = fn self => fn adjustment =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (adjustment, 
				  fn adjustment =>
				     set_adjustment_ (self, adjustment)))
	val get_adjustment_ : cptr -> cptr
	    = _import "gtk_spin_button_get_adjustment" : cptr -> cptr;
	val get_adjustment : 'a t -> base Adjustment.t
	    = fn self => Adjustment.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_adjustment_ self))
	val set_digits_ : cptr * int -> unit
	    = _import "gtk_spin_button_set_digits" : cptr * int -> unit;
	val set_digits : 'a t -> int -> unit
	    = fn self => fn digits =>
		 GObject.withPtr (self, fn self => set_digits_ (self, digits))
	val get_digits_ : cptr -> int
	    = _import "gtk_spin_button_get_digits" : cptr -> int;
	val get_digits : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_digits_ self)
	val set_increments_ : cptr * real * real -> unit
	    = _import "gtk_spin_button_set_increments"
		      : cptr * real * real -> unit;
	val set_increments : 'a t -> real -> real -> unit
	    = fn self => fn step => fn page =>
		 GObject.withPtr
		   (self, fn self => set_increments_ (self, step, page))
	val set_range_ : cptr * real * real -> unit
	    = _import "gtk_spin_button_set_range" : cptr * real * real -> unit;
	val set_range : 'a t -> real -> real -> unit
	    = fn self => fn min => fn max =>
		 GObject.withPtr (self, fn self => set_range_ (self, min, max))
	val get_value_ : cptr -> real
	    = _import "gtk_spin_button_get_value" : cptr -> real;
	val get_value : 'a t -> real
	    = fn self => GObject.withPtr (self, fn self => get_value_ self)
	val get_value_as_int_ : cptr -> int
	    = _import "gtk_spin_button_get_value_as_int" : cptr -> int;
	val get_value_as_int : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_value_as_int_ self)
	val set_value_ : cptr * real -> unit
	    = _import "gtk_spin_button_set_value" : cptr * real -> unit;
	val set_value : 'a t -> real -> unit
	    = fn self => fn value =>
		 GObject.withPtr (self, fn self => set_value_ (self, value))
	val set_update_policy_ : cptr * int -> unit
	    = _import "gtk_spin_button_set_update_policy" : cptr * int -> unit;
	val set_update_policy : 'a t -> update_policy -> unit
	    = fn self => fn policy =>
		 GObject.withPtr
		   (self, fn self => set_update_policy_ (self, policy))
	val get_update_policy_ : cptr -> int
	    = _import "gtk_spin_button_get_update_policy" : cptr -> int;
	val get_update_policy : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_update_policy_ self)
	val set_numeric_ : cptr * bool -> unit
	    = _import "gtk_spin_button_set_numeric" : cptr * bool -> unit;
	val set_numeric : 'a t -> bool -> unit
	    = fn self => fn numeric =>
		 GObject.withPtr
		   (self, fn self => set_numeric_ (self, numeric))
	val get_numeric_ : cptr -> bool
	    = _import "gtk_spin_button_get_numeric" : cptr -> bool;
	val get_numeric : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_numeric_ self)
	val spin_ : cptr * int * real -> unit
	    = _import "gtk_spin_button_spin" : cptr * int * real -> unit;
	val spin : 'a t -> spintype -> real -> unit
	    = fn self => fn direction => fn increment =>
		 GObject.withPtr
		   (self, fn self => spin_ (self, direction, increment))
	val set_wrap_ : cptr * bool -> unit
	    = _import "gtk_spin_button_set_wrap" : cptr * bool -> unit;
	val set_wrap : 'a t -> bool -> unit
	    = fn self => fn wrap =>
		 GObject.withPtr (self, fn self => set_wrap_ (self, wrap))
	val get_wrap_ : cptr -> bool
	    = _import "gtk_spin_button_get_wrap" : cptr -> bool;
	val get_wrap : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_wrap_ self)
	val set_snap_to_ticks_ : cptr * bool -> unit
	    = _import "gtk_spin_button_set_snap_to_ticks"
		      : cptr * bool -> unit;
	val set_snap_to_ticks : 'a t -> bool -> unit
	    = fn self => fn snap_to_ticks =>
		 GObject.withPtr
		   (self, fn self => set_snap_to_ticks_ (self, snap_to_ticks))
	val get_snap_to_ticks_ : cptr -> bool
	    = _import "gtk_spin_button_get_snap_to_ticks" : cptr -> bool;
	val get_snap_to_ticks : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_snap_to_ticks_ self)
	val update_ : cptr -> unit
	    = _import "gtk_spin_button_update" : cptr -> unit;
	val update : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => update_ self)
	local open Signal
	      infixr -->
	in val input_sig : (real -> int) -> 'a t Signal.signal
	       = fn f => signal "input" false (real --> return_int) f
	   val output_sig : (unit -> bool) -> 'a t Signal.signal
	       = fn f => signal "output" false (void --> return_bool) f
	   val value_changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "value-changed" false (void --> return_void) f
	   val change_value_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "change-value" false (unit --> return_void) f
	end
    end
    structure Statusbar :>
      sig
	type base
	type 'a statusbar_t
	type 'a t = 'a statusbar_t HBox.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toStatusbar : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val get_context_id : 'a t -> string -> int
	val push : 'a t -> int -> string -> int
	val pop : 'a t -> int -> unit
	val remove : 'a t -> int -> int -> unit
	val set_has_resize_grip : 'a t -> bool -> unit
	val get_has_resize_grip : 'a t -> bool
	val text_pushed_sig : (int -> char -> unit) -> 'a t Signal.signal
	val text_popped_sig : (int -> char -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a statusbar_t = unit
	type 'a t = 'a statusbar_t HBox.t
	fun inherit w con = HBox.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toStatusbar obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_statusbar_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_statusbar_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_context_id_ : cptr * CString.cstring -> int
	    = _import "gtk_statusbar_get_context_id"
		      : cptr * CString.cstring -> int;
	val get_context_id : 'a t -> string -> int
	    = fn self => fn context_description =>
		 GObject.withPtr
		   (self, 
		    fn self => get_context_id_ (self, 
						CString.fromString
						  context_description))
	val push_ : cptr * int * CString.cstring -> int
	    = _import "gtk_statusbar_push"
		      : cptr * int * CString.cstring -> int;
	val push : 'a t -> int -> string -> int
	    = fn self => fn context_id => fn text =>
		 GObject.withPtr (self, 
				  fn self => push_ (self, context_id, 
						    CString.fromString text))
	val pop_ : cptr * int -> unit
	    = _import "gtk_statusbar_pop" : cptr * int -> unit;
	val pop : 'a t -> int -> unit
	    = fn self => fn context_id =>
		 GObject.withPtr (self, fn self => pop_ (self, context_id))
	val remove_ : cptr * int * int -> unit
	    = _import "gtk_statusbar_remove" : cptr * int * int -> unit;
	val remove : 'a t -> int -> int -> unit
	    = fn self => fn context_id => fn message_id =>
		 GObject.withPtr
		   (self, fn self => remove_ (self, context_id, message_id))
	val set_has_resize_grip_ : cptr * bool -> unit
	    = _import "gtk_statusbar_set_has_resize_grip"
		      : cptr * bool -> unit;
	val set_has_resize_grip : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_has_resize_grip_ (self, setting))
	val get_has_resize_grip_ : cptr -> bool
	    = _import "gtk_statusbar_get_has_resize_grip" : cptr -> bool;
	val get_has_resize_grip : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_has_resize_grip_ self)
	local open Signal
	      infixr -->
	in val text_pushed_sig : (int -> char -> unit) -> 'a t Signal.signal
	       = fn f => signal "text-pushed" false
			        (int --> char --> return_void) f
	   val text_popped_sig : (int -> char -> unit) -> 'a t Signal.signal
	       = fn f => signal "text-popped" false
			        (int --> char --> return_void) f
	end
    end
    structure Table :>
      sig
	type base
	type 'a table_t
	type 'a t = 'a table_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTable : 'a t -> base t
	val get_type : unit -> GType.t
	val new : int -> int -> bool -> base t
	val resize : 'a t -> int -> int -> unit
	val set_row_spacing : 'a t -> int -> int -> unit
	val get_row_spacing : 'a t -> int -> int
	val set_col_spacing : 'a t -> int -> int -> unit
	val get_col_spacing : 'a t -> int -> int
	val set_row_spacings : 'a t -> int -> unit
	val get_default_row_spacing : 'a t -> int
	val set_col_spacings : 'a t -> int -> unit
	val get_default_col_spacing : 'a t -> int
	val set_homogeneous : 'a t -> bool -> unit
	val get_homogeneous : 'a t -> bool
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a table_t = unit
	type 'a t = 'a table_t Container.t
	fun inherit w con = Container.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTable obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_table_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : int * int * bool -> cptr
	    = _import "gtk_table_new" : int * int * bool -> cptr;
	val new : int -> int -> bool -> base t
	    = fn rows => fn columns => fn homogeneous =>
		 make (new_ (rows, columns, homogeneous))
	val resize_ : cptr * int * int -> unit
	    = _import "gtk_table_resize" : cptr * int * int -> unit;
	val resize : 'a t -> int -> int -> unit
	    = fn self => fn rows => fn columns =>
		 GObject.withPtr
		   (self, fn self => resize_ (self, rows, columns))
	val set_row_spacing_ : cptr * int * int -> unit
	    = _import "gtk_table_set_row_spacing" : cptr * int * int -> unit;
	val set_row_spacing : 'a t -> int -> int -> unit
	    = fn self => fn row => fn spacing =>
		 GObject.withPtr
		   (self, fn self => set_row_spacing_ (self, row, spacing))
	val get_row_spacing_ : cptr * int -> int
	    = _import "gtk_table_get_row_spacing" : cptr * int -> int;
	val get_row_spacing : 'a t -> int -> int
	    = fn self => fn row =>
		 GObject.withPtr
		   (self, fn self => get_row_spacing_ (self, row))
	val set_col_spacing_ : cptr * int * int -> unit
	    = _import "gtk_table_set_col_spacing" : cptr * int * int -> unit;
	val set_col_spacing : 'a t -> int -> int -> unit
	    = fn self => fn column => fn spacing =>
		 GObject.withPtr
		   (self, fn self => set_col_spacing_ (self, column, spacing))
	val get_col_spacing_ : cptr * int -> int
	    = _import "gtk_table_get_col_spacing" : cptr * int -> int;
	val get_col_spacing : 'a t -> int -> int
	    = fn self => fn column =>
		 GObject.withPtr
		   (self, fn self => get_col_spacing_ (self, column))
	val set_row_spacings_ : cptr * int -> unit
	    = _import "gtk_table_set_row_spacings" : cptr * int -> unit;
	val set_row_spacings : 'a t -> int -> unit
	    = fn self => fn spacing =>
		 GObject.withPtr
		   (self, fn self => set_row_spacings_ (self, spacing))
	val get_default_row_spacing_ : cptr -> int
	    = _import "gtk_table_get_default_row_spacing" : cptr -> int;
	val get_default_row_spacing : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_default_row_spacing_ self)
	val set_col_spacings_ : cptr * int -> unit
	    = _import "gtk_table_set_col_spacings" : cptr * int -> unit;
	val set_col_spacings : 'a t -> int -> unit
	    = fn self => fn spacing =>
		 GObject.withPtr
		   (self, fn self => set_col_spacings_ (self, spacing))
	val get_default_col_spacing_ : cptr -> int
	    = _import "gtk_table_get_default_col_spacing" : cptr -> int;
	val get_default_col_spacing : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_default_col_spacing_ self)
	val set_homogeneous_ : cptr * bool -> unit
	    = _import "gtk_table_set_homogeneous" : cptr * bool -> unit;
	val set_homogeneous : 'a t -> bool -> unit
	    = fn self => fn homogeneous =>
		 GObject.withPtr
		   (self, fn self => set_homogeneous_ (self, homogeneous))
	val get_homogeneous_ : cptr -> bool
	    = _import "gtk_table_get_homogeneous" : cptr -> bool;
	val get_homogeneous : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_homogeneous_ self)
    end
    structure TearoffMenuItem :>
      sig
	type base
	type 'a tearoffmenuitem_t
	type 'a t = 'a tearoffmenuitem_t MenuItem.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTearoffMenuItem : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a tearoffmenuitem_t = unit
	type 'a t = 'a tearoffmenuitem_t MenuItem.t
	fun inherit w con = MenuItem.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTearoffMenuItem obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_tearoff_menu_item_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr
	    = _import "gtk_tearoff_menu_item_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure TextMark :>
      sig
	type base
	type 'a textmark_t
	type 'a t = 'a textmark_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTextMark : 'a t -> base t
	val get_type : unit -> GType.t
	val set_visible : 'a t -> bool -> unit
	val get_visible : 'a t -> bool
	val get_name : 'a t -> string
	val get_deleted : 'a t -> bool
	val get_buffer : 'a t -> base t
	val get_left_gravity : 'a t -> bool
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a textmark_t = unit
	type 'a t = 'a textmark_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTextMark obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_text_mark_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_visible_ : cptr * bool -> unit
	    = _import "gtk_text_mark_set_visible" : cptr * bool -> unit;
	val set_visible : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_visible_ (self, setting))
	val get_visible_ : cptr -> bool
	    = _import "gtk_text_mark_get_visible" : cptr -> bool;
	val get_visible : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_visible_ self)
	val get_name_ : cptr -> CString.t
	    = _import "gtk_text_mark_get_name" : cptr -> CString.t;
	val get_name : 'a t -> string
	    = fn self => GObject.withPtr (self, 
					  fn self => let val t = get_name_ self
						     in CString.toString t end)
	val get_deleted_ : cptr -> bool
	    = _import "gtk_text_mark_get_deleted" : cptr -> bool;
	val get_deleted : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_deleted_ self)
	val get_buffer_ : cptr -> cptr
	    = _import "gtk_text_mark_get_buffer" : cptr -> cptr;
	val get_buffer : 'a t -> base t
	    = fn self => make (GObject.withPtr
				 (self, fn self => get_buffer_ self))
	val get_left_gravity_ : cptr -> bool
	    = _import "gtk_text_mark_get_left_gravity" : cptr -> bool;
	val get_left_gravity : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_left_gravity_ self)
    end
    structure TextTagTable :>
      sig
	type base
	type 'a texttagtable_t
	type 'a t = 'a texttagtable_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTextTagTable : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val add : 'a t -> 'b TextTag.t -> unit
	val remove : 'a t -> 'b TextTag.t -> unit
	val lookup : 'a t -> string -> base TextTag.t
	val get_size : 'a t -> int
	val tag_changed_sig : (unit -> bool -> unit) -> 'a t Signal.signal
	val tag_added_sig : (unit -> unit) -> 'a t Signal.signal
	val tag_removed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a texttagtable_t = unit
	type 'a t = 'a texttagtable_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTextTagTable obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_text_tag_table_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr
	    = _import "gtk_text_tag_table_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val add_ : cptr * cptr -> unit
	    = _import "gtk_text_tag_table_add" : cptr * cptr -> unit;
	val add : 'a t -> 'b TextTag.t -> unit
	    = fn self => fn tag =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (tag, fn tag => add_ (self, tag)))
	val remove_ : cptr * cptr -> unit
	    = _import "gtk_text_tag_table_remove" : cptr * cptr -> unit;
	val remove : 'a t -> 'b TextTag.t -> unit
	    = fn self => fn tag =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (tag, fn tag => remove_ (self, tag)))
	val lookup_ : cptr * CString.cstring -> cptr
	    = _import "gtk_text_tag_table_lookup"
		      : cptr * CString.cstring -> cptr;
	val lookup : 'a t -> string -> base TextTag.t
	    = fn self => fn name =>
		 TextTag.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self =>
				   lookup_ (self, CString.fromString name)))
	val get_size_ : cptr -> int
	    = _import "gtk_text_tag_table_get_size" : cptr -> int;
	val get_size : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_size_ self)
	local open Signal
	      infixr -->
	in val tag_changed_sig : (unit -> bool -> unit) -> 'a t Signal.signal
	       = fn f => signal "tag-changed" false
			        (unit --> bool --> return_void) f
	   val tag_added_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "tag-added" false (unit --> return_void) f
	   val tag_removed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "tag-removed" false (unit --> return_void) f
	end
    end
    structure TextBuffer :>
      sig
	type base
	type 'a textbuffer_t
	type 'a t = 'a textbuffer_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTextBuffer : 'a t -> base t
	val get_type : unit -> GType.t
	val new : 'a TextTagTable.t -> base t
	val get_line_count : 'a t -> int
	val get_char_count : 'a t -> int
	val get_tag_table : 'a t -> base TextTagTable.t
	val set_text : 'a t -> string -> int -> unit
	val insert : 'a t -> TextIter.t -> string -> int -> unit
	val insert' : 'a t -> TextIter.t -> string -> unit
	val insert_at_cursor : 'a t -> string -> int -> unit
	val insert_at_cursor' : 'a t -> string -> unit
	val insert_interactive
	  : 'a t -> TextIter.t -> string -> int -> bool -> bool
	val insert_interactive_at_cursor
	  : 'a t -> string -> int -> bool -> bool
	val insert_range
	  : 'a t -> TextIter.t -> TextIter.t -> TextIter.t -> unit
	val insert_range_interactive
	  : 'a t -> TextIter.t -> TextIter.t -> TextIter.t -> bool -> bool
	val insert_with_tags
	  : 'a t -> TextIter.t -> string -> int -> 'b TextTag.t -> unit
	val insert_with_tags_by_name
	  : 'a t -> TextIter.t -> string -> int -> string -> unit
	val delete : 'a t -> TextIter.t -> TextIter.t -> unit
	val delete_interactive
	  : 'a t -> TextIter.t -> TextIter.t -> bool -> bool
	val get_text : 'a t -> TextIter.t -> TextIter.t -> bool -> string
	val get_text' : 'a t -> TextIter.t -> TextIter.t -> string
	val get_slice : 'a t -> TextIter.t -> TextIter.t -> bool -> string
	val get_slice' : 'a t -> TextIter.t -> TextIter.t -> string
	val insert_child_anchor
	  : 'a t -> TextIter.t -> 'b TextChildAnchor.t -> unit
	val create_child_anchor : 'a t -> TextIter.t -> base TextChildAnchor.t
	val create_mark : 'a t -> string option -> TextIter.t -> bool
			  -> base TextMark.t
	val create_mark' : 'a t -> TextIter.t -> base TextMark.t
	val move_mark : 'a t -> 'b TextMark.t -> TextIter.t -> unit
	val delete_mark : 'a t -> 'b TextMark.t -> unit
	val get_mark : 'a t -> string -> base TextMark.t
	val move_mark_by_name : 'a t -> string -> TextIter.t -> unit
	val delete_mark_by_name : 'a t -> string -> unit
	val get_insert : 'a t -> base TextMark.t
	val get_selection_bound : 'a t -> base TextMark.t
	val place_cursor : 'a t -> TextIter.t -> unit
	val select_range : 'a t -> TextIter.t -> TextIter.t -> unit
	val apply_tag
	  : 'a t -> 'b TextTag.t -> TextIter.t -> TextIter.t -> unit
	val remove_tag
	  : 'a t -> 'b TextTag.t -> TextIter.t -> TextIter.t -> unit
	val apply_tag_by_name
	  : 'a t -> string -> TextIter.t -> TextIter.t -> unit
	val remove_tag_by_name
	  : 'a t -> string -> TextIter.t -> TextIter.t -> unit
	val remove_all_tags : 'a t -> TextIter.t -> TextIter.t -> unit
	val create_tag : 'a t -> string -> string -> base TextTag.t
	val getiter_at_line_offset : 'a t -> int -> int -> TextIter.t
	val getiter_at_line_index : 'a t -> int -> int -> TextIter.t
	val getiter_at_offset : 'a t -> int -> TextIter.t
	val getiter_at_line : 'a t -> int -> TextIter.t
	val get_startiter : 'a t -> TextIter.t
	val get_enditer : 'a t -> TextIter.t
	val get_bounds : 'a t -> TextIter.t * TextIter.t
	val getiter_at_mark : 'a t -> 'b TextMark.t -> TextIter.t
	val getiter_at_child_anchor
	  : 'a t -> 'b TextChildAnchor.t -> TextIter.t
	val get_modified : 'a t -> bool
	val set_modified : 'a t -> bool -> unit
	val add_selection_clipboard : 'a t -> 'b Clipboard.t -> unit
	val remove_selection_clipboard : 'a t -> 'b Clipboard.t -> unit
	val cut_clipboard : 'a t -> 'b Clipboard.t -> bool -> unit
	val copy_clipboard : 'a t -> 'b Clipboard.t -> unit
	val paste_clipboard
	  : 'a t -> 'b Clipboard.t -> TextIter.t option -> bool -> unit
	val paste_clipboard' : 'a t -> 'b Clipboard.t -> bool -> unit
	val get_selection_bounds : 'a t -> bool * TextIter.t * TextIter.t
	val delete_selection : 'a t -> bool -> bool -> bool
	val begin_user_action : 'a t -> unit
	val end_user_action : 'a t -> unit
	val changed_sig : (unit -> unit) -> 'a t Signal.signal
	val insert_text_sig : (unit -> char -> int -> unit)
			      -> 'a t Signal.signal
	val insert_pixbuf_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	val insert_child_anchor_sig
	  : (unit -> unit -> unit) -> 'a t Signal.signal
	val delete_range_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	val modified_changed_sig : (unit -> unit) -> 'a t Signal.signal
	val mark_set_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	val mark_deleted_sig : (unit -> unit) -> 'a t Signal.signal
	val apply_tag_sig : (unit -> unit -> unit -> unit)
			    -> 'a t Signal.signal
	val remove_tag_sig : (unit -> unit -> unit -> unit)
			     -> 'a t Signal.signal
	val begin_user_action_sig : (unit -> unit) -> 'a t Signal.signal
	val end_user_action_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a textbuffer_t = unit
	type 'a t = 'a textbuffer_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTextBuffer obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_text_buffer_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr -> cptr = _import "gtk_text_buffer_new" : cptr -> cptr;
	val new : 'a TextTagTable.t -> base t
	    = fn table => make (GObject.withPtr
				  (table, fn table => new_ table))
	val get_line_count_ : cptr -> int
	    = _import "gtk_text_buffer_get_line_count" : cptr -> int;
	val get_line_count : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_line_count_ self)
	val get_char_count_ : cptr -> int
	    = _import "gtk_text_buffer_get_char_count" : cptr -> int;
	val get_char_count : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_char_count_ self)
	val get_tag_table_ : cptr -> cptr
	    = _import "gtk_text_buffer_get_tag_table" : cptr -> cptr;
	val get_tag_table : 'a t -> base TextTagTable.t
	    = fn self => TextTagTable.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_tag_table_ self))
	val set_text_ : cptr * CString.cstring * int -> unit
	    = _import "gtk_text_buffer_set_text"
		      : cptr * CString.cstring * int -> unit;
	val set_text : 'a t -> string -> int -> unit
	    = fn self => fn text => fn len =>
		 GObject.withPtr
		   (self, 
		    fn self => set_text_ (self, CString.fromString text, len))
	val insert_ : cptr * cptr * CString.cstring * int -> unit
	    = _import "gtk_text_buffer_insert"
		      : cptr * cptr * CString.cstring * int -> unit;
	val insert : 'a t -> TextIter.t -> string -> int -> unit
	    = fn self => fn iter => fn text => fn len =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       insert_ (self, iter, CString.fromString text, len))
	val insert' : 'a t -> TextIter.t -> string -> unit
	    = fn self => fn iter => fn text =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       insert_ (self, iter, CString.fromString text, ~1))
	val insert_at_cursor_ : cptr * CString.cstring * int -> unit
	    = _import "gtk_text_buffer_insert_at_cursor"
		      : cptr * CString.cstring * int -> unit;
	val insert_at_cursor : 'a t -> string -> int -> unit
	    = fn self => fn text => fn len =>
		 GObject.withPtr
		   (self, 
		    fn self => insert_at_cursor_
				 (self, CString.fromString text, len))
	val insert_at_cursor' : 'a t -> string -> unit
	    = fn self => fn text =>
		 GObject.withPtr
		   (self, 
		    fn self => insert_at_cursor_
				 (self, CString.fromString text, ~1))
	val insert_interactive_
	  : cptr * cptr * CString.cstring * int * bool -> bool
	    = _import "gtk_text_buffer_insert_interactive"
		      : cptr * cptr * CString.cstring * int * bool -> bool;
	val insert_interactive
	  : 'a t -> TextIter.t -> string -> int -> bool -> bool
	    = fn self => fn iter => fn text => fn len => fn default_editable =>
		 GObject.withPtr
		   (self, 
		    fn self => insert_interactive_
				 (self, iter, CString.fromString text, len, 
				  default_editable))
	val insert_interactive_at_cursor_
	  : cptr * CString.cstring * int * bool -> bool
	    = _import "gtk_text_buffer_insert_interactive_at_cursor"
		      : cptr * CString.cstring * int * bool -> bool;
	val insert_interactive_at_cursor
	  : 'a t -> string -> int -> bool -> bool
	    = fn self => fn text => fn len => fn default_editable =>
		 GObject.withPtr (self, 
				  fn self => insert_interactive_at_cursor_
					       (self, CString.fromString text, 
						len, default_editable))
	val insert_range_ : cptr * cptr * cptr * cptr -> unit
	    = _import "gtk_text_buffer_insert_range"
		      : cptr * cptr * cptr * cptr -> unit;
	val insert_range
	  : 'a t -> TextIter.t -> TextIter.t -> TextIter.t -> unit
	    = fn self => fn iter => fn start => fn en =>
		 GObject.withPtr
		   (self, fn self => insert_range_ (self, iter, start, en))
	val insert_range_interactive_
	  : cptr * cptr * cptr * cptr * bool -> bool
	    = _import "gtk_text_buffer_insert_range_interactive"
		      : cptr * cptr * cptr * cptr * bool -> bool;
	val insert_range_interactive
	  : 'a t -> TextIter.t -> TextIter.t -> TextIter.t -> bool -> bool
	    = fn self => fn iter => fn start => fn en => fn default_editable =>
		 GObject.withPtr (self, 
				  fn self => insert_range_interactive_
					       (self, iter, start, en, 
						default_editable))
	val insert_with_tags_
	  : cptr * cptr * CString.cstring * int * cptr -> unit
	    = _import "gtk_text_buffer_insert_with_tags"
		      : cptr * cptr * CString.cstring * int * cptr -> unit;
	val insert_with_tags
	  : 'a t -> TextIter.t -> string -> int -> 'b TextTag.t -> unit
	    = fn self => fn iter => fn text => fn len => fn first_tag =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (first_tag, 
				  fn first_tag =>
				     insert_with_tags_
				       (self, iter, CString.fromString text, 
					len, first_tag)))
	val insert_with_tags_by_name_
	  : cptr * cptr * CString.cstring * int * CString.cstring -> unit
	    = _import "gtk_text_buffer_insert_with_tags_by_name"
		      : cptr * cptr * CString.cstring * int * CString.cstring
			-> unit;
	val insert_with_tags_by_name
	  : 'a t -> TextIter.t -> string -> int -> string -> unit
	    = fn self => fn iter => fn text => fn len => fn first_tag_name =>
		 GObject.withPtr
		   (self, 
		    fn self => insert_with_tags_by_name_
				 (self, iter, CString.fromString text, len, 
				  CString.fromString first_tag_name))
	val delete_ : cptr * cptr * cptr -> unit
	    = _import "gtk_text_buffer_delete" : cptr * cptr * cptr -> unit;
	val delete : 'a t -> TextIter.t -> TextIter.t -> unit
	    = fn self => fn start => fn en =>
		 GObject.withPtr (self, fn self => delete_ (self, start, en))
	val delete_interactive_ : cptr * cptr * cptr * bool -> bool
	    = _import "gtk_text_buffer_delete_interactive"
		      : cptr * cptr * cptr * bool -> bool;
	val delete_interactive
	  : 'a t -> TextIter.t -> TextIter.t -> bool -> bool
	    = fn self => fn start_iter => fn end_iter => fn default_editable =>
		 GObject.withPtr (self, 
				  fn self => delete_interactive_
					       (self, start_iter, end_iter, 
						default_editable))
	val get_text_ : cptr * cptr * cptr * bool -> CString.t
	    = _import "gtk_text_buffer_get_text"
		      : cptr * cptr * cptr * bool -> CString.t;
	val get_text : 'a t -> TextIter.t -> TextIter.t -> bool -> string
	    = fn self => fn start => fn en => fn include_hidden_chars =>
		 GObject.withPtr
		   (self, 
		    fn self => let val t = get_text_ (self, start, en, 
						      include_hidden_chars)
			       in CString.toString t end)
	val get_text' : 'a t -> TextIter.t -> TextIter.t -> string
	    = fn self => fn start => fn en =>
		 GObject.withPtr
		   (self, 
		    fn self => let val t = get_text_ (self, start, en, true)
			       in CString.toString t end)
	val get_slice_ : cptr * cptr * cptr * bool -> CString.t
	    = _import "gtk_text_buffer_get_slice"
		      : cptr * cptr * cptr * bool -> CString.t;
	val get_slice : 'a t -> TextIter.t -> TextIter.t -> bool -> string
	    = fn self => fn start => fn en => fn include_hidden_chars =>
		 GObject.withPtr
		   (self, 
		    fn self => let val t = get_slice_ (self, start, en, 
						       include_hidden_chars)
			       in CString.toString t end)
	val get_slice' : 'a t -> TextIter.t -> TextIter.t -> string
	    = fn self => fn start => fn en =>
		 GObject.withPtr
		   (self, 
		    fn self => let val t = get_slice_ (self, start, en, true)
			       in CString.toString t end)
	val insert_child_anchor_ : cptr * cptr * cptr -> unit
	    = _import "gtk_text_buffer_insert_child_anchor"
		      : cptr * cptr * cptr -> unit;
	val insert_child_anchor
	  : 'a t -> TextIter.t -> 'b TextChildAnchor.t -> unit
	    = fn self => fn iter => fn anchor =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (anchor, 
				  fn anchor => insert_child_anchor_
						 (self, iter, anchor)))
	val create_child_anchor_ : cptr * cptr -> cptr
	    = _import "gtk_text_buffer_create_child_anchor"
		      : cptr * cptr -> cptr;
	val create_child_anchor : 'a t -> TextIter.t -> base TextChildAnchor.t
	    = fn self => fn iter =>
		 TextChildAnchor.inherit
		   ()
		   (fn () => GObject.withPtr (self, 
					      fn self => create_child_anchor_
							   (self, iter)))
	val create_mark_ : cptr * CString.cstring * cptr * bool -> cptr
	    = _import "gtk_text_buffer_create_mark"
		      : cptr * CString.cstring * cptr * bool -> cptr;
	val create_mark : 'a t -> string option -> TextIter.t -> bool
			  -> base TextMark.t
	    = fn self => fn mark_name => fn wher => fn left_gravity =>
		 TextMark.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self => create_mark_
					     (self, 
					      CString.fromString
						(getOpt (mark_name, "")), 
					      wher, left_gravity)))
	val create_mark' : 'a t -> TextIter.t -> base TextMark.t
	    = fn self => fn wher =>
		 TextMark.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self => create_mark_
					     (self, CString.fromString "", 
					      wher, false)))
	val move_mark_ : cptr * cptr * cptr -> unit
	    = _import "gtk_text_buffer_move_mark" : cptr * cptr * cptr -> unit;
	val move_mark : 'a t -> 'b TextMark.t -> TextIter.t -> unit
	    = fn self => fn mark => fn wher =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (mark, fn mark => move_mark_ (self, mark, wher)))
	val delete_mark_ : cptr * cptr -> unit
	    = _import "gtk_text_buffer_delete_mark" : cptr * cptr -> unit;
	val delete_mark : 'a t -> 'b TextMark.t -> unit
	    = fn self => fn mark =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (mark, fn mark => delete_mark_ (self, mark)))
	val get_mark_ : cptr * CString.cstring -> cptr
	    = _import "gtk_text_buffer_get_mark"
		      : cptr * CString.cstring -> cptr;
	val get_mark : 'a t -> string -> base TextMark.t
	    = fn self => fn name =>
		 TextMark.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self =>
				   get_mark_ (self, CString.fromString name)))
	val move_mark_by_name_ : cptr * CString.cstring * cptr -> unit
	    = _import "gtk_text_buffer_move_mark_by_name"
		      : cptr * CString.cstring * cptr -> unit;
	val move_mark_by_name : 'a t -> string -> TextIter.t -> unit
	    = fn self => fn name => fn wher =>
		 GObject.withPtr
		   (self, 
		    fn self => move_mark_by_name_
				 (self, CString.fromString name, wher))
	val delete_mark_by_name_ : cptr * CString.cstring -> unit
	    = _import "gtk_text_buffer_delete_mark_by_name"
		      : cptr * CString.cstring -> unit;
	val delete_mark_by_name : 'a t -> string -> unit
	    = fn self => fn name =>
		 GObject.withPtr (self, 
				  fn self => delete_mark_by_name_
					       (self, CString.fromString name))
	val get_insert_ : cptr -> cptr
	    = _import "gtk_text_buffer_get_insert" : cptr -> cptr;
	val get_insert : 'a t -> base TextMark.t
	    = fn self => TextMark.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_insert_ self))
	val get_selection_bound_ : cptr -> cptr
	    = _import "gtk_text_buffer_get_selection_bound" : cptr -> cptr;
	val get_selection_bound : 'a t -> base TextMark.t
	    = fn self =>
		 TextMark.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_selection_bound_ self))
	val place_cursor_ : cptr * cptr -> unit
	    = _import "gtk_text_buffer_place_cursor" : cptr * cptr -> unit;
	val place_cursor : 'a t -> TextIter.t -> unit
	    = fn self => fn wher =>
		 GObject.withPtr (self, fn self => place_cursor_ (self, wher))
	val select_range_ : cptr * cptr * cptr -> unit
	    = _import "gtk_text_buffer_select_range"
		      : cptr * cptr * cptr -> unit;
	val select_range : 'a t -> TextIter.t -> TextIter.t -> unit
	    = fn self => fn ins => fn bound =>
		 GObject.withPtr
		   (self, fn self => select_range_ (self, ins, bound))
	val apply_tag_ : cptr * cptr * cptr * cptr -> unit
	    = _import "gtk_text_buffer_apply_tag"
		      : cptr * cptr * cptr * cptr -> unit;
	val apply_tag
	  : 'a t -> 'b TextTag.t -> TextIter.t -> TextIter.t -> unit
	    = fn self => fn tag => fn start => fn en =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (tag, fn tag => apply_tag_ (self, tag, start, en)))
	val remove_tag_ : cptr * cptr * cptr * cptr -> unit
	    = _import "gtk_text_buffer_remove_tag"
		      : cptr * cptr * cptr * cptr -> unit;
	val remove_tag
	  : 'a t -> 'b TextTag.t -> TextIter.t -> TextIter.t -> unit
	    = fn self => fn tag => fn start => fn en =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (tag, fn tag => remove_tag_ (self, tag, start, en)))
	val apply_tag_by_name_ : cptr * CString.cstring * cptr * cptr -> unit
	    = _import "gtk_text_buffer_apply_tag_by_name"
		      : cptr * CString.cstring * cptr * cptr -> unit;
	val apply_tag_by_name
	  : 'a t -> string -> TextIter.t -> TextIter.t -> unit
	    = fn self => fn name => fn start => fn en =>
		 GObject.withPtr
		   (self, 
		    fn self => apply_tag_by_name_
				 (self, CString.fromString name, start, en))
	val remove_tag_by_name_ : cptr * CString.cstring * cptr * cptr -> unit
	    = _import "gtk_text_buffer_remove_tag_by_name"
		      : cptr * CString.cstring * cptr * cptr -> unit;
	val remove_tag_by_name
	  : 'a t -> string -> TextIter.t -> TextIter.t -> unit
	    = fn self => fn name => fn start => fn en =>
		 GObject.withPtr
		   (self, 
		    fn self => remove_tag_by_name_
				 (self, CString.fromString name, start, en))
	val remove_all_tags_ : cptr * cptr * cptr -> unit
	    = _import "gtk_text_buffer_remove_all_tags"
		      : cptr * cptr * cptr -> unit;
	val remove_all_tags : 'a t -> TextIter.t -> TextIter.t -> unit
	    = fn self => fn start => fn en =>
		 GObject.withPtr
		   (self, fn self => remove_all_tags_ (self, start, en))
	val create_tag_ : cptr * CString.cstring * CString.cstring -> cptr
	    = _import "gtk_text_buffer_create_tag"
		      : cptr * CString.cstring * CString.cstring -> cptr;
	val create_tag : 'a t -> string -> string -> base TextTag.t
	    = fn self => fn tag_name => fn first_property_name =>
		 TextTag.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self =>
				   create_tag_
				     (self, CString.fromString tag_name, 
				      CString.fromString first_property_name)))
	val getiter_at_line_offset_ : cptr * cptr * int * int -> unit
	    = _import "gtk_text_buffer_get_iter_at_line_offset"
		      : cptr * cptr * int * int -> unit;
	val getiter_at_line_offset : 'a t -> int -> int -> TextIter.t
	    = fn self => fn line_number => fn char_offset =>
		 let val iter = TextIter.alloc_GtkTextIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => getiter_at_line_offset_
					       (self, iter, line_number, 
						char_offset))
		 in iter end
	val getiter_at_line_index_ : cptr * cptr * int * int -> unit
	    = _import "gtk_text_buffer_get_iter_at_line_index"
		      : cptr * cptr * int * int -> unit;
	val getiter_at_line_index : 'a t -> int -> int -> TextIter.t
	    = fn self => fn line_number => fn byte_index =>
		 let val iter = TextIter.alloc_GtkTextIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => getiter_at_line_index_
					       (self, iter, line_number, 
						byte_index))
		 in iter end
	val getiter_at_offset_ : cptr * cptr * int -> unit
	    = _import "gtk_text_buffer_get_iter_at_offset"
		      : cptr * cptr * int -> unit;
	val getiter_at_offset : 'a t -> int -> TextIter.t
	    = fn self => fn char_offset =>
		 let val iter = TextIter.alloc_GtkTextIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => getiter_at_offset_
					       (self, iter, char_offset))
		 in iter end
	val getiter_at_line_ : cptr * cptr * int -> unit
	    = _import "gtk_text_buffer_get_iter_at_line"
		      : cptr * cptr * int -> unit;
	val getiter_at_line : 'a t -> int -> TextIter.t
	    = fn self => fn line_number =>
		 let val iter = TextIter.alloc_GtkTextIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => getiter_at_line_
					       (self, iter, line_number))
		 in iter end
	val get_startiter_ : cptr * cptr -> unit
	    = _import "gtk_text_buffer_get_start_iter" : cptr * cptr -> unit;
	val get_startiter : 'a t -> TextIter.t
	    = fn self =>
		 let val iter = TextIter.alloc_GtkTextIter ()
		     val ret = GObject.withPtr
				 (self, fn self => get_startiter_ (self, iter))
		 in iter end
	val get_enditer_ : cptr * cptr -> unit
	    = _import "gtk_text_buffer_get_end_iter" : cptr * cptr -> unit;
	val get_enditer : 'a t -> TextIter.t
	    = fn self =>
		 let val iter = TextIter.alloc_GtkTextIter ()
		     val ret = GObject.withPtr
				 (self, fn self => get_enditer_ (self, iter))
		 in iter end
	val get_bounds_ : cptr * cptr * cptr -> unit
	    = _import "gtk_text_buffer_get_bounds"
		      : cptr * cptr * cptr -> unit;
	val get_bounds : 'a t -> TextIter.t * TextIter.t
	    = fn self => let val (start, en) = (TextIter.alloc_GtkTextIter (), 
						TextIter.alloc_GtkTextIter ())
			     val ret = GObject.withPtr
					 (self, 
					  fn self => get_bounds_
						       (self, start, en))
			 in (start, en) end
	val getiter_at_mark_ : cptr * cptr * cptr -> unit
	    = _import "gtk_text_buffer_get_iter_at_mark"
		      : cptr * cptr * cptr -> unit;
	val getiter_at_mark : 'a t -> 'b TextMark.t -> TextIter.t
	    = fn self => fn mark =>
		 let val iter = TextIter.alloc_GtkTextIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self =>
				     GObject.withPtr
				       (mark, 
					fn mark => getiter_at_mark_
						     (self, iter, mark)))
		 in iter end
	val getiter_at_child_anchor_ : cptr * cptr * cptr -> unit
	    = _import "gtk_text_buffer_get_iter_at_child_anchor"
		      : cptr * cptr * cptr -> unit;
	val getiter_at_child_anchor
	  : 'a t -> 'b TextChildAnchor.t -> TextIter.t
	    = fn self => fn anchor =>
		 let val iter = TextIter.alloc_GtkTextIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => GObject.withPtr
					       (anchor, 
						fn anchor =>
						   getiter_at_child_anchor_
						     (self, iter, anchor)))
		 in iter end
	val get_modified_ : cptr -> bool
	    = _import "gtk_text_buffer_get_modified" : cptr -> bool;
	val get_modified : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_modified_ self)
	val set_modified_ : cptr * bool -> unit
	    = _import "gtk_text_buffer_set_modified" : cptr * bool -> unit;
	val set_modified : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_modified_ (self, setting))
	val add_selection_clipboard_ : cptr * cptr -> unit
	    = _import "gtk_text_buffer_add_selection_clipboard"
		      : cptr * cptr -> unit;
	val add_selection_clipboard : 'a t -> 'b Clipboard.t -> unit
	    = fn self => fn clipboard =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (clipboard, 
				  fn clipboard => add_selection_clipboard_
						    (self, clipboard)))
	val remove_selection_clipboard_ : cptr * cptr -> unit
	    = _import "gtk_text_buffer_remove_selection_clipboard"
		      : cptr * cptr -> unit;
	val remove_selection_clipboard : 'a t -> 'b Clipboard.t -> unit
	    = fn self => fn clipboard =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (clipboard, 
				  fn clipboard => remove_selection_clipboard_
						    (self, clipboard)))
	val cut_clipboard_ : cptr * cptr * bool -> unit
	    = _import "gtk_text_buffer_cut_clipboard"
		      : cptr * cptr * bool -> unit;
	val cut_clipboard : 'a t -> 'b Clipboard.t -> bool -> unit
	    = fn self => fn clipboard => fn default_editable =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (clipboard, 
				  fn clipboard =>
				     cut_clipboard_ (self, clipboard, 
						     default_editable)))
	val copy_clipboard_ : cptr * cptr -> unit
	    = _import "gtk_text_buffer_copy_clipboard" : cptr * cptr -> unit;
	val copy_clipboard : 'a t -> 'b Clipboard.t -> unit
	    = fn self => fn clipboard =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (clipboard, 
				  fn clipboard => copy_clipboard_
						    (self, clipboard)))
	val paste_clipboard_ : cptr * cptr * cptr * bool -> unit
	    = _import "gtk_text_buffer_paste_clipboard"
		      : cptr * cptr * cptr * bool -> unit;
	val paste_clipboard
	  : 'a t -> 'b Clipboard.t -> TextIter.t option -> bool -> unit
	    = fn self => fn clipboard => fn override_location => 
	      fn default_editable =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (clipboard, 
				  fn clipboard =>
				     paste_clipboard_
				       (self, clipboard, 
					getOpt (override_location, 
						GObject.null), 
					default_editable)))
	val paste_clipboard' : 'a t -> 'b Clipboard.t -> bool -> unit
	    = fn self => fn clipboard => fn default_editable =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (clipboard, 
				  fn clipboard =>
				     paste_clipboard_
				       (self, clipboard, GObject.null, 
					default_editable)))
	val get_selection_bounds_ : cptr * cptr * cptr -> bool
	    = _import "gtk_text_buffer_get_selection_bounds"
		      : cptr * cptr * cptr -> bool;
	val get_selection_bounds : 'a t -> bool * TextIter.t * TextIter.t
	    = fn self => let val (start, en) = (TextIter.alloc_GtkTextIter (), 
						TextIter.alloc_GtkTextIter ())
			     val ret = GObject.withPtr
					 (self, 
					  fn self => get_selection_bounds_
						       (self, start, en))
			 in (ret, start, en) end
	val delete_selection_ : cptr * bool * bool -> bool
	    = _import "gtk_text_buffer_delete_selection"
		      : cptr * bool * bool -> bool;
	val delete_selection : 'a t -> bool -> bool -> bool
	    = fn self => fn interactive => fn default_editable =>
		 GObject.withPtr (self, 
				  fn self => delete_selection_
					       (self, interactive, 
						default_editable))
	val begin_user_action_ : cptr -> unit
	    = _import "gtk_text_buffer_begin_user_action" : cptr -> unit;
	val begin_user_action : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => begin_user_action_ self)
	val end_user_action_ : cptr -> unit
	    = _import "gtk_text_buffer_end_user_action" : cptr -> unit;
	val end_user_action : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => end_user_action_ self)
	local open Signal
	      infixr -->
	in
	  val changed_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "changed" false (void --> return_void) f
	  val insert_text_sig : (unit -> char -> int -> unit)
				-> 'a t Signal.signal
	      = fn f => signal "insert-text" false
			       (unit --> char --> int --> return_void) f
	  val insert_pixbuf_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "insert-pixbuf" false
			       (unit --> unit --> return_void) f
	  val insert_child_anchor_sig
	    : (unit -> unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "insert-child-anchor" false
			       (unit --> unit --> return_void) f
	  val delete_range_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "delete-range" false
			       (unit --> unit --> return_void) f
	  val modified_changed_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f =>
		   signal "modified-changed" false (void --> return_void) f
	  val mark_set_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	      = fn f =>
		   signal "mark-set" false (unit --> unit --> return_void) f
	  val mark_deleted_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "mark-deleted" false (unit --> return_void) f
	  val apply_tag_sig : (unit -> unit -> unit -> unit)
			      -> 'a t Signal.signal
	      = fn f => signal "apply-tag" false
			       (unit --> unit --> unit --> return_void) f
	  val remove_tag_sig : (unit -> unit -> unit -> unit)
			       -> 'a t Signal.signal
	      = fn f => signal "remove-tag" false
			       (unit --> unit --> unit --> return_void) f
	  val begin_user_action_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f =>
		   signal "begin-user-action" false (void --> return_void) f
	  val end_user_action_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "end-user-action" false (void --> return_void) f
	end
    end
    structure TextView :>
      sig
	type base
	type 'a textview_t
	type 'a t = 'a textview_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTextView : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val new_withbuffer : 'a TextBuffer.t -> base t
	val setbuffer : 'a t -> 'b TextBuffer.t -> unit
	val get_buffer : 'a t -> base TextBuffer.t
	val scroll_toiter
	  : 'a t -> TextIter.t -> real -> bool -> real -> real -> bool
	val scroll_toiter' : 'a t -> TextIter.t -> real -> bool
	val scroll_to_mark
	  : 'a t -> 'b TextMark.t -> real -> bool -> real -> real -> unit
	val scroll_to_mark' : 'a t -> 'b TextMark.t -> real -> unit
	val scroll_mark_onscreen : 'a t -> 'b TextMark.t -> unit
	val move_mark_onscreen : 'a t -> 'b TextMark.t -> bool
	val place_cursor_onscreen : 'a t -> bool
	val set_cursor_visible : 'a t -> bool -> unit
	val get_cursor_visible : 'a t -> bool
	val getiter_at_location : 'a t -> int -> int -> TextIter.t
	val set_border_window_size : 'a t -> text_window_type_t -> int -> unit
	val get_border_window_size : 'a t -> text_window_type_t -> int
	val forward_display_line : 'a t -> TextIter.t -> bool
	val backward_display_line : 'a t -> TextIter.t -> bool
	val forward_display_line_end : 'a t -> TextIter.t -> bool
	val backward_display_line_start : 'a t -> TextIter.t -> bool
	val starts_display_line : 'a t -> TextIter.t -> bool
	val move_visually : 'a t -> TextIter.t -> int -> bool
	val add_child_at_anchor
	  : 'a t -> 'b Widget.t -> 'c TextChildAnchor.t -> unit
	val add_child_in_window
	  : 'a t -> 'b Widget.t -> text_window_type_t -> int -> int -> unit
	val move_child : 'a t -> 'b Widget.t -> int -> int -> unit
	val set_wrap_mode : 'a t -> wrap_mode -> unit
	val get_wrap_mode : 'a t -> wrap_mode
	val set_editable : 'a t -> bool -> unit
	val get_editable : 'a t -> bool
	val set_overwrite : 'a t -> bool -> unit
	val get_overwrite : 'a t -> bool
	val set_accepts_tab : 'a t -> bool -> unit
	val get_accepts_tab : 'a t -> bool
	val set_pixels_above_lines : 'a t -> int -> unit
	val get_pixels_above_lines : 'a t -> int
	val set_pixels_below_lines : 'a t -> int -> unit
	val get_pixels_below_lines : 'a t -> int
	val set_pixels_inside_wrap : 'a t -> int -> unit
	val get_pixels_inside_wrap : 'a t -> int
	val set_justification : 'a t -> justification -> unit
	val get_justification : 'a t -> justification
	val set_left_margin : 'a t -> int -> unit
	val get_left_margin : 'a t -> int
	val set_right_margin : 'a t -> int -> unit
	val get_right_margin : 'a t -> int
	val set_indent : 'a t -> int -> unit
	val get_indent : 'a t -> int
	val get_default_attributes : 'a t -> TextAttributes.t
	val move_focus_sig : (unit -> unit) -> 'a t Signal.signal
	val set_scroll_adjustments_sig
	  : (unit -> unit -> unit) -> 'a t Signal.signal
	val move_cursor_sig : (unit -> int -> bool -> unit)
			      -> 'a t Signal.signal
	val select_all_sig : (bool -> unit) -> 'a t Signal.signal
	val page_horizontally_sig : (int -> bool -> unit) -> 'a t Signal.signal
	val move_viewport_sig : (unit -> int -> unit) -> 'a t Signal.signal
	val set_anchor_sig : (unit -> unit) -> 'a t Signal.signal
	val insert_at_cursor_sig : (char -> unit) -> 'a t Signal.signal
	val delete_from_cursor_sig
	  : (unit -> int -> unit) -> 'a t Signal.signal
	val cut_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	val copy_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	val paste_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	val toggle_overwrite_sig : (unit -> unit) -> 'a t Signal.signal
	val populate_popup_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a textview_t = unit
	type 'a t = 'a textview_t Container.t
	fun inherit w con = Container.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTextView obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_text_view_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_text_view_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_withbuffer_ : cptr -> cptr
	    = _import "gtk_text_view_new_with_buffer" : cptr -> cptr;
	val new_withbuffer : 'a TextBuffer.t -> base t
	    = fn buffer =>
		 make (GObject.withPtr
			 (buffer, fn buffer => new_withbuffer_ buffer))
	val setbuffer_ : cptr * cptr -> unit
	    = _import "gtk_text_view_set_buffer" : cptr * cptr -> unit;
	val setbuffer : 'a t -> 'b TextBuffer.t -> unit
	    = fn self => fn buffer =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (buffer, fn buffer => setbuffer_ (self, buffer)))
	val get_buffer_ : cptr -> cptr
	    = _import "gtk_text_view_get_buffer" : cptr -> cptr;
	val get_buffer : 'a t -> base TextBuffer.t
	    = fn self => TextBuffer.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_buffer_ self))
	val scroll_toiter_ : cptr * cptr * real * bool * real * real -> bool
	    = _import "gtk_text_view_scroll_to_iter"
		      : cptr * cptr * real * bool * real * real -> bool;
	val scroll_toiter
	  : 'a t -> TextIter.t -> real -> bool -> real -> real -> bool
	    = fn self => fn iter => fn within_margin => fn use_align => 
	      fn xalign => fn yalign =>
		 GObject.withPtr
		   (self, 
		    fn self => scroll_toiter_ (self, iter, within_margin, 
					       use_align, xalign, yalign))
	val scroll_toiter' : 'a t -> TextIter.t -> real -> bool
	    = fn self => fn iter => fn within_margin =>
		 GObject.withPtr (self, 
				  fn self => scroll_toiter_
					       (self, iter, within_margin, 
						false, 0.5, 0.5))
	val scroll_to_mark_ : cptr * cptr * real * bool * real * real -> unit
	    = _import "gtk_text_view_scroll_to_mark"
		      : cptr * cptr * real * bool * real * real -> unit;
	val scroll_to_mark
	  : 'a t -> 'b TextMark.t -> real -> bool -> real -> real -> unit
	    = fn self => fn mark => fn within_margin => fn use_align => 
	      fn xalign => fn yalign =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (mark, 
				  fn mark => scroll_to_mark_
					       (self, mark, within_margin, 
						use_align, xalign, yalign)))
	val scroll_to_mark' : 'a t -> 'b TextMark.t -> real -> unit
	    = fn self => fn mark => fn within_margin =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (mark, 
				  fn mark => scroll_to_mark_
					       (self, mark, within_margin, 
						false, 0.5, 0.5)))
	val scroll_mark_onscreen_ : cptr * cptr -> unit
	    = _import "gtk_text_view_scroll_mark_onscreen"
		      : cptr * cptr -> unit;
	val scroll_mark_onscreen : 'a t -> 'b TextMark.t -> unit
	    = fn self => fn mark =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (mark, fn mark => scroll_mark_onscreen_ (self, mark)))
	val move_mark_onscreen_ : cptr * cptr -> bool
	    = _import "gtk_text_view_move_mark_onscreen" : cptr * cptr -> bool;
	val move_mark_onscreen : 'a t -> 'b TextMark.t -> bool
	    = fn self => fn mark =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (mark, fn mark => move_mark_onscreen_ (self, mark)))
	val place_cursor_onscreen_ : cptr -> bool
	    = _import "gtk_text_view_place_cursor_onscreen" : cptr -> bool;
	val place_cursor_onscreen : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => place_cursor_onscreen_ self)
	val set_cursor_visible_ : cptr * bool -> unit
	    = _import "gtk_text_view_set_cursor_visible" : cptr * bool -> unit;
	val set_cursor_visible : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_cursor_visible_ (self, setting))
	val get_cursor_visible_ : cptr -> bool
	    = _import "gtk_text_view_get_cursor_visible" : cptr -> bool;
	val get_cursor_visible : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_cursor_visible_ self)
	val getiter_at_location_ : cptr * cptr * int * int -> unit
	    = _import "gtk_text_view_get_iter_at_location"
		      : cptr * cptr * int * int -> unit;
	val getiter_at_location : 'a t -> int -> int -> TextIter.t
	    = fn self => fn x => fn y =>
		 let val iter = TextIter.alloc_GtkTextIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => getiter_at_location_
					       (self, iter, x, y))
		 in iter end
	val set_border_window_size_ : cptr * int * int -> unit
	    = _import "gtk_text_view_set_border_window_size"
		      : cptr * int * int -> unit;
	val set_border_window_size : 'a t -> text_window_type_t -> int -> unit
	    = fn self => fn typ => fn size =>
		 GObject.withPtr
		   (self, fn self => set_border_window_size_ (self, typ, size))
	val get_border_window_size_ : cptr * int -> int
	    = _import "gtk_text_view_get_border_window_size"
		      : cptr * int -> int;
	val get_border_window_size : 'a t -> text_window_type_t -> int
	    = fn self => fn typ =>
		 GObject.withPtr
		   (self, fn self => get_border_window_size_ (self, typ))
	val forward_display_line_ : cptr * cptr -> bool
	    = _import "gtk_text_view_forward_display_line"
		      : cptr * cptr -> bool;
	val forward_display_line : 'a t -> TextIter.t -> bool
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, fn self => forward_display_line_ (self, iter))
	val backward_display_line_ : cptr * cptr -> bool
	    = _import "gtk_text_view_backward_display_line"
		      : cptr * cptr -> bool;
	val backward_display_line : 'a t -> TextIter.t -> bool
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, fn self => backward_display_line_ (self, iter))
	val forward_display_line_end_ : cptr * cptr -> bool
	    = _import "gtk_text_view_forward_display_line_end"
		      : cptr * cptr -> bool;
	val forward_display_line_end : 'a t -> TextIter.t -> bool
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, fn self => forward_display_line_end_ (self, iter))
	val backward_display_line_start_ : cptr * cptr -> bool
	    = _import "gtk_text_view_backward_display_line_start"
		      : cptr * cptr -> bool;
	val backward_display_line_start : 'a t -> TextIter.t -> bool
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, fn self => backward_display_line_start_ (self, iter))
	val starts_display_line_ : cptr * cptr -> bool
	    = _import "gtk_text_view_starts_display_line"
		      : cptr * cptr -> bool;
	val starts_display_line : 'a t -> TextIter.t -> bool
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, fn self => starts_display_line_ (self, iter))
	val move_visually_ : cptr * cptr * int -> bool
	    = _import "gtk_text_view_move_visually"
		      : cptr * cptr * int -> bool;
	val move_visually : 'a t -> TextIter.t -> int -> bool
	    = fn self => fn iter => fn count =>
		 GObject.withPtr
		   (self, fn self => move_visually_ (self, iter, count))
	val add_child_at_anchor_ : cptr * cptr * cptr -> unit
	    = _import "gtk_text_view_add_child_at_anchor"
		      : cptr * cptr * cptr -> unit;
	val add_child_at_anchor
	  : 'a t -> 'b Widget.t -> 'c TextChildAnchor.t -> unit
	    = fn self => fn child => fn anchor =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => GObject.withPtr
						(anchor, 
						 fn anchor =>
						    add_child_at_anchor_
						      (self, child, anchor))))
	val add_child_in_window_ : cptr * cptr * int * int * int -> unit
	    = _import "gtk_text_view_add_child_in_window"
		      : cptr * cptr * int * int * int -> unit;
	val add_child_in_window
	  : 'a t -> 'b Widget.t -> text_window_type_t -> int -> int -> unit
	    = fn self => fn child => fn which_window => fn xpos => fn ypos =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => add_child_in_window_
						(self, child, which_window, 
						 xpos, ypos)))
	val move_child_ : cptr * cptr * int * int -> unit
	    = _import "gtk_text_view_move_child"
		      : cptr * cptr * int * int -> unit;
	val move_child : 'a t -> 'b Widget.t -> int -> int -> unit
	    = fn self => fn child => fn xpos => fn ypos =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (child, 
				  fn child => move_child_
						(self, child, xpos, ypos)))
	val set_wrap_mode_ : cptr * int -> unit
	    = _import "gtk_text_view_set_wrap_mode" : cptr * int -> unit;
	val set_wrap_mode : 'a t -> wrap_mode -> unit
	    = fn self => fn wrap_mode =>
		 GObject.withPtr
		   (self, fn self => set_wrap_mode_ (self, wrap_mode))
	val get_wrap_mode_ : cptr -> int
	    = _import "gtk_text_view_get_wrap_mode" : cptr -> int;
	val get_wrap_mode : 'a t -> wrap_mode
	    = fn self => GObject.withPtr (self, fn self => get_wrap_mode_ self)
	val set_editable_ : cptr * bool -> unit
	    = _import "gtk_text_view_set_editable" : cptr * bool -> unit;
	val set_editable : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_editable_ (self, setting))
	val get_editable_ : cptr -> bool
	    = _import "gtk_text_view_get_editable" : cptr -> bool;
	val get_editable : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_editable_ self)
	val set_overwrite_ : cptr * bool -> unit
	    = _import "gtk_text_view_set_overwrite" : cptr * bool -> unit;
	val set_overwrite : 'a t -> bool -> unit
	    = fn self => fn overwrite =>
		 GObject.withPtr
		   (self, fn self => set_overwrite_ (self, overwrite))
	val get_overwrite_ : cptr -> bool
	    = _import "gtk_text_view_get_overwrite" : cptr -> bool;
	val get_overwrite : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_overwrite_ self)
	val set_accepts_tab_ : cptr * bool -> unit
	    = _import "gtk_text_view_set_accepts_tab" : cptr * bool -> unit;
	val set_accepts_tab : 'a t -> bool -> unit
	    = fn self => fn accepts_tab =>
		 GObject.withPtr
		   (self, fn self => set_accepts_tab_ (self, accepts_tab))
	val get_accepts_tab_ : cptr -> bool
	    = _import "gtk_text_view_get_accepts_tab" : cptr -> bool;
	val get_accepts_tab : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_accepts_tab_ self)
	val set_pixels_above_lines_ : cptr * int -> unit
	    = _import "gtk_text_view_set_pixels_above_lines"
		      : cptr * int -> unit;
	val set_pixels_above_lines : 'a t -> int -> unit
	    = fn self => fn pixels_above_lines =>
		 GObject.withPtr (self, 
				  fn self => set_pixels_above_lines_
					       (self, pixels_above_lines))
	val get_pixels_above_lines_ : cptr -> int
	    = _import "gtk_text_view_get_pixels_above_lines" : cptr -> int;
	val get_pixels_above_lines : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_pixels_above_lines_ self)
	val set_pixels_below_lines_ : cptr * int -> unit
	    = _import "gtk_text_view_set_pixels_below_lines"
		      : cptr * int -> unit;
	val set_pixels_below_lines : 'a t -> int -> unit
	    = fn self => fn pixels_below_lines =>
		 GObject.withPtr (self, 
				  fn self => set_pixels_below_lines_
					       (self, pixels_below_lines))
	val get_pixels_below_lines_ : cptr -> int
	    = _import "gtk_text_view_get_pixels_below_lines" : cptr -> int;
	val get_pixels_below_lines : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_pixels_below_lines_ self)
	val set_pixels_inside_wrap_ : cptr * int -> unit
	    = _import "gtk_text_view_set_pixels_inside_wrap"
		      : cptr * int -> unit;
	val set_pixels_inside_wrap : 'a t -> int -> unit
	    = fn self => fn pixels_inside_wrap =>
		 GObject.withPtr (self, 
				  fn self => set_pixels_inside_wrap_
					       (self, pixels_inside_wrap))
	val get_pixels_inside_wrap_ : cptr -> int
	    = _import "gtk_text_view_get_pixels_inside_wrap" : cptr -> int;
	val get_pixels_inside_wrap : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_pixels_inside_wrap_ self)
	val set_justification_ : cptr * int -> unit
	    = _import "gtk_text_view_set_justification" : cptr * int -> unit;
	val set_justification : 'a t -> justification -> unit
	    = fn self => fn justification =>
		 GObject.withPtr
		   (self, fn self => set_justification_ (self, justification))
	val get_justification_ : cptr -> int
	    = _import "gtk_text_view_get_justification" : cptr -> int;
	val get_justification : 'a t -> justification
	    = fn self => GObject.withPtr
			   (self, fn self => get_justification_ self)
	val set_left_margin_ : cptr * int -> unit
	    = _import "gtk_text_view_set_left_margin" : cptr * int -> unit;
	val set_left_margin : 'a t -> int -> unit
	    = fn self => fn left_margin =>
		 GObject.withPtr
		   (self, fn self => set_left_margin_ (self, left_margin))
	val get_left_margin_ : cptr -> int
	    = _import "gtk_text_view_get_left_margin" : cptr -> int;
	val get_left_margin : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_left_margin_ self)
	val set_right_margin_ : cptr * int -> unit
	    = _import "gtk_text_view_set_right_margin" : cptr * int -> unit;
	val set_right_margin : 'a t -> int -> unit
	    = fn self => fn right_margin =>
		 GObject.withPtr
		   (self, fn self => set_right_margin_ (self, right_margin))
	val get_right_margin_ : cptr -> int
	    = _import "gtk_text_view_get_right_margin" : cptr -> int;
	val get_right_margin : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_right_margin_ self)
	val set_indent_ : cptr * int -> unit
	    = _import "gtk_text_view_set_indent" : cptr * int -> unit;
	val set_indent : 'a t -> int -> unit
	    = fn self => fn indent =>
		 GObject.withPtr (self, fn self => set_indent_ (self, indent))
	val get_indent_ : cptr -> int
	    = _import "gtk_text_view_get_indent" : cptr -> int;
	val get_indent : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_indent_ self)
	val get_default_attributes_ : cptr -> cptr
	    = _import "gtk_text_view_get_default_attributes" : cptr -> cptr;
	val get_default_attributes : 'a t -> TextAttributes.t
	    = fn self => GObject.withPtr
			   (self, fn self => get_default_attributes_ self)
	local open Signal
	      infixr -->
	in
	  val move_focus_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "move-focus" false (unit --> return_void) f
	  val set_scroll_adjustments_sig
	    : (unit -> unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "set-scroll-adjustments" false
			       (unit --> unit --> return_void) f
	  val move_cursor_sig : (unit -> int -> bool -> unit)
				-> 'a t Signal.signal
	      = fn f => signal "move-cursor" false
			       (unit --> int --> bool --> return_void) f
	  val select_all_sig : (bool -> unit) -> 'a t Signal.signal
	      = fn f => signal "select-all" false (bool --> return_void) f
	  val page_horizontally_sig
	    : (int -> bool -> unit) -> 'a t Signal.signal
	      = fn f => signal "page-horizontally" false
			       (int --> bool --> return_void) f
	  val move_viewport_sig : (unit -> int -> unit) -> 'a t Signal.signal
	      = fn f => signal "move-viewport" false
			       (unit --> int --> return_void) f
	  val set_anchor_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "set-anchor" false (void --> return_void) f
	  val insert_at_cursor_sig : (char -> unit) -> 'a t Signal.signal
	      = fn f =>
		   signal "insert-at-cursor" false (char --> return_void) f
	  val delete_from_cursor_sig
	    : (unit -> int -> unit) -> 'a t Signal.signal
	      = fn f => signal "delete-from-cursor" false
			       (unit --> int --> return_void) f
	  val cut_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "cut-clipboard" false (void --> return_void) f
	  val copy_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "copy-clipboard" false (void --> return_void) f
	  val paste_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "paste-clipboard" false (void --> return_void) f
	  val toggle_overwrite_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f =>
		   signal "toggle-overwrite" false (void --> return_void) f
	  val populate_popup_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "populate-popup" false (unit --> return_void) f
	end
    end
    structure Toolbar :>
      sig
	type base
	type 'a toolbar_t
	type 'a t = 'a toolbar_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toToolbar : 'a t -> base t
	type childtype
	val CHILD_SPACE : childtype
	val CHILD_BUTTON : childtype
	val CHILD_TOGGLEBUTTON : childtype
	val CHILD_RADIOBUTTON : childtype
	val CHILD_WIDGET : childtype
	type space_style
	val SPACE_EMPTY : space_style
	val SPACE_LINE : space_style
	type style
	val ICONS : style
	val TEXT : style
	val BOTH : style
	val BOTH_HORIZ : style
	val get_type : unit -> GType.t
	val new : unit -> base t
	val insert : 'a t -> 'b ToolItem.t -> int -> unit
	val get_item_index : 'a t -> 'b ToolItem.t -> int
	val get_n_items : 'a t -> int
	val get_nth_item : 'a t -> int -> base ToolItem.t
	val get_drop_index : 'a t -> int -> int -> int
	val set_drop_highlight_item
	  : 'a t -> 'b ToolItem.t option -> int -> unit
	val set_drop_highlight_item' : 'a t -> int -> unit
	val set_show_arrow : 'a t -> bool -> unit
	val get_show_arrow : 'a t -> bool
	val get_relief_style : 'a t -> relief_style
	val set_orientation : 'a t -> orientation -> unit
	val set_style : 'a t -> style -> unit
	val set_tooltips : 'a t -> bool -> unit
	val unset_style : 'a t -> unit
	val get_orientation : 'a t -> orientation
	val get_style : 'a t -> style
	val get_icon_size : 'a t -> icon_size
	val get_tooltips : 'a t -> bool
	val move_focus_sig : (unit -> bool) -> 'a t Signal.signal
	val orientation_changed_sig : (unit -> unit) -> 'a t Signal.signal
	val style_changed_sig : (unit -> unit) -> 'a t Signal.signal
	val popup_context_menu_sig
	  : (int -> int -> int -> bool) -> 'a t Signal.signal
	val focus_home_or_end_sig : (bool -> bool) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a toolbar_t = unit
	type 'a t = 'a toolbar_t Container.t
	fun inherit w con = Container.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toToolbar obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type childtype = int
	val get_childtype_
	  : int ref * int ref * int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_toolbar_childtype"
		      : int ref * int ref * int ref * int ref * int ref
			-> unit;
	val (CHILD_SPACE, CHILD_BUTTON, CHILD_TOGGLEBUTTON, CHILD_RADIOBUTTON, 
	     CHILD_WIDGET)
	    = let val (x0, x1, x2, x3, x4)
		      = (ref 0, ref 0, ref 0, ref 0, ref 0)
	      in get_childtype_ (x0, x1, x2, x3, x4)
	       ; (!x0, !x1, !x2, !x3, !x4)
	      end
	type space_style = int
	val get_space_style_ : int ref * int ref -> unit
	    = _import "mgtk_get_gtk_toolbar_space_style"
		      : int ref * int ref -> unit;
	val (SPACE_EMPTY, SPACE_LINE) = let val (x0, x1) = (ref 0, ref 0)
					in get_space_style_ (x0, x1)
					 ; (!x0, !x1)
					end
	type style = int
	val get_style_ : int ref * int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_toolbar_style"
		      : int ref * int ref * int ref * int ref -> unit;
	val (ICONS, TEXT, BOTH, BOTH_HORIZ)
	    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	      in get_style_ (x0, x1, x2, x3)
	       ; (!x0, !x1, !x2, !x3)
	      end
	val get_type_ : unit -> GType.t
	    = _import "gtk_toolbar_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_toolbar_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val insert_ : cptr * cptr * int -> unit
	    = _import "gtk_toolbar_insert" : cptr * cptr * int -> unit;
	val insert : 'a t -> 'b ToolItem.t -> int -> unit
	    = fn self => fn item => fn pos =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (item, fn item => insert_ (self, item, pos)))
	val get_item_index_ : cptr * cptr -> int
	    = _import "gtk_toolbar_get_item_index" : cptr * cptr -> int;
	val get_item_index : 'a t -> 'b ToolItem.t -> int
	    = fn self => fn item =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (item, fn item => get_item_index_ (self, item)))
	val get_n_items_ : cptr -> int
	    = _import "gtk_toolbar_get_n_items" : cptr -> int;
	val get_n_items : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_n_items_ self)
	val get_nth_item_ : cptr * int -> cptr
	    = _import "gtk_toolbar_get_nth_item" : cptr * int -> cptr;
	val get_nth_item : 'a t -> int -> base ToolItem.t
	    = fn self => fn n =>
		 ToolItem.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_nth_item_ (self, n)))
	val get_drop_index_ : cptr * int * int -> int
	    = _import "gtk_toolbar_get_drop_index" : cptr * int * int -> int;
	val get_drop_index : 'a t -> int -> int -> int
	    = fn self => fn x => fn y =>
		 GObject.withPtr
		   (self, fn self => get_drop_index_ (self, x, y))
	val set_drop_highlight_item_ : cptr * cptr * int -> unit
	    = _import "gtk_toolbar_set_drop_highlight_item"
		      : cptr * cptr * int -> unit;
	val set_drop_highlight_item
	  : 'a t -> 'b ToolItem.t option -> int -> unit
	    = fn self => fn tool_item => fn index =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withOpt
				 (tool_item, 
				  fn tool_item => set_drop_highlight_item_
						    (self, tool_item, index)))
	val set_drop_highlight_item' : 'a t -> int -> unit
	    = fn self => fn index =>
		 GObject.withPtr (self, 
				  fn self => set_drop_highlight_item_
					       (self, GObject.null, index))
	val set_show_arrow_ : cptr * bool -> unit
	    = _import "gtk_toolbar_set_show_arrow" : cptr * bool -> unit;
	val set_show_arrow : 'a t -> bool -> unit
	    = fn self => fn show_arrow =>
		 GObject.withPtr
		   (self, fn self => set_show_arrow_ (self, show_arrow))
	val get_show_arrow_ : cptr -> bool
	    = _import "gtk_toolbar_get_show_arrow" : cptr -> bool;
	val get_show_arrow : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_show_arrow_ self)
	val get_relief_style_ : cptr -> int
	    = _import "gtk_toolbar_get_relief_style" : cptr -> int;
	val get_relief_style : 'a t -> relief_style
	    = fn self => GObject.withPtr
			   (self, fn self => get_relief_style_ self)
	val set_orientation_ : cptr * int -> unit
	    = _import "gtk_toolbar_set_orientation" : cptr * int -> unit;
	val set_orientation : 'a t -> orientation -> unit
	    = fn self => fn orientation =>
		 GObject.withPtr
		   (self, fn self => set_orientation_ (self, orientation))
	val set_style_ : cptr * int -> unit
	    = _import "gtk_toolbar_set_style" : cptr * int -> unit;
	val set_style : 'a t -> style -> unit
	    = fn self => fn style =>
		 GObject.withPtr (self, fn self => set_style_ (self, style))
	val set_tooltips_ : cptr * bool -> unit
	    = _import "gtk_toolbar_set_tooltips" : cptr * bool -> unit;
	val set_tooltips : 'a t -> bool -> unit
	    = fn self => fn enable =>
		 GObject.withPtr
		   (self, fn self => set_tooltips_ (self, enable))
	val unset_style_ : cptr -> unit
	    = _import "gtk_toolbar_unset_style" : cptr -> unit;
	val unset_style : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => unset_style_ self)
	val get_orientation_ : cptr -> int
	    = _import "gtk_toolbar_get_orientation" : cptr -> int;
	val get_orientation : 'a t -> orientation
	    = fn self => GObject.withPtr
			   (self, fn self => get_orientation_ self)
	val get_style_ : cptr -> int
	    = _import "gtk_toolbar_get_style" : cptr -> int;
	val get_style : 'a t -> style
	    = fn self => GObject.withPtr (self, fn self => get_style_ self)
	val get_icon_size_ : cptr -> int
	    = _import "gtk_toolbar_get_icon_size" : cptr -> int;
	val get_icon_size : 'a t -> icon_size
	    = fn self => GObject.withPtr (self, fn self => get_icon_size_ self)
	val get_tooltips_ : cptr -> bool
	    = _import "gtk_toolbar_get_tooltips" : cptr -> bool;
	val get_tooltips : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_tooltips_ self)
	local open Signal
	      infixr -->
	in val move_focus_sig : (unit -> bool) -> 'a t Signal.signal
	       = fn f => signal "move-focus" false (unit --> return_bool) f
	   val orientation_changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "orientation-changed" false
			        (unit --> return_void) f
	   val style_changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "style-changed" false (unit --> return_void) f
	   val popup_context_menu_sig
	     : (int -> int -> int -> bool) -> 'a t Signal.signal
	       = fn f => signal "popup-context-menu" false
			        (int --> int --> int --> return_bool) f
	   val focus_home_or_end_sig : (bool -> bool) -> 'a t Signal.signal
	       = fn f =>
		    signal "focus-home-or-end" false (bool --> return_bool) f
	end
    end
    structure TreeModelFilter :>
      sig
	type base
	type 'a treemodelfilter_t
	type 'a t = 'a treemodelfilter_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTreeModelFilter : 'a t -> base t
	val asTreeModel : 'a t -> base TreeModel.t
	val asTreeDragSource : 'a t -> base TreeDragSource.t
	val set_visible_column : 'a t -> int -> unit
	val get_model : 'a t -> base t
	val convert_childiter_toiter : 'a t -> TreeIter.t -> TreeIter.t -> unit
	val convertiter_to_childiter : 'a t -> TreeIter.t -> TreeIter.t -> unit
	val convert_child_path_to_path
	  : 'a t -> 'b TreePath.t -> base TreePath.t
	val convert_path_to_child_path
	  : 'a t -> 'b TreePath.t -> base TreePath.t
	val refilter : 'a t -> unit
	val clear_cache : 'a t -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a treemodelfilter_t = unit
	type 'a t = 'a treemodelfilter_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeModelFilter obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asTreeModel obj
	  = TreeModel.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asTreeDragSource obj
	  = TreeDragSource.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	val set_visible_column_ : cptr * int -> unit
	    = _import "gtk_tree_model_filter_set_visible_column"
		      : cptr * int -> unit;
	val set_visible_column : 'a t -> int -> unit
	    = fn self => fn column =>
		 GObject.withPtr
		   (self, fn self => set_visible_column_ (self, column))
	val get_model_ : cptr -> cptr
	    = _import "gtk_tree_model_filter_get_model" : cptr -> cptr;
	val get_model : 'a t -> base t
	    = fn self => make (GObject.withPtr
				 (self, fn self => get_model_ self))
	val convert_childiter_toiter_ : cptr * cptr * cptr -> unit
	    = _import "gtk_tree_model_filter_convert_child_iter_to_iter"
		      : cptr * cptr * cptr -> unit;
	val convert_childiter_toiter : 'a t -> TreeIter.t -> TreeIter.t -> unit
	    = fn self => fn filter_iter => fn child_iter =>
		 GObject.withPtr (self, 
				  fn self => convert_childiter_toiter_
					       (self, filter_iter, child_iter))
	val convertiter_to_childiter_ : cptr * cptr * cptr -> unit
	    = _import "gtk_tree_model_filter_convert_iter_to_child_iter"
		      : cptr * cptr * cptr -> unit;
	val convertiter_to_childiter : 'a t -> TreeIter.t -> TreeIter.t -> unit
	    = fn self => fn child_iter => fn filter_iter =>
		 GObject.withPtr (self, 
				  fn self => convertiter_to_childiter_
					       (self, child_iter, filter_iter))
	val convert_child_path_to_path_ : cptr * cptr -> cptr
	    = _import "gtk_tree_model_filter_convert_child_path_to_path"
		      : cptr * cptr -> cptr;
	val convert_child_path_to_path
	  : 'a t -> 'b TreePath.t -> base TreePath.t
	    = fn self => fn child_path =>
		 TreePath.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self => GObject.withPtr
					     (child_path, 
					      fn child_path =>
						 convert_child_path_to_path_
						   (self, child_path))))
	val convert_path_to_child_path_ : cptr * cptr -> cptr
	    = _import "gtk_tree_model_filter_convert_path_to_child_path"
		      : cptr * cptr -> cptr;
	val convert_path_to_child_path
	  : 'a t -> 'b TreePath.t -> base TreePath.t
	    = fn self => fn filter_path =>
		 TreePath.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self => GObject.withPtr
					     (filter_path, 
					      fn filter_path =>
						 convert_path_to_child_path_
						   (self, filter_path))))
	val refilter_ : cptr -> unit
	    = _import "gtk_tree_model_filter_refilter" : cptr -> unit;
	val refilter : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => refilter_ self)
	val clear_cache_ : cptr -> unit
	    = _import "gtk_tree_model_filter_clear_cache" : cptr -> unit;
	val clear_cache : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => clear_cache_ self)
    end
    structure TreeModelSort :>
      sig
	type base
	type 'a treemodelsort_t
	type 'a t = 'a treemodelsort_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTreeModelSort : 'a t -> base t
	val asTreeModel : 'a t -> base TreeModel.t
	val asTreeSortable : 'a t -> base TreeSortable.t
	val new_with_model : 'a t -> base t
	val get_model : 'a t -> base t
	val convert_child_path_to_path
	  : 'a t -> 'b TreePath.t -> base TreePath.t
	val convert_childiter_toiter : 'a t -> TreeIter.t -> TreeIter.t
	val convert_path_to_child_path
	  : 'a t -> 'b TreePath.t -> base TreePath.t
	val convertiter_to_childiter : 'a t -> TreeIter.t -> TreeIter.t
	val reset_default_sort_func : 'a t -> unit
	val clear_cache : 'a t -> unit
	val sortiter_is_valid : 'a t -> TreeIter.t -> bool
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a treemodelsort_t = unit
	type 'a t = 'a treemodelsort_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeModelSort obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asTreeModel obj
	  = TreeModel.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asTreeSortable obj
	  = TreeSortable.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_with_model_ : cptr -> cptr
	    = _import "gtk_tree_model_sort_new_with_model" : cptr -> cptr;
	val new_with_model : 'a t -> base t
	    = fn child_model =>
		 make (GObject.withPtr (child_model, 
					fn child_model =>
					   new_with_model_ child_model))
	val get_model_ : cptr -> cptr
	    = _import "gtk_tree_model_sort_get_model" : cptr -> cptr;
	val get_model : 'a t -> base t
	    = fn self => make (GObject.withPtr
				 (self, fn self => get_model_ self))
	val convert_child_path_to_path_ : cptr * cptr -> cptr
	    = _import "gtk_tree_model_sort_convert_child_path_to_path"
		      : cptr * cptr -> cptr;
	val convert_child_path_to_path
	  : 'a t -> 'b TreePath.t -> base TreePath.t
	    = fn self => fn child_path =>
		 TreePath.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self => GObject.withPtr
					     (child_path, 
					      fn child_path =>
						 convert_child_path_to_path_
						   (self, child_path))))
	val convert_childiter_toiter_ : cptr * cptr * cptr -> unit
	    = _import "gtk_tree_model_sort_convert_child_iter_to_iter"
		      : cptr * cptr * cptr -> unit;
	val convert_childiter_toiter : 'a t -> TreeIter.t -> TreeIter.t
	    = fn self => fn child_iter =>
		 let val sort_iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => convert_childiter_toiter_
					       (self, sort_iter, child_iter))
		 in sort_iter end
	val convert_path_to_child_path_ : cptr * cptr -> cptr
	    = _import "gtk_tree_model_sort_convert_path_to_child_path"
		      : cptr * cptr -> cptr;
	val convert_path_to_child_path
	  : 'a t -> 'b TreePath.t -> base TreePath.t
	    = fn self => fn sorted_path =>
		 TreePath.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self => GObject.withPtr
					     (sorted_path, 
					      fn sorted_path =>
						 convert_path_to_child_path_
						   (self, sorted_path))))
	val convertiter_to_childiter_ : cptr * cptr * cptr -> unit
	    = _import "gtk_tree_model_sort_convert_iter_to_child_iter"
		      : cptr * cptr * cptr -> unit;
	val convertiter_to_childiter : 'a t -> TreeIter.t -> TreeIter.t
	    = fn self => fn sorted_iter =>
		 let val child_iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => convertiter_to_childiter_
					       (self, child_iter, sorted_iter))
		 in child_iter end
	val reset_default_sort_func_ : cptr -> unit
	    = _import "gtk_tree_model_sort_reset_default_sort_func"
		      : cptr -> unit;
	val reset_default_sort_func : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => reset_default_sort_func_ self)
	val clear_cache_ : cptr -> unit
	    = _import "gtk_tree_model_sort_clear_cache" : cptr -> unit;
	val clear_cache : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => clear_cache_ self)
	val sortiter_is_valid_ : cptr * cptr -> bool
	    = _import "gtk_tree_model_sort_iter_is_valid"
		      : cptr * cptr -> bool;
	val sortiter_is_valid : 'a t -> TreeIter.t -> bool
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, fn self => sortiter_is_valid_ (self, iter))
    end
    structure TreeViewColumn :>
      sig
	type base
	type 'a treeviewcolumn_t
	type 'a t = 'a treeviewcolumn_t Object.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTreeViewColumn : 'a t -> base t
	val asCellLayout : 'a t -> base CellLayout.t
	type sizing
	val TREE_VIEW_COLUMN_GROW_ONLY : sizing
	val TREE_VIEW_COLUMN_AUTOSIZE : sizing
	val TREE_VIEW_COLUMN_FIXED : sizing
	val get_type : unit -> GType.t
	val new : unit -> base t
	val new_with_attributes : string -> 'a CellRenderer.t -> base t
	val pack_start : 'a t -> 'b CellRenderer.t -> bool -> unit
	val pack_start' : 'a t -> 'b CellRenderer.t -> unit
	val pack_end : 'a t -> 'b CellRenderer.t -> bool -> unit
	val pack_end' : 'a t -> 'b CellRenderer.t -> unit
	val clear : 'a t -> unit
	val add_attribute : 'a t -> 'b CellRenderer.t -> string -> int -> unit
	val set_attributes : 'a t -> 'b CellRenderer.t -> unit
	val clear_attributes : 'a t -> 'b CellRenderer.t -> unit
	val set_spacing : 'a t -> int -> unit
	val get_spacing : 'a t -> int
	val set_visible : 'a t -> bool -> unit
	val get_visible : 'a t -> bool
	val set_resizable : 'a t -> bool -> unit
	val get_resizable : 'a t -> bool
	val set_sizing : 'a t -> sizing -> unit
	val get_sizing : 'a t -> int
	val get_width : 'a t -> int
	val get_fixed_width : 'a t -> int
	val set_fixed_width : 'a t -> int -> unit
	val set_min_width : 'a t -> int -> unit
	val get_min_width : 'a t -> int
	val set_max_width : 'a t -> int -> unit
	val get_max_width : 'a t -> int
	val clicked : 'a t -> unit
	val set_title : 'a t -> string -> unit
	val get_title : 'a t -> string
	val set_expand : 'a t -> bool -> unit
	val get_expand : 'a t -> bool
	val set_clickable : 'a t -> bool -> unit
	val get_clickable : 'a t -> bool
	val set_widget : 'a t -> 'b Widget.t option -> unit
	val set_widget' : 'a t -> unit
	val get_widget : 'a t -> base Widget.t
	val set_alignment : 'a t -> real -> unit
	val get_alignment : 'a t -> real
	val set_reorderable : 'a t -> bool -> unit
	val get_reorderable : 'a t -> bool
	val set_sort_column_id : 'a t -> int -> unit
	val get_sort_column_id : 'a t -> int
	val set_sort_indicator : 'a t -> bool -> unit
	val get_sort_indicator : 'a t -> bool
	val set_sort_order : 'a t -> sorttype -> unit
	val get_sort_order : 'a t -> sorttype
	val cell_set_cell_data
	  : 'a t -> 'b TreeModel.t -> TreeIter.t -> bool -> bool -> unit
	val cell_is_visible : 'a t -> bool
	val focus_cell : 'a t -> 'b CellRenderer.t -> unit
	val cell_get_position : 'a t -> 'b CellRenderer.t -> int * int
	val clicked_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a treeviewcolumn_t = unit
	type 'a t = 'a treeviewcolumn_t Object.t
	fun inherit w con = Object.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeViewColumn obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asCellLayout obj
	  = CellLayout.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	type sizing = int
	val get_sizing_ : int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_treeviewcolumn_sizing"
		      : int ref * int ref * int ref -> unit;
	val (TREE_VIEW_COLUMN_GROW_ONLY, TREE_VIEW_COLUMN_AUTOSIZE, 
	     TREE_VIEW_COLUMN_FIXED)
	    = let val (x0, x1, x2) = (ref 0, ref 0, ref 0)
	      in get_sizing_ (x0, x1, x2)
	       ; (!x0, !x1, !x2)
	      end
	val get_type_ : unit -> GType.t
	    = _import "gtk_tree_view_column_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr
	    = _import "gtk_tree_view_column_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_attributes_ : CString.cstring * cptr -> cptr
	    = _import "gtk_tree_view_column_new_with_attributes"
		      : CString.cstring * cptr -> cptr;
	val new_with_attributes : string -> 'a CellRenderer.t -> base t
	    = fn title => fn cell =>
		 make (GObject.withPtr
			 (cell, 
			  fn cell => new_with_attributes_
				       (CString.fromString title, cell)))
	val pack_start_ : cptr * cptr * bool -> unit
	    = _import "gtk_tree_view_column_pack_start"
		      : cptr * cptr * bool -> unit;
	val pack_start : 'a t -> 'b CellRenderer.t -> bool -> unit
	    = fn self => fn cell => fn expand =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (cell, fn cell => pack_start_ (self, cell, expand)))
	val pack_start' : 'a t -> 'b CellRenderer.t -> unit
	    = fn self => fn cell =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (cell, fn cell => pack_start_ (self, cell, true)))
	val pack_end_ : cptr * cptr * bool -> unit
	    = _import "gtk_tree_view_column_pack_end"
		      : cptr * cptr * bool -> unit;
	val pack_end : 'a t -> 'b CellRenderer.t -> bool -> unit
	    = fn self => fn cell => fn expand =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (cell, fn cell => pack_end_ (self, cell, expand)))
	val pack_end' : 'a t -> 'b CellRenderer.t -> unit
	    = fn self => fn cell =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (cell, fn cell => pack_end_ (self, cell, true)))
	val clear_ : cptr -> unit
	    = _import "gtk_tree_view_column_clear" : cptr -> unit;
	val clear : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => clear_ self)
	val add_attribute_ : cptr * cptr * CString.cstring * int -> unit
	    = _import "gtk_tree_view_column_add_attribute"
		      : cptr * cptr * CString.cstring * int -> unit;
	val add_attribute : 'a t -> 'b CellRenderer.t -> string -> int -> unit
	    = fn self => fn cell_renderer => fn attribute => fn column =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (cell_renderer, 
				  fn cell_renderer =>
				     add_attribute_
				       (self, cell_renderer, 
					CString.fromString attribute, column)))
	val set_attributes_ : cptr * cptr -> unit
	    = _import "gtk_tree_view_column_set_attributes"
		      : cptr * cptr -> unit;
	val set_attributes : 'a t -> 'b CellRenderer.t -> unit
	    = fn self => fn cell_renderer =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (cell_renderer, 
				  fn cell_renderer =>
				     set_attributes_ (self, cell_renderer)))
	val clear_attributes_ : cptr * cptr -> unit
	    = _import "gtk_tree_view_column_clear_attributes"
		      : cptr * cptr -> unit;
	val clear_attributes : 'a t -> 'b CellRenderer.t -> unit
	    = fn self => fn cell_renderer =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr (cell_renderer, 
						fn cell_renderer =>
						   clear_attributes_
						     (self, cell_renderer)))
	val set_spacing_ : cptr * int -> unit
	    = _import "gtk_tree_view_column_set_spacing" : cptr * int -> unit;
	val set_spacing : 'a t -> int -> unit
	    = fn self => fn spacing =>
		 GObject.withPtr
		   (self, fn self => set_spacing_ (self, spacing))
	val get_spacing_ : cptr -> int
	    = _import "gtk_tree_view_column_get_spacing" : cptr -> int;
	val get_spacing : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_spacing_ self)
	val set_visible_ : cptr * bool -> unit
	    = _import "gtk_tree_view_column_set_visible" : cptr * bool -> unit;
	val set_visible : 'a t -> bool -> unit
	    = fn self => fn visible =>
		 GObject.withPtr
		   (self, fn self => set_visible_ (self, visible))
	val get_visible_ : cptr -> bool
	    = _import "gtk_tree_view_column_get_visible" : cptr -> bool;
	val get_visible : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_visible_ self)
	val set_resizable_ : cptr * bool -> unit
	    = _import "gtk_tree_view_column_set_resizable"
		      : cptr * bool -> unit;
	val set_resizable : 'a t -> bool -> unit
	    = fn self => fn resizable =>
		 GObject.withPtr
		   (self, fn self => set_resizable_ (self, resizable))
	val get_resizable_ : cptr -> bool
	    = _import "gtk_tree_view_column_get_resizable" : cptr -> bool;
	val get_resizable : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_resizable_ self)
	val set_sizing_ : cptr * int -> unit
	    = _import "gtk_tree_view_column_set_sizing" : cptr * int -> unit;
	val set_sizing : 'a t -> sizing -> unit
	    = fn self => fn typ =>
		 GObject.withPtr (self, fn self => set_sizing_ (self, typ))
	val get_sizing_ : cptr -> int
	    = _import "gtk_tree_view_column_get_sizing" : cptr -> int;
	val get_sizing : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_sizing_ self)
	val get_width_ : cptr -> int
	    = _import "gtk_tree_view_column_get_width" : cptr -> int;
	val get_width : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_width_ self)
	val get_fixed_width_ : cptr -> int
	    = _import "gtk_tree_view_column_get_fixed_width" : cptr -> int;
	val get_fixed_width : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_fixed_width_ self)
	val set_fixed_width_ : cptr * int -> unit
	    = _import "gtk_tree_view_column_set_fixed_width"
		      : cptr * int -> unit;
	val set_fixed_width : 'a t -> int -> unit
	    = fn self => fn fixed_width =>
		 GObject.withPtr
		   (self, fn self => set_fixed_width_ (self, fixed_width))
	val set_min_width_ : cptr * int -> unit
	    = _import "gtk_tree_view_column_set_min_width"
		      : cptr * int -> unit;
	val set_min_width : 'a t -> int -> unit
	    = fn self => fn min_width =>
		 GObject.withPtr
		   (self, fn self => set_min_width_ (self, min_width))
	val get_min_width_ : cptr -> int
	    = _import "gtk_tree_view_column_get_min_width" : cptr -> int;
	val get_min_width : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_min_width_ self)
	val set_max_width_ : cptr * int -> unit
	    = _import "gtk_tree_view_column_set_max_width"
		      : cptr * int -> unit;
	val set_max_width : 'a t -> int -> unit
	    = fn self => fn max_width =>
		 GObject.withPtr
		   (self, fn self => set_max_width_ (self, max_width))
	val get_max_width_ : cptr -> int
	    = _import "gtk_tree_view_column_get_max_width" : cptr -> int;
	val get_max_width : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_max_width_ self)
	val clicked_ : cptr -> unit
	    = _import "gtk_tree_view_column_clicked" : cptr -> unit;
	val clicked : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => clicked_ self)
	val set_title_ : cptr * CString.cstring -> unit
	    = _import "gtk_tree_view_column_set_title"
		      : cptr * CString.cstring -> unit;
	val set_title : 'a t -> string -> unit
	    = fn self => fn title =>
		 GObject.withPtr
		   (self, 
		    fn self => set_title_ (self, CString.fromString title))
	val get_title_ : cptr -> CString.t
	    = _import "gtk_tree_view_column_get_title" : cptr -> CString.t;
	val get_title : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_title_ self
				       in CString.toString t end)
	val set_expand_ : cptr * bool -> unit
	    = _import "gtk_tree_view_column_set_expand" : cptr * bool -> unit;
	val set_expand : 'a t -> bool -> unit
	    = fn self => fn expand =>
		 GObject.withPtr (self, fn self => set_expand_ (self, expand))
	val get_expand_ : cptr -> bool
	    = _import "gtk_tree_view_column_get_expand" : cptr -> bool;
	val get_expand : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_expand_ self)
	val set_clickable_ : cptr * bool -> unit
	    = _import "gtk_tree_view_column_set_clickable"
		      : cptr * bool -> unit;
	val set_clickable : 'a t -> bool -> unit
	    = fn self => fn active =>
		 GObject.withPtr
		   (self, fn self => set_clickable_ (self, active))
	val get_clickable_ : cptr -> bool
	    = _import "gtk_tree_view_column_get_clickable" : cptr -> bool;
	val get_clickable : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_clickable_ self)
	val set_widget_ : cptr * cptr -> unit
	    = _import "gtk_tree_view_column_set_widget" : cptr * cptr -> unit;
	val set_widget : 'a t -> 'b Widget.t option -> unit
	    = fn self => fn widget =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withOpt
			 (widget, fn widget => set_widget_ (self, widget)))
	val set_widget' : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => set_widget_ (self, GObject.null))
	val get_widget_ : cptr -> cptr
	    = _import "gtk_tree_view_column_get_widget" : cptr -> cptr;
	val get_widget : 'a t -> base Widget.t
	    = fn self => Widget.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_widget_ self))
	val set_alignment_ : cptr * real -> unit
	    = _import "gtk_tree_view_column_set_alignment"
		      : cptr * real -> unit;
	val set_alignment : 'a t -> real -> unit
	    = fn self => fn xalign =>
		 GObject.withPtr
		   (self, fn self => set_alignment_ (self, xalign))
	val get_alignment_ : cptr -> real
	    = _import "gtk_tree_view_column_get_alignment" : cptr -> real;
	val get_alignment : 'a t -> real
	    = fn self => GObject.withPtr (self, fn self => get_alignment_ self)
	val set_reorderable_ : cptr * bool -> unit
	    = _import "gtk_tree_view_column_set_reorderable"
		      : cptr * bool -> unit;
	val set_reorderable : 'a t -> bool -> unit
	    = fn self => fn reorderable =>
		 GObject.withPtr
		   (self, fn self => set_reorderable_ (self, reorderable))
	val get_reorderable_ : cptr -> bool
	    = _import "gtk_tree_view_column_get_reorderable" : cptr -> bool;
	val get_reorderable : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_reorderable_ self)
	val set_sort_column_id_ : cptr * int -> unit
	    = _import "gtk_tree_view_column_set_sort_column_id"
		      : cptr * int -> unit;
	val set_sort_column_id : 'a t -> int -> unit
	    = fn self => fn sort_column_id =>
		 GObject.withPtr (self, 
				  fn self => set_sort_column_id_
					       (self, sort_column_id))
	val get_sort_column_id_ : cptr -> int
	    = _import "gtk_tree_view_column_get_sort_column_id" : cptr -> int;
	val get_sort_column_id : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_sort_column_id_ self)
	val set_sort_indicator_ : cptr * bool -> unit
	    = _import "gtk_tree_view_column_set_sort_indicator"
		      : cptr * bool -> unit;
	val set_sort_indicator : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_sort_indicator_ (self, setting))
	val get_sort_indicator_ : cptr -> bool
	    = _import "gtk_tree_view_column_get_sort_indicator" : cptr -> bool;
	val get_sort_indicator : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_sort_indicator_ self)
	val set_sort_order_ : cptr * int -> unit
	    = _import "gtk_tree_view_column_set_sort_order"
		      : cptr * int -> unit;
	val set_sort_order : 'a t -> sorttype -> unit
	    = fn self => fn order =>
		 GObject.withPtr
		   (self, fn self => set_sort_order_ (self, order))
	val get_sort_order_ : cptr -> int
	    = _import "gtk_tree_view_column_get_sort_order" : cptr -> int;
	val get_sort_order : 'a t -> sorttype
	    = fn self => GObject.withPtr
			   (self, fn self => get_sort_order_ self)
	val cell_set_cell_data_ : cptr * cptr * cptr * bool * bool -> unit
	    = _import "gtk_tree_view_column_cell_set_cell_data"
		      : cptr * cptr * cptr * bool * bool -> unit;
	val cell_set_cell_data
	  : 'a t -> 'b TreeModel.t -> TreeIter.t -> bool -> bool -> unit
	    = fn self => fn tree_model => fn iter => fn is_expander => 
	      fn is_expanded =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (tree_model, 
			  fn tree_model => cell_set_cell_data_
					     (self, tree_model, iter, 
					      is_expander, is_expanded)))
	val cell_is_visible_ : cptr -> bool
	    = _import "gtk_tree_view_column_cell_is_visible" : cptr -> bool;
	val cell_is_visible : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => cell_is_visible_ self)
	val focus_cell_ : cptr * cptr -> unit
	    = _import "gtk_tree_view_column_focus_cell" : cptr * cptr -> unit;
	val focus_cell : 'a t -> 'b CellRenderer.t -> unit
	    = fn self => fn cell =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (cell, fn cell => focus_cell_ (self, cell)))
	val cell_get_position_ : cptr * cptr * int ref * int ref -> unit
	    = _import "gtk_tree_view_column_cell_get_position"
		      : cptr * cptr * int ref * int ref -> unit;
	val cell_get_position : 'a t -> 'b CellRenderer.t -> int * int
	    = fn self => fn cell_renderer =>
		 let val (start_pos, width) = (ref 0, ref 0)
		     val ret = GObject.withPtr
				 (self, 
				  fn self => GObject.withPtr
					       (cell_renderer, 
						fn cell_renderer =>
						   cell_get_position_
						     (self, cell_renderer, 
						      start_pos, width)))
		 in (!start_pos, !width) end
	local open Signal
	      infixr -->
	in val clicked_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "clicked" false (void --> return_void) f
	end
    end
    structure TreeView :>
      sig
	type base
	type 'a treeview_t
	type 'a t = 'a treeview_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTreeView : 'a t -> base t
	type drop_position
	val TREE_VIEW_DROP_BEFORE : drop_position
	val TREE_VIEW_DROP_AFTER : drop_position
	val TREE_VIEW_DROP_INTO_OR_BEFORE : drop_position
	val TREE_VIEW_DROP_INTO_OR_AFTER : drop_position
	val get_type : unit -> GType.t
	val new : unit -> base t
	val new_with_model : 'a TreeModel.t -> base t
	val get_model : 'a t -> base TreeModel.t
	val set_model : 'a t -> 'b TreeModel.t option -> unit
	val set_model' : 'a t -> unit
	val get_selection : 'a t -> base t
	val get_hadjustment : 'a t -> base Adjustment.t
	val set_hadjustment : 'a t -> 'b Adjustment.t -> unit
	val get_vadjustment : 'a t -> base Adjustment.t
	val set_vadjustment : 'a t -> 'b Adjustment.t -> unit
	val get_headers_visible : 'a t -> bool
	val set_headers_visible : 'a t -> bool -> unit
	val columns_autosize : 'a t -> unit
	val set_headers_clickable : 'a t -> bool -> unit
	val set_rules_hint : 'a t -> bool -> unit
	val get_rules_hint : 'a t -> bool
	val append_column : 'a t -> 'b TreeViewColumn.t -> int
	val remove_column : 'a t -> 'b TreeViewColumn.t -> int
	val insert_column : 'a t -> 'b TreeViewColumn.t -> int -> int
	val insert_column_with_attributes
	  : 'a t -> int -> string -> 'b CellRenderer.t -> int
	val get_column : 'a t -> int -> base TreeViewColumn.t
	val move_column_after
	  : 'a t -> 'b TreeViewColumn.t -> 'c TreeViewColumn.t option -> unit
	val move_column_after' : 'a t -> 'b TreeViewColumn.t -> unit
	val set_expander_column : 'a t -> 'b TreeViewColumn.t -> unit
	val get_expander_column : 'a t -> base TreeViewColumn.t
	val scroll_to_point : 'a t -> int -> int -> unit
	val scroll_to_cell : 'a t -> 'b TreePath.t 
			  -> 'c TreeViewColumn.t option -> bool -> real -> real
			     -> unit
	val scroll_to_cell' : 'a t -> 'b TreePath.t -> unit
	val row_activated
	  : 'a t -> 'b TreePath.t -> 'c TreeViewColumn.t -> unit
	val expand_all : 'a t -> unit
	val collapse_all : 'a t -> unit
	val expand_to_path : 'a t -> 'b TreePath.t -> unit
	val expand_row : 'a t -> 'b TreePath.t -> bool -> bool
	val collapse_row : 'a t -> 'b TreePath.t -> unit
	val row_expanded : 'a t -> 'b TreePath.t -> bool
	val set_reorderable : 'a t -> bool -> unit
	val get_reorderable : 'a t -> bool
	val set_cursor
	  : 'a t -> 'b TreePath.t -> 'c TreeViewColumn.t option -> bool -> unit
	val set_cursor' : 'a t -> 'b TreePath.t -> unit
	val set_cursor_on_cell
	  : 'a t -> 'b TreePath.t -> 'c TreeViewColumn.t option 
	 -> 'd CellRenderer.t option -> bool
	    -> unit
	val set_cursor_on_cell' : 'a t -> 'b TreePath.t -> unit
	val unset_rows_drag_source : 'a t -> unit
	val unset_rows_drag_dest : 'a t -> unit
	val set_drag_dest_row : 'a t -> 'b TreePath.t -> drop_position -> unit
	val set_enable_search : 'a t -> bool -> unit
	val get_enable_search : 'a t -> bool
	val get_search_column : 'a t -> int
	val set_search_column : 'a t -> int -> unit
	val set_scroll_adjustments_sig
	  : (unit -> unit -> unit) -> 'a t Signal.signal
	val row_activated_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	val test_expand_row_sig : (unit -> unit -> bool) -> 'a t Signal.signal
	val test_collapse_row_sig
	  : (unit -> unit -> bool) -> 'a t Signal.signal
	val row_expanded_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	val row_collapsed_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	val columns_changed_sig : (unit -> unit) -> 'a t Signal.signal
	val cursor_changed_sig : (unit -> unit) -> 'a t Signal.signal
	val move_cursor_sig : (unit -> int -> bool) -> 'a t Signal.signal
	val select_all_sig : (unit -> bool) -> 'a t Signal.signal
	val unselect_all_sig : (unit -> bool) -> 'a t Signal.signal
	val select_cursor_row_sig : (bool -> bool) -> 'a t Signal.signal
	val toggle_cursor_row_sig : (unit -> bool) -> 'a t Signal.signal
	val expand_collapse_cursor_row_sig
	  : (bool -> bool -> bool -> bool) -> 'a t Signal.signal
	val select_cursor_parent_sig : (unit -> bool) -> 'a t Signal.signal
	val start_interactive_search_sig : (unit -> bool) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a treeview_t = unit
	type 'a t = 'a treeview_t Container.t
	fun inherit w con = Container.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeView obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type drop_position = int
	val get_drop_position_ : int ref * int ref * int ref * int ref -> unit
	    = _import "mgtk_get_gtk_treeview_drop_position"
		      : int ref * int ref * int ref * int ref -> unit;
	val (TREE_VIEW_DROP_BEFORE, TREE_VIEW_DROP_AFTER, 
	     TREE_VIEW_DROP_INTO_OR_BEFORE, TREE_VIEW_DROP_INTO_OR_AFTER)
	    = let val (x0, x1, x2, x3) = (ref 0, ref 0, ref 0, ref 0)
	      in get_drop_position_ (x0, x1, x2, x3)
	       ; (!x0, !x1, !x2, !x3)
	      end
	val get_type_ : unit -> GType.t
	    = _import "gtk_tree_view_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_tree_view_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_model_ : cptr -> cptr
	    = _import "gtk_tree_view_new_with_model" : cptr -> cptr;
	val new_with_model : 'a TreeModel.t -> base t
	    = fn model => make (GObject.withPtr
				  (model, fn model => new_with_model_ model))
	val get_model_ : cptr -> cptr
	    = _import "gtk_tree_view_get_model" : cptr -> cptr;
	val get_model : 'a t -> base TreeModel.t
	    = fn self => TreeModel.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_model_ self))
	val set_model_ : cptr * cptr -> unit
	    = _import "gtk_tree_view_set_model" : cptr * cptr -> unit;
	val set_model : 'a t -> 'b TreeModel.t option -> unit
	    = fn self => fn model =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withOpt
				 (model, fn model => set_model_ (self, model)))
	val set_model' : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => set_model_ (self, GObject.null))
	val get_selection_ : cptr -> cptr
	    = _import "gtk_tree_view_get_selection" : cptr -> cptr;
	val get_selection : 'a t -> base t
	    = fn self => make (GObject.withPtr
				 (self, fn self => get_selection_ self))
	val get_hadjustment_ : cptr -> cptr
	    = _import "gtk_tree_view_get_hadjustment" : cptr -> cptr;
	val get_hadjustment : 'a t -> base Adjustment.t
	    = fn self =>
		 Adjustment.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_hadjustment_ self))
	val set_hadjustment_ : cptr * cptr -> unit
	    = _import "gtk_tree_view_set_hadjustment" : cptr * cptr -> unit;
	val set_hadjustment : 'a t -> 'b Adjustment.t -> unit
	    = fn self => fn adjustment =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (adjustment, 
				  fn adjustment => set_hadjustment_
						     (self, adjustment)))
	val get_vadjustment_ : cptr -> cptr
	    = _import "gtk_tree_view_get_vadjustment" : cptr -> cptr;
	val get_vadjustment : 'a t -> base Adjustment.t
	    = fn self =>
		 Adjustment.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_vadjustment_ self))
	val set_vadjustment_ : cptr * cptr -> unit
	    = _import "gtk_tree_view_set_vadjustment" : cptr * cptr -> unit;
	val set_vadjustment : 'a t -> 'b Adjustment.t -> unit
	    = fn self => fn adjustment =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (adjustment, 
				  fn adjustment => set_vadjustment_
						     (self, adjustment)))
	val get_headers_visible_ : cptr -> bool
	    = _import "gtk_tree_view_get_headers_visible" : cptr -> bool;
	val get_headers_visible : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_headers_visible_ self)
	val set_headers_visible_ : cptr * bool -> unit
	    = _import "gtk_tree_view_set_headers_visible"
		      : cptr * bool -> unit;
	val set_headers_visible : 'a t -> bool -> unit
	    = fn self => fn headers_visible =>
		 GObject.withPtr (self, 
				  fn self => set_headers_visible_
					       (self, headers_visible))
	val columns_autosize_ : cptr -> unit
	    = _import "gtk_tree_view_columns_autosize" : cptr -> unit;
	val columns_autosize : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => columns_autosize_ self)
	val set_headers_clickable_ : cptr * bool -> unit
	    = _import "gtk_tree_view_set_headers_clickable"
		      : cptr * bool -> unit;
	val set_headers_clickable : 'a t -> bool -> unit
	    = fn self => fn active =>
		 GObject.withPtr
		   (self, fn self => set_headers_clickable_ (self, active))
	val set_rules_hint_ : cptr * bool -> unit
	    = _import "gtk_tree_view_set_rules_hint" : cptr * bool -> unit;
	val set_rules_hint : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 GObject.withPtr
		   (self, fn self => set_rules_hint_ (self, setting))
	val get_rules_hint_ : cptr -> bool
	    = _import "gtk_tree_view_get_rules_hint" : cptr -> bool;
	val get_rules_hint : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_rules_hint_ self)
	val append_column_ : cptr * cptr -> int
	    = _import "gtk_tree_view_append_column" : cptr * cptr -> int;
	val append_column : 'a t -> 'b TreeViewColumn.t -> int
	    = fn self => fn column =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (column, fn column => append_column_ (self, column)))
	val remove_column_ : cptr * cptr -> int
	    = _import "gtk_tree_view_remove_column" : cptr * cptr -> int;
	val remove_column : 'a t -> 'b TreeViewColumn.t -> int
	    = fn self => fn column =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (column, fn column => remove_column_ (self, column)))
	val insert_column_ : cptr * cptr * int -> int
	    = _import "gtk_tree_view_insert_column" : cptr * cptr * int -> int;
	val insert_column : 'a t -> 'b TreeViewColumn.t -> int -> int
	    = fn self => fn column => fn position =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (column, 
				  fn column => insert_column_
						 (self, column, position)))
	val insert_column_with_attributes_
	  : cptr * int * CString.cstring * cptr -> int
	    = _import "gtk_tree_view_insert_column_with_attributes"
		      : cptr * int * CString.cstring * cptr -> int;
	val insert_column_with_attributes
	  : 'a t -> int -> string -> 'b CellRenderer.t -> int
	    = fn self => fn position => fn title => fn cell =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (cell, 
			  fn cell => insert_column_with_attributes_
				       (self, position, 
					CString.fromString title, cell)))
	val get_column_ : cptr * int -> cptr
	    = _import "gtk_tree_view_get_column" : cptr * int -> cptr;
	val get_column : 'a t -> int -> base TreeViewColumn.t
	    = fn self => fn n =>
		 TreeViewColumn.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_column_ (self, n)))
	val move_column_after_ : cptr * cptr * cptr -> unit
	    = _import "gtk_tree_view_move_column_after"
		      : cptr * cptr * cptr -> unit;
	val move_column_after
	  : 'a t -> 'b TreeViewColumn.t -> 'c TreeViewColumn.t option -> unit
	    = fn self => fn column => fn base_column =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (column, 
				  fn column => GObject.withOpt
						 (base_column, 
						  fn base_column =>
						     move_column_after_
						       (self, column, 
							base_column))))
	val move_column_after' : 'a t -> 'b TreeViewColumn.t -> unit
	    = fn self => fn column =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (column, 
				  fn column => move_column_after_
						 (self, column, GObject.null)))
	val set_expander_column_ : cptr * cptr -> unit
	    = _import "gtk_tree_view_set_expander_column"
		      : cptr * cptr -> unit;
	val set_expander_column : 'a t -> 'b TreeViewColumn.t -> unit
	    = fn self => fn column =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (column, 
				  fn column => set_expander_column_
						 (self, column)))
	val get_expander_column_ : cptr -> cptr
	    = _import "gtk_tree_view_get_expander_column" : cptr -> cptr;
	val get_expander_column : 'a t -> base TreeViewColumn.t
	    = fn self =>
		 TreeViewColumn.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_expander_column_ self))
	val scroll_to_point_ : cptr * int * int -> unit
	    = _import "gtk_tree_view_scroll_to_point"
		      : cptr * int * int -> unit;
	val scroll_to_point : 'a t -> int -> int -> unit
	    = fn self => fn tree_x => fn tree_y =>
		 GObject.withPtr
		   (self, fn self => scroll_to_point_ (self, tree_x, tree_y))
	val scroll_to_cell_ : cptr * cptr * cptr * bool * real * real -> unit
	    = _import "gtk_tree_view_scroll_to_cell"
		      : cptr * cptr * cptr * bool * real * real -> unit;
	val scroll_to_cell : 'a t -> 'b TreePath.t 
			  -> 'c TreeViewColumn.t option -> bool -> real -> real
			     -> unit
	    = fn self => fn path => fn column => fn use_align => 
	      fn row_align => fn col_align =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (path, 
				  fn path => GObject.withOpt
					       (column, 
						fn column =>
						   scroll_to_cell_
						     (self, path, column, 
						      use_align, row_align, 
						      col_align))))
	val scroll_to_cell' : 'a t -> 'b TreePath.t -> unit
	    = fn self => fn path =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (path, 
				  fn path => scroll_to_cell_
					       (self, path, GObject.null, 
						false, 0.0, 0.0)))
	val row_activated_ : cptr * cptr * cptr -> unit
	    = _import "gtk_tree_view_row_activated"
		      : cptr * cptr * cptr -> unit;
	val row_activated
	  : 'a t -> 'b TreePath.t -> 'c TreeViewColumn.t -> unit
	    = fn self => fn path => fn column =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (path, 
				  fn path => GObject.withPtr
					       (column, 
						fn column =>
						   row_activated_
						     (self, path, column))))
	val expand_all_ : cptr -> unit
	    = _import "gtk_tree_view_expand_all" : cptr -> unit;
	val expand_all : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => expand_all_ self)
	val collapse_all_ : cptr -> unit
	    = _import "gtk_tree_view_collapse_all" : cptr -> unit;
	val collapse_all : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => collapse_all_ self)
	val expand_to_path_ : cptr * cptr -> unit
	    = _import "gtk_tree_view_expand_to_path" : cptr * cptr -> unit;
	val expand_to_path : 'a t -> 'b TreePath.t -> unit
	    = fn self => fn path =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (path, fn path => expand_to_path_ (self, path)))
	val expand_row_ : cptr * cptr * bool -> bool
	    = _import "gtk_tree_view_expand_row" : cptr * cptr * bool -> bool;
	val expand_row : 'a t -> 'b TreePath.t -> bool -> bool
	    = fn self => fn path => fn open_all =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (path, fn path => expand_row_ (self, path, open_all)))
	val collapse_row_ : cptr * cptr -> unit
	    = _import "gtk_tree_view_collapse_row" : cptr * cptr -> unit;
	val collapse_row : 'a t -> 'b TreePath.t -> unit
	    = fn self => fn path =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (path, fn path => collapse_row_ (self, path)))
	val row_expanded_ : cptr * cptr -> bool
	    = _import "gtk_tree_view_row_expanded" : cptr * cptr -> bool;
	val row_expanded : 'a t -> 'b TreePath.t -> bool
	    = fn self => fn path =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (path, fn path => row_expanded_ (self, path)))
	val set_reorderable_ : cptr * bool -> unit
	    = _import "gtk_tree_view_set_reorderable" : cptr * bool -> unit;
	val set_reorderable : 'a t -> bool -> unit
	    = fn self => fn reorderable =>
		 GObject.withPtr
		   (self, fn self => set_reorderable_ (self, reorderable))
	val get_reorderable_ : cptr -> bool
	    = _import "gtk_tree_view_get_reorderable" : cptr -> bool;
	val get_reorderable : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_reorderable_ self)
	val set_cursor_ : cptr * cptr * cptr * bool -> unit
	    = _import "gtk_tree_view_set_cursor"
		      : cptr * cptr * cptr * bool -> unit;
	val set_cursor
	  : 'a t -> 'b TreePath.t -> 'c TreeViewColumn.t option -> bool -> unit
	    = fn self => fn path => fn focus_column => fn start_editing =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (path, 
				  fn path => GObject.withOpt
					       (focus_column, 
						fn focus_column =>
						   set_cursor_
						     (self, path, 
						      focus_column, 
						      start_editing))))
	val set_cursor' : 'a t -> 'b TreePath.t -> unit
	    = fn self => fn path =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (path, 
				  fn path => set_cursor_
					       (self, path, 
						GObject.null, false)))
	val set_cursor_on_cell_ : cptr * cptr * cptr * cptr * bool -> unit
	    = _import "gtk_tree_view_set_cursor_on_cell"
		      : cptr * cptr * cptr * cptr * bool -> unit;
	val set_cursor_on_cell
	  : 'a t -> 'b TreePath.t -> 'c TreeViewColumn.t option 
	 -> 'd CellRenderer.t option -> bool
	    -> unit
	    = fn self => fn path => fn focus_column => fn focus_cell => 
	      fn start_editing =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (path, 
			  fn path =>
			     GObject.withOpt
			       (focus_column, 
				fn focus_column =>
				   GObject.withOpt
				     (focus_cell, 
				      fn focus_cell =>
					 set_cursor_on_cell_
					   (self, path, focus_column, 
					    focus_cell, start_editing)))))
	val set_cursor_on_cell' : 'a t -> 'b TreePath.t -> unit
	    = fn self => fn path =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (path, 
				  fn path => set_cursor_on_cell_
					       (self, path, GObject.null, 
						GObject.null, false)))
	val unset_rows_drag_source_ : cptr -> unit
	    = _import "gtk_tree_view_unset_rows_drag_source" : cptr -> unit;
	val unset_rows_drag_source : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => unset_rows_drag_source_ self)
	val unset_rows_drag_dest_ : cptr -> unit
	    = _import "gtk_tree_view_unset_rows_drag_dest" : cptr -> unit;
	val unset_rows_drag_dest : 'a t -> unit
	    = fn self => GObject.withPtr
			   (self, fn self => unset_rows_drag_dest_ self)
	val set_drag_dest_row_ : cptr * cptr * int -> unit
	    = _import "gtk_tree_view_set_drag_dest_row"
		      : cptr * cptr * int -> unit;
	val set_drag_dest_row : 'a t -> 'b TreePath.t -> drop_position -> unit
	    = fn self => fn path => fn pos =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (path, 
				  fn path => set_drag_dest_row_
					       (self, path, pos)))
	val set_enable_search_ : cptr * bool -> unit
	    = _import "gtk_tree_view_set_enable_search" : cptr * bool -> unit;
	val set_enable_search : 'a t -> bool -> unit
	    = fn self => fn enable_search =>
		 GObject.withPtr
		   (self, fn self => set_enable_search_ (self, enable_search))
	val get_enable_search_ : cptr -> bool
	    = _import "gtk_tree_view_get_enable_search" : cptr -> bool;
	val get_enable_search : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_enable_search_ self)
	val get_search_column_ : cptr -> int
	    = _import "gtk_tree_view_get_search_column" : cptr -> int;
	val get_search_column : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => get_search_column_ self)
	val set_search_column_ : cptr * int -> unit
	    = _import "gtk_tree_view_set_search_column" : cptr * int -> unit;
	val set_search_column : 'a t -> int -> unit
	    = fn self => fn column =>
		 GObject.withPtr
		   (self, fn self => set_search_column_ (self, column))
	local open Signal
	      infixr -->
	in
	  val set_scroll_adjustments_sig
	    : (unit -> unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "set-scroll-adjustments" false
			       (unit --> unit --> return_void) f
	  val row_activated_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "row-activated" false
			       (unit --> unit --> return_void) f
	  val test_expand_row_sig
	    : (unit -> unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "test-expand-row" false
			       (unit --> unit --> return_bool) f
	  val test_collapse_row_sig
	    : (unit -> unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "test-collapse-row" false
			       (unit --> unit --> return_bool) f
	  val row_expanded_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "row-expanded" false
			       (unit --> unit --> return_void) f
	  val row_collapsed_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "row-collapsed" false
			       (unit --> unit --> return_void) f
	  val columns_changed_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "columns-changed" false (void --> return_void) f
	  val cursor_changed_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "cursor-changed" false (void --> return_void) f
	  val move_cursor_sig : (unit -> int -> bool) -> 'a t Signal.signal
	      = fn f => signal "move-cursor" false
			       (unit --> int --> return_bool) f
	  val select_all_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "select-all" false (void --> return_bool) f
	  val unselect_all_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "unselect-all" false (void --> return_bool) f
	  val select_cursor_row_sig : (bool -> bool) -> 'a t Signal.signal
	      = fn f =>
		   signal "select-cursor-row" false (bool --> return_bool) f
	  val toggle_cursor_row_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f =>
		   signal "toggle-cursor-row" false (void --> return_bool) f
	  val expand_collapse_cursor_row_sig
	    : (bool -> bool -> bool -> bool) -> 'a t Signal.signal
	      = fn f => signal "expand-collapse-cursor-row" false
			       (bool --> bool --> bool --> return_bool) f
	  val select_cursor_parent_sig : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "select-cursor-parent" false
			       (void --> return_bool) f
	  val start_interactive_search_sig
	    : (unit -> bool) -> 'a t Signal.signal
	      = fn f => signal "start-interactive-search" false
			       (void --> return_bool) f
	end
    end
    structure TreeSelection :>
      sig
	type base
	type 'a treeselection_t
	type 'a t = 'a treeselection_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTreeSelection : 'a t -> base t
	val get_type : unit -> GType.t
	val set_mode : 'a t -> selection_mode -> unit
	val get_mode : 'a t -> selection_mode
	val get_user_data : 'a t -> cptr
	val get_treeview : 'a t -> base TreeView.t
	val count_selected_rows : 'a t -> int
	val select_path : 'a t -> 'b TreePath.t -> unit
	val unselect_path : 'a t -> 'b TreePath.t -> unit
	val selectiter : 'a t -> TreeIter.t -> unit
	val unselectiter : 'a t -> TreeIter.t -> unit
	val path_is_selected : 'a t -> 'b TreePath.t -> bool
	val iter_is_selected : 'a t -> TreeIter.t -> bool
	val select_all : 'a t -> unit
	val unselect_all : 'a t -> unit
	val select_range : 'a t -> 'b TreePath.t -> 'c TreePath.t -> unit
	val unselect_range : 'a t -> 'b TreePath.t -> 'c TreePath.t -> unit
	val changed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a treeselection_t = unit
	type 'a t = 'a treeselection_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeSelection obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_tree_selection_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_mode_ : cptr * int -> unit
	    = _import "gtk_tree_selection_set_mode" : cptr * int -> unit;
	val set_mode : 'a t -> selection_mode -> unit
	    = fn self => fn typ =>
		 GObject.withPtr (self, fn self => set_mode_ (self, typ))
	val get_mode_ : cptr -> int
	    = _import "gtk_tree_selection_get_mode" : cptr -> int;
	val get_mode : 'a t -> selection_mode
	    = fn self => GObject.withPtr (self, fn self => get_mode_ self)
	val get_user_data_ : cptr -> cptr
	    = _import "gtk_tree_selection_get_user_data" : cptr -> cptr;
	val get_user_data : 'a t -> cptr
	    = fn self => GObject.withPtr (self, fn self => get_user_data_ self)
	val get_treeview_ : cptr -> cptr
	    = _import "gtk_tree_selection_get_tree_view" : cptr -> cptr;
	val get_treeview : 'a t -> base TreeView.t
	    = fn self => TreeView.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_treeview_ self))
	val count_selected_rows_ : cptr -> int
	    = _import "gtk_tree_selection_count_selected_rows" : cptr -> int;
	val count_selected_rows : 'a t -> int
	    = fn self => GObject.withPtr
			   (self, fn self => count_selected_rows_ self)
	val select_path_ : cptr * cptr -> unit
	    = _import "gtk_tree_selection_select_path" : cptr * cptr -> unit;
	val select_path : 'a t -> 'b TreePath.t -> unit
	    = fn self => fn path =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (path, fn path => select_path_ (self, path)))
	val unselect_path_ : cptr * cptr -> unit
	    = _import "gtk_tree_selection_unselect_path" : cptr * cptr -> unit;
	val unselect_path : 'a t -> 'b TreePath.t -> unit
	    = fn self => fn path =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (path, fn path => unselect_path_ (self, path)))
	val selectiter_ : cptr * cptr -> unit
	    = _import "gtk_tree_selection_select_iter" : cptr * cptr -> unit;
	val selectiter : 'a t -> TreeIter.t -> unit
	    = fn self => fn iter =>
		 GObject.withPtr (self, fn self => selectiter_ (self, iter))
	val unselectiter_ : cptr * cptr -> unit
	    = _import "gtk_tree_selection_unselect_iter" : cptr * cptr -> unit;
	val unselectiter : 'a t -> TreeIter.t -> unit
	    = fn self => fn iter =>
		 GObject.withPtr (self, fn self => unselectiter_ (self, iter))
	val path_is_selected_ : cptr * cptr -> bool
	    = _import "gtk_tree_selection_path_is_selected"
		      : cptr * cptr -> bool;
	val path_is_selected : 'a t -> 'b TreePath.t -> bool
	    = fn self => fn path =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (path, fn path => path_is_selected_ (self, path)))
	val iter_is_selected_ : cptr * cptr -> bool
	    = _import "gtk_tree_selection_iter_is_selected"
		      : cptr * cptr -> bool;
	val iter_is_selected : 'a t -> TreeIter.t -> bool
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, fn self => iter_is_selected_ (self, iter))
	val select_all_ : cptr -> unit
	    = _import "gtk_tree_selection_select_all" : cptr -> unit;
	val select_all : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => select_all_ self)
	val unselect_all_ : cptr -> unit
	    = _import "gtk_tree_selection_unselect_all" : cptr -> unit;
	val unselect_all : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => unselect_all_ self)
	val select_range_ : cptr * cptr * cptr -> unit
	    = _import "gtk_tree_selection_select_range"
		      : cptr * cptr * cptr -> unit;
	val select_range : 'a t -> 'b TreePath.t -> 'c TreePath.t -> unit
	    = fn self => fn start_path => fn end_path =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (start_path, 
				  fn start_path =>
				     GObject.withPtr
				       (end_path, 
					fn end_path =>
					   select_range_
					     (self, start_path, end_path))))
	val unselect_range_ : cptr * cptr * cptr -> unit
	    = _import "gtk_tree_selection_unselect_range"
		      : cptr * cptr * cptr -> unit;
	val unselect_range : 'a t -> 'b TreePath.t -> 'c TreePath.t -> unit
	    = fn self => fn start_path => fn end_path =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (start_path, 
				  fn start_path =>
				     GObject.withPtr
				       (end_path, 
					fn end_path =>
					   unselect_range_
					     (self, start_path, end_path))))
	local open Signal
	      infixr -->
	in val changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "changed" false (void --> return_void) f
	end
    end
    structure TreeStore :>
      sig
	type base
	type 'a treestore_t
	type 'a t = 'a treestore_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTreeStore : 'a t -> base t
	val asTreeModel : 'a t -> base TreeModel.t
	val asTreeDragSource : 'a t -> base TreeDragSource.t
	val asTreeDragDest : 'a t -> base TreeDragDest.t
	val asTreeSortable : 'a t -> base TreeSortable.t
	val get_type : unit -> GType.t
	val new : int -> base t
	val newv : int -> GType.t list -> base t
	val set_value : 'a t -> TreeIter.t -> int -> GValue.GValue -> unit
	val set : 'a t -> TreeIter.t -> unit
	val remove : 'a t -> TreeIter.t -> bool * TreeIter.t
	val insert : 'a t -> TreeIter.t -> int -> TreeIter.t
	val insert_before : 'a t -> TreeIter.t -> TreeIter.t -> TreeIter.t
	val insert_after : 'a t -> TreeIter.t -> TreeIter.t -> TreeIter.t
	val prepend : 'a t -> TreeIter.t -> TreeIter.t
	val append : 'a t -> TreeIter.t -> TreeIter.t
	val is_ancestor : 'a t -> TreeIter.t -> TreeIter.t -> bool
	val storeiter_depth : 'a t -> TreeIter.t -> int
	val clear : 'a t -> unit
	val storeiter_is_valid : 'a t -> TreeIter.t -> bool
	val reorder : 'a t -> TreeIter.t -> int list -> unit
	val swap : 'a t -> TreeIter.t -> TreeIter.t -> unit
	val move_after : 'a t -> TreeIter.t -> TreeIter.t option -> unit
	val move_after' : 'a t -> TreeIter.t -> unit
	val move_before : 'a t -> TreeIter.t -> TreeIter.t option -> unit
	val move_before' : 'a t -> TreeIter.t -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a treestore_t = unit
	type 'a t = 'a treestore_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeStore obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asTreeModel obj
	  = TreeModel.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asTreeDragSource obj
	  = TreeDragSource.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asTreeDragDest obj
	  = TreeDragDest.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	fun asTreeSortable obj
	  = TreeSortable.inherit
	      () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_tree_store_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : int -> cptr = _import "gtk_tree_store_new" : int -> cptr;
	val new : int -> base t = fn n_columns => make (new_ n_columns)
	val newv_ : int * GType.t array -> cptr
	    = _import "gtk_tree_store_newv" : int * GType.t array -> cptr;
	val newv : int -> GType.t list -> base t
	    = fn n_columns => fn types =>
		 make (newv_ (n_columns, Array.fromList types))
	val set_value_ : cptr * cptr * int * GValue.GValue -> unit
	    = _import "gtk_tree_store_set_value"
		      : cptr * cptr * int * GValue.GValue -> unit;
	val set_value : 'a t -> TreeIter.t -> int -> GValue.GValue -> unit
	    = fn self => fn iter => fn column => fn value =>
		 GObject.withPtr
		   (self, fn self => set_value_ (self, iter, column, value))
	val set_ : cptr * cptr -> unit
	    = _import "gtk_tree_store_set" : cptr * cptr -> unit;
	val set : 'a t -> TreeIter.t -> unit
	    = fn self => fn iter =>
		 GObject.withPtr (self, fn self => set_ (self, iter))
	val remove_ : cptr * cptr -> bool
	    = _import "gtk_tree_store_remove" : cptr * cptr -> bool;
	val remove : 'a t -> TreeIter.t -> bool * TreeIter.t
	    = fn self => fn iter =>
		 let val iter = iter
		     val ret = GObject.withPtr
				 (self, fn self => remove_ (self, iter))
		 in (ret, iter) end
	val insert_ : cptr * cptr * cptr * int -> unit
	    = _import "gtk_tree_store_insert"
		      : cptr * cptr * cptr * int -> unit;
	val insert : 'a t -> TreeIter.t -> int -> TreeIter.t
	    = fn self => fn parent => fn position =>
		 let val iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => insert_ (self, iter, parent, 
						      position))
		 in iter end
	val insert_before_ : cptr * cptr * cptr * cptr -> unit
	    = _import "gtk_tree_store_insert_before"
		      : cptr * cptr * cptr * cptr -> unit;
	val insert_before : 'a t -> TreeIter.t -> TreeIter.t -> TreeIter.t
	    = fn self => fn parent => fn sibling =>
		 let val iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => insert_before_
					       (self, iter, parent, sibling))
		 in iter end
	val insert_after_ : cptr * cptr * cptr * cptr -> unit
	    = _import "gtk_tree_store_insert_after"
		      : cptr * cptr * cptr * cptr -> unit;
	val insert_after : 'a t -> TreeIter.t -> TreeIter.t -> TreeIter.t
	    = fn self => fn parent => fn sibling =>
		 let val iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => insert_after_
					       (self, iter, parent, sibling))
		 in iter end
	val prepend_ : cptr * cptr * cptr -> unit
	    = _import "gtk_tree_store_prepend" : cptr * cptr * cptr -> unit;
	val prepend : 'a t -> TreeIter.t -> TreeIter.t
	    = fn self => fn parent =>
		 let val iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => prepend_ (self, iter, parent))
		 in iter end
	val append_ : cptr * cptr * cptr -> unit
	    = _import "gtk_tree_store_append" : cptr * cptr * cptr -> unit;
	val append : 'a t -> TreeIter.t -> TreeIter.t
	    = fn self => fn parent =>
		 let val iter = TreeIter.alloc_GtkTreeIter ()
		     val ret = GObject.withPtr
				 (self, 
				  fn self => append_ (self, iter, parent))
		 in iter end
	val is_ancestor_ : cptr * cptr * cptr -> bool
	    = _import "gtk_tree_store_is_ancestor"
		      : cptr * cptr * cptr -> bool;
	val is_ancestor : 'a t -> TreeIter.t -> TreeIter.t -> bool
	    = fn self => fn iter => fn descendant =>
		 GObject.withPtr
		   (self, fn self => is_ancestor_ (self, iter, descendant))
	val storeiter_depth_ : cptr * cptr -> int
	    = _import "gtk_tree_store_iter_depth" : cptr * cptr -> int;
	val storeiter_depth : 'a t -> TreeIter.t -> int
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, fn self => storeiter_depth_ (self, iter))
	val clear_ : cptr -> unit
	    = _import "gtk_tree_store_clear" : cptr -> unit;
	val clear : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => clear_ self)
	val storeiter_is_valid_ : cptr * cptr -> bool
	    = _import "gtk_tree_store_iter_is_valid" : cptr * cptr -> bool;
	val storeiter_is_valid : 'a t -> TreeIter.t -> bool
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, fn self => storeiter_is_valid_ (self, iter))
	val reorder_ : cptr * cptr * int array -> unit
	    = _import "gtk_tree_store_reorder"
		      : cptr * cptr * int array -> unit;
	val reorder : 'a t -> TreeIter.t -> int list -> unit
	    = fn self => fn parent => fn new_order =>
		 GObject.withPtr
		   (self, 
		    fn self => reorder_ (self, parent, 
					 Array.fromList new_order))
	val swap_ : cptr * cptr * cptr -> unit
	    = _import "gtk_tree_store_swap" : cptr * cptr * cptr -> unit;
	val swap : 'a t -> TreeIter.t -> TreeIter.t -> unit
	    = fn self => fn a => fn b =>
		 GObject.withPtr (self, fn self => swap_ (self, a, b))
	val move_after_ : cptr * cptr * cptr -> unit
	    = _import "gtk_tree_store_move_after" : cptr * cptr * cptr -> unit;
	val move_after : 'a t -> TreeIter.t -> TreeIter.t option -> unit
	    = fn self => fn iter => fn position =>
		 GObject.withPtr
		   (self, 
		    fn self => move_after_ (self, iter, 
					    getOpt (position, GObject.null)))
	val move_after' : 'a t -> TreeIter.t -> unit
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, fn self => move_after_ (self, iter, GObject.null))
	val move_before_ : cptr * cptr * cptr -> unit
	    = _import "gtk_tree_store_move_before"
		      : cptr * cptr * cptr -> unit;
	val move_before : 'a t -> TreeIter.t -> TreeIter.t option -> unit
	    = fn self => fn iter => fn position =>
		 GObject.withPtr
		   (self, 
		    fn self => move_before_ (self, iter, 
					     getOpt (position, GObject.null)))
	val move_before' : 'a t -> TreeIter.t -> unit
	    = fn self => fn iter =>
		 GObject.withPtr
		   (self, fn self => move_before_ (self, iter, GObject.null))
    end
    structure UIManager :>
      sig
	type base
	type 'a uimanager_t
	type 'a t = 'a uimanager_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toUIManager : 'a t -> base t
	type itemtype
	val AUTO : itemtype
	val MENUBAR : itemtype
	val MENU : itemtype
	val TOOLBAR : itemtype
	val PLACEHOLDER : itemtype
	val POPUP : itemtype
	val MENUITEM : itemtype
	val TOOLITEM : itemtype
	val SEPARATOR : itemtype
	val ACCELERATOR : itemtype
	val get_type : unit -> GType.t
	val new : unit -> base t
	val set_add_tearoffs : 'a t -> bool -> unit
	val get_add_tearoffs : 'a t -> bool
	val insert_actiongroup : 'a t -> 'b ActionGroup.t -> int -> unit
	val remove_actiongroup : 'a t -> 'b ActionGroup.t -> unit
	val get_accelgroup : 'a t -> base AccelGroup.t
	val get_widget : 'a t -> string -> base Widget.t
	val get_action : 'a t -> string -> base Action.t
	val add_ui : 'a t -> int -> string -> string -> string option 
		  -> itemtype list -> bool
		     -> unit
	val add_ui'
	  : 'a t -> int -> string -> string -> itemtype list -> bool -> unit
	val remove_ui : 'a t -> int -> unit
	val get_ui : 'a t -> string
	val ensure_update : 'a t -> unit
	val new_merge_id : 'a t -> int
	val add_widget_sig : (unit -> unit) -> 'a t Signal.signal
	val actions_changed_sig : (unit -> unit) -> 'a t Signal.signal
	val connect_proxy_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	val disconnect_proxy_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	val pre_activate_sig : (unit -> unit) -> 'a t Signal.signal
	val post_activate_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a uimanager_t = unit
	type 'a t = 'a uimanager_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toUIManager obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	type itemtype = int
	val get_itemtype_ : int ref * int ref * int ref * int ref * int ref 
			  * int ref * int ref * int ref * int ref * int ref
			    -> unit
	    = _import "mgtk_get_gtk_ui_manager_itemtype"
		      : int ref * int ref * int ref * int ref * int ref 
		      * int ref * int ref * int ref * int ref * int ref
			-> unit;
	val (AUTO, MENUBAR, MENU, TOOLBAR, PLACEHOLDER, POPUP, MENUITEM, 
	     TOOLITEM, SEPARATOR, ACCELERATOR)
	    = let val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9)
		      = (ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, 
			 ref 0, ref 0, ref 0)
	      in get_itemtype_ (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9)
	       ; (!x0, !x1, !x2, !x3, !x4, !x5, !x6, !x7, !x8, !x9)
	      end
	val get_type_ : unit -> GType.t
	    = _import "gtk_ui_manager_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_ui_manager_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val set_add_tearoffs_ : cptr * bool -> unit
	    = _import "gtk_ui_manager_set_add_tearoffs" : cptr * bool -> unit;
	val set_add_tearoffs : 'a t -> bool -> unit
	    = fn self => fn add_tearoffs =>
		 GObject.withPtr
		   (self, fn self => set_add_tearoffs_ (self, add_tearoffs))
	val get_add_tearoffs_ : cptr -> bool
	    = _import "gtk_ui_manager_get_add_tearoffs" : cptr -> bool;
	val get_add_tearoffs : 'a t -> bool
	    = fn self => GObject.withPtr
			   (self, fn self => get_add_tearoffs_ self)
	val insert_actiongroup_ : cptr * cptr * int -> unit
	    = _import "gtk_ui_manager_insert_action_group"
		      : cptr * cptr * int -> unit;
	val insert_actiongroup : 'a t -> 'b ActionGroup.t -> int -> unit
	    = fn self => fn action_group => fn pos =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr
				 (action_group, 
				  fn action_group =>
				     insert_actiongroup_
				       (self, action_group, pos)))
	val remove_actiongroup_ : cptr * cptr -> unit
	    = _import "gtk_ui_manager_remove_action_group"
		      : cptr * cptr -> unit;
	val remove_actiongroup : 'a t -> 'b ActionGroup.t -> unit
	    = fn self => fn action_group =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withPtr (action_group, 
						fn action_group =>
						   remove_actiongroup_
						     (self, action_group)))
	val get_accelgroup_ : cptr -> cptr
	    = _import "gtk_ui_manager_get_accel_group" : cptr -> cptr;
	val get_accelgroup : 'a t -> base AccelGroup.t
	    = fn self => AccelGroup.inherit
			   ()
			   (fn () => GObject.withPtr
				       (self, fn self => get_accelgroup_ self))
	val get_widget_ : cptr * CString.cstring -> cptr
	    = _import "gtk_ui_manager_get_widget"
		      : cptr * CString.cstring -> cptr;
	val get_widget : 'a t -> string -> base Widget.t
	    = fn self => fn path =>
		 Widget.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self => get_widget_
					     (self, CString.fromString path)))
	val get_action_ : cptr * CString.cstring -> cptr
	    = _import "gtk_ui_manager_get_action"
		      : cptr * CString.cstring -> cptr;
	val get_action : 'a t -> string -> base Action.t
	    = fn self => fn path =>
		 Action.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, 
				fn self => get_action_
					     (self, CString.fromString path)))
	val add_ui_ : cptr * int * CString.cstring * CString.cstring 
		    * CString.cstring * int * bool
		      -> unit
	    = _import "gtk_ui_manager_add_ui"
		      : cptr * int * CString.cstring * CString.cstring 
		      * CString.cstring * int * bool
			-> unit;
	val add_ui : 'a t -> int -> string -> string -> string option 
		  -> itemtype list -> bool
		     -> unit
	    = fn self => fn merge_id => fn path => fn name => fn action => 
	      fn typ => fn top =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       add_ui_ (self, merge_id, CString.fromString path, 
				CString.fromString name, 
				CString.fromString (getOpt (action, "")), 
				Flags.set typ, top))
	val add_ui'
	  : 'a t -> int -> string -> string -> itemtype list -> bool -> unit
	    = fn self => fn merge_id => fn path => fn name => fn typ => 
	      fn top =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       add_ui_ (self, merge_id, CString.fromString path, 
				CString.fromString name, 
				CString.fromString "", Flags.set typ, top))
	val remove_ui_ : cptr * int -> unit
	    = _import "gtk_ui_manager_remove_ui" : cptr * int -> unit;
	val remove_ui : 'a t -> int -> unit
	    = fn self => fn merge_id =>
		 GObject.withPtr (self, fn self => remove_ui_ (self, merge_id))
	val get_ui_ : cptr -> CString.t
	    = _import "gtk_ui_manager_get_ui" : cptr -> CString.t;
	val get_ui : 'a t -> string
	    = fn self => GObject.withPtr (self, 
					  fn self => let val t = get_ui_ self
						     in CString.toString t end)
	val ensure_update_ : cptr -> unit
	    = _import "gtk_ui_manager_ensure_update" : cptr -> unit;
	val ensure_update : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => ensure_update_ self)
	val new_merge_id_ : cptr -> int
	    = _import "gtk_ui_manager_new_merge_id" : cptr -> int;
	val new_merge_id : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => new_merge_id_ self)
	local open Signal
	      infixr -->
	in
	  val add_widget_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "add-widget" false (unit --> return_void) f
	  val actions_changed_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "actions-changed" false (void --> return_void) f
	  val connect_proxy_sig : (unit -> unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "connect-proxy" false
			       (unit --> unit --> return_void) f
	  val disconnect_proxy_sig
	    : (unit -> unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "disconnect-proxy" false
			       (unit --> unit --> return_void) f
	  val pre_activate_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "pre-activate" false (unit --> return_void) f
	  val post_activate_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "post-activate" false (unit --> return_void) f
	end
    end
    structure VButtonBox :>
      sig
	type base
	type 'a vbuttonbox_t
	type 'a t = 'a vbuttonbox_t ButtonBox.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toVButtonBox : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a vbuttonbox_t = unit
	type 'a t = 'a vbuttonbox_t ButtonBox.t
	fun inherit w con = ButtonBox.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toVButtonBox obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_vbutton_box_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_vbutton_box_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure Viewport :>
      sig
	type base
	type 'a viewport_t
	type 'a t = 'a viewport_t Bin.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toViewport : 'a t -> base t
	val get_type : unit -> GType.t
	val new : 'a Adjustment.t -> 'b Adjustment.t -> base t
	val get_hadjustment : 'a t -> base Adjustment.t
	val get_vadjustment : 'a t -> base Adjustment.t
	val set_hadjustment : 'a t -> 'b Adjustment.t option -> unit
	val set_hadjustment' : 'a t -> unit
	val set_vadjustment : 'a t -> 'b Adjustment.t option -> unit
	val set_vadjustment' : 'a t -> unit
	val set_shadowtype : 'a t -> shadowtype -> unit
	val get_shadowtype : 'a t -> shadowtype
	val set_scroll_adjustments_sig
	  : (unit -> unit -> unit) -> 'a t Signal.signal
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a viewport_t = unit
	type 'a t = 'a viewport_t Bin.t
	fun inherit w con = Bin.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toViewport obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_viewport_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr * cptr -> cptr
	    = _import "gtk_viewport_new" : cptr * cptr -> cptr;
	val new : 'a Adjustment.t -> 'b Adjustment.t -> base t
	    = fn hadjustment => fn vadjustment =>
		 make (GObject.withPtr
			 (hadjustment, 
			  fn hadjustment =>
			     GObject.withPtr
			       (vadjustment, 
				fn vadjustment =>
				   new_ (hadjustment, vadjustment))))
	val get_hadjustment_ : cptr -> cptr
	    = _import "gtk_viewport_get_hadjustment" : cptr -> cptr;
	val get_hadjustment : 'a t -> base Adjustment.t
	    = fn self =>
		 Adjustment.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_hadjustment_ self))
	val get_vadjustment_ : cptr -> cptr
	    = _import "gtk_viewport_get_vadjustment" : cptr -> cptr;
	val get_vadjustment : 'a t -> base Adjustment.t
	    = fn self =>
		 Adjustment.inherit
		   ()
		   (fn () => GObject.withPtr
			       (self, fn self => get_vadjustment_ self))
	val set_hadjustment_ : cptr * cptr -> unit
	    = _import "gtk_viewport_set_hadjustment" : cptr * cptr -> unit;
	val set_hadjustment : 'a t -> 'b Adjustment.t option -> unit
	    = fn self => fn adjustment =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withOpt
				 (adjustment, 
				  fn adjustment => set_hadjustment_
						     (self, adjustment)))
	val set_hadjustment' : 'a t -> unit
	    = fn self =>
		 GObject.withPtr
		   (self, fn self => set_hadjustment_ (self, GObject.null))
	val set_vadjustment_ : cptr * cptr -> unit
	    = _import "gtk_viewport_set_vadjustment" : cptr * cptr -> unit;
	val set_vadjustment : 'a t -> 'b Adjustment.t option -> unit
	    = fn self => fn adjustment =>
		 GObject.withPtr
		   (self, 
		    fn self => GObject.withOpt
				 (adjustment, 
				  fn adjustment => set_vadjustment_
						     (self, adjustment)))
	val set_vadjustment' : 'a t -> unit
	    = fn self =>
		 GObject.withPtr
		   (self, fn self => set_vadjustment_ (self, GObject.null))
	val set_shadowtype_ : cptr * int -> unit
	    = _import "gtk_viewport_set_shadow_type" : cptr * int -> unit;
	val set_shadowtype : 'a t -> shadowtype -> unit
	    = fn self => fn typ =>
		 GObject.withPtr (self, fn self => set_shadowtype_ (self, typ))
	val get_shadowtype_ : cptr -> int
	    = _import "gtk_viewport_get_shadow_type" : cptr -> int;
	val get_shadowtype : 'a t -> shadowtype
	    = fn self => GObject.withPtr
			   (self, fn self => get_shadowtype_ self)
	local open Signal
	      infixr -->
	in val set_scroll_adjustments_sig
	     : (unit -> unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "set-scroll-adjustments" false
			        (unit --> unit --> return_void) f
	end
    end
    structure VPaned :>
      sig
	type base
	type 'a vpaned_t
	type 'a t = 'a vpaned_t Paned.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toVPaned : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a vpaned_t = unit
	type 'a t = 'a vpaned_t Paned.t
	fun inherit w con = Paned.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toVPaned obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_vpaned_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_vpaned_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure VRuler :>
      sig
	type base
	type 'a vruler_t
	type 'a t = 'a vruler_t Ruler.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toVRuler : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a vruler_t = unit
	type 'a t = 'a vruler_t Ruler.t
	fun inherit w con = Ruler.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toVRuler obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_vruler_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_vruler_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure VScale :>
      sig
	type base
	type 'a vscale_t
	type 'a t = 'a vscale_t Scale.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toVScale : 'a t -> base t
	val get_type : unit -> GType.t
	val new : 'a Adjustment.t -> base t
	val new_with_range : real -> real -> real -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a vscale_t = unit
	type 'a t = 'a vscale_t Scale.t
	fun inherit w con = Scale.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toVScale obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_vscale_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr -> cptr = _import "gtk_vscale_new" : cptr -> cptr;
	val new : 'a Adjustment.t -> base t
	    = fn adjustment =>
		 make (GObject.withPtr
			 (adjustment, fn adjustment => new_ adjustment))
	val new_with_range_ : real * real * real -> cptr
	    = _import "gtk_vscale_new_with_range" : real * real * real -> cptr;
	val new_with_range : real -> real -> real -> base t
	    = fn min => fn max => fn step =>
		 make (new_with_range_ (min, max, step))
    end
    structure VScrollbar :>
      sig
	type base
	type 'a vscrollbar_t
	type 'a t = 'a vscrollbar_t Scrollbar.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toVScrollbar : 'a t -> base t
	val get_type : unit -> GType.t
	val new : 'a Adjustment.t -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a vscrollbar_t = unit
	type 'a t = 'a vscrollbar_t Scrollbar.t
	fun inherit w con = Scrollbar.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toVScrollbar obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_vscrollbar_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr -> cptr = _import "gtk_vscrollbar_new" : cptr -> cptr;
	val new : 'a Adjustment.t -> base t
	    = fn adjustment =>
		 make (GObject.withPtr
			 (adjustment, fn adjustment => new_ adjustment))
    end
    structure VSeparator :>
      sig
	type base
	type 'a vseparator_t
	type 'a t = 'a vseparator_t Separator.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toVSeparator : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a vseparator_t = unit
	type 'a t = 'a vseparator_t Separator.t
	fun inherit w con = Separator.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toVSeparator obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_vseparator_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_vseparator_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure WindowGroup :>
      sig
	type base
	type 'a windowgroup_t
	type 'a t = 'a windowgroup_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toWindowGroup : 'a t -> base t
	val new : unit -> base t
	val add_window : 'a t -> 'b t -> unit
	val remove_window : 'a t -> 'b t -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a windowgroup_t = unit
	type 'a t = 'a windowgroup_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toWindowGroup obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val new_ : unit -> cptr
	    = _import "gtk_window_group_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val add_window_ : cptr * cptr -> unit
	    = _import "gtk_window_group_add_window" : cptr * cptr -> unit;
	val add_window : 'a t -> 'b t -> unit
	    = fn self => fn window =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (window, fn window => add_window_ (self, window)))
	val remove_window_ : cptr * cptr -> unit
	    = _import "gtk_window_group_remove_window" : cptr * cptr -> unit;
	val remove_window : 'a t -> 'b t -> unit
	    = fn self => fn window =>
		 GObject.withPtr
		   (self, 
		    fn self =>
		       GObject.withPtr
			 (window, fn window => remove_window_ (self, window)))
    end
    structure CTreeNode :>
      sig
	type t = GObject.cptr
	type base
	val alloc_GtkCTreeNode : unit -> t
      end = struct
	type cptr = GObject.cptr
	type t = GObject.cptr
	type base = unit
	val alloc_GtkCTreeNode = _import "alloc_GtkCTreeNode" : unit -> t;
    end
    structure FileInfo :>
      sig
	type base
	type 'a fileinfo_t
	type 'a t = 'a fileinfo_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toFileInfo : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val copy : 'a t -> base t
	val free : 'a t -> unit
	val get_display_name : 'a t -> string
	val get_display_key : 'a t -> string
	val set_display_name : 'a t -> string -> unit
	val get_is_folder : 'a t -> bool
	val set_is_folder : 'a t -> bool -> unit
	val get_is_hidden : 'a t -> bool
	val set_is_hidden : 'a t -> bool -> unit
	val get_mime_type : 'a t -> string
	val set_mime_type : 'a t -> string -> unit
	val get_size : 'a t -> int
	val set_size : 'a t -> int -> unit
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a fileinfo_t = unit
	type 'a t = 'a fileinfo_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toFileInfo obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_file_info_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = _import "gtk_file_info_new" : unit -> cptr;
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val copy_ : cptr -> cptr = _import "gtk_file_info_copy" : cptr -> cptr;
	val copy : 'a t -> base t
	    = fn self => make (GObject.withPtr (self, fn self => copy_ self))
	val free_ : cptr -> unit = _import "gtk_file_info_free" : cptr -> unit;
	val free : 'a t -> unit
	    = fn self => GObject.withPtr (self, fn self => free_ self)
	val get_display_name_ : cptr -> CString.t
	    = _import "gtk_file_info_get_display_name" : cptr -> CString.t;
	val get_display_name : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_display_name_ self
				       in CString.toString t end)
	val get_display_key_ : cptr -> CString.t
	    = _import "gtk_file_info_get_display_key" : cptr -> CString.t;
	val get_display_key : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_display_key_ self
				       in CString.toString t end)
	val set_display_name_ : cptr * CString.cstring -> unit
	    = _import "gtk_file_info_set_display_name"
		      : cptr * CString.cstring -> unit;
	val set_display_name : 'a t -> string -> unit
	    = fn self => fn display_name =>
		 GObject.withPtr
		   (self, 
		    fn self => set_display_name_
				 (self, CString.fromString display_name))
	val get_is_folder_ : cptr -> bool
	    = _import "gtk_file_info_get_is_folder" : cptr -> bool;
	val get_is_folder : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_is_folder_ self)
	val set_is_folder_ : cptr * bool -> unit
	    = _import "gtk_file_info_set_is_folder" : cptr * bool -> unit;
	val set_is_folder : 'a t -> bool -> unit
	    = fn self => fn is_folder =>
		 GObject.withPtr
		   (self, fn self => set_is_folder_ (self, is_folder))
	val get_is_hidden_ : cptr -> bool
	    = _import "gtk_file_info_get_is_hidden" : cptr -> bool;
	val get_is_hidden : 'a t -> bool
	    = fn self => GObject.withPtr (self, fn self => get_is_hidden_ self)
	val set_is_hidden_ : cptr * bool -> unit
	    = _import "gtk_file_info_set_is_hidden" : cptr * bool -> unit;
	val set_is_hidden : 'a t -> bool -> unit
	    = fn self => fn is_hidden =>
		 GObject.withPtr
		   (self, fn self => set_is_hidden_ (self, is_hidden))
	val get_mime_type_ : cptr -> CString.t
	    = _import "gtk_file_info_get_mime_type" : cptr -> CString.t;
	val get_mime_type : 'a t -> string
	    = fn self => GObject.withPtr
			   (self, 
			    fn self => let val t = get_mime_type_ self
				       in CString.toString t end)
	val set_mime_type_ : cptr * CString.cstring -> unit
	    = _import "gtk_file_info_set_mime_type"
		      : cptr * CString.cstring -> unit;
	val set_mime_type : 'a t -> string -> unit
	    = fn self => fn mime_type =>
		 GObject.withPtr
		   (self, 
		    fn self => set_mime_type_
				 (self, CString.fromString mime_type))
	val get_size_ : cptr -> int
	    = _import "gtk_file_info_get_size" : cptr -> int;
	val get_size : 'a t -> int
	    = fn self => GObject.withPtr (self, fn self => get_size_ self)
	val set_size_ : cptr * int -> unit
	    = _import "gtk_file_info_set_size" : cptr * int -> unit;
	val set_size : 'a t -> int -> unit
	    = fn self => fn size =>
		 GObject.withPtr (self, fn self => set_size_ (self, size))
    end
    structure FileFolder :>
      sig
	type base
	type 'a filefolder_t
	type 'a t = 'a filefolder_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toFileFolder : 'a t -> base t
	val get_type : unit -> GType.t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a filefolder_t = unit
	type 'a t = 'a filefolder_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toFileFolder obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_file_folder_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
    end
    structure FileSystem :>
      sig
	type base
	type 'a filesystem_t
	type 'a t = 'a filesystem_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toFileSystem : 'a t -> base t
	val get_type : unit -> GType.t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a filesystem_t = unit
	type 'a t = 'a filesystem_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toFileSystem obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val get_type_ : unit -> GType.t
	    = _import "gtk_file_system_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
    end
    structure AccelMap :>
      sig
	type base
	type 'a accelmap_t
	type 'a t = 'a accelmap_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toAccelMap : 'a t -> base t
	val load : string -> unit
	val save : string -> unit
	val load_fd : int -> unit
	val save_fd : int -> unit
	val lock_path : string -> unit
	val unlock_path : string -> unit
	val add_filter : string -> unit
	val get_type : unit -> GType.t
	val get : unit -> base t
      end = struct
	type cptr = GObject.cptr
	type base = unit
	type 'a accelmap_t = unit
	type 'a t = 'a accelmap_t GObject.t
	fun inherit w con = GObject.inherit () con
	fun make ptr = inherit () (fn () => ptr)
	fun toAccelMap obj
	  = inherit () (fn () => GObject.withPtr (obj, fn obj => obj))
	val load_ : CString.cstring -> unit
	    = _import "gtk_accel_map_load" : CString.cstring -> unit;
	val load : string -> unit
	    = fn file_name => load_ (CString.fromString file_name)
	val save_ : CString.cstring -> unit
	    = _import "gtk_accel_map_save" : CString.cstring -> unit;
	val save : string -> unit
	    = fn file_name => save_ (CString.fromString file_name)
	val load_fd_ : int -> unit
	    = _import "gtk_accel_map_load_fd" : int -> unit;
	val load_fd : int -> unit = fn fd => load_fd_ fd
	val save_fd_ : int -> unit
	    = _import "gtk_accel_map_save_fd" : int -> unit;
	val save_fd : int -> unit = fn fd => save_fd_ fd
	val lock_path_ : CString.cstring -> unit
	    = _import "gtk_accel_map_lock_path" : CString.cstring -> unit;
	val lock_path : string -> unit
	    = fn accel_path => lock_path_ (CString.fromString accel_path)
	val unlock_path_ : CString.cstring -> unit
	    = _import "gtk_accel_map_unlock_path" : CString.cstring -> unit;
	val unlock_path : string -> unit
	    = fn accel_path => unlock_path_ (CString.fromString accel_path)
	val add_filter_ : CString.cstring -> unit
	    = _import "gtk_accel_map_add_filter" : CString.cstring -> unit;
	val add_filter : string -> unit
	    = fn filter_pattern =>
		 add_filter_ (CString.fromString filter_pattern)
	val get_type_ : unit -> GType.t
	    = _import "gtk_accel_map_get_type" : unit -> GType.t;
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val get_ : unit -> cptr = _import "gtk_accel_map_get" : unit -> cptr;
	val get : unit -> base t = fn dummy => make (get_ dummy)
    end
end
