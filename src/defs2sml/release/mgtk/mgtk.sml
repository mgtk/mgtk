structure Gtk  = struct
    structure GtkBasis :> 
      sig
        val init : string list -> string list
        val main : unit -> unit
        val main_quit : unit -> unit
    
        val symb : string -> Dynlib.symHandle
      end = struct
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
                        [] => (print "mGTK Warning: Gtk.init \
    				  \ called with empty list\n";
                               [CommandLine.name()])
                      | _  => args
            in
                init_(vector args)
            end
        val main : unit -> unit = app1(symb "mgtk_main")
        val main_quit : unit -> unit = app1(symb "mgtk_main_quit")
    end
    
    structure GObject :>
      sig
        type cptr
        type base
        type 'a t
        type constructor = unit -> cptr
    
        val null : cptr
    
        val repr     : 'a t -> cptr
        val inherit  : 'a -> constructor -> 'a t
        val toObject : 'a t -> base t
      end = struct
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
    
    structure GValue :>
      sig
        type GValues
        type GValue
    
        type 'a setter = GValue -> 'a -> unit
        val setBool   : bool setter
        val setInt    : int setter
        val setChar   : char setter
        val setReal   : real setter
        val setString : string setter
    
        type 'a getter = GValues -> int -> 'a
        val getBool   : bool getter
        val getInt    : int getter
        val getChar   : char getter
        val getReal   : real getter
        val getString : string getter
      end = struct
        open Dynlib
        val symb = GtkBasis.symb
    
        prim_type GValues
        prim_type GValue
    
        (* UNSAFE: no error checking in the set and get functions! *)
        type 'a setter = GValue -> 'a -> unit
        val setBool   : bool setter   = app2(symb "mgtk_set_bool")
        val setInt    : int setter    = app2(symb "mgtk_set_int")
        val setChar   : char setter   = app2(symb "mgtk_set_char")
        val setReal   : real setter   = app2(symb "mgtk_set_real")
        val setString : string setter = app2(symb "mgtk_set_string")
    
        type 'a getter = GValues -> int -> 'a
        val getBool   : bool getter   = app2(symb "mgtk_get_pos_bool")
        val getInt    : int getter    = app2(symb "mgtk_get_pos_int")
        val getChar   : char getter   = app2(symb "mgtk_get_pos_char")
        val getReal   : real getter   = app2(symb "mgtk_get_pos_real")
        val getString : string getter = app2(symb "mgtk_get_pos_string")
    (*
            val getLong   : int getter    = app2(symb "mgtk_get_pos_long")
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
        val string : (string, 'rest) read
        val unit   : (unit,   'rest) read
    
        val void : (unit, 'rest) read
    
        val return_bool   : bool   return
        val return_int    : int    return
        val return_char   : char   return
        val return_real   : real   return
        val return_string : string return
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
            open GValue
    
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
                    SOME f => f data    
                    (* FIXME: we need a handle here, but what should it do *)
                  | NONE   => 
                    raise Fail("mgtk: Unknown callback function (id: "^
                               Int.toString id^")")
    
            val dummy = 
                ( Callback.register "mgtk_callback_dispatch" dispatch
                ; Callback.register "mgtk_callback_destroy" destroy
                )
    
            fun register f = localId(fn id => (add (id, f); id))
            val signal_connect : GO.cptr -> string -> int -> bool -> int
                               = app4(symb"mgtk_signal_connect")
    
        in
        datatype state = S of GValue * GValues * int * int
        type ('a, 'b) trans   = 'a * state -> 'b * state
        type ('a, 'rest) read = ('a -> 'rest, 'rest) trans
        type 'a return        = ('a, unit) trans
    
        fun state f ret arg max = (f, S(ret, arg, max, 0+1))
        (* NOTE: the +1 is for the object connected to *)
    
        fun wrap conv f (ret, arg, max) = ignore(conv(state f ret arg max)) 
    
        fun getter get (f, S(ret, arg, max, next)) = 
            (* FIXME: it should be < but that gives problems with
               return_unit.  Currently unsafe! *)        
            if next < max  
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
    
        fun real x          = getter getReal x
        fun return_real x   = setter setReal x
    
        fun string x        = getter getString x
        fun return_string x = setter setString x
    
        
        (* FIXME: convince Ken that this correct *)
        fun void (f, state) = (f(), state)
        fun return_void (f, S(ret,arg,max,next)) = (f, S(ret,arg,max+1,next))
    
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
    
        fun areTheseSet flags flag = ((W(set flags)) andb (W flag))<>0w0
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
    
        open Dynlib
        val symb = GtkBasis.symb
       
        type t = int
    
        val int    = app1(symb "mgtk_g_type_int") ()
        val real   = app1(symb "mgtk_g_type_real") ()
        val string = app1(symb "mgtk_g_type_string") ()
    
        val toInt = fn t => t
        val toString : t -> string = app1(symb "mgtk_g_type_name")
    end
    open Dynlib
    type cptr = GObject.cptr
    val repr = GObject.repr
    val symb = GtkBasis.symb
    type requisition = GObject.cptr
    type ctree_node = GObject.cptr
    type icon_set = GObject.cptr
    type icon_source = GObject.cptr
    type selection_data = GObject.cptr
    type text_attributes = GObject.cptr
    type textiter = GObject.cptr
    type treeiter = GObject.cptr
    type accel_flags = int
    val get_accel_flags_ : unit -> int * int * int
	= app1 (symb"mgtk_get_gtk_accel_flags")
    val (ACCEL_VISIBLE, ACCEL_LOCKED, ACCEL_MASK) = get_accel_flags_ ()
    type celltype = int
    val get_celltype_ : unit -> int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_celltype")
    val (CELL_EMPTY, CELL_TEXT, CELL_PIXMAP, CELL_PIXTEXT, CELL_WIDGET)
	= get_celltype_ ()
    type debug_flag = int
    val get_debug_flag_ : unit -> int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_debug_flag")
    val (DEBUG_MISC, DEBUG_PLUGSOCKET, DEBUG_TEXT, DEBUG_TREE, DEBUG_UPDATES)
	= get_debug_flag_ ()
    type responsetype = int
    val get_responsetype_ : unit -> int * int * int * int * int * int * int 
				  * int * int * int * int
	= app1 (symb"mgtk_get_gtk_responsetype")
    val (RESPONSE_NONE, RESPONSE_REJECT, RESPONSE_ACCEPT, 
	 RESPONSE_DELETE_EVENT, RESPONSE_OK, RESPONSE_CANCEL, RESPONSE_CLOSE, 
	 RESPONSE_YES, RESPONSE_NO, RESPONSE_APPLY, RESPONSE_HELP)
	= get_responsetype_ ()
    type dest_defaults = int
    val get_dest_defaults_ : unit -> int * int * int * int
	= app1 (symb"mgtk_get_gtk_dest_defaults")
    val (DEST_DEFAULT_MOTION, DEST_DEFAULT_HIGHLIGHT, DEST_DEFAULT_DROP, 
	 DEST_DEFAULT_ALL)
	= get_dest_defaults_ ()
    type target_flags = int
    val get_target_flags_ : unit -> int * int
	= app1 (symb"mgtk_get_gtk_target_flags")
    val (TARGET_SAME_APP, TARGET_SAME_WIDGET) = get_target_flags_ ()
    type anchortype = int
    val get_anchortype_
      : unit -> int * int * int * int * int * int * int * int * int * int 
	      * int * int * int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_anchortype")
    val (ANCHOR_CENTER, ANCHOR_NORTH, ANCHOR_NORTH_WEST, ANCHOR_NORTH_EAST, 
	 ANCHOR_SOUTH, ANCHOR_SOUTH_WEST, ANCHOR_SOUTH_EAST, ANCHOR_WEST, 
	 ANCHOR_EAST, ANCHOR_N, ANCHOR_NW, ANCHOR_NE, ANCHOR_S, ANCHOR_SW, 
	 ANCHOR_SE, ANCHOR_W, ANCHOR_E)
	= get_anchortype_ ()
    type arrowtype = int
    val get_arrowtype_ : unit -> int * int * int * int
	= app1 (symb"mgtk_get_gtk_arrowtype")
    val (ARROW_UP, ARROW_DOWN, ARROW_LEFT, ARROW_RIGHT) = get_arrowtype_ ()
    type attach_options = int
    val get_attach_options_ : unit -> int * int * int
	= app1 (symb"mgtk_get_gtk_attach_options")
    val (EXPAND, SHRINK, FILL) = get_attach_options_ ()
    type curvetype = int
    val get_curvetype_ : unit -> int * int * int
	= app1 (symb"mgtk_get_gtk_curvetype")
    val (CURVE_TYPE_LINEAR, CURVE_TYPE_SPLINE, CURVE_TYPE_FREE)
	= get_curvetype_ ()
    type deletetype = int
    val get_deletetype_ : unit -> int * int * int * int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_deletetype")
    val (DELETE_CHARS, DELETE_WORD_ENDS, DELETE_WORDS, DELETE_DISPLAY_LINES, 
	 DELETE_DISPLAY_LINE_ENDS, DELETE_PARAGRAPH_ENDS, DELETE_PARAGRAPHS, 
	 DELETE_WHITESPACE)
	= get_deletetype_ ()
    type directiontype = int
    val get_directiontype_ : unit -> int * int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_directiontype")
    val (DIR_TAB_FORWARD, DIR_TAB_BACKWARD, DIR_UP, DIR_DOWN, DIR_LEFT, 
	 DIR_RIGHT)
	= get_directiontype_ ()
    type expander_style = int
    val get_expander_style_ : unit -> int * int * int * int
	= app1 (symb"mgtk_get_gtk_expander_style")
    val (EXPANDER_COLLAPSED, EXPANDER_SEMI_COLLAPSED, EXPANDER_SEMI_EXPANDED, 
	 EXPANDER_EXPANDED)
	= get_expander_style_ ()
    type icon_size = int
    val get_icon_size_ : unit -> int * int * int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_icon_size")
    val (ICON_SIZE_INVALID, ICON_SIZE_MENU, ICON_SIZE_SMALL_TOOLBAR, 
	 ICON_SIZE_LARGE_TOOLBAR, ICON_SIZE_BUTTON, ICON_SIZE_DND, 
	 ICON_SIZE_DIALOG)
	= get_icon_size_ ()
    type sidetype = int
    val get_sidetype_ : unit -> int * int * int * int
	= app1 (symb"mgtk_get_gtk_sidetype")
    val (SIDE_TOP, SIDE_BOTTOM, SIDE_LEFT, SIDE_RIGHT) = get_sidetype_ ()
    type text_direction = int
    val get_text_direction_ : unit -> int * int * int
	= app1 (symb"mgtk_get_gtk_text_direction")
    val (TEXT_DIR_NONE, TEXT_DIR_LTR, TEXT_DIR_RTL) = get_text_direction_ ()
    type justification = int
    val get_justification_ : unit -> int * int * int * int
	= app1 (symb"mgtk_get_gtk_justification")
    val (JUSTIFY_LEFT, JUSTIFY_RIGHT, JUSTIFY_CENTER, JUSTIFY_FILL)
	= get_justification_ ()
    type matchtype = int
    val get_matchtype_ : unit -> int * int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_matchtype")
    val (MATCH_ALL, MATCH_ALL_TAIL, MATCH_HEAD, MATCH_TAIL, MATCH_EXACT, 
	 MATCH_LAST)
	= get_matchtype_ ()
    type metrictype = int
    val get_metrictype_ : unit -> int * int * int
	= app1 (symb"mgtk_get_gtk_metrictype")
    val (PIXELS, INCHES, CENTIMETERS) = get_metrictype_ ()
    type movement_step = int
    val get_movement_step_
      : unit -> int * int * int * int * int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_movement_step")
    val (MOVEMENT_LOGICAL_POSITIONS, MOVEMENT_VISUAL_POSITIONS, 
	 MOVEMENT_WORDS, MOVEMENT_DISPLAY_LINES, MOVEMENT_DISPLAY_LINE_ENDS, 
	 MOVEMENT_PARAGRAPHS, MOVEMENT_PARAGRAPH_ENDS, MOVEMENT_PAGES, 
	 MOVEMENT_BUFFER_ENDS)
	= get_movement_step_ ()
    type orientation = int
    val get_orientation_ : unit -> int * int
	= app1 (symb"mgtk_get_gtk_orientation")
    val (ORIENTATION_HORIZONTAL, ORIENTATION_VERTICAL) = get_orientation_ ()
    type cornertype = int
    val get_cornertype_ : unit -> int * int * int * int
	= app1 (symb"mgtk_get_gtk_cornertype")
    val (CORNER_TOP_LEFT, CORNER_BOTTOM_LEFT, CORNER_TOP_RIGHT, 
	 CORNER_BOTTOM_RIGHT)
	= get_cornertype_ ()
    type packtype = int
    val get_packtype_ : unit -> int * int = app1 (symb"mgtk_get_gtk_packtype")
    val (PACK_START, PACK_END) = get_packtype_ ()
    type path_prioritytype = int
    val get_path_prioritytype_ : unit -> int * int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_path_prioritytype")
    val (PATH_PRIO_LOWEST, PATH_PRIO_GTK, PATH_PRIO_APPLICATION, 
	 PATH_PRIO_THEME, PATH_PRIO_RC, PATH_PRIO_HIGHEST)
	= get_path_prioritytype_ ()
    type pathtype = int
    val get_pathtype_ : unit -> int * int * int
	= app1 (symb"mgtk_get_gtk_pathtype")
    val (PATH_WIDGET, PATH_WIDGET_CLASS, PATH_CLASS) = get_pathtype_ ()
    type policytype = int
    val get_policytype_ : unit -> int * int * int
	= app1 (symb"mgtk_get_gtk_policytype")
    val (POLICY_ALWAYS, POLICY_AUTOMATIC, POLICY_NEVER) = get_policytype_ ()
    type positiontype = int
    val get_positiontype_ : unit -> int * int * int * int
	= app1 (symb"mgtk_get_gtk_positiontype")
    val (POS_LEFT, POS_RIGHT, POS_TOP, POS_BOTTOM) = get_positiontype_ ()
    type previewtype = int
    val get_previewtype_ : unit -> int * int
	= app1 (symb"mgtk_get_gtk_previewtype")
    val (PREVIEW_COLOR, PREVIEW_GRAYSCALE) = get_previewtype_ ()
    type relief_style = int
    val get_relief_style_ : unit -> int * int * int
	= app1 (symb"mgtk_get_gtk_relief_style")
    val (RELIEF_NORMAL, RELIEF_HALF, RELIEF_NONE) = get_relief_style_ ()
    type resize_mode = int
    val get_resize_mode_ : unit -> int * int * int
	= app1 (symb"mgtk_get_gtk_resize_mode")
    val (RESIZE_PARENT, RESIZE_QUEUE, RESIZE_IMMEDIATE) = get_resize_mode_ ()
    type scrolltype = int
    val get_scrolltype_
      : unit -> int * int * int * int * int * int * int * int * int * int 
	      * int * int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_scrolltype")
    val (SCROLL_NONE, SCROLL_JUMP, SCROLL_STEP_BACKWARD, SCROLL_STEP_FORWARD, 
	 SCROLL_PAGE_BACKWARD, SCROLL_PAGE_FORWARD, SCROLL_STEP_UP, 
	 SCROLL_STEP_DOWN, SCROLL_PAGE_UP, SCROLL_PAGE_DOWN, SCROLL_STEP_LEFT, 
	 SCROLL_STEP_RIGHT, SCROLL_PAGE_LEFT, SCROLL_PAGE_RIGHT, SCROLL_START, 
	 SCROLL_END)
	= get_scrolltype_ ()
    type selection_mode = int
    val get_selection_mode_ : unit -> int * int * int * int
	= app1 (symb"mgtk_get_gtk_selection_mode")
    val (SELECTION_NONE, SELECTION_SINGLE, SELECTION_BROWSE, 
	 SELECTION_MULTIPLE)
	= get_selection_mode_ ()
    type shadowtype = int
    val get_shadowtype_ : unit -> int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_shadowtype")
    val (SHADOW_NONE, SHADOW_IN, SHADOW_OUT, SHADOW_ETCHED_IN, 
	 SHADOW_ETCHED_OUT)
	= get_shadowtype_ ()
    type statetype = int
    val get_statetype_ : unit -> int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_statetype")
    val (STATE_NORMAL, STATE_ACTIVE, STATE_PRELIGHT, STATE_SELECTED, 
	 STATE_INSENSITIVE)
	= get_statetype_ ()
    type submenu_direction = int
    val get_submenu_direction_ : unit -> int * int
	= app1 (symb"mgtk_get_gtk_submenu_direction")
    val (DIRECTION_LEFT, DIRECTION_RIGHT) = get_submenu_direction_ ()
    type submenu_placement = int
    val get_submenu_placement_ : unit -> int * int
	= app1 (symb"mgtk_get_gtk_submenu_placement")
    val (TOP_BOTTOM, LEFT_RIGHT) = get_submenu_placement_ ()
    type updatetype = int
    val get_updatetype_ : unit -> int * int * int
	= app1 (symb"mgtk_get_gtk_updatetype")
    val (UPDATE_CONTINUOUS, UPDATE_DISCONTINUOUS, UPDATE_DELAYED)
	= get_updatetype_ ()
    type visibility = int
    val get_visibility_ : unit -> int * int * int
	= app1 (symb"mgtk_get_gtk_visibility")
    val (VISIBILITY_NONE, VISIBILITY_PARTIAL, VISIBILITY_FULL)
	= get_visibility_ ()
    type wrap_mode = int
    val get_wrap_mode_ : unit -> int * int * int
	= app1 (symb"mgtk_get_gtk_wrap_mode")
    val (WRAP_NONE, WRAP_CHAR, WRAP_WORD) = get_wrap_mode_ ()
    type sorttype = int
    val get_sorttype_ : unit -> int * int = app1 (symb"mgtk_get_gtk_sorttype")
    val (SORT_ASCENDING, SORT_DESCENDING) = get_sorttype_ ()
    type imagetype = int
    val get_imagetype_ : unit -> int * int * int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_imagetype")
    val (IMAGE_EMPTY, IMAGE_PIXMAP, IMAGE_IMAGE, IMAGE_PIXBUF, IMAGE_STOCK, 
	 IMAGE_ICON_SET, IMAGE_ANIMATION)
	= get_imagetype_ ()
    type messagetype = int
    val get_messagetype_ : unit -> int * int * int * int
	= app1 (symb"mgtk_get_gtk_messagetype")
    val (MESSAGE_INFO, MESSAGE_WARNING, MESSAGE_QUESTION, MESSAGE_ERROR)
	= get_messagetype_ ()
    type buttonstype = int
    val get_buttonstype_ : unit -> int * int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_buttonstype")
    val (BUTTONS_NONE, BUTTONS_OK, BUTTONS_CLOSE, BUTTONS_CANCEL, 
	 BUTTONS_YES_NO, BUTTONS_OK_CANCEL)
	= get_buttonstype_ ()
    type arg_flags = int
    val get_arg_flags_ : unit -> int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_arg_flags")
    val (ARG_READABLE, ARG_WRITABLE, ARG_CONSTRUCT, ARG_CONSTRUCT_ONLY, 
	 ARG_CHILD_ARG)
	= get_arg_flags_ ()
    type rc_flags = int
    val get_rc_flags_ : unit -> int * int * int * int
	= app1 (symb"mgtk_get_gtk_rc_flags")
    val (RC_FG, RC_BG, RC_TEXT, RC_BASE) = get_rc_flags_ ()
    type rc_tokentype = int
    val get_rc_tokentype_
      : unit -> int * int * int * int * int * int * int * int * int * int 
	      * int * int * int * int * int * int * int * int * int * int 
	      * int * int * int * int * int * int * int * int * int * int 
	      * int * int * int * int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_rc_tokentype")
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
	= get_rc_tokentype_ ()
    type spintype = int
    val get_spintype_ : unit -> int * int * int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_spintype")
    val (SPIN_STEP_FORWARD, SPIN_STEP_BACKWARD, SPIN_PAGE_FORWARD, 
	 SPIN_PAGE_BACKWARD, SPIN_HOME, SPIN_END, SPIN_USER_DEFINED)
	= get_spintype_ ()
    type text_search_flags = int
    val get_text_search_flags_ : unit -> int * int
	= app1 (symb"mgtk_get_gtk_text_search_flags")
    val (TEXT_SEARCH_VISIBLE_ONLY, TEXT_SEARCH_TEXT_ONLY)
	= get_text_search_flags_ ()
    type text_window_type_t = int
    val get_text_window_type_t_
      : unit -> int * int * int * int * int * int * int
	= app1 (symb"mgtk_get_gtk_text_window_type")
    val (TEXT_WINDOW_PRIVATE, TEXT_WINDOW_WIDGET, TEXT_WINDOW_TEXT, 
	 TEXT_WINDOW_LEFT, TEXT_WINDOW_RIGHT, TEXT_WINDOW_TOP, 
	 TEXT_WINDOW_BOTTOM)
	= get_text_window_type_t_ ()
    type tree_path = GObject.cptr
    type function = GObject.cptr
    
    
    val accelerator_get_default_mod_mask_ : unit -> int
	= app1 (symb"mgtk_gtk_accelerator_get_default_mod_mask")
    val accelerator_get_default_mod_mask : unit -> int
	= fn dummy => accelerator_get_default_mod_mask_ dummy
    
    val accel_map_load_ : string -> unit = app1 (symb"mgtk_gtk_accel_map_load")
    val accel_map_load : string -> unit
	= fn file_name => accel_map_load_ file_name
    val accel_map_save_ : string -> unit = app1 (symb"mgtk_gtk_accel_map_save")
    val accel_map_save : string -> unit
	= fn file_name => accel_map_save_ file_name
    
    val accel_map_load_fd_ : int -> unit
	= app1 (symb"mgtk_gtk_accel_map_load_fd")
    val accel_map_load_fd : int -> unit = fn fd => accel_map_load_fd_ fd
    val accel_map_save_fd_ : int -> unit
	= app1 (symb"mgtk_gtk_accel_map_save_fd")
    val accel_map_save_fd : int -> unit = fn fd => accel_map_save_fd_ fd
    val accel_map_add_filter_ : string -> unit
	= app1 (symb"mgtk_gtk_accel_map_add_filter")
    val accel_map_add_filter : string -> unit
	= fn filter_pattern => accel_map_add_filter_ filter_pattern
    
    
    
    
    
    val icon_size_lookup_ : int -> int ref -> int ref -> bool
	= app3 (symb"mgtk_gtk_icon_size_lookup")
    val icon_size_lookup : icon_size -> bool * int * int
	= fn size => let val (width, height) = (ref 0, ref 0)
			 val ret = icon_size_lookup_ size width height
		     in (ret, !width, !height) end
    val icon_size_register_ : string -> int -> int -> int
	= app3 (symb"mgtk_gtk_icon_size_register")
    val icon_size_register : string -> int -> int -> icon_size
	= fn name => fn width => fn height =>
	     icon_size_register_ name width height
    val icon_size_register_alias_ : string -> int -> unit
	= app2 (symb"mgtk_gtk_icon_size_register_alias")
    val icon_size_register_alias : string -> icon_size -> unit
	= fn alias => fn target => icon_size_register_alias_ alias target
    val icon_size_from_name_ : string -> int
	= app1 (symb"mgtk_gtk_icon_size_from_name")
    val icon_size_from_name : string -> icon_size
	= fn name => icon_size_from_name_ name
    val icon_size_get_name_ : int -> string
	= app1 (symb"mgtk_gtk_icon_size_get_name")
    val icon_size_get_name : icon_size -> string
	= fn size => icon_size_get_name_ size
    val icon_set_new_ : unit -> cptr = app1 (symb"mgtk_gtk_icon_set_new")
    val icon_set_new : unit -> icon_set = fn dummy => icon_set_new_ dummy
    val icon_source_get_type_ : unit -> GType.t
	= app1 (symb"mgtk_gtk_icon_source_get_type")
    val icon_source_get_type : unit -> GType.t
	= fn dummy => icon_source_get_type_ dummy
    val check_version_ : int -> int -> int -> string
	= app3 (symb"mgtk_gtk_check_version")
    val check_version : int -> int -> int -> string
	= fn required_major => fn required_minor => fn required_micro =>
	     check_version_ required_major required_minor required_micro
    val exit_ : int -> unit = app1 (symb"mgtk_gtk_exit")
    val exit : int -> unit = fn error_code => exit_ error_code
    val disable_setlocale_ : unit -> unit
	= app1 (symb"mgtk_gtk_disable_setlocale")
    val disable_setlocale : unit -> unit = fn dummy => disable_setlocale_ dummy
    val set_locale_ : unit -> string = app1 (symb"mgtk_gtk_set_locale")
    val set_locale : unit -> string = fn dummy => set_locale_ dummy
    
    val events_pending_ : unit -> int = app1 (symb"mgtk_gtk_events_pending")
    val events_pending : unit -> int = fn dummy => events_pending_ dummy
    val main_ : unit -> unit = app1 (symb"mgtk_gtk_main")
    val main : unit -> unit = fn dummy => main_ dummy
    val main_level_ : unit -> int = app1 (symb"mgtk_gtk_main_level")
    val main_level : unit -> int = fn dummy => main_level_ dummy
    val main_quit_ : unit -> unit = app1 (symb"mgtk_gtk_main_quit")
    val main_quit : unit -> unit = fn dummy => main_quit_ dummy
    val main_iteration_ : unit -> bool = app1 (symb"mgtk_gtk_main_iteration")
    val main_iteration : unit -> bool = fn dummy => main_iteration_ dummy
    val main_iteration_do_ : bool -> bool
	= app1 (symb"mgtk_gtk_main_iteration_do")
    val main_iteration_do : bool option -> bool
	= fn blocking => main_iteration_do_ (getOpt (blocking, true))
    val main_iteration_do' : unit -> bool = fn dummy => main_iteration_do_ true
    val rc_add_default_file_ : string -> unit
	= app1 (symb"mgtk_gtk_rc_add_default_file")
    val rc_add_default_file : string -> unit
	= fn filename => rc_add_default_file_ filename
    
    
    
    
    
    val border_get_type_ : unit -> GType.t
	= app1 (symb"mgtk_gtk_border_get_type")
    val border_get_type : unit -> GType.t = fn dummy => border_get_type_ dummy
    val tips_query_get_type_ : unit -> GType.t
	= app1 (symb"mgtk_gtk_tips_query_get_type")
    val tips_query_get_type : unit -> GType.t
	= fn dummy => tips_query_get_type_ dummy
    val tree_path_new_ : unit -> cptr = app1 (symb"mgtk_gtk_tree_path_new")
    val tree_path_new : unit -> tree_path = fn dummy => tree_path_new_ dummy
    val tree_path_new_from_string_ : string -> cptr
	= app1 (symb"mgtk_gtk_tree_path_new_from_string")
    val tree_path_new_from_string : string -> tree_path
	= fn path => tree_path_new_from_string_ path
    val requisition_get_type_ : unit -> GType.t
	= app1 (symb"mgtk_gtk_requisition_get_type")
    val requisition_get_type : unit -> GType.t
	= fn dummy => requisition_get_type_ dummy
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a accelgroup_t = unit
	type 'a t = 'a accelgroup_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toAccelGroup obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_accelgroup_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_accelgroup_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val lock_ : cptr -> unit = app1 (symb"mgtk_gtk_accelgroup_lock")
	val lock : 'a t -> unit = fn self => lock_ (repr self)
	val unlock_ : cptr -> unit = app1 (symb"mgtk_gtk_accelgroup_unlock")
	val unlock : 'a t -> unit = fn self => unlock_ (repr self)
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
    structure IconFactory :>
      sig
	type base
	type 'a iconfactory_t
	type 'a t = 'a iconfactory_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toIconFactory : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val add : 'a t -> string -> icon_set -> unit
	val lookup : 'a t -> string -> icon_set
	val add_default : 'a t -> unit
	val remove_default : 'a t -> unit
	val lookup_default : string -> icon_set
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a iconfactory_t = unit
	type 'a t = 'a iconfactory_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toIconFactory obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_icon_factory_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_icon_factory_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val add_ : cptr -> string -> cptr -> unit
	    = app3 (symb"mgtk_gtk_icon_factory_add")
	val add : 'a t -> string -> icon_set -> unit
	    = fn self => fn stock_id => fn icon_set =>
		 add_ (repr self) stock_id icon_set
	val lookup_ : cptr -> string -> cptr
	    = app2 (symb"mgtk_gtk_icon_factory_lookup")
	val lookup : 'a t -> string -> icon_set
	    = fn self => fn stock_id => lookup_ (repr self) stock_id
	val add_default_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_icon_factory_add_default")
	val add_default : 'a t -> unit = fn self => add_default_ (repr self)
	val remove_default_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_icon_factory_remove_default")
	val remove_default : 'a t -> unit
	    = fn self => remove_default_ (repr self)
	val lookup_default_ : string -> cptr
	    = app1 (symb"mgtk_gtk_icon_factory_lookup_default")
	val lookup_default : string -> icon_set
	    = fn stock_id => lookup_default_ stock_id
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a object_t = unit
	type 'a t = 'a object_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toObject obj = inherit () (fn () => repr obj)
	type flags = int
	val get_flags_ : unit -> int * int * int * int
	    = app1 (symb"mgtk_get_gtk_object_flags")
	val (IN_DESTRUCTION, FLOATING, RESERVED_1, RESERVED_2) = get_flags_ ()
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_object_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : GType.t -> string -> cptr = app2 (symb"mgtk_gtk_object_new")
	val new : GType.t -> string -> base t
	    = fn typ => fn first_property_name =>
		 make (new_ typ first_property_name)
	val sink_ : cptr -> unit = app1 (symb"mgtk_gtk_object_sink")
	val sink : 'a t -> unit = fn self => sink_ (repr self)
	val destroy_ : cptr -> unit = app1 (symb"mgtk_gtk_object_destroy")
	val destroy : 'a t -> unit = fn self => destroy_ (repr self)
	local open Signal
	      infixr -->
	in val destroy_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "destroy" false (void --> return_void) f
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
	val new : real option -> real option -> real option -> real option 
	       -> real option -> real option
		  -> base t
	val new' : unit -> base t
	val changed : 'a t -> unit
	val value_changed : 'a t -> unit
	val clamp_page : 'a t -> real -> real -> unit
	val get_value : 'a t -> real
	val set_value : 'a t -> real -> unit
	val changed_sig : (unit -> unit) -> 'a t Signal.signal
	val value_changed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a adjustment_t = unit
	type 'a t = 'a adjustment_t Object.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Object.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toAdjustment obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_adjustment_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : real * real * real * real * real * real -> cptr
	    = app1 (symb"mgtk_gtk_adjustment_new")
	val new : real option -> real option -> real option -> real option 
	       -> real option -> real option
		  -> base t
	    = fn value => fn lower => fn upper => fn step_incr => 
	      fn page_incr => fn page_size =>
		 make (new_ (getOpt (value, 0.0), getOpt (lower, 0.0), 
			     getOpt (upper, 0.0), getOpt (step_incr, 0.0), 
			     getOpt (page_incr, 0.0), getOpt (page_size, 0.0)))
	val new' : unit -> base t
	    = fn dummy => make (new_ (0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
	val changed_ : cptr -> unit = app1 (symb"mgtk_gtk_adjustment_changed")
	val changed : 'a t -> unit = fn self => changed_ (repr self)
	val value_changed_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_adjustment_value_changed")
	val value_changed : 'a t -> unit
	    = fn self => value_changed_ (repr self)
	val clamp_page_ : cptr -> real -> real -> unit
	    = app3 (symb"mgtk_gtk_adjustment_clamp_page")
	val clamp_page : 'a t -> real -> real -> unit
	    = fn self => fn lower => fn upper =>
		 clamp_page_ (repr self) lower upper
	val get_value_ : cptr -> real
	    = app1 (symb"mgtk_gtk_adjustment_get_value")
	val get_value : 'a t -> real = fn self => get_value_ (repr self)
	val set_value_ : cptr -> real -> unit
	    = app2 (symb"mgtk_gtk_adjustment_set_value")
	val set_value : 'a t -> real -> unit
	    = fn self => fn value => set_value_ (repr self) value
	local open Signal
	      infixr -->
	in val changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "changed" false (void --> return_void) f
	   val value_changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "value-changed" false (void --> return_void) f
	end
    end
    structure Widget :>
      sig
	type base
	type 'a widget_t
	type 'a t = 'a widget_t Object.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toWidget : 'a t -> base t
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
	type helptype
	val HELP_TOOLTIP : helptype
	val HELP_WHATS_THIS : helptype
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
	val map : 'a t -> unit
	val unmap : 'a t -> unit
	val realize : 'a t -> unit
	val unrealize : 'a t -> unit
	val queue_draw : 'a t -> unit
	val queue_draw_area : 'a t -> int -> int -> int -> int -> unit
	val queue_clear : 'a t -> unit
	val queue_clear_area : 'a t -> int -> int -> int -> int -> unit
	val queue_resize : 'a t -> unit
	val get_child_requisition : 'a t -> requisition -> unit
	val set_accel_path : 'a t -> string -> 'b AccelGroup.t -> unit
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
	val set_uposition : 'a t -> int -> int -> unit
	val set_usize : 'a t -> int -> int -> unit
	val set_events : 'a t -> int -> unit
	val add_events : 'a t -> int -> unit
	val get_toplevel : 'a t -> base t
	val get_ancestor : 'a t -> GType.t -> base t
	val get_settings : 'a t -> base t
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
	val modify_style : 'a t -> 'b t -> unit
	val get_modifier_style : 'a t -> base t
	val set_composite_name : 'a t -> string -> unit
	val get_composite_name : 'a t -> string
	val reset_rc_styles : 'a t -> unit
	val push_composite_child : unit -> unit
	val pop_composite_child : unit -> unit
	val pop_colormap : unit -> unit
	val style_get : 'a t -> string -> unit
	val get_default_style : unit -> base t
	val set_direction : 'a t -> text_direction -> unit
	val get_direction : 'a t -> text_direction
	val set_default_direction : text_direction -> unit
	val get_default_direction : unit -> text_direction
	val reset_shapes : 'a t -> unit
	val path : 'a t -> int * char * char
	val class_path : 'a t -> int * char * char
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
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a widget_t = unit
	type 'a t = 'a widget_t Object.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Object.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toWidget obj = inherit () (fn () => repr obj)
	type flags = int
	val get_flags_
	  : unit -> int * int * int * int * int * int * int * int * int * int 
		  * int * int * int * int * int * int * int * int
	    = app1 (symb"mgtk_get_gtk_widget_flags")
	val (TOPLEVEL, NO_WINDOW, REALIZED, MAPPED, VISIBLE, SENSITIVE, 
	     PARENT_SENSITIVE, CAN_FOCUS, HAS_FOCUS, CAN_DEFAULT, HAS_DEFAULT, 
	     HAS_GRAB, RC_STYLE, COMPOSITE_CHILD, NO_REPARENT, APP_PAINTABLE, 
	     RECEIVES_DEFAULT, DOUBLE_BUFFERED)
	    = get_flags_ ()
	type helptype = int
	val get_helptype_ : unit -> int * int
	    = app1 (symb"mgtk_get_gtk_widget_helptype")
	val (HELP_TOOLTIP, HELP_WHATS_THIS) = get_helptype_ ()
	val drag_check_threshold_ : cptr -> int -> int -> int -> int -> bool
	    = app5 (symb"mgtk_gtk_drag_check_threshold")
	val drag_check_threshold : 'a t -> int -> int -> int -> int -> bool
	    = fn self => fn start_x => fn start_y => fn current_x => 
	      fn current_y =>
		 drag_check_threshold_
		   (repr self) start_x start_y current_x current_y
	val drag_highlight_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_drag_highlight")
	val drag_highlight : 'a t -> unit
	    = fn self => drag_highlight_ (repr self)
	val drag_unhighlight_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_drag_unhighlight")
	val drag_unhighlight : 'a t -> unit
	    = fn self => drag_unhighlight_ (repr self)
	val drag_dest_unset_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_drag_dest_unset")
	val drag_dest_unset : 'a t -> unit
	    = fn self => drag_dest_unset_ (repr self)
	val drag_source_unset_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_drag_source_unset")
	val drag_source_unset : 'a t -> unit
	    = fn self => drag_source_unset_ (repr self)
	val drag_source_set_icon_stock_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_drag_source_set_icon_stock")
	val drag_source_set_icon_stock : 'a t -> string -> unit
	    = fn self => fn stock_id =>
		 drag_source_set_icon_stock_ (repr self) stock_id
	val grab_add_ : cptr -> unit = app1 (symb"mgtk_gtk_grab_add")
	val grab_add : 'a t -> unit = fn self => grab_add_ (repr self)
	val grab_remove_ : cptr -> unit = app1 (symb"mgtk_gtk_grab_remove")
	val grab_remove : 'a t -> unit = fn self => grab_remove_ (repr self)
	val rc_get_style_ : cptr -> cptr = app1 (symb"mgtk_gtk_rc_get_style")
	val rc_get_style : 'a t -> base t
	    = fn self => make (rc_get_style_ (repr self))
	val selection_remove_all_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_selection_remove_all")
	val selection_remove_all : 'a t -> unit
	    = fn self => selection_remove_all_ (repr self)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_widget_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : GType.t -> string -> cptr = app2 (symb"mgtk_gtk_widget_new")
	val new : GType.t -> string -> base t
	    = fn typ => fn first_property_name =>
		 make (new_ typ first_property_name)
	val ref_ : cptr -> cptr = app1 (symb"mgtk_gtk_widget_ref")
	val refe : 'a t -> base t = fn self => make (ref_ (repr self))
	val unref_ : cptr -> unit = app1 (symb"mgtk_gtk_widget_unref")
	val unref : 'a t -> unit = fn self => unref_ (repr self)
	val destroy_ : cptr -> unit = app1 (symb"mgtk_gtk_widget_destroy")
	val destroy : 'a t -> unit = fn self => destroy_ (repr self)
	val destroyed_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_widget_destroyed")
	val destroyed : 'a t -> 'b t -> unit
	    = fn self => fn widget_pointer =>
		 destroyed_ (repr self) (repr widget_pointer)
	val set_ : cptr -> string -> unit = app2 (symb"mgtk_gtk_widget_set")
	val set : 'a t -> string -> unit
	    = fn self => fn first_property_name =>
		 set_ (repr self) first_property_name
	val unparent_ : cptr -> unit = app1 (symb"mgtk_gtk_widget_unparent")
	val unparent : 'a t -> unit = fn self => unparent_ (repr self)
	val show_ : cptr -> unit = app1 (symb"mgtk_gtk_widget_show")
	val show : 'a t -> unit = fn self => show_ (repr self)
	val show_now_ : cptr -> unit = app1 (symb"mgtk_gtk_widget_show_now")
	val show_now : 'a t -> unit = fn self => show_now_ (repr self)
	val hide_ : cptr -> unit = app1 (symb"mgtk_gtk_widget_hide")
	val hide : 'a t -> unit = fn self => hide_ (repr self)
	val show_all_ : cptr -> unit = app1 (symb"mgtk_gtk_widget_show_all")
	val show_all : 'a t -> unit = fn self => show_all_ (repr self)
	val hide_all_ : cptr -> unit = app1 (symb"mgtk_gtk_widget_hide_all")
	val hide_all : 'a t -> unit = fn self => hide_all_ (repr self)
	val map_ : cptr -> unit = app1 (symb"mgtk_gtk_widget_map")
	val map : 'a t -> unit = fn self => map_ (repr self)
	val unmap_ : cptr -> unit = app1 (symb"mgtk_gtk_widget_unmap")
	val unmap : 'a t -> unit = fn self => unmap_ (repr self)
	val realize_ : cptr -> unit = app1 (symb"mgtk_gtk_widget_realize")
	val realize : 'a t -> unit = fn self => realize_ (repr self)
	val unrealize_ : cptr -> unit = app1 (symb"mgtk_gtk_widget_unrealize")
	val unrealize : 'a t -> unit = fn self => unrealize_ (repr self)
	val queue_draw_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_widget_queue_draw")
	val queue_draw : 'a t -> unit = fn self => queue_draw_ (repr self)
	val queue_draw_area_ : cptr -> int -> int -> int -> int -> unit
	    = app5 (symb"mgtk_gtk_widget_queue_draw_area")
	val queue_draw_area : 'a t -> int -> int -> int -> int -> unit
	    = fn self => fn x => fn y => fn width => fn height =>
		 queue_draw_area_ (repr self) x y width height
	val queue_clear_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_widget_queue_clear")
	val queue_clear : 'a t -> unit = fn self => queue_clear_ (repr self)
	val queue_clear_area_ : cptr -> int -> int -> int -> int -> unit
	    = app5 (symb"mgtk_gtk_widget_queue_clear_area")
	val queue_clear_area : 'a t -> int -> int -> int -> int -> unit
	    = fn self => fn x => fn y => fn width => fn height =>
		 queue_clear_area_ (repr self) x y width height
	val queue_resize_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_widget_queue_resize")
	val queue_resize : 'a t -> unit = fn self => queue_resize_ (repr self)
	val get_child_requisition_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_widget_get_child_requisition")
	val get_child_requisition : 'a t -> requisition -> unit
	    = fn self => fn requisition =>
		 get_child_requisition_ (repr self) requisition
	val set_accel_path_ : cptr -> string -> cptr -> unit
	    = app3 (symb"mgtk_gtk_widget_set_accel_path")
	val set_accel_path : 'a t -> string -> 'b AccelGroup.t -> unit
	    = fn self => fn accel_path => fn accel_group =>
		 set_accel_path_ (repr self) accel_path (repr accel_group)
	val mnemonic_activate_ : cptr -> bool -> bool
	    = app2 (symb"mgtk_gtk_widget_mnemonic_activate")
	val mnemonic_activate : 'a t -> bool -> bool
	    = fn self => fn group_cycling =>
		 mnemonic_activate_ (repr self) group_cycling
	val activate_ : cptr -> bool = app1 (symb"mgtk_gtk_widget_activate")
	val activate : 'a t -> bool = fn self => activate_ (repr self)
	val set_scroll_adjustments_ : cptr -> cptr -> cptr -> bool
	    = app3 (symb"mgtk_gtk_widget_set_scroll_adjustments")
	val set_scroll_adjustments
	  : 'a t -> 'b Adjustment.t option -> 'c Adjustment.t option -> bool
	    = fn self => fn hadjustment => fn vadjustment =>
		 set_scroll_adjustments_
		   (repr self)
		   (getOpt (Option.map repr hadjustment, GObject.null))
		   (getOpt (Option.map repr vadjustment, GObject.null))
	val set_scroll_adjustments' : 'a t -> bool
	    = fn self => set_scroll_adjustments_
			   (repr self) GObject.null GObject.null
	val reparent_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_widget_reparent")
	val reparent : 'a t -> 'b t -> unit
	    = fn self => fn new_parent =>
		 reparent_ (repr self) (repr new_parent)
	val freeze_child_notify_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_widget_freeze_child_notify")
	val freeze_child_notify : 'a t -> unit
	    = fn self => freeze_child_notify_ (repr self)
	val child_notify_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_widget_child_notify")
	val child_notify : 'a t -> string -> unit
	    = fn self => fn child_property =>
		 child_notify_ (repr self) child_property
	val thaw_child_notify_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_widget_thaw_child_notify")
	val thaw_child_notify : 'a t -> unit
	    = fn self => thaw_child_notify_ (repr self)
	val is_focus_ : cptr -> bool = app1 (symb"mgtk_gtk_widget_is_focus")
	val is_focus : 'a t -> bool = fn self => is_focus_ (repr self)
	val grab_focus_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_widget_grab_focus")
	val grab_focus : 'a t -> unit = fn self => grab_focus_ (repr self)
	val grab_default_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_widget_grab_default")
	val grab_default : 'a t -> unit = fn self => grab_default_ (repr self)
	val set_name_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_widget_set_name")
	val set_name : 'a t -> string -> unit
	    = fn self => fn name => set_name_ (repr self) name
	val get_name_ : cptr -> string = app1 (symb"mgtk_gtk_widget_get_name")
	val get_name : 'a t -> string = fn self => get_name_ (repr self)
	val set_state_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_widget_set_state")
	val set_state : 'a t -> statetype -> unit
	    = fn self => fn state => set_state_ (repr self) state
	val set_sensitive_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_widget_set_sensitive")
	val set_sensitive : 'a t -> bool -> unit
	    = fn self => fn sensitive => set_sensitive_ (repr self) sensitive
	val set_app_paintable_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_widget_set_app_paintable")
	val set_app_paintable : 'a t -> bool -> unit
	    = fn self => fn app_paintable =>
		 set_app_paintable_ (repr self) app_paintable
	val set_double_buffered_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_widget_set_double_buffered")
	val set_double_buffered : 'a t -> bool -> unit
	    = fn self => fn double_buffered =>
		 set_double_buffered_ (repr self) double_buffered
	val set_redraw_on_allocate_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_widget_set_redraw_on_allocate")
	val set_redraw_on_allocate : 'a t -> bool -> unit
	    = fn self => fn redraw_on_allocate =>
		 set_redraw_on_allocate_ (repr self) redraw_on_allocate
	val set_parent_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_widget_set_parent")
	val set_parent : 'a t -> 'b t -> unit
	    = fn self => fn parent => set_parent_ (repr self) (repr parent)
	val set_child_visible_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_widget_set_child_visible")
	val set_child_visible : 'a t -> bool -> unit
	    = fn self => fn is_visible =>
		 set_child_visible_ (repr self) is_visible
	val get_child_visible_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_widget_get_child_visible")
	val get_child_visible : 'a t -> bool
	    = fn self => get_child_visible_ (repr self)
	val get_parent_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_widget_get_parent")
	val get_parent : 'a t -> base t
	    = fn self => make (get_parent_ (repr self))
	val child_focus_ : cptr -> int -> bool
	    = app2 (symb"mgtk_gtk_widget_child_focus")
	val child_focus : 'a t -> directiontype -> bool
	    = fn self => fn direction => child_focus_ (repr self) direction
	val set_size_request_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_widget_set_size_request")
	val set_size_request : 'a t -> int -> int -> unit
	    = fn self => fn width => fn height =>
		 set_size_request_ (repr self) width height
	val get_size_request_ : cptr -> int ref -> int ref -> unit
	    = app3 (symb"mgtk_gtk_widget_get_size_request")
	val get_size_request : 'a t -> int * int
	    = fn self => let val (width, height) = (ref 0, ref 0)
			     val ret = get_size_request_
					 (repr self) width height
			 in (!width, !height) end
	val set_uposition_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_widget_set_uposition")
	val set_uposition : 'a t -> int -> int -> unit
	    = fn self => fn x => fn y => set_uposition_ (repr self) x y
	val set_usize_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_widget_set_usize")
	val set_usize : 'a t -> int -> int -> unit
	    = fn self => fn width => fn height =>
		 set_usize_ (repr self) width height
	val set_events_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_widget_set_events")
	val set_events : 'a t -> int -> unit
	    = fn self => fn events => set_events_ (repr self) events
	val add_events_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_widget_add_events")
	val add_events : 'a t -> int -> unit
	    = fn self => fn events => add_events_ (repr self) events
	val get_toplevel_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_widget_get_toplevel")
	val get_toplevel : 'a t -> base t
	    = fn self => make (get_toplevel_ (repr self))
	val get_ancestor_ : cptr -> GType.t -> cptr
	    = app2 (symb"mgtk_gtk_widget_get_ancestor")
	val get_ancestor : 'a t -> GType.t -> base t
	    = fn self => fn widget_type =>
		 make (get_ancestor_ (repr self) widget_type)
	val get_settings_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_widget_get_settings")
	val get_settings : 'a t -> base t
	    = fn self => make (get_settings_ (repr self))
	val get_events_ : cptr -> int = app1 (symb"mgtk_gtk_widget_get_events")
	val get_events : 'a t -> int = fn self => get_events_ (repr self)
	val get_pointer_ : cptr -> int ref -> int ref -> unit
	    = app3 (symb"mgtk_gtk_widget_get_pointer")
	val get_pointer : 'a t -> int * int
	    = fn self => let val (x, y) = (ref 0, ref 0)
			     val ret = get_pointer_ (repr self) x y
			 in (!x, !y) end
	val is_ancestor_ : cptr -> cptr -> bool
	    = app2 (symb"mgtk_gtk_widget_is_ancestor")
	val is_ancestor : 'a t -> 'b t -> bool
	    = fn self => fn ancestor =>
		 is_ancestor_ (repr self) (repr ancestor)
	val translate_coordinates_
	  : cptr * cptr * int * int * int ref * int ref -> bool
	    = app1 (symb"mgtk_gtk_widget_translate_coordinates")
	val translate_coordinates
	  : 'a t -> 'b t -> int -> int -> bool * int * int
	    = fn self => fn dest_widget => fn src_x => fn src_y =>
		 let val (dest_x, dest_y) = (ref 0, ref 0)
		     val ret = translate_coordinates_
				 (repr self, repr dest_widget, src_x, src_y, 
				  dest_x, dest_y)
		 in (ret, !dest_x, !dest_y) end
	val hide_on_delete_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_widget_hide_on_delete")
	val hide_on_delete : 'a t -> bool
	    = fn self => hide_on_delete_ (repr self)
	val set_style_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_widget_set_style")
	val set_style : 'a t -> 'b t option -> unit
	    = fn self => fn style =>
		 set_style_ (repr self)
			    (getOpt (Option.map repr style, GObject.null))
	val set_style' : 'a t -> unit
	    = fn self => set_style_ (repr self) GObject.null
	val ensure_style_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_widget_ensure_style")
	val ensure_style : 'a t -> unit = fn self => ensure_style_ (repr self)
	val get_style_ : cptr -> cptr = app1 (symb"mgtk_gtk_widget_get_style")
	val get_style : 'a t -> base t
	    = fn self => make (get_style_ (repr self))
	val modify_style_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_widget_modify_style")
	val modify_style : 'a t -> 'b t -> unit
	    = fn self => fn style => modify_style_ (repr self) (repr style)
	val get_modifier_style_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_widget_get_modifier_style")
	val get_modifier_style : 'a t -> base t
	    = fn self => make (get_modifier_style_ (repr self))
	val set_composite_name_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_widget_set_composite_name")
	val set_composite_name : 'a t -> string -> unit
	    = fn self => fn name => set_composite_name_ (repr self) name
	val get_composite_name_ : cptr -> string
	    = app1 (symb"mgtk_gtk_widget_get_composite_name")
	val get_composite_name : 'a t -> string
	    = fn self => get_composite_name_ (repr self)
	val reset_rc_styles_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_widget_reset_rc_styles")
	val reset_rc_styles : 'a t -> unit
	    = fn self => reset_rc_styles_ (repr self)
	val push_composite_child_ : unit -> unit
	    = app1 (symb"mgtk_gtk_widget_push_composite_child")
	val push_composite_child : unit -> unit
	    = fn dummy => push_composite_child_ dummy
	val pop_composite_child_ : unit -> unit
	    = app1 (symb"mgtk_gtk_widget_pop_composite_child")
	val pop_composite_child : unit -> unit
	    = fn dummy => pop_composite_child_ dummy
	val pop_colormap_ : unit -> unit
	    = app1 (symb"mgtk_gtk_widget_pop_colormap")
	val pop_colormap : unit -> unit = fn dummy => pop_colormap_ dummy
	val style_get_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_widget_style_get")
	val style_get : 'a t -> string -> unit
	    = fn self => fn first_property_name =>
		 style_get_ (repr self) first_property_name
	val get_default_style_ : unit -> cptr
	    = app1 (symb"mgtk_gtk_widget_get_default_style")
	val get_default_style : unit -> base t
	    = fn dummy => make (get_default_style_ dummy)
	val set_direction_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_widget_set_direction")
	val set_direction : 'a t -> text_direction -> unit
	    = fn self => fn dir => set_direction_ (repr self) dir
	val get_direction_ : cptr -> int
	    = app1 (symb"mgtk_gtk_widget_get_direction")
	val get_direction : 'a t -> text_direction
	    = fn self => get_direction_ (repr self)
	val set_default_direction_ : int -> unit
	    = app1 (symb"mgtk_gtk_widget_set_default_direction")
	val set_default_direction : text_direction -> unit
	    = fn dir => set_default_direction_ dir
	val get_default_direction_ : unit -> int
	    = app1 (symb"mgtk_gtk_widget_get_default_direction")
	val get_default_direction : unit -> text_direction
	    = fn dummy => get_default_direction_ dummy
	val reset_shapes_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_widget_reset_shapes")
	val reset_shapes : 'a t -> unit = fn self => reset_shapes_ (repr self)
	val path_ : cptr -> int ref -> char ref -> char ref -> unit
	    = app4 (symb"mgtk_gtk_widget_path")
	val path : 'a t -> int * char * char
	    = fn self => let val (path_length, path, path_reversed)
				 = (ref 0, ref #" ", ref #" ")
			     val ret = path_ (repr self) path_length path
					     path_reversed
			 in (!path_length, !path, !path_reversed) end
	val class_path_ : cptr -> int ref -> char ref -> char ref -> unit
	    = app4 (symb"mgtk_gtk_widget_class_path")
	val class_path : 'a t -> int * char * char
	    = fn self => let val (path_length, path, path_reversed)
				 = (ref 0, ref #" ", ref #" ")
			     val ret = class_path_ (repr self) path_length path
						   path_reversed
			 in (!path_length, !path, !path_reversed) end
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
	end
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a editable_t = unit
	type 'a t = 'a editable_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toEditable obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_editable_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val select_region_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_editable_select_region")
	val select_region : 'a t -> int -> int -> unit
	    = fn self => fn start => fn en =>
		 select_region_ (repr self) start en
	val insert_text_ : cptr -> string -> int -> int ref -> unit
	    = app4 (symb"mgtk_gtk_editable_insert_text")
	val insert_text : 'a t -> string -> int -> int -> int
	    = fn self => fn new_text => fn new_text_length => fn position =>
		 let val position = ref position
		     val ret = insert_text_
				 (repr self) new_text new_text_length position
		 in !position end
	val delete_text_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_editable_delete_text")
	val delete_text : 'a t -> int -> int -> unit
	    = fn self => fn start_pos => fn end_pos =>
		 delete_text_ (repr self) start_pos end_pos
	val get_chars_ : cptr -> int -> int -> string
	    = app3 (symb"mgtk_gtk_editable_get_chars")
	val get_chars : 'a t -> int -> int -> string
	    = fn self => fn start_pos => fn end_pos =>
		 get_chars_ (repr self) start_pos end_pos
	val cut_clipboard_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_editable_cut_clipboard")
	val cut_clipboard : 'a t -> unit
	    = fn self => cut_clipboard_ (repr self)
	val copy_clipboard_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_editable_copy_clipboard")
	val copy_clipboard : 'a t -> unit
	    = fn self => copy_clipboard_ (repr self)
	val paste_clipboard_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_editable_paste_clipboard")
	val paste_clipboard : 'a t -> unit
	    = fn self => paste_clipboard_ (repr self)
	val delete_selection_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_editable_delete_selection")
	val delete_selection : 'a t -> unit
	    = fn self => delete_selection_ (repr self)
	val set_position_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_editable_set_position")
	val set_position : 'a t -> int -> unit
	    = fn self => fn position => set_position_ (repr self) position
	val get_position_ : cptr -> int
	    = app1 (symb"mgtk_gtk_editable_get_position")
	val get_position : 'a t -> int = fn self => get_position_ (repr self)
	val set_editable_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_editable_set_editable")
	val set_editable : 'a t -> bool -> unit
	    = fn self => fn is_editable =>
		 set_editable_ (repr self) is_editable
	val get_editable_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_editable_get_editable")
	val get_editable : 'a t -> bool = fn self => get_editable_ (repr self)
	local open Signal
	      infixr -->
	in val changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "changed" false (void --> return_void) f
	end
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a itemfactory_t = unit
	type 'a t = 'a itemfactory_t Object.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Object.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toItemFactory obj = inherit () (fn () => repr obj)
	val get_item_ : cptr -> string -> cptr
	    = app2 (symb"mgtk_gtk_item_factory_get_item")
	val get_item : 'a t -> string -> base Widget.t
	    = fn self => fn path =>
		 Widget.inherit () (fn () => get_item_ (repr self) path)
	val get_widget_ : cptr -> string -> cptr
	    = app2 (symb"mgtk_gtk_item_factory_get_widget")
	val get_widget : 'a t -> string -> base Widget.t
	    = fn self => fn path =>
		 Widget.inherit () (fn () => get_widget_ (repr self) path)
	val get_widget_by_action_ : cptr -> int -> cptr
	    = app2 (symb"mgtk_gtk_item_factory_get_widget_by_action")
	val get_widget_by_action : 'a t -> int -> base Widget.t
	    = fn self => fn action =>
		 Widget.inherit
		   () (fn () => get_widget_by_action_ (repr self) action)
	val get_item_by_action_ : cptr -> int -> cptr
	    = app2 (symb"mgtk_gtk_item_factory_get_item_by_action")
	val get_item_by_action : 'a t -> int -> base Widget.t
	    = fn self => fn action =>
		 Widget.inherit
		   () (fn () => get_item_by_action_ (repr self) action)
	val delete_item_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_item_factory_delete_item")
	val delete_item : 'a t -> string -> unit
	    = fn self => fn path => delete_item_ (repr self) path
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a imcontext_t = unit
	type 'a t = 'a imcontext_t Object.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Object.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toIMContext obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_im_context_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val focus_in_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_im_context_focus_in")
	val focus_in : 'a t -> unit = fn self => focus_in_ (repr self)
	val focus_out_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_im_context_focus_out")
	val focus_out : 'a t -> unit = fn self => focus_out_ (repr self)
	val reset_ : cptr -> unit = app1 (symb"mgtk_gtk_im_context_reset")
	val reset : 'a t -> unit = fn self => reset_ (repr self)
	val set_use_preedit_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_im_context_set_use_preedit")
	val set_use_preedit : 'a t -> bool -> unit
	    = fn self => fn use_preedit =>
		 set_use_preedit_ (repr self) use_preedit
	val set_surrounding_ : cptr -> string -> int -> int -> unit
	    = app4 (symb"mgtk_gtk_im_context_set_surrounding")
	val set_surrounding : 'a t -> string -> int -> int -> unit
	    = fn self => fn text => fn len => fn cursor_index =>
		 set_surrounding_ (repr self) text len cursor_index
	val delete_surrounding_ : cptr -> int -> int -> bool
	    = app3 (symb"mgtk_gtk_im_context_delete_surrounding")
	val delete_surrounding : 'a t -> int -> int -> bool
	    = fn self => fn offset => fn n_chars =>
		 delete_surrounding_ (repr self) offset n_chars
	val simple_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_im_context_simple_get_type")
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a imcontextsimple_t = unit
	type 'a t = 'a imcontextsimple_t IMContext.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in IMContext.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toIMContextSimple obj = inherit () (fn () => repr obj)
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_im_context_simple_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
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
	val append_menuitems : 'a t -> 'b t -> unit
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a immulticontext_t = unit
	type 'a t = 'a immulticontext_t IMContext.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in IMContext.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toIMMulticontext obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_im_multicontext_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_im_multicontext_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val append_menuitems_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_im_multicontext_append_menuitems")
	val append_menuitems : 'a t -> 'b t -> unit
	    = fn self => fn menushell =>
		 append_menuitems_ (repr self) (repr menushell)
    end
    structure CellRenderer :>
      sig
	type base
	type 'a cellrenderer_t
	type 'a t = 'a cellrenderer_t Object.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toCellRenderer : 'a t -> base t
	type state
	val CELL_RENDERER_SELECTED : state
	val CELL_RENDERER_PRELIT : state
	val CELL_RENDERER_INSENSITIVE : state
	val CELL_RENDERER_SORTED : state
	type mode
	val CELL_RENDERER_MODE_INERT : mode
	val CELL_RENDERER_MODE_ACTIVATABLE : mode
	val CELL_RENDERER_MODE_EDITABLE : mode
	val get_type : unit -> GType.t
	val set_fixed_size : 'a t -> int -> int -> unit
	val pixbuf_get_type : unit -> GType.t
	val text_get_type : unit -> GType.t
	val toggle_get_type : unit -> GType.t
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a cellrenderer_t = unit
	type 'a t = 'a cellrenderer_t Object.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Object.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toCellRenderer obj = inherit () (fn () => repr obj)
	type state = int
	val get_state_ : unit -> int * int * int * int
	    = app1 (symb"mgtk_get_gtk_cellrenderer_state")
	val (CELL_RENDERER_SELECTED, CELL_RENDERER_PRELIT, 
	     CELL_RENDERER_INSENSITIVE, CELL_RENDERER_SORTED)
	    = get_state_ ()
	type mode = int
	val get_mode_ : unit -> int * int * int
	    = app1 (symb"mgtk_get_gtk_cellrenderer_mode")
	val (CELL_RENDERER_MODE_INERT, CELL_RENDERER_MODE_ACTIVATABLE, 
	     CELL_RENDERER_MODE_EDITABLE)
	    = get_mode_ ()
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_cellrenderer_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_fixed_size_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_cellrenderer_set_fixed_size")
	val set_fixed_size : 'a t -> int -> int -> unit
	    = fn self => fn width => fn height =>
		 set_fixed_size_ (repr self) width height
	val pixbuf_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_cellrenderer_pixbuf_get_type")
	val pixbuf_get_type : unit -> GType.t
	    = fn dummy => pixbuf_get_type_ dummy
	val text_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_cellrenderer_text_get_type")
	val text_get_type : unit -> GType.t = fn dummy => text_get_type_ dummy
	val toggle_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_cellrenderer_toggle_get_type")
	val toggle_get_type : unit -> GType.t
	    = fn dummy => toggle_get_type_ dummy
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a celleditable_t = unit
	type 'a t = 'a celleditable_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toCellEditable obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_cell_editable_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val editing_done_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_cell_editable_editing_done")
	val editing_done : 'a t -> unit = fn self => editing_done_ (repr self)
	val remove_widget_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_cell_editable_remove_widget")
	val remove_widget : 'a t -> unit
	    = fn self => remove_widget_ (repr self)
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a cellrenderertoggle_t = unit
	type 'a t = 'a cellrenderertoggle_t CellRenderer.t
	fun inherit w con
	  = let val con = let val ptr = con () in fn () => ptr end
		val witness = ()
	    in CellRenderer.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toCellRendererToggle obj = inherit () (fn () => repr obj)
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_cellrenderer_toggle_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_radio_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_cellrenderer_toggle_get_radio")
	val get_radio : 'a t -> bool = fn self => get_radio_ (repr self)
	val set_radio_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_cellrenderer_toggle_set_radio")
	val set_radio : 'a t -> bool -> unit
	    = fn self => fn radio => set_radio_ (repr self) radio
	val get_active_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_cellrenderer_toggle_get_active")
	val get_active : 'a t -> bool = fn self => get_active_ (repr self)
	val set_active_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_cellrenderer_toggle_set_active")
	val set_active : 'a t -> bool -> unit
	    = fn self => fn setting => set_active_ (repr self) setting
	local open Signal
	      infixr -->
	in val toggled_sig : (char -> unit) -> 'a t Signal.signal
	       = fn f => signal "toggled" false (char --> return_void) f
	end
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a cellrenderertext_t = unit
	type 'a t = 'a cellrenderertext_t CellRenderer.t
	fun inherit w con
	  = let val con = let val ptr = con () in fn () => ptr end
		val witness = ()
	    in CellRenderer.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toCellRendererText obj = inherit () (fn () => repr obj)
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_cellrenderer_text_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val set_fixed_height_from_font_ : cptr -> int -> unit
	    = app2
		(symb"mgtk_gtk_cellrenderer_text_set_fixed_height_from_font")
	val set_fixed_height_from_font : 'a t -> int -> unit
	    = fn self => fn number_of_rows =>
		 set_fixed_height_from_font_ (repr self) number_of_rows
	local open Signal
	      infixr -->
	in val edited_sig : (char -> char -> unit) -> 'a t Signal.signal
	       = fn f =>
		    signal "edited" false (char --> char --> return_void) f
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a cellrendererpixbuf_t = unit
	type 'a t = 'a cellrendererpixbuf_t CellRenderer.t
	fun inherit w con
	  = let val con = let val ptr = con () in fn () => ptr end
		val witness = ()
	    in CellRenderer.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toCellRendererPixbuf obj = inherit () (fn () => repr obj)
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_cellrenderer_pixbuf_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure RcStyle :>
      sig
	type base
	type 'a rcstyle_t
	type 'a t = 'a rcstyle_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toRcStyle : 'a t -> base t
	val rc_get_style_by_paths
	  : 'a t -> string -> string -> GType.t -> base t
	val rc_reparse_all_for_settings : 'a t -> bool -> bool
	val rc_parse : string -> unit
	val rc_parse_string : string -> unit
	val rc_reparse_all : unit -> bool
	val rc_add_widget_name_style : 'a t -> string -> unit
	val rc_add_widget_class_style : 'a t -> string -> unit
	val rc_add_class_style : 'a t -> string -> unit
	val rc_style_get_type : unit -> GType.t
	val rc_style_copy : 'a t -> base t
	val rc_style_ref : 'a t -> unit
	val rc_style_unref : 'a t -> unit
	val rc_find_module_in_path : string -> string
	val rc_get_theme_dir : unit -> string
	val rc_get_module_dir : unit -> string
	val rc_get_im_module_path : unit -> string
	val rc_get_im_module_file : unit -> string
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a rcstyle_t = unit
	type 'a t = 'a rcstyle_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toRcStyle obj = inherit () (fn () => repr obj)
	val rc_get_style_by_paths_
	  : cptr -> string -> string -> GType.t -> cptr
	    = app4 (symb"mgtk_gtk_rc_get_style_by_paths")
	val rc_get_style_by_paths
	  : 'a t -> string -> string -> GType.t -> base t
	    = fn settings => fn widget_path => fn class_path => fn typ =>
		 make (rc_get_style_by_paths_
			 (repr settings) widget_path class_path typ)
	val rc_reparse_all_for_settings_ : cptr -> bool -> bool
	    = app2 (symb"mgtk_gtk_rc_reparse_all_for_settings")
	val rc_reparse_all_for_settings : 'a t -> bool -> bool
	    = fn settings => fn force_load =>
		 rc_reparse_all_for_settings_ (repr settings) force_load
	val rc_parse_ : string -> unit = app1 (symb"mgtk_gtk_rc_parse")
	val rc_parse : string -> unit = fn filename => rc_parse_ filename
	val rc_parse_string_ : string -> unit
	    = app1 (symb"mgtk_gtk_rc_parse_string")
	val rc_parse_string : string -> unit
	    = fn rc_string => rc_parse_string_ rc_string
	val rc_reparse_all_ : unit -> bool
	    = app1 (symb"mgtk_gtk_rc_reparse_all")
	val rc_reparse_all : unit -> bool = fn dummy => rc_reparse_all_ dummy
	val rc_add_widget_name_style_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_rc_add_widget_name_style")
	val rc_add_widget_name_style : 'a t -> string -> unit
	    = fn self => fn pattern =>
		 rc_add_widget_name_style_ (repr self) pattern
	val rc_add_widget_class_style_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_rc_add_widget_class_style")
	val rc_add_widget_class_style : 'a t -> string -> unit
	    = fn self => fn pattern =>
		 rc_add_widget_class_style_ (repr self) pattern
	val rc_add_class_style_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_rc_add_class_style")
	val rc_add_class_style : 'a t -> string -> unit
	    = fn self => fn pattern => rc_add_class_style_ (repr self) pattern
	val rc_style_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_rc_style_get_type")
	val rc_style_get_type : unit -> GType.t
	    = fn dummy => rc_style_get_type_ dummy
	val rc_style_copy_ : cptr -> cptr = app1 (symb"mgtk_gtk_rc_style_copy")
	val rc_style_copy : 'a t -> base t
	    = fn self => make (rc_style_copy_ (repr self))
	val rc_style_ref_ : cptr -> unit = app1 (symb"mgtk_gtk_rc_style_ref")
	val rc_style_ref : 'a t -> unit = fn self => rc_style_ref_ (repr self)
	val rc_style_unref_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_rc_style_unref")
	val rc_style_unref : 'a t -> unit
	    = fn self => rc_style_unref_ (repr self)
	val rc_find_module_in_path_ : string -> string
	    = app1 (symb"mgtk_gtk_rc_find_module_in_path")
	val rc_find_module_in_path : string -> string
	    = fn module_file => rc_find_module_in_path_ module_file
	val rc_get_theme_dir_ : unit -> string
	    = app1 (symb"mgtk_gtk_rc_get_theme_dir")
	val rc_get_theme_dir : unit -> string
	    = fn dummy => rc_get_theme_dir_ dummy
	val rc_get_module_dir_ : unit -> string
	    = app1 (symb"mgtk_gtk_rc_get_module_dir")
	val rc_get_module_dir : unit -> string
	    = fn dummy => rc_get_module_dir_ dummy
	val rc_get_im_module_path_ : unit -> string
	    = app1 (symb"mgtk_gtk_rc_get_im_module_path")
	val rc_get_im_module_path : unit -> string
	    = fn dummy => rc_get_im_module_path_ dummy
	val rc_get_im_module_file_ : unit -> string
	    = app1 (symb"mgtk_gtk_rc_get_im_module_file")
	val rc_get_im_module_file : unit -> string
	    = fn dummy => rc_get_im_module_file_ dummy
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a settings_t = unit
	type 'a t = 'a settings_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toSettings obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_settings_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val get_default_ : unit -> cptr
	    = app1 (symb"mgtk_gtk_settings_get_default")
	val get_default : unit -> base t
	    = fn dummy => make (get_default_ dummy)
	val set_string_property_ : cptr -> string -> string -> string -> unit
	    = app4 (symb"mgtk_gtk_settings_set_string_property")
	val set_string_property : 'a t -> string -> string -> string -> unit
	    = fn self => fn name => fn v_string => fn origin =>
		 set_string_property_ (repr self) name v_string origin
	val set_double_property_ : cptr -> string -> real -> string -> unit
	    = app4 (symb"mgtk_gtk_settings_set_double_property")
	val set_double_property : 'a t -> string -> real -> string -> unit
	    = fn self => fn name => fn v_double => fn origin =>
		 set_double_property_ (repr self) name v_double origin
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a sizegroup_t = unit
	type 'a t = 'a sizegroup_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toSizeGroup obj = inherit () (fn () => repr obj)
	type mode = int
	val get_mode_ : unit -> int * int * int * int
	    = app1 (symb"mgtk_get_gtk_size_group_mode")
	val (NON, HORIZONTAL, VERTICAL, BOTH) = get_mode_ ()
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_size_group_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : int -> cptr = app1 (symb"mgtk_gtk_size_group_new")
	val new : mode -> base t = fn mode => make (new_ mode)
	val set_mode_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_size_group_set_mode")
	val set_mode : 'a t -> mode -> unit
	    = fn self => fn mode => set_mode_ (repr self) mode
	val get_mode_ : cptr -> int = app1 (symb"mgtk_gtk_size_group_get_mode")
	val get_mode : 'a t -> mode = fn self => get_mode_ (repr self)
	val add_widget_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_size_group_add_widget")
	val add_widget : 'a t -> 'b Widget.t -> unit
	    = fn self => fn widget => add_widget_ (repr self) (repr widget)
	val remove_widget_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_size_group_remove_widget")
	val remove_widget : 'a t -> 'b Widget.t -> unit
	    = fn self => fn widget => remove_widget_ (repr self) (repr widget)
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
	val refe : 'a t -> base t
	val unref : 'a t -> unit
	val lookup_icon_set : 'a t -> string -> icon_set
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a style_t = unit
	type 'a t = 'a style_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toStyle obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_style_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_style_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val copy_ : cptr -> cptr = app1 (symb"mgtk_gtk_style_copy")
	val copy : 'a t -> base t = fn self => make (copy_ (repr self))
	val detach_ : cptr -> unit = app1 (symb"mgtk_gtk_style_detach")
	val detach : 'a t -> unit = fn self => detach_ (repr self)
	val ref_ : cptr -> cptr = app1 (symb"mgtk_gtk_style_ref")
	val refe : 'a t -> base t = fn self => make (ref_ (repr self))
	val unref_ : cptr -> unit = app1 (symb"mgtk_gtk_style_unref")
	val unref : 'a t -> unit = fn self => unref_ (repr self)
	val lookup_icon_set_ : cptr -> string -> cptr
	    = app2 (symb"mgtk_gtk_style_lookup_icon_set")
	val lookup_icon_set : 'a t -> string -> icon_set
	    = fn self => fn stock_id => lookup_icon_set_ (repr self) stock_id
    end
    structure TextBuffer :>
      sig
	type base
	type 'a textbuffer_t
	type 'a t = 'a textbuffer_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTextBuffer : 'a t -> base t
	val get_type : unit -> GType.t
	val new : 'a t option -> base t
	val new' : unit -> base t
	val get_line_count : 'a t -> int
	val get_char_count : 'a t -> int
	val get_tag_table : 'a t -> base t
	val set_text : 'a t -> string -> int -> unit
	val insert : 'a t -> textiter -> string -> int option -> unit
	val insert' : 'a t -> textiter -> string -> unit
	val insert_at_cursor : 'a t -> string -> int option -> unit
	val insert_at_cursor' : 'a t -> string -> unit
	val insert_interactive
	  : 'a t -> textiter -> string -> int -> bool -> bool
	val insert_interactive_at_cursor
	  : 'a t -> string -> int -> bool -> bool
	val insert_range : 'a t -> textiter -> textiter -> textiter -> unit
	val insert_range_interactive
	  : 'a t -> textiter -> textiter -> textiter -> bool -> bool
	val insert_with_tags
	  : 'a t -> textiter -> string -> int -> 'b t -> unit
	val insert_with_tags_by_name
	  : 'a t -> textiter -> string -> int -> string -> unit
	val delete : 'a t -> textiter -> textiter -> unit
	val delete_interactive : 'a t -> textiter -> textiter -> bool -> bool
	val get_text : 'a t -> textiter -> textiter -> bool option -> string
	val get_text' : 'a t -> textiter -> textiter -> string
	val get_slice : 'a t -> textiter -> textiter -> bool option -> string
	val get_slice' : 'a t -> textiter -> textiter -> string
	val insert_child_anchor : 'a t -> textiter -> 'b t -> unit
	val create_child_anchor : 'a t -> textiter -> base t
	val create_mark
	  : 'a t -> string option -> textiter -> bool option -> base t
	val create_mark' : 'a t -> textiter -> base t
	val move_mark : 'a t -> 'b t -> textiter -> unit
	val delete_mark : 'a t -> 'b t -> unit
	val get_mark : 'a t -> string -> base t
	val move_mark_by_name : 'a t -> string -> textiter -> unit
	val delete_mark_by_name : 'a t -> string -> unit
	val get_insert : 'a t -> base t
	val get_selection_bound : 'a t -> base t
	val place_cursor : 'a t -> textiter -> unit
	val apply_tag : 'a t -> 'b t -> textiter -> textiter -> unit
	val remove_tag : 'a t -> 'b t -> textiter -> textiter -> unit
	val apply_tag_by_name : 'a t -> string -> textiter -> textiter -> unit
	val remove_tag_by_name : 'a t -> string -> textiter -> textiter -> unit
	val remove_all_tags : 'a t -> textiter -> textiter -> unit
	val create_tag : 'a t -> string -> string -> base t
	val getiter_at_line_offset : 'a t -> int -> int -> textiter
	val getiter_at_line_index : 'a t -> int -> int -> textiter
	val getiter_at_offset : 'a t -> int -> textiter
	val getiter_at_line : 'a t -> int -> textiter
	val get_startiter : 'a t -> textiter
	val get_enditer : 'a t -> textiter
	val get_bounds : 'a t -> textiter * textiter
	val getiter_at_mark : 'a t -> 'b t -> textiter
	val getiter_at_child_anchor : 'a t -> 'b t -> textiter
	val get_modified : 'a t -> bool
	val set_modified : 'a t -> bool -> unit
	val add_selection_clipboard : 'a t -> 'b t -> unit
	val remove_selection_clipboard : 'a t -> 'b t -> unit
	val cut_clipboard : 'a t -> 'b t -> bool -> unit
	val copy_clipboard : 'a t -> 'b t -> unit
	val paste_clipboard : 'a t -> 'b t -> textiter -> bool -> unit
	val get_selection_bounds : 'a t -> bool * textiter * textiter
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a textbuffer_t = unit
	type 'a t = 'a textbuffer_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTextBuffer obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_textbuffer_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr -> cptr = app1 (symb"mgtk_gtk_textbuffer_new")
	val new : 'a t option -> base t
	    = fn table => make (new_ (getOpt (Option.map repr table, 
					      GObject.null)))
	val new' : unit -> base t = fn dummy => make (new_ GObject.null)
	val get_line_count_ : cptr -> int
	    = app1 (symb"mgtk_gtk_textbuffer_get_line_count")
	val get_line_count : 'a t -> int
	    = fn self => get_line_count_ (repr self)
	val get_char_count_ : cptr -> int
	    = app1 (symb"mgtk_gtk_textbuffer_get_char_count")
	val get_char_count : 'a t -> int
	    = fn self => get_char_count_ (repr self)
	val get_tag_table_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_textbuffer_get_tag_table")
	val get_tag_table : 'a t -> base t
	    = fn self => make (get_tag_table_ (repr self))
	val set_text_ : cptr -> string -> int -> unit
	    = app3 (symb"mgtk_gtk_textbuffer_set_text")
	val set_text : 'a t -> string -> int -> unit
	    = fn self => fn text => fn len => set_text_ (repr self) text len
	val insert_ : cptr -> cptr -> string -> int -> unit
	    = app4 (symb"mgtk_gtk_textbuffer_insert")
	val insert : 'a t -> textiter -> string -> int option -> unit
	    = fn self => fn iter => fn text => fn len =>
		 insert_ (repr self) iter text (getOpt (len, ~1))
	val insert' : 'a t -> textiter -> string -> unit
	    = fn self => fn iter => fn text => insert_ (repr self) iter text ~1
	val insert_at_cursor_ : cptr -> string -> int -> unit
	    = app3 (symb"mgtk_gtk_textbuffer_insert_at_cursor")
	val insert_at_cursor : 'a t -> string -> int option -> unit
	    = fn self => fn text => fn len =>
		 insert_at_cursor_ (repr self) text (getOpt (len, ~1))
	val insert_at_cursor' : 'a t -> string -> unit
	    = fn self => fn text => insert_at_cursor_ (repr self) text ~1
	val insert_interactive_ : cptr -> cptr -> string -> int -> bool -> bool
	    = app5 (symb"mgtk_gtk_textbuffer_insert_interactive")
	val insert_interactive
	  : 'a t -> textiter -> string -> int -> bool -> bool
	    = fn self => fn iter => fn text => fn len => fn default_editable =>
		 insert_interactive_ (repr self) iter text len default_editable
	val insert_interactive_at_cursor_
	  : cptr -> string -> int -> bool -> bool
	    = app4 (symb"mgtk_gtk_textbuffer_insert_interactive_at_cursor")
	val insert_interactive_at_cursor
	  : 'a t -> string -> int -> bool -> bool
	    = fn self => fn text => fn len => fn default_editable =>
		 insert_interactive_at_cursor_
		   (repr self) text len default_editable
	val insert_range_ : cptr -> cptr -> cptr -> cptr -> unit
	    = app4 (symb"mgtk_gtk_textbuffer_insert_range")
	val insert_range : 'a t -> textiter -> textiter -> textiter -> unit
	    = fn self => fn iter => fn start => fn en =>
		 insert_range_ (repr self) iter start en
	val insert_range_interactive_
	  : cptr -> cptr -> cptr -> cptr -> bool -> bool
	    = app5 (symb"mgtk_gtk_textbuffer_insert_range_interactive")
	val insert_range_interactive
	  : 'a t -> textiter -> textiter -> textiter -> bool -> bool
	    = fn self => fn iter => fn start => fn en => fn default_editable =>
		 insert_range_interactive_
		   (repr self) iter start en default_editable
	val insert_with_tags_ : cptr -> cptr -> string -> int -> cptr -> unit
	    = app5 (symb"mgtk_gtk_textbuffer_insert_with_tags")
	val insert_with_tags
	  : 'a t -> textiter -> string -> int -> 'b t -> unit
	    = fn self => fn iter => fn text => fn len => fn first_tag =>
		 insert_with_tags_ (repr self) iter text len (repr first_tag)
	val insert_with_tags_by_name_
	  : cptr -> cptr -> string -> int -> string -> unit
	    = app5 (symb"mgtk_gtk_textbuffer_insert_with_tags_by_name")
	val insert_with_tags_by_name
	  : 'a t -> textiter -> string -> int -> string -> unit
	    = fn self => fn iter => fn text => fn len => fn first_tag_name =>
		 insert_with_tags_by_name_
		   (repr self) iter text len first_tag_name
	val delete_ : cptr -> cptr -> cptr -> unit
	    = app3 (symb"mgtk_gtk_textbuffer_delete")
	val delete : 'a t -> textiter -> textiter -> unit
	    = fn self => fn start => fn en => delete_ (repr self) start en
	val delete_interactive_ : cptr -> cptr -> cptr -> bool -> bool
	    = app4 (symb"mgtk_gtk_textbuffer_delete_interactive")
	val delete_interactive : 'a t -> textiter -> textiter -> bool -> bool
	    = fn self => fn start_iter => fn end_iter => fn default_editable =>
		 delete_interactive_
		   (repr self) start_iter end_iter default_editable
	val get_text_ : cptr -> cptr -> cptr -> bool -> string
	    = app4 (symb"mgtk_gtk_textbuffer_get_text")
	val get_text : 'a t -> textiter -> textiter -> bool option -> string
	    = fn self => fn start => fn en => fn include_hidden_chars =>
		 get_text_ (repr self) start en
			   (getOpt (include_hidden_chars, true))
	val get_text' : 'a t -> textiter -> textiter -> string
	    = fn self => fn start => fn en =>
		 get_text_ (repr self) start en true
	val get_slice_ : cptr -> cptr -> cptr -> bool -> string
	    = app4 (symb"mgtk_gtk_textbuffer_get_slice")
	val get_slice : 'a t -> textiter -> textiter -> bool option -> string
	    = fn self => fn start => fn en => fn include_hidden_chars =>
		 get_slice_ (repr self) start en
			    (getOpt (include_hidden_chars, true))
	val get_slice' : 'a t -> textiter -> textiter -> string
	    = fn self => fn start => fn en =>
		 get_slice_ (repr self) start en true
	val insert_child_anchor_ : cptr -> cptr -> cptr -> unit
	    = app3 (symb"mgtk_gtk_textbuffer_insert_child_anchor")
	val insert_child_anchor : 'a t -> textiter -> 'b t -> unit
	    = fn self => fn iter => fn anchor =>
		 insert_child_anchor_ (repr self) iter (repr anchor)
	val create_child_anchor_ : cptr -> cptr -> cptr
	    = app2 (symb"mgtk_gtk_textbuffer_create_child_anchor")
	val create_child_anchor : 'a t -> textiter -> base t
	    = fn self => fn iter =>
		 make (create_child_anchor_ (repr self) iter)
	val create_mark_ : cptr -> string -> cptr -> bool -> cptr
	    = app4 (symb"mgtk_gtk_textbuffer_create_mark")
	val create_mark
	  : 'a t -> string option -> textiter -> bool option -> base t
	    = fn self => fn mark_name => fn wher => fn left_gravity =>
		 make (create_mark_ (repr self) (getOpt (mark_name, "")) wher
				    (getOpt (left_gravity, false)))
	val create_mark' : 'a t -> textiter -> base t
	    = fn self => fn wher =>
		 make (create_mark_ (repr self) "" wher false)
	val move_mark_ : cptr -> cptr -> cptr -> unit
	    = app3 (symb"mgtk_gtk_textbuffer_move_mark")
	val move_mark : 'a t -> 'b t -> textiter -> unit
	    = fn self => fn mark => fn wher =>
		 move_mark_ (repr self) (repr mark) wher
	val delete_mark_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_textbuffer_delete_mark")
	val delete_mark : 'a t -> 'b t -> unit
	    = fn self => fn mark => delete_mark_ (repr self) (repr mark)
	val get_mark_ : cptr -> string -> cptr
	    = app2 (symb"mgtk_gtk_textbuffer_get_mark")
	val get_mark : 'a t -> string -> base t
	    = fn self => fn name => make (get_mark_ (repr self) name)
	val move_mark_by_name_ : cptr -> string -> cptr -> unit
	    = app3 (symb"mgtk_gtk_textbuffer_move_mark_by_name")
	val move_mark_by_name : 'a t -> string -> textiter -> unit
	    = fn self => fn name => fn wher =>
		 move_mark_by_name_ (repr self) name wher
	val delete_mark_by_name_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_textbuffer_delete_mark_by_name")
	val delete_mark_by_name : 'a t -> string -> unit
	    = fn self => fn name => delete_mark_by_name_ (repr self) name
	val get_insert_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_textbuffer_get_insert")
	val get_insert : 'a t -> base t
	    = fn self => make (get_insert_ (repr self))
	val get_selection_bound_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_textbuffer_get_selection_bound")
	val get_selection_bound : 'a t -> base t
	    = fn self => make (get_selection_bound_ (repr self))
	val place_cursor_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_textbuffer_place_cursor")
	val place_cursor : 'a t -> textiter -> unit
	    = fn self => fn wher => place_cursor_ (repr self) wher
	val apply_tag_ : cptr -> cptr -> cptr -> cptr -> unit
	    = app4 (symb"mgtk_gtk_textbuffer_apply_tag")
	val apply_tag : 'a t -> 'b t -> textiter -> textiter -> unit
	    = fn self => fn tag => fn start => fn en =>
		 apply_tag_ (repr self) (repr tag) start en
	val remove_tag_ : cptr -> cptr -> cptr -> cptr -> unit
	    = app4 (symb"mgtk_gtk_textbuffer_remove_tag")
	val remove_tag : 'a t -> 'b t -> textiter -> textiter -> unit
	    = fn self => fn tag => fn start => fn en =>
		 remove_tag_ (repr self) (repr tag) start en
	val apply_tag_by_name_ : cptr -> string -> cptr -> cptr -> unit
	    = app4 (symb"mgtk_gtk_textbuffer_apply_tag_by_name")
	val apply_tag_by_name : 'a t -> string -> textiter -> textiter -> unit
	    = fn self => fn name => fn start => fn en =>
		 apply_tag_by_name_ (repr self) name start en
	val remove_tag_by_name_ : cptr -> string -> cptr -> cptr -> unit
	    = app4 (symb"mgtk_gtk_textbuffer_remove_tag_by_name")
	val remove_tag_by_name : 'a t -> string -> textiter -> textiter -> unit
	    = fn self => fn name => fn start => fn en =>
		 remove_tag_by_name_ (repr self) name start en
	val remove_all_tags_ : cptr -> cptr -> cptr -> unit
	    = app3 (symb"mgtk_gtk_textbuffer_remove_all_tags")
	val remove_all_tags : 'a t -> textiter -> textiter -> unit
	    = fn self => fn start => fn en =>
		 remove_all_tags_ (repr self) start en
	val create_tag_ : cptr -> string -> string -> cptr
	    = app3 (symb"mgtk_gtk_textbuffer_create_tag")
	val create_tag : 'a t -> string -> string -> base t
	    = fn self => fn tag_name => fn first_property_name =>
		 make (create_tag_ (repr self) tag_name first_property_name)
	val getiter_at_line_offset_ : cptr -> cptr ref -> int -> int -> unit
	    = app4 (symb"mgtk_gtk_textbuffer_getiter_at_line_offset")
	val getiter_at_line_offset : 'a t -> int -> int -> textiter
	    = fn self => fn line_number => fn char_offset =>
		 let val iter = ref GObject.null
		     val ret = getiter_at_line_offset_
				 (repr self) iter line_number char_offset
		 in !iter end
	val getiter_at_line_index_ : cptr -> cptr ref -> int -> int -> unit
	    = app4 (symb"mgtk_gtk_textbuffer_getiter_at_line_index")
	val getiter_at_line_index : 'a t -> int -> int -> textiter
	    = fn self => fn line_number => fn byte_index =>
		 let val iter = ref GObject.null
		     val ret = getiter_at_line_index_
				 (repr self) iter line_number byte_index
		 in !iter end
	val getiter_at_offset_ : cptr -> cptr ref -> int -> unit
	    = app3 (symb"mgtk_gtk_textbuffer_getiter_at_offset")
	val getiter_at_offset : 'a t -> int -> textiter
	    = fn self => fn char_offset =>
		 let val iter = ref GObject.null
		     val ret = getiter_at_offset_ (repr self) iter char_offset
		 in !iter end
	val getiter_at_line_ : cptr -> cptr ref -> int -> unit
	    = app3 (symb"mgtk_gtk_textbuffer_getiter_at_line")
	val getiter_at_line : 'a t -> int -> textiter
	    = fn self => fn line_number =>
		 let val iter = ref GObject.null
		     val ret = getiter_at_line_ (repr self) iter line_number
		 in !iter end
	val get_startiter_ : cptr -> cptr ref -> unit
	    = app2 (symb"mgtk_gtk_textbuffer_get_startiter")
	val get_startiter : 'a t -> textiter
	    = fn self => let val iter = ref GObject.null
			     val ret = get_startiter_ (repr self) iter
			 in !iter end
	val get_enditer_ : cptr -> cptr ref -> unit
	    = app2 (symb"mgtk_gtk_textbuffer_get_enditer")
	val get_enditer : 'a t -> textiter
	    = fn self => let val iter = ref GObject.null
			     val ret = get_enditer_ (repr self) iter
			 in !iter end
	val get_bounds_ : cptr -> cptr ref -> cptr ref -> unit
	    = app3 (symb"mgtk_gtk_textbuffer_get_bounds")
	val get_bounds : 'a t -> textiter * textiter
	    = fn self => let val (start, en) = (ref GObject.null, 
						ref GObject.null)
			     val ret = get_bounds_ (repr self) start en
			 in (!start, !en) end
	val getiter_at_mark_ : cptr -> cptr ref -> cptr -> unit
	    = app3 (symb"mgtk_gtk_textbuffer_getiter_at_mark")
	val getiter_at_mark : 'a t -> 'b t -> textiter
	    = fn self => fn mark =>
		 let val iter = ref GObject.null
		     val ret = getiter_at_mark_ (repr self) iter (repr mark)
		 in !iter end
	val getiter_at_child_anchor_ : cptr -> cptr ref -> cptr -> unit
	    = app3 (symb"mgtk_gtk_textbuffer_getiter_at_child_anchor")
	val getiter_at_child_anchor : 'a t -> 'b t -> textiter
	    = fn self => fn anchor =>
		 let val iter = ref GObject.null
		     val ret = getiter_at_child_anchor_
				 (repr self) iter (repr anchor)
		 in !iter end
	val get_modified_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_textbuffer_get_modified")
	val get_modified : 'a t -> bool = fn self => get_modified_ (repr self)
	val set_modified_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_textbuffer_set_modified")
	val set_modified : 'a t -> bool -> unit
	    = fn self => fn setting => set_modified_ (repr self) setting
	val add_selection_clipboard_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_textbuffer_add_selection_clipboard")
	val add_selection_clipboard : 'a t -> 'b t -> unit
	    = fn self => fn clipboard =>
		 add_selection_clipboard_ (repr self) (repr clipboard)
	val remove_selection_clipboard_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_textbuffer_remove_selection_clipboard")
	val remove_selection_clipboard : 'a t -> 'b t -> unit
	    = fn self => fn clipboard => remove_selection_clipboard_
					   (repr self) (repr clipboard)
	val cut_clipboard_ : cptr -> cptr -> bool -> unit
	    = app3 (symb"mgtk_gtk_textbuffer_cut_clipboard")
	val cut_clipboard : 'a t -> 'b t -> bool -> unit
	    = fn self => fn clipboard => fn default_editable =>
		 cut_clipboard_ (repr self) (repr clipboard) default_editable
	val copy_clipboard_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_textbuffer_copy_clipboard")
	val copy_clipboard : 'a t -> 'b t -> unit
	    = fn self => fn clipboard =>
		 copy_clipboard_ (repr self) (repr clipboard)
	val paste_clipboard_ : cptr -> cptr -> cptr -> bool -> unit
	    = app4 (symb"mgtk_gtk_textbuffer_paste_clipboard")
	val paste_clipboard : 'a t -> 'b t -> textiter -> bool -> unit
	    = fn self => fn clipboard => fn override_location => 
	      fn default_editable =>
		 paste_clipboard_ (repr self) (repr clipboard)
				  override_location default_editable
	val get_selection_bounds_ : cptr -> cptr ref -> cptr ref -> bool
	    = app3 (symb"mgtk_gtk_textbuffer_get_selection_bounds")
	val get_selection_bounds : 'a t -> bool * textiter * textiter
	    = fn self =>
		 let val (start, en) = (ref GObject.null, ref GObject.null)
		     val ret = get_selection_bounds_ (repr self) start en
		 in (ret, !start, !en) end
	val delete_selection_ : cptr -> bool -> bool -> bool
	    = app3 (symb"mgtk_gtk_textbuffer_delete_selection")
	val delete_selection : 'a t -> bool -> bool -> bool
	    = fn self => fn interactive => fn default_editable =>
		 delete_selection_ (repr self) interactive default_editable
	val begin_user_action_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_textbuffer_begin_user_action")
	val begin_user_action : 'a t -> unit
	    = fn self => begin_user_action_ (repr self)
	val end_user_action_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_textbuffer_end_user_action")
	val end_user_action : 'a t -> unit
	    = fn self => end_user_action_ (repr self)
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a textchildanchor_t = unit
	type 'a t = 'a textchildanchor_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTextChildAnchor obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_textchildanchor_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_textchildanchor_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_deleted_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_textchildanchor_get_deleted")
	val get_deleted : 'a t -> bool = fn self => get_deleted_ (repr self)
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
	val get_buffer : 'a t -> base TextBuffer.t
	val get_left_gravity : 'a t -> bool
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a textmark_t = unit
	type 'a t = 'a textmark_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTextMark obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_textmark_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_visible_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_textmark_set_visible")
	val set_visible : 'a t -> bool -> unit
	    = fn self => fn setting => set_visible_ (repr self) setting
	val get_visible_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_textmark_get_visible")
	val get_visible : 'a t -> bool = fn self => get_visible_ (repr self)
	val get_name_ : cptr -> string
	    = app1 (symb"mgtk_gtk_textmark_get_name")
	val get_name : 'a t -> string = fn self => get_name_ (repr self)
	val get_deleted_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_textmark_get_deleted")
	val get_deleted : 'a t -> bool = fn self => get_deleted_ (repr self)
	val get_buffer_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_textmark_get_buffer")
	val get_buffer : 'a t -> base TextBuffer.t
	    = fn self => TextBuffer.inherit
			   () (fn () => get_buffer_ (repr self))
	val get_left_gravity_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_textmark_get_left_gravity")
	val get_left_gravity : 'a t -> bool
	    = fn self => get_left_gravity_ (repr self)
    end
    structure TextTag :>
      sig
	type base
	type 'a texttag_t
	type 'a t = 'a texttag_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTextTag : 'a t -> base t
	val get_type : unit -> GType.t
	val new : string option -> base t
	val new' : unit -> base t
	val get_priority : 'a t -> int
	val set_priority : 'a t -> int -> unit
	val table_get_type : unit -> GType.t
	val event_sig : (unit -> unit -> unit -> bool) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a texttag_t = unit
	type 'a t = 'a texttag_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTextTag obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_text_tag_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : string -> cptr = app1 (symb"mgtk_gtk_text_tag_new")
	val new : string option -> base t
	    = fn name => make (new_ (getOpt (name, "")))
	val new' : unit -> base t = fn dummy => make (new_ "")
	val get_priority_ : cptr -> int
	    = app1 (symb"mgtk_gtk_text_tag_get_priority")
	val get_priority : 'a t -> int = fn self => get_priority_ (repr self)
	val set_priority_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_text_tag_set_priority")
	val set_priority : 'a t -> int -> unit
	    = fn self => fn priority => set_priority_ (repr self) priority
	val table_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_text_tag_table_get_type")
	val table_get_type : unit -> GType.t
	    = fn dummy => table_get_type_ dummy
	local open Signal
	      infixr -->
	in val event_sig : (unit -> unit -> unit -> bool) -> 'a t Signal.signal
	       = fn f => signal "event" false
			        (unit --> unit --> unit --> return_bool) f
	end
    end
    structure TextTagTable :>
      sig
	type base
	type 'a texttagtable_t
	type 'a t = 'a texttagtable_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTextTagTable : 'a t -> base t
	val new : unit -> base t
	val add : 'a t -> 'b t -> unit
	val remove : 'a t -> 'b t -> unit
	val lookup : 'a t -> string -> base t
	val get_size : 'a t -> int
	val tag_changed_sig : (unit -> bool -> unit) -> 'a t Signal.signal
	val tag_added_sig : (unit -> unit) -> 'a t Signal.signal
	val tag_removed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a texttagtable_t = unit
	type 'a t = 'a texttagtable_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTextTagTable obj = inherit () (fn () => repr obj)
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_text_tag_table_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val add_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_text_tag_table_add")
	val add : 'a t -> 'b t -> unit
	    = fn self => fn tag => add_ (repr self) (repr tag)
	val remove_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_text_tag_table_remove")
	val remove : 'a t -> 'b t -> unit
	    = fn self => fn tag => remove_ (repr self) (repr tag)
	val lookup_ : cptr -> string -> cptr
	    = app2 (symb"mgtk_gtk_text_tag_table_lookup")
	val lookup : 'a t -> string -> base t
	    = fn self => fn name => make (lookup_ (repr self) name)
	val get_size_ : cptr -> int
	    = app1 (symb"mgtk_gtk_text_tag_table_get_size")
	val get_size : 'a t -> int = fn self => get_size_ (repr self)
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
	val set_delay : 'a t -> int -> unit
	val set_tip : 'a t -> 'b Widget.t -> string -> string option -> unit
	val set_tip' : 'a t -> 'b Widget.t -> string -> unit
	val force_window : 'a t -> unit
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a tooltips_t = unit
	type 'a t = 'a tooltips_t Object.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Object.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTooltips obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_tooltips_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_tooltips_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val enable_ : cptr -> unit = app1 (symb"mgtk_gtk_tooltips_enable")
	val enable : 'a t -> unit = fn self => enable_ (repr self)
	val disable_ : cptr -> unit = app1 (symb"mgtk_gtk_tooltips_disable")
	val disable : 'a t -> unit = fn self => disable_ (repr self)
	val set_delay_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_tooltips_set_delay")
	val set_delay : 'a t -> int -> unit
	    = fn self => fn delay => set_delay_ (repr self) delay
	val set_tip_ : cptr -> cptr -> string -> string -> unit
	    = app4 (symb"mgtk_gtk_tooltips_set_tip")
	val set_tip : 'a t -> 'b Widget.t -> string -> string option -> unit
	    = fn self => fn widget => fn tip_text => fn tip_private =>
		 set_tip_ (repr self) (repr widget) tip_text
			  (getOpt (tip_private, ""))
	val set_tip' : 'a t -> 'b Widget.t -> string -> unit
	    = fn self => fn widget => fn tip_text =>
		 set_tip_ (repr self) (repr widget) tip_text ""
	val force_window_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_tooltips_force_window")
	val force_window : 'a t -> unit = fn self => force_window_ (repr self)
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
	val getiter : 'a t -> tree_path -> bool * treeiter
	val getiter_from_string : 'a t -> string -> bool * treeiter
	val getiter_root : 'a t -> treeiter -> bool
	val getiter_first : 'a t -> bool * treeiter
	val get_path : 'a t -> treeiter -> tree_path
	val iter_next : 'a t -> treeiter -> bool * treeiter
	val iter_children : 'a t -> treeiter option -> bool * treeiter
	val iter_children' : 'a t -> treeiter -> bool * treeiter
	val iter_has_child : 'a t -> treeiter -> bool
	val iter_n_children : 'a t -> treeiter option -> int
	val iter_n_children' : 'a t -> int
	val iter_nth_child : 'a t -> treeiter option -> int -> bool * treeiter
	val iter_nth_child' : 'a t -> treeiter -> int -> bool * treeiter
	val iter_parent : 'a t -> treeiter -> bool * treeiter
	val ref_node : 'a t -> treeiter -> unit
	val unref_node : 'a t -> treeiter -> unit
	val get : 'a t -> treeiter -> unit
	val row_changed : 'a t -> tree_path -> treeiter -> unit
	val row_inserted : 'a t -> tree_path -> treeiter -> unit
	val row_has_child_toggled : 'a t -> tree_path -> treeiter -> unit
	val row_deleted : 'a t -> tree_path -> unit
	val sort_get_type : unit -> GType.t
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a treemodel_t = unit
	type 'a t = 'a treemodel_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeModel obj = inherit () (fn () => repr obj)
	type flags = int
	val get_flags_ : unit -> int * int
	    = app1 (symb"mgtk_get_gtk_treemodel_flags")
	val (TREE_MODEL_ITERS_PERSIST, TREE_MODEL_LIST_ONLY) = get_flags_ ()
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_treemodel_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val get_flags_ : cptr -> int
	    = app1 (symb"mgtk_gtk_treemodel_get_flags")
	val get_flags : 'a t -> flags list
	    = fn self => Flags.get (get_flags_ (repr self))
	val get_n_columns_ : cptr -> int
	    = app1 (symb"mgtk_gtk_treemodel_get_n_columns")
	val get_n_columns : 'a t -> int = fn self => get_n_columns_ (repr self)
	val get_columntype_ : cptr -> int -> GType.t
	    = app2 (symb"mgtk_gtk_treemodel_get_columntype")
	val get_columntype : 'a t -> int -> GType.t
	    = fn self => fn index => get_columntype_ (repr self) index
	val getiter_ : cptr -> cptr ref -> cptr -> bool
	    = app3 (symb"mgtk_gtk_treemodel_getiter")
	val getiter : 'a t -> tree_path -> bool * treeiter
	    = fn self => fn path =>
		 let val iter = ref GObject.null
		     val ret = getiter_ (repr self) iter path
		 in (ret, !iter) end
	val getiter_from_string_ : cptr -> cptr ref -> string -> bool
	    = app3 (symb"mgtk_gtk_treemodel_getiter_from_string")
	val getiter_from_string : 'a t -> string -> bool * treeiter
	    = fn self => fn path_string =>
		 let val iter = ref GObject.null
		     val ret = getiter_from_string_
				 (repr self) iter path_string
		 in (ret, !iter) end
	val getiter_root_ : cptr -> cptr -> bool
	    = app2 (symb"mgtk_gtk_treemodel_getiter_root")
	val getiter_root : 'a t -> treeiter -> bool
	    = fn self => fn iter => getiter_root_ (repr self) iter
	val getiter_first_ : cptr -> cptr ref -> bool
	    = app2 (symb"mgtk_gtk_treemodel_getiter_first")
	val getiter_first : 'a t -> bool * treeiter
	    = fn self => let val iter = ref GObject.null
			     val ret = getiter_first_ (repr self) iter
			 in (ret, !iter) end
	val get_path_ : cptr -> cptr -> cptr
	    = app2 (symb"mgtk_gtk_treemodel_get_path")
	val get_path : 'a t -> treeiter -> tree_path
	    = fn self => fn iter => get_path_ (repr self) iter
	val iter_next_ : cptr -> cptr ref -> bool
	    = app2 (symb"mgtk_gtk_treemodel_iter_next")
	val iter_next : 'a t -> treeiter -> bool * treeiter
	    = fn self => fn iter => let val iter = ref iter
					val ret = iter_next_ (repr self) iter
				    in (ret, !iter) end
	val iter_children_ : cptr -> cptr ref -> cptr -> bool
	    = app3 (symb"mgtk_gtk_treemodel_iter_children")
	val iter_children : 'a t -> treeiter option -> bool * treeiter
	    = fn self => fn parent =>
		 let val iter = ref GObject.null
		     val ret = iter_children_ (repr self) iter
					      (getOpt (parent, GObject.null))
		 in (ret, !iter) end
	val iter_children' : 'a t -> treeiter -> bool * treeiter
	    = fn self => fn iter =>
		 let val iter = ref GObject.null
		     val ret = iter_children_ (repr self) iter GObject.null
		 in (ret, !iter) end
	val iter_has_child_ : cptr -> cptr -> bool
	    = app2 (symb"mgtk_gtk_treemodel_iter_has_child")
	val iter_has_child : 'a t -> treeiter -> bool
	    = fn self => fn iter => iter_has_child_ (repr self) iter
	val iter_n_children_ : cptr -> cptr -> int
	    = app2 (symb"mgtk_gtk_treemodel_iter_n_children")
	val iter_n_children : 'a t -> treeiter option -> int
	    = fn self => fn iter =>
		 iter_n_children_ (repr self) (getOpt (iter, GObject.null))
	val iter_n_children' : 'a t -> int
	    = fn self => iter_n_children_ (repr self) GObject.null
	val iter_nth_child_ : cptr -> cptr ref -> cptr -> int -> bool
	    = app4 (symb"mgtk_gtk_treemodel_iter_nth_child")
	val iter_nth_child : 'a t -> treeiter option -> int -> bool * treeiter
	    = fn self => fn parent => fn n =>
		 let val iter = ref GObject.null
		     val ret = iter_nth_child_
				 (repr self) iter
				 (getOpt (parent, GObject.null)) n
		 in (ret, !iter) end
	val iter_nth_child' : 'a t -> treeiter -> int -> bool * treeiter
	    = fn self => fn iter => fn n =>
		 let val iter = ref GObject.null
		     val ret = iter_nth_child_ (repr self) iter GObject.null n
		 in (ret, !iter) end
	val iter_parent_ : cptr -> cptr ref -> cptr -> bool
	    = app3 (symb"mgtk_gtk_treemodel_iter_parent")
	val iter_parent : 'a t -> treeiter -> bool * treeiter
	    = fn self => fn child =>
		 let val iter = ref GObject.null
		     val ret = iter_parent_ (repr self) iter child
		 in (ret, !iter) end
	val ref_node_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_treemodel_ref_node")
	val ref_node : 'a t -> treeiter -> unit
	    = fn self => fn iter => ref_node_ (repr self) iter
	val unref_node_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_treemodel_unref_node")
	val unref_node : 'a t -> treeiter -> unit
	    = fn self => fn iter => unref_node_ (repr self) iter
	val get_ : cptr -> cptr -> unit = app2 (symb"mgtk_gtk_treemodel_get")
	val get : 'a t -> treeiter -> unit
	    = fn self => fn iter => get_ (repr self) iter
	val row_changed_ : cptr -> cptr -> cptr -> unit
	    = app3 (symb"mgtk_gtk_treemodel_row_changed")
	val row_changed : 'a t -> tree_path -> treeiter -> unit
	    = fn self => fn path => fn iter =>
		 row_changed_ (repr self) path iter
	val row_inserted_ : cptr -> cptr -> cptr -> unit
	    = app3 (symb"mgtk_gtk_treemodel_row_inserted")
	val row_inserted : 'a t -> tree_path -> treeiter -> unit
	    = fn self => fn path => fn iter =>
		 row_inserted_ (repr self) path iter
	val row_has_child_toggled_ : cptr -> cptr -> cptr -> unit
	    = app3 (symb"mgtk_gtk_treemodel_row_has_child_toggled")
	val row_has_child_toggled : 'a t -> tree_path -> treeiter -> unit
	    = fn self => fn path => fn iter =>
		 row_has_child_toggled_ (repr self) path iter
	val row_deleted_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_treemodel_row_deleted")
	val row_deleted : 'a t -> tree_path -> unit
	    = fn self => fn path => row_deleted_ (repr self) path
	val sort_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_treemodel_sort_get_type")
	val sort_get_type : unit -> GType.t = fn dummy => sort_get_type_ dummy
    end
    structure TreeDragSource :>
      sig
	type base
	type 'a treedragsource_t
	type 'a t = 'a treedragsource_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTreeDragSource : 'a t -> base t
	val get_type : unit -> GType.t
	val row_draggable : 'a t -> tree_path -> bool
	val drag_data_delete : 'a t -> tree_path -> bool
	val drag_data_get : 'a t -> tree_path -> selection_data -> bool
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a treedragsource_t = unit
	type 'a t = 'a treedragsource_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeDragSource obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_tree_drag_source_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val row_draggable_ : cptr -> cptr -> bool
	    = app2 (symb"mgtk_gtk_tree_drag_source_row_draggable")
	val row_draggable : 'a t -> tree_path -> bool
	    = fn self => fn path => row_draggable_ (repr self) path
	val drag_data_delete_ : cptr -> cptr -> bool
	    = app2 (symb"mgtk_gtk_tree_drag_source_drag_data_delete")
	val drag_data_delete : 'a t -> tree_path -> bool
	    = fn self => fn path => drag_data_delete_ (repr self) path
	val drag_data_get_ : cptr -> cptr -> cptr -> bool
	    = app3 (symb"mgtk_gtk_tree_drag_source_drag_data_get")
	val drag_data_get : 'a t -> tree_path -> selection_data -> bool
	    = fn self => fn path => fn selection_data =>
		 drag_data_get_ (repr self) path selection_data
    end
    structure TreeDragDest :>
      sig
	type base
	type 'a treedragdest_t
	type 'a t = 'a treedragdest_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTreeDragDest : 'a t -> base t
	val get_type : unit -> GType.t
	val drag_data_received : 'a t -> tree_path -> selection_data -> bool
	val row_drop_possible : 'a t -> tree_path -> selection_data -> bool
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a treedragdest_t = unit
	type 'a t = 'a treedragdest_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeDragDest obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_tree_drag_dest_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val drag_data_received_ : cptr -> cptr -> cptr -> bool
	    = app3 (symb"mgtk_gtk_tree_drag_dest_drag_data_received")
	val drag_data_received : 'a t -> tree_path -> selection_data -> bool
	    = fn self => fn dest => fn selection_data =>
		 drag_data_received_ (repr self) dest selection_data
	val row_drop_possible_ : cptr -> cptr -> cptr -> bool
	    = app3 (symb"mgtk_gtk_tree_drag_dest_row_drop_possible")
	val row_drop_possible : 'a t -> tree_path -> selection_data -> bool
	    = fn self => fn dest_path => fn selection_data =>
		 row_drop_possible_ (repr self) dest_path selection_data
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a treesortable_t = unit
	type 'a t = 'a treesortable_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeSortable obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_tree_sortable_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val sort_column_changed_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_tree_sortable_sort_column_changed")
	val sort_column_changed : 'a t -> unit
	    = fn self => sort_column_changed_ (repr self)
	val set_sort_column_id_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_tree_sortable_set_sort_column_id")
	val set_sort_column_id : 'a t -> int -> sorttype -> unit
	    = fn self => fn sort_column_id => fn order =>
		 set_sort_column_id_ (repr self) sort_column_id order
	val has_default_sort_func_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_tree_sortable_has_default_sort_func")
	val has_default_sort_func : 'a t -> bool
	    = fn self => has_default_sort_func_ (repr self)
    end
    structure ListStore :>
      sig
	type base
	type 'a liststore_t
	type 'a t = 'a liststore_t TreeModel.t TreeDragSource.t TreeDragDest.t
		      TreeSortable.t
		      GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toListStore : 'a t -> base t
	val new : int -> base t
	val newv : int -> GType.t list -> base t
	val set_column_types : 'a t -> int -> GType.t list -> unit
	val set : 'a t -> treeiter -> unit
	val remove : 'a t -> treeiter -> treeiter
	val insert : 'a t -> int -> treeiter
	val insert_before : 'a t -> treeiter -> treeiter
	val insert_after : 'a t -> treeiter -> treeiter
	val prepend : 'a t -> treeiter
	val append : 'a t -> treeiter
	val clear : 'a t -> unit
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a liststore_t = unit
	type 'a t = 'a liststore_t TreeModel.t TreeDragSource.t TreeDragDest.t
		      TreeSortable.t
		      GObject.t
	fun inherit w con
	  = let val con = let val ptr = con () in fn () => ptr end
		val witness = ()
		val witness = TreeModel.inherit witness con
		val witness = TreeDragSource.inherit witness con
		val witness = TreeDragDest.inherit witness con
		val witness = TreeSortable.inherit witness con
	    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toListStore obj = inherit () (fn () => repr obj)
	val new_ : int -> cptr = app1 (symb"mgtk_gtk_list_store_new")
	val new : int -> base t = fn n_columns => make (new_ n_columns)
	val newv_ : int -> GType.t list -> cptr
	    = app2 (symb"mgtk_gtk_list_store_newv")
	val newv : int -> GType.t list -> base t
	    = fn n_columns => fn types => make (newv_ n_columns types)
	val set_column_types_ : cptr -> int -> GType.t list -> unit
	    = app3 (symb"mgtk_gtk_list_store_set_column_types")
	val set_column_types : 'a t -> int -> GType.t list -> unit
	    = fn self => fn n_columns => fn types =>
		 set_column_types_ (repr self) n_columns types
	val set_ : cptr -> cptr -> unit = app2 (symb"mgtk_gtk_list_store_set")
	val set : 'a t -> treeiter -> unit
	    = fn self => fn iter => set_ (repr self) iter
	val remove_ : cptr -> cptr ref -> unit
	    = app2 (symb"mgtk_gtk_list_store_remove")
	val remove : 'a t -> treeiter -> treeiter
	    = fn self => fn iter => let val iter = ref iter
					val ret = remove_ (repr self) iter
				    in !iter end
	val insert_ : cptr -> cptr ref -> int -> unit
	    = app3 (symb"mgtk_gtk_list_store_insert")
	val insert : 'a t -> int -> treeiter
	    = fn self => fn position =>
		 let val iter = ref GObject.null
		     val ret = insert_ (repr self) iter position
		 in !iter end
	val insert_before_ : cptr -> cptr ref -> cptr -> unit
	    = app3 (symb"mgtk_gtk_list_store_insert_before")
	val insert_before : 'a t -> treeiter -> treeiter
	    = fn self => fn sibling =>
		 let val iter = ref GObject.null
		     val ret = insert_before_ (repr self) iter sibling
		 in !iter end
	val insert_after_ : cptr -> cptr ref -> cptr -> unit
	    = app3 (symb"mgtk_gtk_list_store_insert_after")
	val insert_after : 'a t -> treeiter -> treeiter
	    = fn self => fn sibling =>
		 let val iter = ref GObject.null
		     val ret = insert_after_ (repr self) iter sibling
		 in !iter end
	val prepend_ : cptr -> cptr ref -> unit
	    = app2 (symb"mgtk_gtk_list_store_prepend")
	val prepend : 'a t -> treeiter
	    = fn self => let val iter = ref GObject.null
			     val ret = prepend_ (repr self) iter
			 in !iter end
	val append_ : cptr -> cptr ref -> unit
	    = app2 (symb"mgtk_gtk_list_store_append")
	val append : 'a t -> treeiter
	    = fn self => let val iter = ref GObject.null
			     val ret = append_ (repr self) iter
			 in !iter end
	val clear_ : cptr -> unit = app1 (symb"mgtk_gtk_list_store_clear")
	val clear : 'a t -> unit = fn self => clear_ (repr self)
    end
    structure TreeModelSort :>
      sig
	type base
	type 'a treemodelsort_t
	type 'a t = 'a treemodelsort_t TreeModel.t TreeSortable.t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTreeModelSort : 'a t -> base t
	val new_with_model : 'a t -> base t
	val get_model : 'a t -> base t
	val convert_child_path_to_path : 'a t -> tree_path -> tree_path
	val convert_childiter_toiter : 'a t -> treeiter -> treeiter
	val convert_path_to_child_path : 'a t -> tree_path -> tree_path
	val convertiter_to_childiter : 'a t -> treeiter -> treeiter
	val reset_default_sort_func : 'a t -> unit
	val clear_cache : 'a t -> unit
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a treemodelsort_t = unit
	type 'a t = 'a treemodelsort_t TreeModel.t TreeSortable.t GObject.t
	fun inherit w con
	  = let val con = let val ptr = con () in fn () => ptr end
		val witness = ()
		val witness = TreeModel.inherit witness con
		val witness = TreeSortable.inherit witness con
	    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeModelSort obj = inherit () (fn () => repr obj)
	val new_with_model_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_treemodel_sort_new_with_model")
	val new_with_model : 'a t -> base t
	    = fn child_model => make (new_with_model_ (repr child_model))
	val get_model_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_treemodel_sort_get_model")
	val get_model : 'a t -> base t
	    = fn self => make (get_model_ (repr self))
	val convert_child_path_to_path_ : cptr -> cptr -> cptr
	    = app2 (symb"mgtk_gtk_treemodel_sort_convert_child_path_to_path")
	val convert_child_path_to_path : 'a t -> tree_path -> tree_path
	    = fn self => fn child_path =>
		 convert_child_path_to_path_ (repr self) child_path
	val convert_childiter_toiter_ : cptr -> cptr ref -> cptr -> unit
	    = app3 (symb"mgtk_gtk_treemodel_sort_convert_childiter_toiter")
	val convert_childiter_toiter : 'a t -> treeiter -> treeiter
	    = fn self => fn child_iter =>
		 let val sort_iter = ref GObject.null
		     val ret = convert_childiter_toiter_
				 (repr self) sort_iter child_iter
		 in !sort_iter end
	val convert_path_to_child_path_ : cptr -> cptr -> cptr
	    = app2 (symb"mgtk_gtk_treemodel_sort_convert_path_to_child_path")
	val convert_path_to_child_path : 'a t -> tree_path -> tree_path
	    = fn self => fn sorted_path =>
		 convert_path_to_child_path_ (repr self) sorted_path
	val convertiter_to_childiter_ : cptr -> cptr ref -> cptr -> unit
	    = app3 (symb"mgtk_gtk_treemodel_sort_convertiter_to_childiter")
	val convertiter_to_childiter : 'a t -> treeiter -> treeiter
	    = fn self => fn sorted_iter =>
		 let val child_iter = ref GObject.null
		     val ret = convertiter_to_childiter_
				 (repr self) child_iter sorted_iter
		 in !child_iter end
	val reset_default_sort_func_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_treemodel_sort_reset_default_sort_func")
	val reset_default_sort_func : 'a t -> unit
	    = fn self => reset_default_sort_func_ (repr self)
	val clear_cache_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_treemodel_sort_clear_cache")
	val clear_cache : 'a t -> unit = fn self => clear_cache_ (repr self)
    end
    structure TreeSelection :>
      sig
	type base
	type 'a treeselection_t
	type 'a t = 'a treeselection_t Object.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTreeSelection : 'a t -> base t
	val get_type : unit -> GType.t
	val set_mode : 'a t -> selection_mode -> unit
	val get_mode : 'a t -> selection_mode
	val get_user_data : 'a t -> cptr
	val get_treeview : 'a t -> base t
	val get_selected : 'a t -> bool * base TreeModel.t * treeiter
	val select_path : 'a t -> tree_path -> unit
	val unselect_path : 'a t -> tree_path -> unit
	val selectiter : 'a t -> treeiter -> unit
	val unselectiter : 'a t -> treeiter -> unit
	val path_is_selected : 'a t -> tree_path -> bool
	val iter_is_selected : 'a t -> treeiter -> bool
	val select_all : 'a t -> unit
	val unselect_all : 'a t -> unit
	val select_range : 'a t -> tree_path -> tree_path -> unit
	val changed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a treeselection_t = unit
	type 'a t = 'a treeselection_t Object.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Object.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeSelection obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_treeselection_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_mode_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_treeselection_set_mode")
	val set_mode : 'a t -> selection_mode -> unit
	    = fn self => fn typ => set_mode_ (repr self) typ
	val get_mode_ : cptr -> int
	    = app1 (symb"mgtk_gtk_treeselection_get_mode")
	val get_mode : 'a t -> selection_mode
	    = fn self => get_mode_ (repr self)
	val get_user_data_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_treeselection_get_user_data")
	val get_user_data : 'a t -> cptr
	    = fn self => get_user_data_ (repr self)
	val get_treeview_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_treeselection_get_treeview")
	val get_treeview : 'a t -> base t
	    = fn self => make (get_treeview_ (repr self))
	val get_selected_ : cptr -> cptr ref -> cptr ref -> bool
	    = app3 (symb"mgtk_gtk_treeselection_get_selected")
	val get_selected : 'a t -> bool * base TreeModel.t * treeiter
	    = fn self =>
		 let val (model, iter) = (ref GObject.null, ref GObject.null)
		     val ret = get_selected_ (repr self) model iter
		 in (ret, TreeModel.inherit () (fn () => !model), !iter) end
	val select_path_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_treeselection_select_path")
	val select_path : 'a t -> tree_path -> unit
	    = fn self => fn path => select_path_ (repr self) path
	val unselect_path_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_treeselection_unselect_path")
	val unselect_path : 'a t -> tree_path -> unit
	    = fn self => fn path => unselect_path_ (repr self) path
	val selectiter_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_treeselection_selectiter")
	val selectiter : 'a t -> treeiter -> unit
	    = fn self => fn iter => selectiter_ (repr self) iter
	val unselectiter_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_treeselection_unselectiter")
	val unselectiter : 'a t -> treeiter -> unit
	    = fn self => fn iter => unselectiter_ (repr self) iter
	val path_is_selected_ : cptr -> cptr -> bool
	    = app2 (symb"mgtk_gtk_treeselection_path_is_selected")
	val path_is_selected : 'a t -> tree_path -> bool
	    = fn self => fn path => path_is_selected_ (repr self) path
	val iter_is_selected_ : cptr -> cptr -> bool
	    = app2 (symb"mgtk_gtk_treeselection_iter_is_selected")
	val iter_is_selected : 'a t -> treeiter -> bool
	    = fn self => fn iter => iter_is_selected_ (repr self) iter
	val select_all_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_treeselection_select_all")
	val select_all : 'a t -> unit = fn self => select_all_ (repr self)
	val unselect_all_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_treeselection_unselect_all")
	val unselect_all : 'a t -> unit = fn self => unselect_all_ (repr self)
	val select_range_ : cptr -> cptr -> cptr -> unit
	    = app3 (symb"mgtk_gtk_treeselection_select_range")
	val select_range : 'a t -> tree_path -> tree_path -> unit
	    = fn self => fn start_path => fn end_path =>
		 select_range_ (repr self) start_path end_path
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
	type 'a t = 'a treestore_t TreeModel.t TreeDragSource.t TreeDragDest.t
		      TreeSortable.t
		      GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTreeStore : 'a t -> base t
	val get_type : unit -> GType.t
	val new : int -> base t
	val newv : int -> GType.t list -> base t
	val set : 'a t -> treeiter -> unit
	val remove : 'a t -> treeiter -> treeiter
	val insert : 'a t -> treeiter -> int -> treeiter
	val insert_before : 'a t -> treeiter -> treeiter -> treeiter
	val insert_after : 'a t -> treeiter -> treeiter -> treeiter
	val prepend : 'a t -> treeiter -> treeiter
	val append : 'a t -> treeiter -> treeiter
	val is_ancestor : 'a t -> treeiter -> treeiter -> bool
	val storeiter_depth : 'a t -> treeiter -> int
	val clear : 'a t -> unit
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a treestore_t = unit
	type 'a t = 'a treestore_t TreeModel.t TreeDragSource.t TreeDragDest.t
		      TreeSortable.t
		      GObject.t
	fun inherit w con
	  = let val con = let val ptr = con () in fn () => ptr end
		val witness = ()
		val witness = TreeModel.inherit witness con
		val witness = TreeDragSource.inherit witness con
		val witness = TreeDragDest.inherit witness con
		val witness = TreeSortable.inherit witness con
	    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeStore obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_tree_store_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : int -> cptr = app1 (symb"mgtk_gtk_tree_store_new")
	val new : int -> base t = fn n_columns => make (new_ n_columns)
	val newv_ : int -> GType.t list -> cptr
	    = app2 (symb"mgtk_gtk_tree_store_newv")
	val newv : int -> GType.t list -> base t
	    = fn n_columns => fn types => make (newv_ n_columns types)
	val set_ : cptr -> cptr -> unit = app2 (symb"mgtk_gtk_tree_store_set")
	val set : 'a t -> treeiter -> unit
	    = fn self => fn iter => set_ (repr self) iter
	val remove_ : cptr -> cptr ref -> unit
	    = app2 (symb"mgtk_gtk_tree_store_remove")
	val remove : 'a t -> treeiter -> treeiter
	    = fn self => fn iter => let val iter = ref iter
					val ret = remove_ (repr self) iter
				    in !iter end
	val insert_ : cptr -> cptr ref -> cptr -> int -> unit
	    = app4 (symb"mgtk_gtk_tree_store_insert")
	val insert : 'a t -> treeiter -> int -> treeiter
	    = fn self => fn parent => fn position =>
		 let val iter = ref GObject.null
		     val ret = insert_ (repr self) iter parent position
		 in !iter end
	val insert_before_ : cptr -> cptr ref -> cptr -> cptr -> unit
	    = app4 (symb"mgtk_gtk_tree_store_insert_before")
	val insert_before : 'a t -> treeiter -> treeiter -> treeiter
	    = fn self => fn parent => fn sibling =>
		 let val iter = ref GObject.null
		     val ret = insert_before_ (repr self) iter parent sibling
		 in !iter end
	val insert_after_ : cptr -> cptr ref -> cptr -> cptr -> unit
	    = app4 (symb"mgtk_gtk_tree_store_insert_after")
	val insert_after : 'a t -> treeiter -> treeiter -> treeiter
	    = fn self => fn parent => fn sibling =>
		 let val iter = ref GObject.null
		     val ret = insert_after_ (repr self) iter parent sibling
		 in !iter end
	val prepend_ : cptr -> cptr ref -> cptr -> unit
	    = app3 (symb"mgtk_gtk_tree_store_prepend")
	val prepend : 'a t -> treeiter -> treeiter
	    = fn self => fn parent =>
		 let val iter = ref GObject.null
		     val ret = prepend_ (repr self) iter parent
		 in !iter end
	val append_ : cptr -> cptr ref -> cptr -> unit
	    = app3 (symb"mgtk_gtk_tree_store_append")
	val append : 'a t -> treeiter -> treeiter
	    = fn self => fn parent =>
		 let val iter = ref GObject.null
		     val ret = append_ (repr self) iter parent
		 in !iter end
	val is_ancestor_ : cptr -> cptr -> cptr -> bool
	    = app3 (symb"mgtk_gtk_tree_store_is_ancestor")
	val is_ancestor : 'a t -> treeiter -> treeiter -> bool
	    = fn self => fn iter => fn descendant =>
		 is_ancestor_ (repr self) iter descendant
	val storeiter_depth_ : cptr -> cptr -> int
	    = app2 (symb"mgtk_gtk_tree_storeiter_depth")
	val storeiter_depth : 'a t -> treeiter -> int
	    = fn self => fn iter => storeiter_depth_ (repr self) iter
	val clear_ : cptr -> unit = app1 (symb"mgtk_gtk_tree_store_clear")
	val clear : 'a t -> unit = fn self => clear_ (repr self)
    end
    structure TreeViewColumn :>
      sig
	type base
	type 'a treeviewcolumn_t
	type 'a t = 'a treeviewcolumn_t Object.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTreeViewColumn : 'a t -> base t
	type sizing
	val TREE_VIEW_COLUMN_GROW_ONLY : sizing
	val TREE_VIEW_COLUMN_AUTOSIZE : sizing
	val TREE_VIEW_COLUMN_FIXED : sizing
	val get_type : unit -> GType.t
	val new : unit -> base t
	val new_with_attributes : string -> 'a CellRenderer.t -> base t
	val pack_start : 'a t -> 'b CellRenderer.t -> bool -> unit
	val pack_end : 'a t -> 'b CellRenderer.t -> bool -> unit
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
	  : 'a t -> 'b TreeModel.t -> treeiter -> bool -> bool -> unit
	val cell_is_visible : 'a t -> bool
	val clicked_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a treeviewcolumn_t = unit
	type 'a t = 'a treeviewcolumn_t Object.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Object.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeViewColumn obj = inherit () (fn () => repr obj)
	type sizing = int
	val get_sizing_ : unit -> int * int * int
	    = app1 (symb"mgtk_get_gtk_treeviewcolumn_sizing")
	val (TREE_VIEW_COLUMN_GROW_ONLY, TREE_VIEW_COLUMN_AUTOSIZE, 
	     TREE_VIEW_COLUMN_FIXED)
	    = get_sizing_ ()
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_treeviewcolumn_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_treeviewcolumn_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_attributes_ : string -> cptr -> cptr
	    = app2 (symb"mgtk_gtk_treeviewcolumn_new_with_attributes")
	val new_with_attributes : string -> 'a CellRenderer.t -> base t
	    = fn title => fn cell =>
		 make (new_with_attributes_ title (repr cell))
	val pack_start_ : cptr -> cptr -> bool -> unit
	    = app3 (symb"mgtk_gtk_treeviewcolumn_pack_start")
	val pack_start : 'a t -> 'b CellRenderer.t -> bool -> unit
	    = fn self => fn cell => fn expand =>
		 pack_start_ (repr self) (repr cell) expand
	val pack_end_ : cptr -> cptr -> bool -> unit
	    = app3 (symb"mgtk_gtk_treeviewcolumn_pack_end")
	val pack_end : 'a t -> 'b CellRenderer.t -> bool -> unit
	    = fn self => fn cell => fn expand =>
		 pack_end_ (repr self) (repr cell) expand
	val clear_ : cptr -> unit = app1 (symb"mgtk_gtk_treeviewcolumn_clear")
	val clear : 'a t -> unit = fn self => clear_ (repr self)
	val add_attribute_ : cptr -> cptr -> string -> int -> unit
	    = app4 (symb"mgtk_gtk_treeviewcolumn_add_attribute")
	val add_attribute : 'a t -> 'b CellRenderer.t -> string -> int -> unit
	    = fn self => fn cell_renderer => fn attribute => fn column =>
		 add_attribute_
		   (repr self) (repr cell_renderer) attribute column
	val set_attributes_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_treeviewcolumn_set_attributes")
	val set_attributes : 'a t -> 'b CellRenderer.t -> unit
	    = fn self => fn cell_renderer =>
		 set_attributes_ (repr self) (repr cell_renderer)
	val clear_attributes_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_treeviewcolumn_clear_attributes")
	val clear_attributes : 'a t -> 'b CellRenderer.t -> unit
	    = fn self => fn cell_renderer =>
		 clear_attributes_ (repr self) (repr cell_renderer)
	val set_spacing_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_treeviewcolumn_set_spacing")
	val set_spacing : 'a t -> int -> unit
	    = fn self => fn spacing => set_spacing_ (repr self) spacing
	val get_spacing_ : cptr -> int
	    = app1 (symb"mgtk_gtk_treeviewcolumn_get_spacing")
	val get_spacing : 'a t -> int = fn self => get_spacing_ (repr self)
	val set_visible_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_treeviewcolumn_set_visible")
	val set_visible : 'a t -> bool -> unit
	    = fn self => fn visible => set_visible_ (repr self) visible
	val get_visible_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_treeviewcolumn_get_visible")
	val get_visible : 'a t -> bool = fn self => get_visible_ (repr self)
	val set_resizable_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_treeviewcolumn_set_resizable")
	val set_resizable : 'a t -> bool -> unit
	    = fn self => fn resizable => set_resizable_ (repr self) resizable
	val get_resizable_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_treeviewcolumn_get_resizable")
	val get_resizable : 'a t -> bool
	    = fn self => get_resizable_ (repr self)
	val set_sizing_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_treeviewcolumn_set_sizing")
	val set_sizing : 'a t -> sizing -> unit
	    = fn self => fn typ => set_sizing_ (repr self) typ
	val get_sizing_ : cptr -> int
	    = app1 (symb"mgtk_gtk_treeviewcolumn_get_sizing")
	val get_sizing : 'a t -> int = fn self => get_sizing_ (repr self)
	val get_width_ : cptr -> int
	    = app1 (symb"mgtk_gtk_treeviewcolumn_get_width")
	val get_width : 'a t -> int = fn self => get_width_ (repr self)
	val get_fixed_width_ : cptr -> int
	    = app1 (symb"mgtk_gtk_treeviewcolumn_get_fixed_width")
	val get_fixed_width : 'a t -> int
	    = fn self => get_fixed_width_ (repr self)
	val set_fixed_width_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_treeviewcolumn_set_fixed_width")
	val set_fixed_width : 'a t -> int -> unit
	    = fn self => fn fixed_width =>
		 set_fixed_width_ (repr self) fixed_width
	val set_min_width_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_treeviewcolumn_set_min_width")
	val set_min_width : 'a t -> int -> unit
	    = fn self => fn min_width => set_min_width_ (repr self) min_width
	val get_min_width_ : cptr -> int
	    = app1 (symb"mgtk_gtk_treeviewcolumn_get_min_width")
	val get_min_width : 'a t -> int = fn self => get_min_width_ (repr self)
	val set_max_width_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_treeviewcolumn_set_max_width")
	val set_max_width : 'a t -> int -> unit
	    = fn self => fn max_width => set_max_width_ (repr self) max_width
	val get_max_width_ : cptr -> int
	    = app1 (symb"mgtk_gtk_treeviewcolumn_get_max_width")
	val get_max_width : 'a t -> int = fn self => get_max_width_ (repr self)
	val clicked_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_treeviewcolumn_clicked")
	val clicked : 'a t -> unit = fn self => clicked_ (repr self)
	val set_title_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_treeviewcolumn_set_title")
	val set_title : 'a t -> string -> unit
	    = fn self => fn title => set_title_ (repr self) title
	val get_title_ : cptr -> string
	    = app1 (symb"mgtk_gtk_treeviewcolumn_get_title")
	val get_title : 'a t -> string = fn self => get_title_ (repr self)
	val set_clickable_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_treeviewcolumn_set_clickable")
	val set_clickable : 'a t -> bool -> unit
	    = fn self => fn active => set_clickable_ (repr self) active
	val get_clickable_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_treeviewcolumn_get_clickable")
	val get_clickable : 'a t -> bool
	    = fn self => get_clickable_ (repr self)
	val set_widget_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_treeviewcolumn_set_widget")
	val set_widget : 'a t -> 'b Widget.t option -> unit
	    = fn self => fn widget =>
		 set_widget_ (repr self)
			     (getOpt (Option.map repr widget, GObject.null))
	val set_widget' : 'a t -> unit
	    = fn self => set_widget_ (repr self) GObject.null
	val get_widget_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_treeviewcolumn_get_widget")
	val get_widget : 'a t -> base Widget.t
	    = fn self => Widget.inherit () (fn () => get_widget_ (repr self))
	val set_alignment_ : cptr -> real -> unit
	    = app2 (symb"mgtk_gtk_treeviewcolumn_set_alignment")
	val set_alignment : 'a t -> real -> unit
	    = fn self => fn xalign => set_alignment_ (repr self) xalign
	val get_alignment_ : cptr -> real
	    = app1 (symb"mgtk_gtk_treeviewcolumn_get_alignment")
	val get_alignment : 'a t -> real
	    = fn self => get_alignment_ (repr self)
	val set_reorderable_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_treeviewcolumn_set_reorderable")
	val set_reorderable : 'a t -> bool -> unit
	    = fn self => fn reorderable =>
		 set_reorderable_ (repr self) reorderable
	val get_reorderable_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_treeviewcolumn_get_reorderable")
	val get_reorderable : 'a t -> bool
	    = fn self => get_reorderable_ (repr self)
	val set_sort_column_id_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_treeviewcolumn_set_sort_column_id")
	val set_sort_column_id : 'a t -> int -> unit
	    = fn self => fn sort_column_id =>
		 set_sort_column_id_ (repr self) sort_column_id
	val get_sort_column_id_ : cptr -> int
	    = app1 (symb"mgtk_gtk_treeviewcolumn_get_sort_column_id")
	val get_sort_column_id : 'a t -> int
	    = fn self => get_sort_column_id_ (repr self)
	val set_sort_indicator_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_treeviewcolumn_set_sort_indicator")
	val set_sort_indicator : 'a t -> bool -> unit
	    = fn self => fn setting => set_sort_indicator_ (repr self) setting
	val get_sort_indicator_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_treeviewcolumn_get_sort_indicator")
	val get_sort_indicator : 'a t -> bool
	    = fn self => get_sort_indicator_ (repr self)
	val set_sort_order_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_treeviewcolumn_set_sort_order")
	val set_sort_order : 'a t -> sorttype -> unit
	    = fn self => fn order => set_sort_order_ (repr self) order
	val get_sort_order_ : cptr -> int
	    = app1 (symb"mgtk_gtk_treeviewcolumn_get_sort_order")
	val get_sort_order : 'a t -> sorttype
	    = fn self => get_sort_order_ (repr self)
	val cell_set_cell_data_ : cptr -> cptr -> cptr -> bool -> bool -> unit
	    = app5 (symb"mgtk_gtk_treeviewcolumn_cell_set_cell_data")
	val cell_set_cell_data
	  : 'a t -> 'b TreeModel.t -> treeiter -> bool -> bool -> unit
	    = fn self => fn tree_model => fn iter => fn is_expander => 
	      fn is_expanded =>
		 cell_set_cell_data_ (repr self) (repr tree_model) iter
				     is_expander is_expanded
	val cell_is_visible_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_treeviewcolumn_cell_is_visible")
	val cell_is_visible : 'a t -> bool
	    = fn self => cell_is_visible_ (repr self)
	local open Signal
	      infixr -->
	in val clicked_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "clicked" false (void --> return_void) f
	end
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
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a separator_t = unit
	type 'a t = 'a separator_t Widget.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Widget.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toSeparator obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_separator_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val menu_item_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_separator_menu_item_get_type")
	val menu_item_get_type : unit -> GType.t
	    = fn dummy => menu_item_get_type_ dummy
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a vseparator_t = unit
	type 'a t = 'a vseparator_t Separator.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Separator.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toVSeparator obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_vseparator_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_vseparator_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a hseparator_t = unit
	type 'a t = 'a hseparator_t Separator.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Separator.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toHSeparator obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_hseparator_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_hseparator_new")
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a ruler_t = unit
	type 'a t = 'a ruler_t Widget.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Widget.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toRuler obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_ruler_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_metric_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_ruler_set_metric")
	val set_metric : 'a t -> metrictype -> unit
	    = fn self => fn metric => set_metric_ (repr self) metric
	val set_range_ : cptr -> real -> real -> real -> real -> unit
	    = app5 (symb"mgtk_gtk_ruler_set_range")
	val set_range : 'a t -> real -> real -> real -> real -> unit
	    = fn self => fn lower => fn upper => fn position => fn max_size =>
		 set_range_ (repr self) lower upper position max_size
	val draw_ticks_ : cptr -> unit = app1 (symb"mgtk_gtk_ruler_draw_ticks")
	val draw_ticks : 'a t -> unit = fn self => draw_ticks_ (repr self)
	val draw_pos_ : cptr -> unit = app1 (symb"mgtk_gtk_ruler_draw_pos")
	val draw_pos : 'a t -> unit = fn self => draw_pos_ (repr self)
	val get_metric_ : cptr -> int = app1 (symb"mgtk_gtk_ruler_get_metric")
	val get_metric : 'a t -> metrictype
	    = fn self => get_metric_ (repr self)
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a vruler_t = unit
	type 'a t = 'a vruler_t Ruler.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Ruler.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toVRuler obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_vruler_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_vruler_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a hruler_t = unit
	type 'a t = 'a hruler_t Ruler.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Ruler.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toHRuler obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_hruler_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_hruler_new")
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a range_t = unit
	type 'a t = 'a range_t Widget.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Widget.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toRange obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_range_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_update_policy_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_range_set_update_policy")
	val set_update_policy : 'a t -> updatetype -> unit
	    = fn self => fn policy => set_update_policy_ (repr self) policy
	val get_update_policy_ : cptr -> int
	    = app1 (symb"mgtk_gtk_range_get_update_policy")
	val get_update_policy : 'a t -> updatetype
	    = fn self => get_update_policy_ (repr self)
	val set_adjustment_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_range_set_adjustment")
	val set_adjustment : 'a t -> 'b Adjustment.t -> unit
	    = fn self => fn adjustment =>
		 set_adjustment_ (repr self) (repr adjustment)
	val get_adjustment_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_range_get_adjustment")
	val get_adjustment : 'a t -> base Adjustment.t
	    = fn self => Adjustment.inherit
			   () (fn () => get_adjustment_ (repr self))
	val set_inverted_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_range_set_inverted")
	val set_inverted : 'a t -> bool -> unit
	    = fn self => fn setting => set_inverted_ (repr self) setting
	val get_inverted_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_range_get_inverted")
	val get_inverted : 'a t -> bool = fn self => get_inverted_ (repr self)
	val set_increments_ : cptr -> real -> real -> unit
	    = app3 (symb"mgtk_gtk_range_set_increments")
	val set_increments : 'a t -> real -> real -> unit
	    = fn self => fn step => fn page =>
		 set_increments_ (repr self) step page
	val set_range_ : cptr -> real -> real -> unit
	    = app3 (symb"mgtk_gtk_range_set_range")
	val set_range : 'a t -> real -> real -> unit
	    = fn self => fn min => fn max => set_range_ (repr self) min max
	val set_value_ : cptr -> real -> unit
	    = app2 (symb"mgtk_gtk_range_set_value")
	val set_value : 'a t -> real -> unit
	    = fn self => fn value => set_value_ (repr self) value
	val get_value_ : cptr -> real = app1 (symb"mgtk_gtk_range_get_value")
	val get_value : 'a t -> real = fn self => get_value_ (repr self)
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
    structure Scrollbar :>
      sig
	type base
	type 'a scrollbar_t
	type 'a t = 'a scrollbar_t Range.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toScrollbar : 'a t -> base t
	val get_type : unit -> GType.t
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a scrollbar_t = unit
	type 'a t = 'a scrollbar_t Range.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Range.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toScrollbar obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_scrollbar_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
    end
    structure VScrollbar :>
      sig
	type base
	type 'a vscrollbar_t
	type 'a t = 'a vscrollbar_t Scrollbar.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toVScrollbar : 'a t -> base t
	val get_type : unit -> GType.t
	val new : 'a Adjustment.t option -> base t
	val new' : unit -> base t
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a vscrollbar_t = unit
	type 'a t = 'a vscrollbar_t Scrollbar.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Scrollbar.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toVScrollbar obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_vscrollbar_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr -> cptr = app1 (symb"mgtk_gtk_vscrollbar_new")
	val new : 'a Adjustment.t option -> base t
	    = fn adjustment => make (new_ (getOpt (Option.map repr adjustment, 
						   GObject.null)))
	val new' : unit -> base t = fn dummy => make (new_ GObject.null)
    end
    structure HScrollbar :>
      sig
	type base
	type 'a hscrollbar_t
	type 'a t = 'a hscrollbar_t Scrollbar.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toHScrollbar : 'a t -> base t
	val get_type : unit -> GType.t
	val new : 'a Adjustment.t option -> base t
	val new' : unit -> base t
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a hscrollbar_t = unit
	type 'a t = 'a hscrollbar_t Scrollbar.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Scrollbar.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toHScrollbar obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_hscrollbar_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr -> cptr = app1 (symb"mgtk_gtk_hscrollbar_new")
	val new : 'a Adjustment.t option -> base t
	    = fn adjustment => make (new_ (getOpt (Option.map repr adjustment, 
						   GObject.null)))
	val new' : unit -> base t = fn dummy => make (new_ GObject.null)
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
	val format_value_sig : (real -> char) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a scale_t = unit
	type 'a t = 'a scale_t Range.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Range.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toScale obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_scale_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_digits_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_scale_set_digits")
	val set_digits : 'a t -> int -> unit
	    = fn self => fn digits => set_digits_ (repr self) digits
	val get_digits_ : cptr -> int = app1 (symb"mgtk_gtk_scale_get_digits")
	val get_digits : 'a t -> int = fn self => get_digits_ (repr self)
	val set_draw_value_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_scale_set_draw_value")
	val set_draw_value : 'a t -> bool -> unit
	    = fn self => fn draw_value =>
		 set_draw_value_ (repr self) draw_value
	val get_draw_value_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_scale_get_draw_value")
	val get_draw_value : 'a t -> bool
	    = fn self => get_draw_value_ (repr self)
	val set_value_pos_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_scale_set_value_pos")
	val set_value_pos : 'a t -> positiontype -> unit
	    = fn self => fn pos => set_value_pos_ (repr self) pos
	val get_value_pos_ : cptr -> int
	    = app1 (symb"mgtk_gtk_scale_get_value_pos")
	val get_value_pos : 'a t -> positiontype
	    = fn self => get_value_pos_ (repr self)
	local open Signal
	      infixr -->
	in val format_value_sig : (real -> char) -> 'a t Signal.signal
	       = fn f => signal "format-value" false (real --> return_char) f
	end
    end
    structure VScale :>
      sig
	type base
	type 'a vscale_t
	type 'a t = 'a vscale_t Scale.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toVScale : 'a t -> base t
	val get_type : unit -> GType.t
	val new : 'a Adjustment.t option -> base t
	val new' : unit -> base t
	val new_with_range : real -> real -> real -> base t
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a vscale_t = unit
	type 'a t = 'a vscale_t Scale.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Scale.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toVScale obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_vscale_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr -> cptr = app1 (symb"mgtk_gtk_vscale_new")
	val new : 'a Adjustment.t option -> base t
	    = fn adjustment => make (new_ (getOpt (Option.map repr adjustment, 
						   GObject.null)))
	val new' : unit -> base t = fn dummy => make (new_ GObject.null)
	val new_with_range_ : real -> real -> real -> cptr
	    = app3 (symb"mgtk_gtk_vscale_new_with_range")
	val new_with_range : real -> real -> real -> base t
	    = fn min => fn max => fn step =>
		 make (new_with_range_ min max step)
    end
    structure HScale :>
      sig
	type base
	type 'a hscale_t
	type 'a t = 'a hscale_t Scale.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toHScale : 'a t -> base t
	val get_type : unit -> GType.t
	val new : 'a Adjustment.t option -> base t
	val new' : unit -> base t
	val new_with_range : real -> real -> real -> base t
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a hscale_t = unit
	type 'a t = 'a hscale_t Scale.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Scale.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toHScale obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_hscale_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr -> cptr = app1 (symb"mgtk_gtk_hscale_new")
	val new : 'a Adjustment.t option -> base t
	    = fn adjustment => make (new_ (getOpt (Option.map repr adjustment, 
						   GObject.null)))
	val new' : unit -> base t = fn dummy => make (new_ GObject.null)
	val new_with_range_ : real -> real -> real -> cptr
	    = app3 (symb"mgtk_gtk_hscale_new_with_range")
	val new_with_range : real -> real -> real -> base t
	    = fn min => fn max => fn step =>
		 make (new_with_range_ min max step)
    end
    structure Progress :>
      sig
	type base
	type 'a progress_t
	type 'a t = 'a progress_t Widget.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toProgress : 'a t -> base t
	val get_type : unit -> GType.t
	val set_show_text : 'a t -> bool -> unit
	val set_text_alignment : 'a t -> real -> real -> unit
	val set_format_string : 'a t -> string -> unit
	val set_adjustment : 'a t -> 'b Adjustment.t -> unit
	val configure : 'a t -> real -> real -> real -> unit
	val set_percentage : 'a t -> real -> unit
	val set_value : 'a t -> real -> unit
	val get_value : 'a t -> real
	val set_activity_mode : 'a t -> bool -> unit
	val get_current_text : 'a t -> string
	val get_text_from_value : 'a t -> real -> string
	val get_current_percentage : 'a t -> real
	val get_percentage_from_value : 'a t -> real -> real
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a progress_t = unit
	type 'a t = 'a progress_t Widget.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Widget.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toProgress obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_progress_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_show_text_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_progress_set_show_text")
	val set_show_text : 'a t -> bool -> unit
	    = fn self => fn show_text => set_show_text_ (repr self) show_text
	val set_text_alignment_ : cptr -> real -> real -> unit
	    = app3 (symb"mgtk_gtk_progress_set_text_alignment")
	val set_text_alignment : 'a t -> real -> real -> unit
	    = fn self => fn x_align => fn y_align =>
		 set_text_alignment_ (repr self) x_align y_align
	val set_format_string_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_progress_set_format_string")
	val set_format_string : 'a t -> string -> unit
	    = fn self => fn format => set_format_string_ (repr self) format
	val set_adjustment_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_progress_set_adjustment")
	val set_adjustment : 'a t -> 'b Adjustment.t -> unit
	    = fn self => fn adjustment =>
		 set_adjustment_ (repr self) (repr adjustment)
	val configure_ : cptr -> real -> real -> real -> unit
	    = app4 (symb"mgtk_gtk_progress_configure")
	val configure : 'a t -> real -> real -> real -> unit
	    = fn self => fn value => fn min => fn max =>
		 configure_ (repr self) value min max
	val set_percentage_ : cptr -> real -> unit
	    = app2 (symb"mgtk_gtk_progress_set_percentage")
	val set_percentage : 'a t -> real -> unit
	    = fn self => fn percentage =>
		 set_percentage_ (repr self) percentage
	val set_value_ : cptr -> real -> unit
	    = app2 (symb"mgtk_gtk_progress_set_value")
	val set_value : 'a t -> real -> unit
	    = fn self => fn value => set_value_ (repr self) value
	val get_value_ : cptr -> real
	    = app1 (symb"mgtk_gtk_progress_get_value")
	val get_value : 'a t -> real = fn self => get_value_ (repr self)
	val set_activity_mode_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_progress_set_activity_mode")
	val set_activity_mode : 'a t -> bool -> unit
	    = fn self => fn activity_mode =>
		 set_activity_mode_ (repr self) activity_mode
	val get_current_text_ : cptr -> string
	    = app1 (symb"mgtk_gtk_progress_get_current_text")
	val get_current_text : 'a t -> string
	    = fn self => get_current_text_ (repr self)
	val get_text_from_value_ : cptr -> real -> string
	    = app2 (symb"mgtk_gtk_progress_get_text_from_value")
	val get_text_from_value : 'a t -> real -> string
	    = fn self => fn value => get_text_from_value_ (repr self) value
	val get_current_percentage_ : cptr -> real
	    = app1 (symb"mgtk_gtk_progress_get_current_percentage")
	val get_current_percentage : 'a t -> real
	    = fn self => get_current_percentage_ (repr self)
	val get_percentage_from_value_ : cptr -> real -> real
	    = app2 (symb"mgtk_gtk_progress_get_percentage_from_value")
	val get_percentage_from_value : 'a t -> real -> real
	    = fn self => fn value =>
		 get_percentage_from_value_ (repr self) value
    end
    structure ProgressBar :>
      sig
	type base
	type 'a progressbar_t
	type 'a t = 'a progressbar_t Progress.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toProgressBar : 'a t -> base t
	type style
	val PROGRESS_CONTINUOUS : style
	val PROGRESS_DISCRETE : style
	type orientation
	val PROGRESS_LEFT_TO_RIGHT : orientation
	val PROGRESS_RIGHT_TO_LEFT : orientation
	val PROGRESS_BOTTOM_TO_TOP : orientation
	val PROGRESS_TOP_TO_BOTTOM : orientation
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
	val new_with_adjustment : 'a Adjustment.t option -> base t
	val new_with_adjustment' : unit -> base t
	val set_bar_style : 'a t -> style -> unit
	val set_discrete_blocks : 'a t -> int -> unit
	val set_activity_step : 'a t -> int -> unit
	val set_activity_blocks : 'a t -> int -> unit
	val update : 'a t -> real -> unit
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a progressbar_t = unit
	type 'a t = 'a progressbar_t Progress.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Progress.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toProgressBar obj = inherit () (fn () => repr obj)
	type style = int
	val get_style_ : unit -> int * int
	    = app1 (symb"mgtk_get_gtk_progressbar_style")
	val (PROGRESS_CONTINUOUS, PROGRESS_DISCRETE) = get_style_ ()
	type orientation = int
	val get_orientation_ : unit -> int * int * int * int
	    = app1 (symb"mgtk_get_gtk_progressbar_orientation")
	val (PROGRESS_LEFT_TO_RIGHT, PROGRESS_RIGHT_TO_LEFT, 
	     PROGRESS_BOTTOM_TO_TOP, PROGRESS_TOP_TO_BOTTOM)
	    = get_orientation_ ()
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_progressbar_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_progressbar_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val pulse_ : cptr -> unit = app1 (symb"mgtk_gtk_progressbar_pulse")
	val pulse : 'a t -> unit = fn self => pulse_ (repr self)
	val set_text_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_progressbar_set_text")
	val set_text : 'a t -> string -> unit
	    = fn self => fn text => set_text_ (repr self) text
	val set_fraction_ : cptr -> real -> unit
	    = app2 (symb"mgtk_gtk_progressbar_set_fraction")
	val set_fraction : 'a t -> real -> unit
	    = fn self => fn fraction => set_fraction_ (repr self) fraction
	val set_pulse_step_ : cptr -> real -> unit
	    = app2 (symb"mgtk_gtk_progressbar_set_pulse_step")
	val set_pulse_step : 'a t -> real -> unit
	    = fn self => fn fraction => set_pulse_step_ (repr self) fraction
	val set_orientation_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_progressbar_set_orientation")
	val set_orientation : 'a t -> orientation -> unit
	    = fn self => fn orientation =>
		 set_orientation_ (repr self) orientation
	val get_text_ : cptr -> string
	    = app1 (symb"mgtk_gtk_progressbar_get_text")
	val get_text : 'a t -> string = fn self => get_text_ (repr self)
	val get_fraction_ : cptr -> real
	    = app1 (symb"mgtk_gtk_progressbar_get_fraction")
	val get_fraction : 'a t -> real = fn self => get_fraction_ (repr self)
	val get_pulse_step_ : cptr -> real
	    = app1 (symb"mgtk_gtk_progressbar_get_pulse_step")
	val get_pulse_step : 'a t -> real
	    = fn self => get_pulse_step_ (repr self)
	val get_orientation_ : cptr -> int
	    = app1 (symb"mgtk_gtk_progressbar_get_orientation")
	val get_orientation : 'a t -> orientation
	    = fn self => get_orientation_ (repr self)
	val new_with_adjustment_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_progressbar_new_with_adjustment")
	val new_with_adjustment : 'a Adjustment.t option -> base t
	    = fn adjustment => make (new_with_adjustment_
				       (getOpt (Option.map repr adjustment, 
						GObject.null)))
	val new_with_adjustment' : unit -> base t
	    = fn dummy => make (new_with_adjustment_ GObject.null)
	val set_bar_style_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_progressbar_set_bar_style")
	val set_bar_style : 'a t -> style -> unit
	    = fn self => fn style => set_bar_style_ (repr self) style
	val set_discrete_blocks_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_progressbar_set_discrete_blocks")
	val set_discrete_blocks : 'a t -> int -> unit
	    = fn self => fn blocks => set_discrete_blocks_ (repr self) blocks
	val set_activity_step_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_progressbar_set_activity_step")
	val set_activity_step : 'a t -> int -> unit
	    = fn self => fn step => set_activity_step_ (repr self) step
	val set_activity_blocks_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_progressbar_set_activity_blocks")
	val set_activity_blocks : 'a t -> int -> unit
	    = fn self => fn blocks => set_activity_blocks_ (repr self) blocks
	val update_ : cptr -> real -> unit
	    = app2 (symb"mgtk_gtk_progressbar_update")
	val update : 'a t -> real -> unit
	    = fn self => fn percentage => update_ (repr self) percentage
    end
    structure Preview :>
      sig
	type base
	type 'a preview_t
	type 'a t = 'a preview_t Widget.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toPreview : 'a t -> base t
	val get_type : unit -> GType.t
	val uninit : unit -> unit
	val new : previewtype -> base t
	val size : 'a t -> int -> int -> unit
	val set_expand : 'a t -> bool -> unit
	val set_gamma : real -> unit
	val set_color_cube : int -> int -> int -> int -> unit
	val set_install_cmap : int -> unit
	val set_reserved : int -> unit
	val reset : unit -> unit
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a preview_t = unit
	type 'a t = 'a preview_t Widget.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Widget.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toPreview obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_preview_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val uninit_ : unit -> unit = app1 (symb"mgtk_gtk_preview_uninit")
	val uninit : unit -> unit = fn dummy => uninit_ dummy
	val new_ : int -> cptr = app1 (symb"mgtk_gtk_preview_new")
	val new : previewtype -> base t = fn typ => make (new_ typ)
	val size_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_preview_size")
	val size : 'a t -> int -> int -> unit
	    = fn self => fn width => fn height =>
		 size_ (repr self) width height
	val set_expand_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_preview_set_expand")
	val set_expand : 'a t -> bool -> unit
	    = fn self => fn expand => set_expand_ (repr self) expand
	val set_gamma_ : real -> unit = app1 (symb"mgtk_gtk_preview_set_gamma")
	val set_gamma : real -> unit = fn gamma => set_gamma_ gamma
	val set_color_cube_ : int -> int -> int -> int -> unit
	    = app4 (symb"mgtk_gtk_preview_set_color_cube")
	val set_color_cube : int -> int -> int -> int -> unit
	    = fn nred_shades => fn ngreen_shades => fn nblue_shades => 
	      fn ngray_shades =>
		 set_color_cube_
		   nred_shades ngreen_shades nblue_shades ngray_shades
	val set_install_cmap_ : int -> unit
	    = app1 (symb"mgtk_gtk_preview_set_install_cmap")
	val set_install_cmap : int -> unit
	    = fn install_cmap => set_install_cmap_ install_cmap
	val set_reserved_ : int -> unit
	    = app1 (symb"mgtk_gtk_preview_set_reserved")
	val set_reserved : int -> unit
	    = fn nreserved => set_reserved_ nreserved
	val reset_ : unit -> unit = app1 (symb"mgtk_gtk_preview_reset")
	val reset : unit -> unit = fn dummy => reset_ dummy
    end
    structure OldEditable :>
      sig
	type base
	type 'a oldeditable_t
	type 'a t = 'a oldeditable_t Editable.t Widget.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toOldEditable : 'a t -> base t
	val get_type : unit -> GType.t
	val changed : 'a t -> unit
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a oldeditable_t = unit
	type 'a t = 'a oldeditable_t Editable.t Widget.t
	fun inherit w con
	  = let val con = let val ptr = con () in fn () => ptr end
		val witness = ()
		val witness = Editable.inherit witness con
	    in Widget.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toOldEditable obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_old_editable_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val changed_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_old_editable_changed")
	val changed : 'a t -> unit = fn self => changed_ (repr self)
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a misc_t = unit
	type 'a t = 'a misc_t Widget.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Widget.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toMisc obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_misc_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_alignment_ : cptr -> real -> real -> unit
	    = app3 (symb"mgtk_gtk_misc_set_alignment")
	val set_alignment : 'a t -> real -> real -> unit
	    = fn self => fn xalign => fn yalign =>
		 set_alignment_ (repr self) xalign yalign
	val set_padding_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_misc_set_padding")
	val set_padding : 'a t -> int -> int -> unit
	    = fn self => fn xpad => fn ypad =>
		 set_padding_ (repr self) xpad ypad
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a pixmap_t = unit
	type 'a t = 'a pixmap_t Misc.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Misc.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toPixmap obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_pixmap_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_build_insensitive_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_pixmap_set_build_insensitive")
	val set_build_insensitive : 'a t -> bool -> unit
	    = fn self => fn build => set_build_insensitive_ (repr self) build
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a arrow_t = unit
	type 'a t = 'a arrow_t Misc.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Misc.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toArrow obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_arrow_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : int -> int -> cptr = app2 (symb"mgtk_gtk_arrow_new")
	val new : arrowtype -> shadowtype -> base t
	    = fn arrow_type => fn shadow_type =>
		 make (new_ arrow_type shadow_type)
	val set_ : cptr -> int -> int -> unit = app3 (symb"mgtk_gtk_arrow_set")
	val set : 'a t -> arrowtype -> shadowtype -> unit
	    = fn self => fn arrow_type => fn shadow_type =>
		 set_ (repr self) arrow_type shadow_type
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
	val new_from_icon_set : icon_set -> icon_size -> base t
	val set_from_file : 'a t -> string option -> unit
	val set_from_file' : 'a t -> unit
	val set_from_stock : 'a t -> string -> icon_size -> unit
	val set_from_icon_set : 'a t -> icon_set -> icon_size -> unit
	val get_storagetype : 'a t -> imagetype
	val menu_item_get_type : unit -> GType.t
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a image_t = unit
	type 'a t = 'a image_t Misc.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Misc.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toImage obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_image_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_image_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_from_file_ : string -> cptr
	    = app1 (symb"mgtk_gtk_image_new_from_file")
	val new_from_file : string -> base t
	    = fn filename => make (new_from_file_ filename)
	val new_from_stock_ : string -> int -> cptr
	    = app2 (symb"mgtk_gtk_image_new_from_stock")
	val new_from_stock : string -> icon_size -> base t
	    = fn stock_id => fn size => make (new_from_stock_ stock_id size)
	val new_from_icon_set_ : cptr -> int -> cptr
	    = app2 (symb"mgtk_gtk_image_new_from_icon_set")
	val new_from_icon_set : icon_set -> icon_size -> base t
	    = fn icon_set => fn size => make (new_from_icon_set_ icon_set size)
	val set_from_file_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_image_set_from_file")
	val set_from_file : 'a t -> string option -> unit
	    = fn self => fn filename =>
		 set_from_file_ (repr self) (getOpt (filename, ""))
	val set_from_file' : 'a t -> unit
	    = fn self => set_from_file_ (repr self) ""
	val set_from_stock_ : cptr -> string -> int -> unit
	    = app3 (symb"mgtk_gtk_image_set_from_stock")
	val set_from_stock : 'a t -> string -> icon_size -> unit
	    = fn self => fn stock_id => fn size =>
		 set_from_stock_ (repr self) stock_id size
	val set_from_icon_set_ : cptr -> cptr -> int -> unit
	    = app3 (symb"mgtk_gtk_image_set_from_icon_set")
	val set_from_icon_set : 'a t -> icon_set -> icon_size -> unit
	    = fn self => fn icon_set => fn size =>
		 set_from_icon_set_ (repr self) icon_set size
	val get_storagetype_ : cptr -> int
	    = app1 (symb"mgtk_gtk_image_get_storagetype")
	val get_storagetype : 'a t -> imagetype
	    = fn self => get_storagetype_ (repr self)
	val menu_item_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_image_menu_item_get_type")
	val menu_item_get_type : unit -> GType.t
	    = fn dummy => menu_item_get_type_ dummy
    end
    structure Label :>
      sig
	type base
	type 'a label_t
	type 'a t = 'a label_t Misc.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toLabel : 'a t -> base t
	val get_type : unit -> GType.t
	val new : string option -> base t
	val new' : unit -> base t
	val new_with_mnemonic : string option -> base t
	val new_with_mnemonic' : unit -> base t
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
	val set : 'a t -> string -> unit
	val parse_uline : 'a t -> string -> int
	val move_cursor_sig : (unit -> int -> bool -> unit)
			      -> 'a t Signal.signal
	val copy_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	val populate_popup_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a label_t = unit
	type 'a t = 'a label_t Misc.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Misc.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toLabel obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_label_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : string -> cptr = app1 (symb"mgtk_gtk_label_new")
	val new : string option -> base t
	    = fn str => make (new_ (getOpt (str, "")))
	val new' : unit -> base t = fn dummy => make (new_ "")
	val new_with_mnemonic_ : string -> cptr
	    = app1 (symb"mgtk_gtk_label_new_with_mnemonic")
	val new_with_mnemonic : string option -> base t
	    = fn str => make (new_with_mnemonic_ (getOpt (str, "")))
	val new_with_mnemonic' : unit -> base t
	    = fn dummy => make (new_with_mnemonic_ "")
	val set_text_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_label_set_text")
	val set_text : 'a t -> string -> unit
	    = fn self => fn str => set_text_ (repr self) str
	val get_text_ : cptr -> string = app1 (symb"mgtk_gtk_label_get_text")
	val get_text : 'a t -> string = fn self => get_text_ (repr self)
	val set_label_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_label_set_label")
	val set_label : 'a t -> string -> unit
	    = fn self => fn str => set_label_ (repr self) str
	val get_label_ : cptr -> string = app1 (symb"mgtk_gtk_label_get_label")
	val get_label : 'a t -> string = fn self => get_label_ (repr self)
	val set_markup_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_label_set_markup")
	val set_markup : 'a t -> string -> unit
	    = fn self => fn str => set_markup_ (repr self) str
	val set_use_markup_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_label_set_use_markup")
	val set_use_markup : 'a t -> bool -> unit
	    = fn self => fn setting => set_use_markup_ (repr self) setting
	val get_use_markup_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_label_get_use_markup")
	val get_use_markup : 'a t -> bool
	    = fn self => get_use_markup_ (repr self)
	val set_use_underline_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_label_set_use_underline")
	val set_use_underline : 'a t -> bool -> unit
	    = fn self => fn setting => set_use_underline_ (repr self) setting
	val get_use_underline_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_label_get_use_underline")
	val get_use_underline : 'a t -> bool
	    = fn self => get_use_underline_ (repr self)
	val set_markup_with_mnemonic_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_label_set_markup_with_mnemonic")
	val set_markup_with_mnemonic : 'a t -> string -> unit
	    = fn self => fn str => set_markup_with_mnemonic_ (repr self) str
	val get_mnemonic_keyval_ : cptr -> int
	    = app1 (symb"mgtk_gtk_label_get_mnemonic_keyval")
	val get_mnemonic_keyval : 'a t -> int
	    = fn self => get_mnemonic_keyval_ (repr self)
	val set_mnemonic_widget_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_label_set_mnemonic_widget")
	val set_mnemonic_widget : 'a t -> 'b Widget.t -> unit
	    = fn self => fn widget =>
		 set_mnemonic_widget_ (repr self) (repr widget)
	val get_mnemonic_widget_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_label_get_mnemonic_widget")
	val get_mnemonic_widget : 'a t -> base Widget.t
	    = fn self => Widget.inherit
			   () (fn () => get_mnemonic_widget_ (repr self))
	val set_text_with_mnemonic_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_label_set_text_with_mnemonic")
	val set_text_with_mnemonic : 'a t -> string -> unit
	    = fn self => fn str => set_text_with_mnemonic_ (repr self) str
	val set_justify_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_label_set_justify")
	val set_justify : 'a t -> justification -> unit
	    = fn self => fn jtype => set_justify_ (repr self) jtype
	val get_justify_ : cptr -> int
	    = app1 (symb"mgtk_gtk_label_get_justify")
	val get_justify : 'a t -> justification
	    = fn self => get_justify_ (repr self)
	val set_pattern_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_label_set_pattern")
	val set_pattern : 'a t -> string -> unit
	    = fn self => fn pattern => set_pattern_ (repr self) pattern
	val set_line_wrap_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_label_set_line_wrap")
	val set_line_wrap : 'a t -> bool -> unit
	    = fn self => fn wrap => set_line_wrap_ (repr self) wrap
	val get_line_wrap_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_label_get_line_wrap")
	val get_line_wrap : 'a t -> bool
	    = fn self => get_line_wrap_ (repr self)
	val set_selectable_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_label_set_selectable")
	val set_selectable : 'a t -> bool -> unit
	    = fn self => fn setting => set_selectable_ (repr self) setting
	val get_selectable_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_label_get_selectable")
	val get_selectable : 'a t -> bool
	    = fn self => get_selectable_ (repr self)
	val select_region_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_label_select_region")
	val select_region : 'a t -> int -> int -> unit
	    = fn self => fn start_offset => fn end_offset =>
		 select_region_ (repr self) start_offset end_offset
	val set_ : cptr -> string -> unit = app2 (symb"mgtk_gtk_label_set")
	val set : 'a t -> string -> unit
	    = fn self => fn str => set_ (repr self) str
	val parse_uline_ : cptr -> string -> int
	    = app2 (symb"mgtk_gtk_label_parse_uline")
	val parse_uline : 'a t -> string -> int
	    = fn self => fn string => parse_uline_ (repr self) string
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
	val accelerator_width : 'a t -> int
	val get_accel_widget : 'a t -> base Widget.t
	val get_accel_width : 'a t -> int
	val set_accel_widget : 'a t -> 'b Widget.t -> unit
	val refetch : 'a t -> bool
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a accellabel_t = unit
	type 'a t = 'a accellabel_t Label.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Label.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toAccelLabel obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_accel_label_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : string -> cptr = app1 (symb"mgtk_gtk_accel_label_new")
	val new : string -> base t = fn string => make (new_ string)
	val accelerator_width_ : cptr -> int
	    = app1 (symb"mgtk_gtk_accel_label_accelerator_width")
	val accelerator_width : 'a t -> int
	    = fn self => accelerator_width_ (repr self)
	val get_accel_widget_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_accel_label_get_accel_widget")
	val get_accel_widget : 'a t -> base Widget.t
	    = fn self => Widget.inherit
			   () (fn () => get_accel_widget_ (repr self))
	val get_accel_width_ : cptr -> int
	    = app1 (symb"mgtk_gtk_accel_label_get_accel_width")
	val get_accel_width : 'a t -> int
	    = fn self => get_accel_width_ (repr self)
	val set_accel_widget_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_accel_label_set_accel_widget")
	val set_accel_widget : 'a t -> 'b Widget.t -> unit
	    = fn self => fn accel_widget =>
		 set_accel_widget_ (repr self) (repr accel_widget)
	val refetch_ : cptr -> bool = app1 (symb"mgtk_gtk_accel_label_refetch")
	val refetch : 'a t -> bool = fn self => refetch_ (repr self)
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a invisible_t = unit
	type 'a t = 'a invisible_t Widget.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Widget.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toInvisible obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_invisible_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_invisible_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure Entry :>
      sig
	type base
	type 'a entry_t
	type 'a t = 'a entry_t Editable.t CellEditable.t Widget.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toEntry : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val new_with_max_length : int option -> base t
	val new_with_max_length' : unit -> base t
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
	val append_text : 'a t -> string -> unit
	val prepend_text : 'a t -> string -> unit
	val move_cursor_sig : (unit -> int -> bool -> unit)
			      -> 'a t Signal.signal
	val insert_at_cursor_sig : (char -> unit) -> 'a t Signal.signal
	val delete_from_cursor_sig
	  : (unit -> int -> unit) -> 'a t Signal.signal
	val cut_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	val copy_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	val paste_clipboard_sig : (unit -> unit) -> 'a t Signal.signal
	val toggle_overwrite_sig : (unit -> unit) -> 'a t Signal.signal
	val populate_popup_sig : (unit -> unit) -> 'a t Signal.signal
	val activate_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a entry_t = unit
	type 'a t = 'a entry_t Editable.t CellEditable.t Widget.t
	fun inherit w con
	  = let val con = let val ptr = con () in fn () => ptr end
		val witness = ()
		val witness = Editable.inherit witness con
		val witness = CellEditable.inherit witness con
	    in Widget.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toEntry obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_entry_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_entry_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_max_length_ : int -> cptr
	    = app1 (symb"mgtk_gtk_entry_new_with_max_length")
	val new_with_max_length : int option -> base t
	    = fn max => make (new_with_max_length_ (getOpt (max, 0)))
	val new_with_max_length' : unit -> base t
	    = fn dummy => make (new_with_max_length_ 0)
	val set_visibility_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_entry_set_visibility")
	val set_visibility : 'a t -> bool -> unit
	    = fn self => fn visible => set_visibility_ (repr self) visible
	val get_visibility_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_entry_get_visibility")
	val get_visibility : 'a t -> bool
	    = fn self => get_visibility_ (repr self)
	val set_invisible_char_ : cptr -> char -> unit
	    = app2 (symb"mgtk_gtk_entry_set_invisible_char")
	val set_invisible_char : 'a t -> char -> unit
	    = fn self => fn ch => set_invisible_char_ (repr self) ch
	val get_invisible_char_ : cptr -> char
	    = app1 (symb"mgtk_gtk_entry_get_invisible_char")
	val get_invisible_char : 'a t -> char
	    = fn self => get_invisible_char_ (repr self)
	val set_has_frame_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_entry_set_has_frame")
	val set_has_frame : 'a t -> bool -> unit
	    = fn self => fn setting => set_has_frame_ (repr self) setting
	val get_has_frame_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_entry_get_has_frame")
	val get_has_frame : 'a t -> bool
	    = fn self => get_has_frame_ (repr self)
	val set_max_length_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_entry_set_max_length")
	val set_max_length : 'a t -> int -> unit
	    = fn self => fn max => set_max_length_ (repr self) max
	val get_max_length_ : cptr -> int
	    = app1 (symb"mgtk_gtk_entry_get_max_length")
	val get_max_length : 'a t -> int
	    = fn self => get_max_length_ (repr self)
	val set_activates_default_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_entry_set_activates_default")
	val set_activates_default : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 set_activates_default_ (repr self) setting
	val get_activates_default_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_entry_get_activates_default")
	val get_activates_default : 'a t -> bool
	    = fn self => get_activates_default_ (repr self)
	val set_width_chars_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_entry_set_width_chars")
	val set_width_chars : 'a t -> int -> unit
	    = fn self => fn n_chars => set_width_chars_ (repr self) n_chars
	val get_width_chars_ : cptr -> int
	    = app1 (symb"mgtk_gtk_entry_get_width_chars")
	val get_width_chars : 'a t -> int
	    = fn self => get_width_chars_ (repr self)
	val set_text_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_entry_set_text")
	val set_text : 'a t -> string -> unit
	    = fn self => fn text => set_text_ (repr self) text
	val get_text_ : cptr -> string = app1 (symb"mgtk_gtk_entry_get_text")
	val get_text : 'a t -> string = fn self => get_text_ (repr self)
	val append_text_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_entry_append_text")
	val append_text : 'a t -> string -> unit
	    = fn self => fn text => append_text_ (repr self) text
	val prepend_text_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_entry_prepend_text")
	val prepend_text : 'a t -> string -> unit
	    = fn self => fn text => prepend_text_ (repr self) text
	local open Signal
	      infixr -->
	in
	  val move_cursor_sig : (unit -> int -> bool -> unit)
				-> 'a t Signal.signal
	      = fn f => signal "move-cursor" false
			       (unit --> int --> bool --> return_void) f
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
	  val activate_sig : (unit -> unit) -> 'a t Signal.signal
	      = fn f => signal "activate" false (void --> return_void) f
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
	val new : 'a Adjustment.t option -> real option -> int option -> base t
	val new' : unit -> base t
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a spinbutton_t = unit
	type 'a t = 'a spinbutton_t Entry.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Entry.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toSpinButton obj = inherit () (fn () => repr obj)
	type update_policy = int
	val get_update_policy_ : unit -> int * int
	    = app1 (symb"mgtk_get_gtk_spin_button_update_policy")
	val (UPDATE_ALWAYS, UPDATE_IF_VALID) = get_update_policy_ ()
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_spin_button_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val configure_ : cptr -> cptr -> real -> int -> unit
	    = app4 (symb"mgtk_gtk_spin_button_configure")
	val configure : 'a t -> 'b Adjustment.t option -> real -> int -> unit
	    = fn self => fn adjustment => fn climb_rate => fn digits =>
		 configure_ (repr self)
			    (getOpt (Option.map repr adjustment, GObject.null))
			    climb_rate digits
	val configure' : 'a t -> real -> int -> unit
	    = fn self => fn climb_rate => fn digits =>
		 configure_ (repr self) GObject.null climb_rate digits
	val new_ : cptr -> real -> int -> cptr
	    = app3 (symb"mgtk_gtk_spin_button_new")
	val new : 'a Adjustment.t option -> real option -> int option -> base t
	    = fn adjustment => fn climb_rate => fn digits =>
		 make (new_ (getOpt (Option.map repr adjustment, GObject.null))
			    (getOpt (climb_rate, 0.0)) (getOpt (digits, 0)))
	val new' : unit -> base t = fn dummy => make (new_ GObject.null 0.0 0)
	val new_with_range_ : real -> real -> real -> cptr
	    = app3 (symb"mgtk_gtk_spin_button_new_with_range")
	val new_with_range : real -> real -> real -> base t
	    = fn min => fn max => fn step =>
		 make (new_with_range_ min max step)
	val set_adjustment_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_spin_button_set_adjustment")
	val set_adjustment : 'a t -> 'b Adjustment.t -> unit
	    = fn self => fn adjustment =>
		 set_adjustment_ (repr self) (repr adjustment)
	val get_adjustment_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_spin_button_get_adjustment")
	val get_adjustment : 'a t -> base Adjustment.t
	    = fn self => Adjustment.inherit
			   () (fn () => get_adjustment_ (repr self))
	val set_digits_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_spin_button_set_digits")
	val set_digits : 'a t -> int -> unit
	    = fn self => fn digits => set_digits_ (repr self) digits
	val get_digits_ : cptr -> int
	    = app1 (symb"mgtk_gtk_spin_button_get_digits")
	val get_digits : 'a t -> int = fn self => get_digits_ (repr self)
	val set_increments_ : cptr -> real -> real -> unit
	    = app3 (symb"mgtk_gtk_spin_button_set_increments")
	val set_increments : 'a t -> real -> real -> unit
	    = fn self => fn step => fn page =>
		 set_increments_ (repr self) step page
	val set_range_ : cptr -> real -> real -> unit
	    = app3 (symb"mgtk_gtk_spin_button_set_range")
	val set_range : 'a t -> real -> real -> unit
	    = fn self => fn min => fn max => set_range_ (repr self) min max
	val get_value_ : cptr -> real
	    = app1 (symb"mgtk_gtk_spin_button_get_value")
	val get_value : 'a t -> real = fn self => get_value_ (repr self)
	val get_value_as_int_ : cptr -> int
	    = app1 (symb"mgtk_gtk_spin_button_get_value_as_int")
	val get_value_as_int : 'a t -> int
	    = fn self => get_value_as_int_ (repr self)
	val set_value_ : cptr -> real -> unit
	    = app2 (symb"mgtk_gtk_spin_button_set_value")
	val set_value : 'a t -> real -> unit
	    = fn self => fn value => set_value_ (repr self) value
	val set_update_policy_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_spin_button_set_update_policy")
	val set_update_policy : 'a t -> update_policy -> unit
	    = fn self => fn policy => set_update_policy_ (repr self) policy
	val get_update_policy_ : cptr -> int
	    = app1 (symb"mgtk_gtk_spin_button_get_update_policy")
	val get_update_policy : 'a t -> int
	    = fn self => get_update_policy_ (repr self)
	val set_numeric_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_spin_button_set_numeric")
	val set_numeric : 'a t -> bool -> unit
	    = fn self => fn numeric => set_numeric_ (repr self) numeric
	val get_numeric_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_spin_button_get_numeric")
	val get_numeric : 'a t -> bool = fn self => get_numeric_ (repr self)
	val spin_ : cptr -> int -> real -> unit
	    = app3 (symb"mgtk_gtk_spin_button_spin")
	val spin : 'a t -> spintype -> real -> unit
	    = fn self => fn direction => fn increment =>
		 spin_ (repr self) direction increment
	val set_wrap_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_spin_button_set_wrap")
	val set_wrap : 'a t -> bool -> unit
	    = fn self => fn wrap => set_wrap_ (repr self) wrap
	val get_wrap_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_spin_button_get_wrap")
	val get_wrap : 'a t -> bool = fn self => get_wrap_ (repr self)
	val set_snap_to_ticks_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_spin_button_set_snap_to_ticks")
	val set_snap_to_ticks : 'a t -> bool -> unit
	    = fn self => fn snap_to_ticks =>
		 set_snap_to_ticks_ (repr self) snap_to_ticks
	val get_snap_to_ticks_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_spin_button_get_snap_to_ticks")
	val get_snap_to_ticks : 'a t -> bool
	    = fn self => get_snap_to_ticks_ (repr self)
	val update_ : cptr -> unit = app1 (symb"mgtk_gtk_spin_button_update")
	val update : 'a t -> unit = fn self => update_ (repr self)
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
    structure DrawingArea :>
      sig
	type base
	type 'a drawingarea_t
	type 'a t = 'a drawingarea_t Widget.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toDrawingArea : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val size : 'a t -> int -> int -> unit
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a drawingarea_t = unit
	type 'a t = 'a drawingarea_t Widget.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Widget.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toDrawingArea obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_drawing_area_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_drawing_area_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val size_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_drawing_area_size")
	val size : 'a t -> int -> int -> unit
	    = fn self => fn width => fn height =>
		 size_ (repr self) width height
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a curve_t = unit
	type 'a t = 'a curve_t DrawingArea.t
	fun inherit w con
	  = let val con = let val ptr = con () in fn () => ptr end
		val witness = ()
	    in DrawingArea.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toCurve obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_curve_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_curve_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val reset_ : cptr -> unit = app1 (symb"mgtk_gtk_curve_reset")
	val reset : 'a t -> unit = fn self => reset_ (repr self)
	val set_gamma_ : cptr -> real -> unit
	    = app2 (symb"mgtk_gtk_curve_set_gamma")
	val set_gamma : 'a t -> real -> unit
	    = fn self => fn gamma => set_gamma_ (repr self) gamma
	val set_range_ : cptr -> real -> real -> real -> real -> unit
	    = app5 (symb"mgtk_gtk_curve_set_range")
	val set_range : 'a t -> real -> real -> real -> real -> unit
	    = fn self => fn min_x => fn max_x => fn min_y => fn max_y =>
		 set_range_ (repr self) min_x max_x min_y max_y
	val set_curvetype_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_curve_set_curvetype")
	val set_curvetype : 'a t -> curvetype -> unit
	    = fn self => fn typ => set_curvetype_ (repr self) typ
	local open Signal
	      infixr -->
	in val curve_type_changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "curve-type-changed" false
			        (void --> return_void) f
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
	val add_sig : (unit -> unit) -> 'a t Signal.signal
	val remove_sig : (unit -> unit) -> 'a t Signal.signal
	val check_resize_sig : (unit -> unit) -> 'a t Signal.signal
	val set_focus_child_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a container_t = unit
	type 'a t = 'a container_t Widget.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Widget.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toContainer obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_container_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val set_border_width_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_container_set_border_width")
	val set_border_width : 'a t -> int -> unit
	    = fn self => fn border_width =>
		 set_border_width_ (repr self) border_width
	val get_border_width_ : cptr -> int
	    = app1 (symb"mgtk_gtk_container_get_border_width")
	val get_border_width : 'a t -> int
	    = fn self => get_border_width_ (repr self)
	val add_ : cptr -> cptr -> unit = app2 (symb"mgtk_gtk_container_add")
	val add : 'a t -> 'b Widget.t -> unit
	    = fn self => fn widget => add_ (repr self) (repr widget)
	val remove_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_container_remove")
	val remove : 'a t -> 'b Widget.t -> unit
	    = fn self => fn widget => remove_ (repr self) (repr widget)
	val set_resize_mode_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_container_set_resize_mode")
	val set_resize_mode : 'a t -> resize_mode -> unit
	    = fn self => fn resize_mode =>
		 set_resize_mode_ (repr self) resize_mode
	val get_resize_mode_ : cptr -> int
	    = app1 (symb"mgtk_gtk_container_get_resize_mode")
	val get_resize_mode : 'a t -> resize_mode
	    = fn self => get_resize_mode_ (repr self)
	val check_resize_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_container_check_resize")
	val check_resize : 'a t -> unit = fn self => check_resize_ (repr self)
	val unset_focus_chain_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_container_unset_focus_chain")
	val unset_focus_chain : 'a t -> unit
	    = fn self => unset_focus_chain_ (repr self)
	val set_reallocate_redraws_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_container_set_reallocate_redraws")
	val set_reallocate_redraws : 'a t -> bool -> unit
	    = fn self => fn needs_redraws =>
		 set_reallocate_redraws_ (repr self) needs_redraws
	val set_focus_child_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_container_set_focus_child")
	val set_focus_child : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child => set_focus_child_ (repr self) (repr child)
	val set_focus_vadjustment_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_container_set_focus_vadjustment")
	val set_focus_vadjustment : 'a t -> 'b Adjustment.t -> unit
	    = fn self => fn adjustment =>
		 set_focus_vadjustment_ (repr self) (repr adjustment)
	val get_focus_vadjustment_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_container_get_focus_vadjustment")
	val get_focus_vadjustment : 'a t -> base Adjustment.t
	    = fn self => Adjustment.inherit
			   () (fn () => get_focus_vadjustment_ (repr self))
	val set_focus_hadjustment_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_container_set_focus_hadjustment")
	val set_focus_hadjustment : 'a t -> 'b Adjustment.t -> unit
	    = fn self => fn adjustment =>
		 set_focus_hadjustment_ (repr self) (repr adjustment)
	val get_focus_hadjustment_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_container_get_focus_hadjustment")
	val get_focus_hadjustment : 'a t -> base Adjustment.t
	    = fn self => Adjustment.inherit
			   () (fn () => get_focus_hadjustment_ (repr self))
	val resize_children_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_container_resize_children")
	val resize_children : 'a t -> unit
	    = fn self => resize_children_ (repr self)
	val childtype_ : cptr -> GType.t
	    = app1 (symb"mgtk_gtk_container_childtype")
	val childtype : 'a t -> GType.t = fn self => childtype_ (repr self)
	val add_with_properties_ : cptr -> cptr -> string -> unit
	    = app3 (symb"mgtk_gtk_container_add_with_properties")
	val add_with_properties : 'a t -> 'b Widget.t -> string -> unit
	    = fn self => fn widget => fn first_prop_name =>
		 add_with_properties_ (repr self) (repr widget) first_prop_name
	val child_set_ : cptr -> cptr -> string -> unit
	    = app3 (symb"mgtk_gtk_container_child_set")
	val child_set : 'a t -> 'b Widget.t -> string -> unit
	    = fn self => fn child => fn first_prop_name =>
		 child_set_ (repr self) (repr child) first_prop_name
	val child_get_ : cptr -> cptr -> string -> unit
	    = app3 (symb"mgtk_gtk_container_child_get")
	val child_get : 'a t -> 'b Widget.t -> string -> unit
	    = fn self => fn child => fn first_prop_name =>
		 child_get_ (repr self) (repr child) first_prop_name
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
	val new_with_model : 'a TreeModel.t option -> base t
	val new_with_model' : unit -> base t
	val get_model : 'a t -> base TreeModel.t
	val set_model : 'a t -> 'b TreeModel.t option -> unit
	val set_model' : 'a t -> unit
	val get_selection : 'a t -> base TreeSelection.t
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
	  : 'a t -> 'b TreeViewColumn.t -> 'c TreeViewColumn.t -> unit
	val set_expander_column : 'a t -> 'b TreeViewColumn.t -> unit
	val get_expander_column : 'a t -> base TreeViewColumn.t
	val scroll_to_point : 'a t -> int -> int -> unit
	val scroll_to_cell : 'a t -> tree_path -> 'b TreeViewColumn.t option 
			  -> bool option -> real option -> real option
			     -> unit
	val scroll_to_cell' : 'a t -> tree_path -> unit
	val row_activated : 'a t -> tree_path -> 'b TreeViewColumn.t -> unit
	val expand_all : 'a t -> unit
	val collapse_all : 'a t -> unit
	val expand_row : 'a t -> tree_path -> bool -> unit
	val collapse_row : 'a t -> tree_path -> unit
	val row_expanded : 'a t -> tree_path -> bool
	val set_reorderable : 'a t -> bool -> unit
	val get_reorderable : 'a t -> bool
	val set_cursor : 'a t -> tree_path -> 'b TreeViewColumn.t option 
		      -> bool option
			 -> unit
	val set_cursor' : 'a t -> tree_path -> unit
	val get_cursor : 'a t -> tree_path * base TreeViewColumn.t
	val unset_rows_drag_source : 'a t -> unit
	val unset_rows_drag_dest : 'a t -> unit
	val set_drag_dest_row : 'a t -> tree_path -> drop_position -> unit
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a treeview_t = unit
	type 'a t = 'a treeview_t Container.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Container.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTreeView obj = inherit () (fn () => repr obj)
	type drop_position = int
	val get_drop_position_ : unit -> int * int * int * int
	    = app1 (symb"mgtk_get_gtk_treeview_drop_position")
	val (TREE_VIEW_DROP_BEFORE, TREE_VIEW_DROP_AFTER, 
	     TREE_VIEW_DROP_INTO_OR_BEFORE, TREE_VIEW_DROP_INTO_OR_AFTER)
	    = get_drop_position_ ()
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_treeview_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_treeview_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_model_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_treeview_new_with_model")
	val new_with_model : 'a TreeModel.t option -> base t
	    = fn model => make (new_with_model_
				  (getOpt (Option.map repr model, 
					   GObject.null)))
	val new_with_model' : unit -> base t
	    = fn dummy => make (new_with_model_ GObject.null)
	val get_model_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_treeview_get_model")
	val get_model : 'a t -> base TreeModel.t
	    = fn self => TreeModel.inherit () (fn () => get_model_ (repr self))
	val set_model_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_treeview_set_model")
	val set_model : 'a t -> 'b TreeModel.t option -> unit
	    = fn self => fn model =>
		 set_model_ (repr self)
			    (getOpt (Option.map repr model, GObject.null))
	val set_model' : 'a t -> unit
	    = fn self => set_model_ (repr self) GObject.null
	val get_selection_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_treeview_get_selection")
	val get_selection : 'a t -> base TreeSelection.t
	    = fn self => TreeSelection.inherit
			   () (fn () => get_selection_ (repr self))
	val get_hadjustment_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_treeview_get_hadjustment")
	val get_hadjustment : 'a t -> base Adjustment.t
	    = fn self => Adjustment.inherit
			   () (fn () => get_hadjustment_ (repr self))
	val set_hadjustment_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_treeview_set_hadjustment")
	val set_hadjustment : 'a t -> 'b Adjustment.t -> unit
	    = fn self => fn adjustment =>
		 set_hadjustment_ (repr self) (repr adjustment)
	val get_vadjustment_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_treeview_get_vadjustment")
	val get_vadjustment : 'a t -> base Adjustment.t
	    = fn self => Adjustment.inherit
			   () (fn () => get_vadjustment_ (repr self))
	val set_vadjustment_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_treeview_set_vadjustment")
	val set_vadjustment : 'a t -> 'b Adjustment.t -> unit
	    = fn self => fn adjustment =>
		 set_vadjustment_ (repr self) (repr adjustment)
	val get_headers_visible_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_treeview_get_headers_visible")
	val get_headers_visible : 'a t -> bool
	    = fn self => get_headers_visible_ (repr self)
	val set_headers_visible_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_treeview_set_headers_visible")
	val set_headers_visible : 'a t -> bool -> unit
	    = fn self => fn headers_visible =>
		 set_headers_visible_ (repr self) headers_visible
	val columns_autosize_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_treeview_columns_autosize")
	val columns_autosize : 'a t -> unit
	    = fn self => columns_autosize_ (repr self)
	val set_headers_clickable_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_treeview_set_headers_clickable")
	val set_headers_clickable : 'a t -> bool -> unit
	    = fn self => fn active => set_headers_clickable_ (repr self) active
	val set_rules_hint_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_treeview_set_rules_hint")
	val set_rules_hint : 'a t -> bool -> unit
	    = fn self => fn setting => set_rules_hint_ (repr self) setting
	val get_rules_hint_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_treeview_get_rules_hint")
	val get_rules_hint : 'a t -> bool
	    = fn self => get_rules_hint_ (repr self)
	val append_column_ : cptr -> cptr -> int
	    = app2 (symb"mgtk_gtk_treeview_append_column")
	val append_column : 'a t -> 'b TreeViewColumn.t -> int
	    = fn self => fn column => append_column_ (repr self) (repr column)
	val remove_column_ : cptr -> cptr -> int
	    = app2 (symb"mgtk_gtk_treeview_remove_column")
	val remove_column : 'a t -> 'b TreeViewColumn.t -> int
	    = fn self => fn column => remove_column_ (repr self) (repr column)
	val insert_column_ : cptr -> cptr -> int -> int
	    = app3 (symb"mgtk_gtk_treeview_insert_column")
	val insert_column : 'a t -> 'b TreeViewColumn.t -> int -> int
	    = fn self => fn column => fn position =>
		 insert_column_ (repr self) (repr column) position
	val insert_column_with_attributes_
	  : cptr -> int -> string -> cptr -> int
	    = app4 (symb"mgtk_gtk_treeview_insert_column_with_attributes")
	val insert_column_with_attributes
	  : 'a t -> int -> string -> 'b CellRenderer.t -> int
	    = fn self => fn position => fn title => fn cell =>
		 insert_column_with_attributes_
		   (repr self) position title (repr cell)
	val get_column_ : cptr -> int -> cptr
	    = app2 (symb"mgtk_gtk_treeview_get_column")
	val get_column : 'a t -> int -> base TreeViewColumn.t
	    = fn self => fn n => TreeViewColumn.inherit
				   () (fn () => get_column_ (repr self) n)
	val move_column_after_ : cptr -> cptr -> cptr -> unit
	    = app3 (symb"mgtk_gtk_treeview_move_column_after")
	val move_column_after
	  : 'a t -> 'b TreeViewColumn.t -> 'c TreeViewColumn.t -> unit
	    = fn self => fn column => fn base_column =>
		 move_column_after_ (repr self) (repr column)
				    (repr base_column)
	val set_expander_column_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_treeview_set_expander_column")
	val set_expander_column : 'a t -> 'b TreeViewColumn.t -> unit
	    = fn self => fn column =>
		 set_expander_column_ (repr self) (repr column)
	val get_expander_column_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_treeview_get_expander_column")
	val get_expander_column : 'a t -> base TreeViewColumn.t
	    = fn self => TreeViewColumn.inherit
			   () (fn () => get_expander_column_ (repr self))
	val scroll_to_point_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_treeview_scroll_to_point")
	val scroll_to_point : 'a t -> int -> int -> unit
	    = fn self => fn tree_x => fn tree_y =>
		 scroll_to_point_ (repr self) tree_x tree_y
	val scroll_to_cell_ : cptr * cptr * cptr * bool * real * real -> unit
	    = app1 (symb"mgtk_gtk_treeview_scroll_to_cell")
	val scroll_to_cell : 'a t -> tree_path -> 'b TreeViewColumn.t option 
			  -> bool option -> real option -> real option
			     -> unit
	    = fn self => fn path => fn column => fn use_align => 
	      fn row_align => fn col_align =>
		 scroll_to_cell_
		   (repr self, path, 
		    getOpt (Option.map repr column, GObject.null), 
		    getOpt (use_align, false), getOpt (row_align, 0.0), 
		    getOpt (col_align, 0.0))
	val scroll_to_cell' : 'a t -> tree_path -> unit
	    = fn self => fn path =>
		 scroll_to_cell_
		   (repr self, path, GObject.null, false, 0.0, 0.0)
	val row_activated_ : cptr -> cptr -> cptr -> unit
	    = app3 (symb"mgtk_gtk_treeview_row_activated")
	val row_activated : 'a t -> tree_path -> 'b TreeViewColumn.t -> unit
	    = fn self => fn path => fn column =>
		 row_activated_ (repr self) path (repr column)
	val expand_all_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_treeview_expand_all")
	val expand_all : 'a t -> unit = fn self => expand_all_ (repr self)
	val collapse_all_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_treeview_collapse_all")
	val collapse_all : 'a t -> unit = fn self => collapse_all_ (repr self)
	val expand_row_ : cptr -> cptr -> bool -> unit
	    = app3 (symb"mgtk_gtk_treeview_expand_row")
	val expand_row : 'a t -> tree_path -> bool -> unit
	    = fn self => fn path => fn open_all =>
		 expand_row_ (repr self) path open_all
	val collapse_row_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_treeview_collapse_row")
	val collapse_row : 'a t -> tree_path -> unit
	    = fn self => fn path => collapse_row_ (repr self) path
	val row_expanded_ : cptr -> cptr -> bool
	    = app2 (symb"mgtk_gtk_treeview_row_expanded")
	val row_expanded : 'a t -> tree_path -> bool
	    = fn self => fn path => row_expanded_ (repr self) path
	val set_reorderable_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_treeview_set_reorderable")
	val set_reorderable : 'a t -> bool -> unit
	    = fn self => fn reorderable =>
		 set_reorderable_ (repr self) reorderable
	val get_reorderable_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_treeview_get_reorderable")
	val get_reorderable : 'a t -> bool
	    = fn self => get_reorderable_ (repr self)
	val set_cursor_ : cptr -> cptr -> cptr -> bool -> unit
	    = app4 (symb"mgtk_gtk_treeview_set_cursor")
	val set_cursor : 'a t -> tree_path -> 'b TreeViewColumn.t option 
		      -> bool option
			 -> unit
	    = fn self => fn path => fn focus_column => fn start_editing =>
		 set_cursor_ (repr self) path
			     (getOpt (Option.map repr focus_column, 
				      GObject.null))
			     (getOpt (start_editing, false))
	val set_cursor' : 'a t -> tree_path -> unit
	    = fn self => fn path =>
		 set_cursor_ (repr self) path GObject.null false
	val get_cursor_ : cptr -> cptr ref -> cptr ref -> unit
	    = app3 (symb"mgtk_gtk_treeview_get_cursor")
	val get_cursor : 'a t -> tree_path * base TreeViewColumn.t
	    = fn self =>
		 let val (path, focus_column)
			 = (ref GObject.null, ref GObject.null)
		     val ret = get_cursor_ (repr self) path focus_column
		 in (!path, 
		     TreeViewColumn.inherit () (fn () => !focus_column))
		 end
	val unset_rows_drag_source_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_treeview_unset_rows_drag_source")
	val unset_rows_drag_source : 'a t -> unit
	    = fn self => unset_rows_drag_source_ (repr self)
	val unset_rows_drag_dest_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_treeview_unset_rows_drag_dest")
	val unset_rows_drag_dest : 'a t -> unit
	    = fn self => unset_rows_drag_dest_ (repr self)
	val set_drag_dest_row_ : cptr -> cptr -> int -> unit
	    = app3 (symb"mgtk_gtk_treeview_set_drag_dest_row")
	val set_drag_dest_row : 'a t -> tree_path -> drop_position -> unit
	    = fn self => fn path => fn pos =>
		 set_drag_dest_row_ (repr self) path pos
	val set_enable_search_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_treeview_set_enable_search")
	val set_enable_search : 'a t -> bool -> unit
	    = fn self => fn enable_search =>
		 set_enable_search_ (repr self) enable_search
	val get_enable_search_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_treeview_get_enable_search")
	val get_enable_search : 'a t -> bool
	    = fn self => get_enable_search_ (repr self)
	val get_search_column_ : cptr -> int
	    = app1 (symb"mgtk_gtk_treeview_get_search_column")
	val get_search_column : 'a t -> int
	    = fn self => get_search_column_ (repr self)
	val set_search_column_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_treeview_set_search_column")
	val set_search_column : 'a t -> int -> unit
	    = fn self => fn column => set_search_column_ (repr self) column
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
    structure Toolbar :>
      sig
	type base
	type 'a toolbar_t
	type 'a t = 'a toolbar_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toToolbar : 'a t -> base t
	type style
	val ICONS : style
	val TEXT : style
	val BOTH : style
	val BOTH_HORIZ : style
	type childtype
	val CHILD_SPACE : childtype
	val CHILD_BUTTON : childtype
	val CHILD_TOGGLEBUTTON : childtype
	val CHILD_RADIOBUTTON : childtype
	val CHILD_WIDGET : childtype
	type space_style
	val SPACE_EMPTY : space_style
	val SPACE_LINE : space_style
	val get_type : unit -> GType.t
	val new : unit -> base t
	val append_space : 'a t -> unit
	val prepend_space : 'a t -> unit
	val insert_space : 'a t -> int -> unit
	val remove_space : 'a t -> int -> unit
	val append_widget
	  : 'a t -> 'b Widget.t -> string option -> string option -> unit
	val append_widget' : 'a t -> 'b Widget.t -> unit
	val prepend_widget
	  : 'a t -> 'b Widget.t -> string option -> string option -> unit
	val prepend_widget' : 'a t -> 'b Widget.t -> unit
	val insert_widget : 'a t -> 'b Widget.t -> string option 
			 -> string option -> int
			    -> unit
	val insert_widget' : 'a t -> 'b Widget.t -> int -> unit
	val set_orientation : 'a t -> orientation -> unit
	val set_style : 'a t -> style -> unit
	val set_icon_size : 'a t -> icon_size -> unit
	val set_tooltips : 'a t -> bool -> unit
	val unset_style : 'a t -> unit
	val unset_icon_size : 'a t -> unit
	val get_orientation : 'a t -> orientation
	val get_style : 'a t -> style
	val get_icon_size : 'a t -> icon_size
	val get_tooltips : 'a t -> bool
	val orientation_changed_sig : (unit -> unit) -> 'a t Signal.signal
	val style_changed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a toolbar_t = unit
	type 'a t = 'a toolbar_t Container.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Container.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toToolbar obj = inherit () (fn () => repr obj)
	type style = int
	val get_style_ : unit -> int * int * int * int
	    = app1 (symb"mgtk_get_gtk_toolbar_style")
	val (ICONS, TEXT, BOTH, BOTH_HORIZ) = get_style_ ()
	type childtype = int
	val get_childtype_ : unit -> int * int * int * int * int
	    = app1 (symb"mgtk_get_gtk_toolbar_childtype")
	val (CHILD_SPACE, CHILD_BUTTON, CHILD_TOGGLEBUTTON, CHILD_RADIOBUTTON, 
	     CHILD_WIDGET)
	    = get_childtype_ ()
	type space_style = int
	val get_space_style_ : unit -> int * int
	    = app1 (symb"mgtk_get_gtk_toolbar_space_style")
	val (SPACE_EMPTY, SPACE_LINE) = get_space_style_ ()
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_toolbar_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_toolbar_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val append_space_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_toolbar_append_space")
	val append_space : 'a t -> unit = fn self => append_space_ (repr self)
	val prepend_space_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_toolbar_prepend_space")
	val prepend_space : 'a t -> unit
	    = fn self => prepend_space_ (repr self)
	val insert_space_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_toolbar_insert_space")
	val insert_space : 'a t -> int -> unit
	    = fn self => fn position => insert_space_ (repr self) position
	val remove_space_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_toolbar_remove_space")
	val remove_space : 'a t -> int -> unit
	    = fn self => fn position => remove_space_ (repr self) position
	val append_widget_ : cptr -> cptr -> string -> string -> unit
	    = app4 (symb"mgtk_gtk_toolbar_append_widget")
	val append_widget
	  : 'a t -> 'b Widget.t -> string option -> string option -> unit
	    = fn self => fn widget => fn tooltip_text => 
	      fn tooltip_private_text =>
		 append_widget_ (repr self) (repr widget)
			        (getOpt (tooltip_text, ""))
			        (getOpt (tooltip_private_text, ""))
	val append_widget' : 'a t -> 'b Widget.t -> unit
	    = fn self => fn widget =>
		 append_widget_ (repr self) (repr widget) "" ""
	val prepend_widget_ : cptr -> cptr -> string -> string -> unit
	    = app4 (symb"mgtk_gtk_toolbar_prepend_widget")
	val prepend_widget
	  : 'a t -> 'b Widget.t -> string option -> string option -> unit
	    = fn self => fn widget => fn tooltip_text => 
	      fn tooltip_private_text =>
		 prepend_widget_ (repr self) (repr widget)
				 (getOpt (tooltip_text, ""))
				 (getOpt (tooltip_private_text, ""))
	val prepend_widget' : 'a t -> 'b Widget.t -> unit
	    = fn self => fn widget =>
		 prepend_widget_ (repr self) (repr widget) "" ""
	val insert_widget_ : cptr -> cptr -> string -> string -> int -> unit
	    = app5 (symb"mgtk_gtk_toolbar_insert_widget")
	val insert_widget : 'a t -> 'b Widget.t -> string option 
			 -> string option -> int
			    -> unit
	    = fn self => fn widget => fn tooltip_text => 
	      fn tooltip_private_text => fn position =>
		 insert_widget_
		   (repr self) (repr widget) (getOpt (tooltip_text, ""))
		   (getOpt (tooltip_private_text, "")) position
	val insert_widget' : 'a t -> 'b Widget.t -> int -> unit
	    = fn self => fn widget => fn position =>
		 insert_widget_ (repr self) (repr widget) "" "" position
	val set_orientation_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_toolbar_set_orientation")
	val set_orientation : 'a t -> orientation -> unit
	    = fn self => fn orientation =>
		 set_orientation_ (repr self) orientation
	val set_style_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_toolbar_set_style")
	val set_style : 'a t -> style -> unit
	    = fn self => fn style => set_style_ (repr self) style
	val set_icon_size_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_toolbar_set_icon_size")
	val set_icon_size : 'a t -> icon_size -> unit
	    = fn self => fn icon_size => set_icon_size_ (repr self) icon_size
	val set_tooltips_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_toolbar_set_tooltips")
	val set_tooltips : 'a t -> bool -> unit
	    = fn self => fn enable => set_tooltips_ (repr self) enable
	val unset_style_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_toolbar_unset_style")
	val unset_style : 'a t -> unit = fn self => unset_style_ (repr self)
	val unset_icon_size_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_toolbar_unset_icon_size")
	val unset_icon_size : 'a t -> unit
	    = fn self => unset_icon_size_ (repr self)
	val get_orientation_ : cptr -> int
	    = app1 (symb"mgtk_gtk_toolbar_get_orientation")
	val get_orientation : 'a t -> orientation
	    = fn self => get_orientation_ (repr self)
	val get_style_ : cptr -> int = app1 (symb"mgtk_gtk_toolbar_get_style")
	val get_style : 'a t -> style = fn self => get_style_ (repr self)
	val get_icon_size_ : cptr -> int
	    = app1 (symb"mgtk_gtk_toolbar_get_icon_size")
	val get_icon_size : 'a t -> icon_size
	    = fn self => get_icon_size_ (repr self)
	val get_tooltips_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_toolbar_get_tooltips")
	val get_tooltips : 'a t -> bool = fn self => get_tooltips_ (repr self)
	local open Signal
	      infixr -->
	in val orientation_changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "orientation-changed" false
			        (unit --> return_void) f
	   val style_changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "style-changed" false (unit --> return_void) f
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
	val new_withbuffer : 'a TextBuffer.t option -> base t
	val new_withbuffer' : unit -> base t
	val setbuffer : 'a t -> 'b TextBuffer.t -> unit
	val get_buffer : 'a t -> base TextBuffer.t
	val scroll_toiter : 'a t -> textiter -> real -> bool option 
			 -> real option -> real option
			    -> bool
	val scroll_toiter' : 'a t -> textiter -> real -> bool
	val scroll_to_mark : 'a t -> 'b TextMark.t -> real -> bool option 
			  -> real option -> real option
			     -> unit
	val scroll_to_mark' : 'a t -> 'b TextMark.t -> real -> unit
	val scroll_mark_onscreen : 'a t -> 'b TextMark.t -> unit
	val move_mark_onscreen : 'a t -> 'b TextMark.t -> bool
	val place_cursor_onscreen : 'a t -> bool
	val set_cursor_visible : 'a t -> bool -> unit
	val get_cursor_visible : 'a t -> bool
	val getiter_at_location : 'a t -> int -> int -> textiter
	val set_border_window_size : 'a t -> text_window_type_t -> int -> unit
	val get_border_window_size : 'a t -> text_window_type_t -> int
	val forward_display_line : 'a t -> textiter -> bool
	val backward_display_line : 'a t -> textiter -> bool
	val forward_display_line_end : 'a t -> textiter -> bool
	val backward_display_line_start : 'a t -> textiter -> bool
	val starts_display_line : 'a t -> textiter -> bool
	val move_visually : 'a t -> textiter -> int -> bool
	val add_child_at_anchor
	  : 'a t -> 'b Widget.t -> 'c TextChildAnchor.t -> unit
	val add_child_in_window
	  : 'a t -> 'b Widget.t -> text_window_type_t -> int -> int -> unit
	val move_child : 'a t -> 'b Widget.t -> int -> int -> unit
	val set_wrap_mode : 'a t -> wrap_mode -> unit
	val get_wrap_mode : 'a t -> wrap_mode
	val set_editable : 'a t -> bool -> unit
	val get_editable : 'a t -> bool
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
	val get_default_attributes : 'a t -> text_attributes
	val move_focus_sig : (unit -> unit) -> 'a t Signal.signal
	val set_scroll_adjustments_sig
	  : (unit -> unit -> unit) -> 'a t Signal.signal
	val move_cursor_sig : (unit -> int -> bool -> unit)
			      -> 'a t Signal.signal
	val page_horizontally_sig : (int -> bool -> unit) -> 'a t Signal.signal
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a textview_t = unit
	type 'a t = 'a textview_t Container.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Container.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTextView obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_textview_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_textview_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_withbuffer_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_textview_new_withbuffer")
	val new_withbuffer : 'a TextBuffer.t option -> base t
	    = fn buffer => make (new_withbuffer_
				   (getOpt (Option.map repr buffer, 
					    GObject.null)))
	val new_withbuffer' : unit -> base t
	    = fn dummy => make (new_withbuffer_ GObject.null)
	val setbuffer_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_textview_setbuffer")
	val setbuffer : 'a t -> 'b TextBuffer.t -> unit
	    = fn self => fn buffer => setbuffer_ (repr self) (repr buffer)
	val get_buffer_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_textview_get_buffer")
	val get_buffer : 'a t -> base TextBuffer.t
	    = fn self => TextBuffer.inherit
			   () (fn () => get_buffer_ (repr self))
	val scroll_toiter_ : cptr * cptr * real * bool * real * real -> bool
	    = app1 (symb"mgtk_gtk_textview_scroll_toiter")
	val scroll_toiter : 'a t -> textiter -> real -> bool option 
			 -> real option -> real option
			    -> bool
	    = fn self => fn iter => fn within_margin => fn use_align => 
	      fn xalign => fn yalign =>
		 scroll_toiter_ (repr self, iter, within_margin, 
				 getOpt (use_align, false), 
				 getOpt (xalign, 0.5), getOpt (yalign, 0.5))
	val scroll_toiter' : 'a t -> textiter -> real -> bool
	    = fn self => fn iter => fn within_margin =>
		 scroll_toiter_
		   (repr self, iter, within_margin, false, 0.5, 0.5)
	val scroll_to_mark_ : cptr * cptr * real * bool * real * real -> unit
	    = app1 (symb"mgtk_gtk_textview_scroll_to_mark")
	val scroll_to_mark : 'a t -> 'b TextMark.t -> real -> bool option 
			  -> real option -> real option
			     -> unit
	    = fn self => fn mark => fn within_margin => fn use_align => 
	      fn xalign => fn yalign =>
		 scroll_to_mark_ (repr self, repr mark, within_margin, 
				  getOpt (use_align, false), 
				  getOpt (xalign, 0.5), getOpt (yalign, 0.5))
	val scroll_to_mark' : 'a t -> 'b TextMark.t -> real -> unit
	    = fn self => fn mark => fn within_margin =>
		 scroll_to_mark_
		   (repr self, repr mark, within_margin, false, 0.5, 0.5)
	val scroll_mark_onscreen_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_textview_scroll_mark_onscreen")
	val scroll_mark_onscreen : 'a t -> 'b TextMark.t -> unit
	    = fn self => fn mark => scroll_mark_onscreen_
				      (repr self) (repr mark)
	val move_mark_onscreen_ : cptr -> cptr -> bool
	    = app2 (symb"mgtk_gtk_textview_move_mark_onscreen")
	val move_mark_onscreen : 'a t -> 'b TextMark.t -> bool
	    = fn self => fn mark => move_mark_onscreen_ (repr self) (repr mark)
	val place_cursor_onscreen_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_textview_place_cursor_onscreen")
	val place_cursor_onscreen : 'a t -> bool
	    = fn self => place_cursor_onscreen_ (repr self)
	val set_cursor_visible_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_textview_set_cursor_visible")
	val set_cursor_visible : 'a t -> bool -> unit
	    = fn self => fn setting => set_cursor_visible_ (repr self) setting
	val get_cursor_visible_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_textview_get_cursor_visible")
	val get_cursor_visible : 'a t -> bool
	    = fn self => get_cursor_visible_ (repr self)
	val getiter_at_location_ : cptr -> cptr ref -> int -> int -> unit
	    = app4 (symb"mgtk_gtk_textview_getiter_at_location")
	val getiter_at_location : 'a t -> int -> int -> textiter
	    = fn self => fn x => fn y =>
		 let val iter = ref GObject.null
		     val ret = getiter_at_location_ (repr self) iter x y
		 in !iter end
	val set_border_window_size_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_textview_set_border_window_size")
	val set_border_window_size : 'a t -> text_window_type_t -> int -> unit
	    = fn self => fn typ => fn size =>
		 set_border_window_size_ (repr self) typ size
	val get_border_window_size_ : cptr -> int -> int
	    = app2 (symb"mgtk_gtk_textview_get_border_window_size")
	val get_border_window_size : 'a t -> text_window_type_t -> int
	    = fn self => fn typ => get_border_window_size_ (repr self) typ
	val forward_display_line_ : cptr -> cptr -> bool
	    = app2 (symb"mgtk_gtk_textview_forward_display_line")
	val forward_display_line : 'a t -> textiter -> bool
	    = fn self => fn iter => forward_display_line_ (repr self) iter
	val backward_display_line_ : cptr -> cptr -> bool
	    = app2 (symb"mgtk_gtk_textview_backward_display_line")
	val backward_display_line : 'a t -> textiter -> bool
	    = fn self => fn iter => backward_display_line_ (repr self) iter
	val forward_display_line_end_ : cptr -> cptr -> bool
	    = app2 (symb"mgtk_gtk_textview_forward_display_line_end")
	val forward_display_line_end : 'a t -> textiter -> bool
	    = fn self => fn iter => forward_display_line_end_ (repr self) iter
	val backward_display_line_start_ : cptr -> cptr -> bool
	    = app2 (symb"mgtk_gtk_textview_backward_display_line_start")
	val backward_display_line_start : 'a t -> textiter -> bool
	    = fn self => fn iter =>
		 backward_display_line_start_ (repr self) iter
	val starts_display_line_ : cptr -> cptr -> bool
	    = app2 (symb"mgtk_gtk_textview_starts_display_line")
	val starts_display_line : 'a t -> textiter -> bool
	    = fn self => fn iter => starts_display_line_ (repr self) iter
	val move_visually_ : cptr -> cptr -> int -> bool
	    = app3 (symb"mgtk_gtk_textview_move_visually")
	val move_visually : 'a t -> textiter -> int -> bool
	    = fn self => fn iter => fn count =>
		 move_visually_ (repr self) iter count
	val add_child_at_anchor_ : cptr -> cptr -> cptr -> unit
	    = app3 (symb"mgtk_gtk_textview_add_child_at_anchor")
	val add_child_at_anchor
	  : 'a t -> 'b Widget.t -> 'c TextChildAnchor.t -> unit
	    = fn self => fn child => fn anchor =>
		 add_child_at_anchor_ (repr self) (repr child) (repr anchor)
	val add_child_in_window_ : cptr -> cptr -> int -> int -> int -> unit
	    = app5 (symb"mgtk_gtk_textview_add_child_in_window")
	val add_child_in_window
	  : 'a t -> 'b Widget.t -> text_window_type_t -> int -> int -> unit
	    = fn self => fn child => fn which_window => fn xpos => fn ypos =>
		 add_child_in_window_
		   (repr self) (repr child) which_window xpos ypos
	val move_child_ : cptr -> cptr -> int -> int -> unit
	    = app4 (symb"mgtk_gtk_textview_move_child")
	val move_child : 'a t -> 'b Widget.t -> int -> int -> unit
	    = fn self => fn child => fn xpos => fn ypos =>
		 move_child_ (repr self) (repr child) xpos ypos
	val set_wrap_mode_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_textview_set_wrap_mode")
	val set_wrap_mode : 'a t -> wrap_mode -> unit
	    = fn self => fn wrap_mode => set_wrap_mode_ (repr self) wrap_mode
	val get_wrap_mode_ : cptr -> int
	    = app1 (symb"mgtk_gtk_textview_get_wrap_mode")
	val get_wrap_mode : 'a t -> wrap_mode
	    = fn self => get_wrap_mode_ (repr self)
	val set_editable_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_textview_set_editable")
	val set_editable : 'a t -> bool -> unit
	    = fn self => fn setting => set_editable_ (repr self) setting
	val get_editable_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_textview_get_editable")
	val get_editable : 'a t -> bool = fn self => get_editable_ (repr self)
	val set_pixels_above_lines_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_textview_set_pixels_above_lines")
	val set_pixels_above_lines : 'a t -> int -> unit
	    = fn self => fn pixels_above_lines =>
		 set_pixels_above_lines_ (repr self) pixels_above_lines
	val get_pixels_above_lines_ : cptr -> int
	    = app1 (symb"mgtk_gtk_textview_get_pixels_above_lines")
	val get_pixels_above_lines : 'a t -> int
	    = fn self => get_pixels_above_lines_ (repr self)
	val set_pixels_below_lines_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_textview_set_pixels_below_lines")
	val set_pixels_below_lines : 'a t -> int -> unit
	    = fn self => fn pixels_below_lines =>
		 set_pixels_below_lines_ (repr self) pixels_below_lines
	val get_pixels_below_lines_ : cptr -> int
	    = app1 (symb"mgtk_gtk_textview_get_pixels_below_lines")
	val get_pixels_below_lines : 'a t -> int
	    = fn self => get_pixels_below_lines_ (repr self)
	val set_pixels_inside_wrap_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_textview_set_pixels_inside_wrap")
	val set_pixels_inside_wrap : 'a t -> int -> unit
	    = fn self => fn pixels_inside_wrap =>
		 set_pixels_inside_wrap_ (repr self) pixels_inside_wrap
	val get_pixels_inside_wrap_ : cptr -> int
	    = app1 (symb"mgtk_gtk_textview_get_pixels_inside_wrap")
	val get_pixels_inside_wrap : 'a t -> int
	    = fn self => get_pixels_inside_wrap_ (repr self)
	val set_justification_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_textview_set_justification")
	val set_justification : 'a t -> justification -> unit
	    = fn self => fn justification =>
		 set_justification_ (repr self) justification
	val get_justification_ : cptr -> int
	    = app1 (symb"mgtk_gtk_textview_get_justification")
	val get_justification : 'a t -> justification
	    = fn self => get_justification_ (repr self)
	val set_left_margin_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_textview_set_left_margin")
	val set_left_margin : 'a t -> int -> unit
	    = fn self => fn left_margin =>
		 set_left_margin_ (repr self) left_margin
	val get_left_margin_ : cptr -> int
	    = app1 (symb"mgtk_gtk_textview_get_left_margin")
	val get_left_margin : 'a t -> int
	    = fn self => get_left_margin_ (repr self)
	val set_right_margin_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_textview_set_right_margin")
	val set_right_margin : 'a t -> int -> unit
	    = fn self => fn right_margin =>
		 set_right_margin_ (repr self) right_margin
	val get_right_margin_ : cptr -> int
	    = app1 (symb"mgtk_gtk_textview_get_right_margin")
	val get_right_margin : 'a t -> int
	    = fn self => get_right_margin_ (repr self)
	val set_indent_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_textview_set_indent")
	val set_indent : 'a t -> int -> unit
	    = fn self => fn indent => set_indent_ (repr self) indent
	val get_indent_ : cptr -> int
	    = app1 (symb"mgtk_gtk_textview_get_indent")
	val get_indent : 'a t -> int = fn self => get_indent_ (repr self)
	val get_default_attributes_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_textview_get_default_attributes")
	val get_default_attributes : 'a t -> text_attributes
	    = fn self => get_default_attributes_ (repr self)
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
	  val page_horizontally_sig
	    : (int -> bool -> unit) -> 'a t Signal.signal
	      = fn f => signal "page-horizontally" false
			       (int --> bool --> return_void) f
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
    structure Table :>
      sig
	type base
	type 'a table_t
	type 'a t = 'a table_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toTable : 'a t -> base t
	val get_type : unit -> GType.t
	val new : int option -> int option -> bool option -> base t
	val new' : unit -> base t
	val resize : 'a t -> int -> int -> unit
	val attach_defaults
	  : 'a t -> 'b Widget.t -> int -> int -> int -> int -> unit
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a table_t = unit
	type 'a t = 'a table_t Container.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Container.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTable obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_table_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : int -> int -> bool -> cptr = app3 (symb"mgtk_gtk_table_new")
	val new : int option -> int option -> bool option -> base t
	    = fn rows => fn columns => fn homogeneous =>
		 make (new_ (getOpt (rows, 1)) (getOpt (columns, 1))
			    (getOpt (homogeneous, false)))
	val new' : unit -> base t = fn dummy => make (new_ 1 1 false)
	val resize_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_table_resize")
	val resize : 'a t -> int -> int -> unit
	    = fn self => fn rows => fn columns =>
		 resize_ (repr self) rows columns
	val attach_defaults_ : cptr * cptr * int * int * int * int -> unit
	    = app1 (symb"mgtk_gtk_table_attach_defaults")
	val attach_defaults
	  : 'a t -> 'b Widget.t -> int -> int -> int -> int -> unit
	    = fn self => fn widget => fn left_attach => fn right_attach => 
	      fn top_attach => fn bottom_attach =>
		 attach_defaults_ (repr self, repr widget, left_attach, 
				   right_attach, top_attach, bottom_attach)
	val set_row_spacing_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_table_set_row_spacing")
	val set_row_spacing : 'a t -> int -> int -> unit
	    = fn self => fn row => fn spacing =>
		 set_row_spacing_ (repr self) row spacing
	val get_row_spacing_ : cptr -> int -> int
	    = app2 (symb"mgtk_gtk_table_get_row_spacing")
	val get_row_spacing : 'a t -> int -> int
	    = fn self => fn row => get_row_spacing_ (repr self) row
	val set_col_spacing_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_table_set_col_spacing")
	val set_col_spacing : 'a t -> int -> int -> unit
	    = fn self => fn column => fn spacing =>
		 set_col_spacing_ (repr self) column spacing
	val get_col_spacing_ : cptr -> int -> int
	    = app2 (symb"mgtk_gtk_table_get_col_spacing")
	val get_col_spacing : 'a t -> int -> int
	    = fn self => fn column => get_col_spacing_ (repr self) column
	val set_row_spacings_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_table_set_row_spacings")
	val set_row_spacings : 'a t -> int -> unit
	    = fn self => fn spacing => set_row_spacings_ (repr self) spacing
	val get_default_row_spacing_ : cptr -> int
	    = app1 (symb"mgtk_gtk_table_get_default_row_spacing")
	val get_default_row_spacing : 'a t -> int
	    = fn self => get_default_row_spacing_ (repr self)
	val set_col_spacings_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_table_set_col_spacings")
	val set_col_spacings : 'a t -> int -> unit
	    = fn self => fn spacing => set_col_spacings_ (repr self) spacing
	val get_default_col_spacing_ : cptr -> int
	    = app1 (symb"mgtk_gtk_table_get_default_col_spacing")
	val get_default_col_spacing : 'a t -> int
	    = fn self => get_default_col_spacing_ (repr self)
	val set_homogeneous_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_table_set_homogeneous")
	val set_homogeneous : 'a t -> bool -> unit
	    = fn self => fn homogeneous =>
		 set_homogeneous_ (repr self) homogeneous
	val get_homogeneous_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_table_get_homogeneous")
	val get_homogeneous : 'a t -> bool
	    = fn self => get_homogeneous_ (repr self)
    end
    structure Socket :>
      sig
	type base
	type 'a socket_t
	type 'a t = 'a socket_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toSocket : 'a t -> base t
	val new : unit -> base t
	val get_type : unit -> GType.t
	val plug_added_sig : (unit -> unit) -> 'a t Signal.signal
	val plug_removed_sig : (unit -> bool) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a socket_t = unit
	type 'a t = 'a socket_t Container.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Container.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toSocket obj = inherit () (fn () => repr obj)
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_socket_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_socket_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	local open Signal
	      infixr -->
	in val plug_added_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "plug-added" false (void --> return_void) f
	   val plug_removed_sig : (unit -> bool) -> 'a t Signal.signal
	       = fn f => signal "plug-removed" false (void --> return_bool) f
	end
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
	val pack1 : 'a t -> 'b Widget.t -> bool option -> bool option -> unit
	val pack1' : 'a t -> 'b Widget.t -> unit
	val pack2 : 'a t -> 'b Widget.t -> bool option -> bool option -> unit
	val pack2' : 'a t -> 'b Widget.t -> unit
	val get_position : 'a t -> int
	val set_position : 'a t -> int -> unit
	val compute_position : 'a t -> int -> int -> int -> unit
	val cycle_child_focus_sig : (bool -> bool) -> 'a t Signal.signal
	val toggle_handle_focus_sig : (unit -> bool) -> 'a t Signal.signal
	val move_handle_sig : (unit -> bool) -> 'a t Signal.signal
	val cycle_handle_focus_sig : (bool -> bool) -> 'a t Signal.signal
	val accept_position_sig : (unit -> bool) -> 'a t Signal.signal
	val cancel_position_sig : (unit -> bool) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a paned_t = unit
	type 'a t = 'a paned_t Container.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Container.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toPaned obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_paned_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val add1_ : cptr -> cptr -> unit = app2 (symb"mgtk_gtk_paned_add1")
	val add1 : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child => add1_ (repr self) (repr child)
	val add2_ : cptr -> cptr -> unit = app2 (symb"mgtk_gtk_paned_add2")
	val add2 : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child => add2_ (repr self) (repr child)
	val pack1_ : cptr -> cptr -> bool -> bool -> unit
	    = app4 (symb"mgtk_gtk_paned_pack1")
	val pack1 : 'a t -> 'b Widget.t -> bool option -> bool option -> unit
	    = fn self => fn child => fn resize => fn shrink =>
		 pack1_ (repr self) (repr child) (getOpt (resize, false))
		        (getOpt (shrink, true))
	val pack1' : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child => pack1_ (repr self) (repr child) false true
	val pack2_ : cptr -> cptr -> bool -> bool -> unit
	    = app4 (symb"mgtk_gtk_paned_pack2")
	val pack2 : 'a t -> 'b Widget.t -> bool option -> bool option -> unit
	    = fn self => fn child => fn resize => fn shrink =>
		 pack2_ (repr self) (repr child) (getOpt (resize, true))
		        (getOpt (shrink, true))
	val pack2' : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child => pack2_ (repr self) (repr child) true true
	val get_position_ : cptr -> int
	    = app1 (symb"mgtk_gtk_paned_get_position")
	val get_position : 'a t -> int = fn self => get_position_ (repr self)
	val set_position_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_paned_set_position")
	val set_position : 'a t -> int -> unit
	    = fn self => fn position => set_position_ (repr self) position
	val compute_position_ : cptr -> int -> int -> int -> unit
	    = app4 (symb"mgtk_gtk_paned_compute_position")
	val compute_position : 'a t -> int -> int -> int -> unit
	    = fn self => fn allocation => fn child1_req => fn child2_req =>
		 compute_position_ (repr self) allocation child1_req child2_req
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a vpaned_t = unit
	type 'a t = 'a vpaned_t Paned.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Paned.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toVPaned obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_vpaned_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_vpaned_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a hpaned_t = unit
	type 'a t = 'a hpaned_t Paned.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Paned.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toHPaned obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_hpaned_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_hpaned_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
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
	val append_page : 'a t -> 'b Widget.t -> 'c Widget.t -> unit
	val append_page_menu
	  : 'a t -> 'b Widget.t -> 'c Widget.t -> 'd Widget.t -> unit
	val prepend_page : 'a t -> 'b Widget.t -> 'c Widget.t -> unit
	val prepend_page_menu
	  : 'a t -> 'b Widget.t -> 'c Widget.t -> 'd Widget.t -> unit
	val insert_page : 'a t -> 'b Widget.t -> 'c Widget.t -> int -> unit
	val insert_page_menu
	  : 'a t -> 'b Widget.t -> 'c Widget.t -> 'd Widget.t -> int -> unit
	val remove_page : 'a t -> int -> unit
	val get_current_page : 'a t -> int
	val get_nth_page : 'a t -> int -> base Widget.t
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
	val set_homogeneous_tabs : 'a t -> bool -> unit
	val set_tab_border : 'a t -> int -> unit
	val set_tab_hborder : 'a t -> int -> unit
	val set_tab_vborder : 'a t -> int -> unit
	val set_scrollable : 'a t -> bool -> unit
	val get_scrollable : 'a t -> bool
	val popup_enable : 'a t -> unit
	val popup_disable : 'a t -> unit
	val get_tab_label : 'a t -> 'b Widget.t -> base Widget.t
	val set_tab_label : 'a t -> 'b Widget.t -> 'c Widget.t -> unit
	val set_tab_label_text : 'a t -> 'b Widget.t -> string -> unit
	val get_tab_label_text : 'a t -> 'b Widget.t -> string
	val get_menu_label : 'a t -> 'b Widget.t -> base Widget.t
	val set_menu_label : 'a t -> 'b Widget.t -> 'c Widget.t -> unit
	val set_menu_label_text : 'a t -> 'b Widget.t -> string -> unit
	val get_menu_label_text : 'a t -> 'b Widget.t -> string
	val set_tab_label_packing
	  : 'a t -> 'b Widget.t -> bool -> bool -> packtype -> unit
	val reorder_child : 'a t -> 'b Widget.t -> int -> unit
	val current_page : 'a t -> int
	val set_page : 'a t -> int -> unit
	val move_focus_out_sig : (unit -> unit) -> 'a t Signal.signal
	val switch_page_sig : (unit -> int -> unit) -> 'a t Signal.signal
	val focus_tab_sig : (unit -> bool) -> 'a t Signal.signal
	val select_page_sig : (bool -> bool) -> 'a t Signal.signal
	val change_current_page_sig : (int -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a notebook_t = unit
	type 'a t = 'a notebook_t Container.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Container.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toNotebook obj = inherit () (fn () => repr obj)
	type tab = int
	val get_tab_ : unit -> int * int
	    = app1 (symb"mgtk_get_gtk_notebook_tab")
	val (TAB_FIRST, TAB_LAST) = get_tab_ ()
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_notebook_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_notebook_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val append_page_ : cptr -> cptr -> cptr -> unit
	    = app3 (symb"mgtk_gtk_notebook_append_page")
	val append_page : 'a t -> 'b Widget.t -> 'c Widget.t -> unit
	    = fn self => fn child => fn tab_label =>
		 append_page_ (repr self) (repr child) (repr tab_label)
	val append_page_menu_ : cptr -> cptr -> cptr -> cptr -> unit
	    = app4 (symb"mgtk_gtk_notebook_append_page_menu")
	val append_page_menu
	  : 'a t -> 'b Widget.t -> 'c Widget.t -> 'd Widget.t -> unit
	    = fn self => fn child => fn tab_label => fn menu_label =>
		 append_page_menu_ (repr self) (repr child) (repr tab_label)
				   (repr menu_label)
	val prepend_page_ : cptr -> cptr -> cptr -> unit
	    = app3 (symb"mgtk_gtk_notebook_prepend_page")
	val prepend_page : 'a t -> 'b Widget.t -> 'c Widget.t -> unit
	    = fn self => fn child => fn tab_label =>
		 prepend_page_ (repr self) (repr child) (repr tab_label)
	val prepend_page_menu_ : cptr -> cptr -> cptr -> cptr -> unit
	    = app4 (symb"mgtk_gtk_notebook_prepend_page_menu")
	val prepend_page_menu
	  : 'a t -> 'b Widget.t -> 'c Widget.t -> 'd Widget.t -> unit
	    = fn self => fn child => fn tab_label => fn menu_label =>
		 prepend_page_menu_ (repr self) (repr child) (repr tab_label)
				    (repr menu_label)
	val insert_page_ : cptr -> cptr -> cptr -> int -> unit
	    = app4 (symb"mgtk_gtk_notebook_insert_page")
	val insert_page : 'a t -> 'b Widget.t -> 'c Widget.t -> int -> unit
	    = fn self => fn child => fn tab_label => fn position =>
		 insert_page_
		   (repr self) (repr child) (repr tab_label) position
	val insert_page_menu_ : cptr -> cptr -> cptr -> cptr -> int -> unit
	    = app5 (symb"mgtk_gtk_notebook_insert_page_menu")
	val insert_page_menu
	  : 'a t -> 'b Widget.t -> 'c Widget.t -> 'd Widget.t -> int -> unit
	    = fn self => fn child => fn tab_label => fn menu_label => 
	      fn position =>
		 insert_page_menu_ (repr self) (repr child) (repr tab_label)
				   (repr menu_label) position
	val remove_page_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_notebook_remove_page")
	val remove_page : 'a t -> int -> unit
	    = fn self => fn page_num => remove_page_ (repr self) page_num
	val get_current_page_ : cptr -> int
	    = app1 (symb"mgtk_gtk_notebook_get_current_page")
	val get_current_page : 'a t -> int
	    = fn self => get_current_page_ (repr self)
	val get_nth_page_ : cptr -> int -> cptr
	    = app2 (symb"mgtk_gtk_notebook_get_nth_page")
	val get_nth_page : 'a t -> int -> base Widget.t
	    = fn self => fn page_num =>
		 Widget.inherit
		   () (fn () => get_nth_page_ (repr self) page_num)
	val page_num_ : cptr -> cptr -> int
	    = app2 (symb"mgtk_gtk_notebook_page_num")
	val page_num : 'a t -> 'b Widget.t -> int
	    = fn self => fn child => page_num_ (repr self) (repr child)
	val set_current_page_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_notebook_set_current_page")
	val set_current_page : 'a t -> int -> unit
	    = fn self => fn page_num => set_current_page_ (repr self) page_num
	val next_page_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_notebook_next_page")
	val next_page : 'a t -> unit = fn self => next_page_ (repr self)
	val prev_page_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_notebook_prev_page")
	val prev_page : 'a t -> unit = fn self => prev_page_ (repr self)
	val set_show_border_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_notebook_set_show_border")
	val set_show_border : 'a t -> bool -> unit
	    = fn self => fn show_border =>
		 set_show_border_ (repr self) show_border
	val get_show_border_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_notebook_get_show_border")
	val get_show_border : 'a t -> bool
	    = fn self => get_show_border_ (repr self)
	val set_show_tabs_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_notebook_set_show_tabs")
	val set_show_tabs : 'a t -> bool -> unit
	    = fn self => fn show_tabs => set_show_tabs_ (repr self) show_tabs
	val get_show_tabs_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_notebook_get_show_tabs")
	val get_show_tabs : 'a t -> bool
	    = fn self => get_show_tabs_ (repr self)
	val set_tab_pos_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_notebook_set_tab_pos")
	val set_tab_pos : 'a t -> positiontype -> unit
	    = fn self => fn pos => set_tab_pos_ (repr self) pos
	val get_tab_pos_ : cptr -> int
	    = app1 (symb"mgtk_gtk_notebook_get_tab_pos")
	val get_tab_pos : 'a t -> positiontype
	    = fn self => get_tab_pos_ (repr self)
	val set_homogeneous_tabs_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_notebook_set_homogeneous_tabs")
	val set_homogeneous_tabs : 'a t -> bool -> unit
	    = fn self => fn homogeneous =>
		 set_homogeneous_tabs_ (repr self) homogeneous
	val set_tab_border_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_notebook_set_tab_border")
	val set_tab_border : 'a t -> int -> unit
	    = fn self => fn border_width =>
		 set_tab_border_ (repr self) border_width
	val set_tab_hborder_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_notebook_set_tab_hborder")
	val set_tab_hborder : 'a t -> int -> unit
	    = fn self => fn tab_hborder =>
		 set_tab_hborder_ (repr self) tab_hborder
	val set_tab_vborder_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_notebook_set_tab_vborder")
	val set_tab_vborder : 'a t -> int -> unit
	    = fn self => fn tab_vborder =>
		 set_tab_vborder_ (repr self) tab_vborder
	val set_scrollable_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_notebook_set_scrollable")
	val set_scrollable : 'a t -> bool -> unit
	    = fn self => fn scrollable =>
		 set_scrollable_ (repr self) scrollable
	val get_scrollable_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_notebook_get_scrollable")
	val get_scrollable : 'a t -> bool
	    = fn self => get_scrollable_ (repr self)
	val popup_enable_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_notebook_popup_enable")
	val popup_enable : 'a t -> unit = fn self => popup_enable_ (repr self)
	val popup_disable_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_notebook_popup_disable")
	val popup_disable : 'a t -> unit
	    = fn self => popup_disable_ (repr self)
	val get_tab_label_ : cptr -> cptr -> cptr
	    = app2 (symb"mgtk_gtk_notebook_get_tab_label")
	val get_tab_label : 'a t -> 'b Widget.t -> base Widget.t
	    = fn self => fn child =>
		 Widget.inherit
		   () (fn () => get_tab_label_ (repr self) (repr child))
	val set_tab_label_ : cptr -> cptr -> cptr -> unit
	    = app3 (symb"mgtk_gtk_notebook_set_tab_label")
	val set_tab_label : 'a t -> 'b Widget.t -> 'c Widget.t -> unit
	    = fn self => fn child => fn tab_label =>
		 set_tab_label_ (repr self) (repr child) (repr tab_label)
	val set_tab_label_text_ : cptr -> cptr -> string -> unit
	    = app3 (symb"mgtk_gtk_notebook_set_tab_label_text")
	val set_tab_label_text : 'a t -> 'b Widget.t -> string -> unit
	    = fn self => fn child => fn tab_text =>
		 set_tab_label_text_ (repr self) (repr child) tab_text
	val get_tab_label_text_ : cptr -> cptr -> string
	    = app2 (symb"mgtk_gtk_notebook_get_tab_label_text")
	val get_tab_label_text : 'a t -> 'b Widget.t -> string
	    = fn self => fn child =>
		 get_tab_label_text_ (repr self) (repr child)
	val get_menu_label_ : cptr -> cptr -> cptr
	    = app2 (symb"mgtk_gtk_notebook_get_menu_label")
	val get_menu_label : 'a t -> 'b Widget.t -> base Widget.t
	    = fn self => fn child =>
		 Widget.inherit
		   () (fn () => get_menu_label_ (repr self) (repr child))
	val set_menu_label_ : cptr -> cptr -> cptr -> unit
	    = app3 (symb"mgtk_gtk_notebook_set_menu_label")
	val set_menu_label : 'a t -> 'b Widget.t -> 'c Widget.t -> unit
	    = fn self => fn child => fn menu_label =>
		 set_menu_label_ (repr self) (repr child) (repr menu_label)
	val set_menu_label_text_ : cptr -> cptr -> string -> unit
	    = app3 (symb"mgtk_gtk_notebook_set_menu_label_text")
	val set_menu_label_text : 'a t -> 'b Widget.t -> string -> unit
	    = fn self => fn child => fn menu_text =>
		 set_menu_label_text_ (repr self) (repr child) menu_text
	val get_menu_label_text_ : cptr -> cptr -> string
	    = app2 (symb"mgtk_gtk_notebook_get_menu_label_text")
	val get_menu_label_text : 'a t -> 'b Widget.t -> string
	    = fn self => fn child =>
		 get_menu_label_text_ (repr self) (repr child)
	val set_tab_label_packing_
	  : cptr -> cptr -> bool -> bool -> int -> unit
	    = app5 (symb"mgtk_gtk_notebook_set_tab_label_packing")
	val set_tab_label_packing
	  : 'a t -> 'b Widget.t -> bool -> bool -> packtype -> unit
	    = fn self => fn child => fn expand => fn fill => fn pack_type =>
		 set_tab_label_packing_
		   (repr self) (repr child) expand fill pack_type
	val reorder_child_ : cptr -> cptr -> int -> unit
	    = app3 (symb"mgtk_gtk_notebook_reorder_child")
	val reorder_child : 'a t -> 'b Widget.t -> int -> unit
	    = fn self => fn child => fn position =>
		 reorder_child_ (repr self) (repr child) position
	val current_page_ : cptr -> int
	    = app1 (symb"mgtk_gtk_notebook_current_page")
	val current_page : 'a t -> int = fn self => current_page_ (repr self)
	val set_page_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_notebook_set_page")
	val set_page : 'a t -> int -> unit
	    = fn self => fn page_num => set_page_ (repr self) page_num
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
    structure MenuShell :>
      sig
	type base
	type 'a menushell_t
	type 'a t = 'a menushell_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toMenuShell : 'a t -> base t
	val append : 'a t -> 'b Widget.t -> unit
	val prepend : 'a t -> 'b Widget.t -> unit
	val insert : 'a t -> 'b Widget.t -> int -> unit
	val deactivate : 'a t -> unit
	val select_item : 'a t -> 'b Widget.t -> unit
	val deselect : 'a t -> unit
	val activate_item : 'a t -> 'b Widget.t -> bool -> unit
	val deactivate_sig : (unit -> unit) -> 'a t Signal.signal
	val selection_done_sig : (unit -> unit) -> 'a t Signal.signal
	val move_current_sig : (unit -> unit) -> 'a t Signal.signal
	val activate_current_sig : (bool -> unit) -> 'a t Signal.signal
	val cancel_sig : (unit -> unit) -> 'a t Signal.signal
	val cycle_focus_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a menushell_t = unit
	type 'a t = 'a menushell_t Container.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Container.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toMenuShell obj = inherit () (fn () => repr obj)
	val append_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_menu_shell_append")
	val append : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child => append_ (repr self) (repr child)
	val prepend_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_menu_shell_prepend")
	val prepend : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child => prepend_ (repr self) (repr child)
	val insert_ : cptr -> cptr -> int -> unit
	    = app3 (symb"mgtk_gtk_menu_shell_insert")
	val insert : 'a t -> 'b Widget.t -> int -> unit
	    = fn self => fn child => fn position =>
		 insert_ (repr self) (repr child) position
	val deactivate_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_menu_shell_deactivate")
	val deactivate : 'a t -> unit = fn self => deactivate_ (repr self)
	val select_item_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_menu_shell_select_item")
	val select_item : 'a t -> 'b Widget.t -> unit
	    = fn self => fn menu_item =>
		 select_item_ (repr self) (repr menu_item)
	val deselect_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_menu_shell_deselect")
	val deselect : 'a t -> unit = fn self => deselect_ (repr self)
	val activate_item_ : cptr -> cptr -> bool -> unit
	    = app3 (symb"mgtk_gtk_menu_shell_activate_item")
	val activate_item : 'a t -> 'b Widget.t -> bool -> unit
	    = fn self => fn menu_item => fn force_deactivate =>
		 activate_item_ (repr self) (repr menu_item) force_deactivate
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
	val bar_get_type : unit -> GType.t
	val item_get_type : unit -> GType.t
	val shell_get_type : unit -> GType.t
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a menu_t = unit
	type 'a t = 'a menu_t MenuShell.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in MenuShell.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toMenu obj = inherit () (fn () => repr obj)
	type directiontype = int
	val get_directiontype_ : unit -> int * int * int * int
	    = app1 (symb"mgtk_get_gtk_menu_directiontype")
	val (DIR_PARENT, DIR_CHILD, DIR_NEXT, DIR_PREV) = get_directiontype_ ()
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_menu_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_menu_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val reposition_ : cptr -> unit = app1 (symb"mgtk_gtk_menu_reposition")
	val reposition : 'a t -> unit = fn self => reposition_ (repr self)
	val popdown_ : cptr -> unit = app1 (symb"mgtk_gtk_menu_popdown")
	val popdown : 'a t -> unit = fn self => popdown_ (repr self)
	val get_active_ : cptr -> cptr = app1 (symb"mgtk_gtk_menu_get_active")
	val get_active : 'a t -> base Widget.t
	    = fn self => Widget.inherit () (fn () => get_active_ (repr self))
	val set_active_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_menu_set_active")
	val set_active : 'a t -> int -> unit
	    = fn self => fn index => set_active_ (repr self) index
	val set_accelgroup_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_menu_set_accelgroup")
	val set_accelgroup : 'a t -> 'b AccelGroup.t -> unit
	    = fn self => fn accel_group =>
		 set_accelgroup_ (repr self) (repr accel_group)
	val get_accelgroup_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_menu_get_accelgroup")
	val get_accelgroup : 'a t -> base AccelGroup.t
	    = fn self => AccelGroup.inherit
			   () (fn () => get_accelgroup_ (repr self))
	val set_accel_path_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_menu_set_accel_path")
	val set_accel_path : 'a t -> string -> unit
	    = fn self => fn accel_path =>
		 set_accel_path_ (repr self) accel_path
	val detach_ : cptr -> unit = app1 (symb"mgtk_gtk_menu_detach")
	val detach : 'a t -> unit = fn self => detach_ (repr self)
	val get_attach_widget_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_menu_get_attach_widget")
	val get_attach_widget : 'a t -> base Widget.t
	    = fn self => Widget.inherit
			   () (fn () => get_attach_widget_ (repr self))
	val set_tearoff_state_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_menu_set_tearoff_state")
	val set_tearoff_state : 'a t -> bool -> unit
	    = fn self => fn torn_off => set_tearoff_state_ (repr self) torn_off
	val get_tearoff_state_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_menu_get_tearoff_state")
	val get_tearoff_state : 'a t -> bool
	    = fn self => get_tearoff_state_ (repr self)
	val set_title_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_menu_set_title")
	val set_title : 'a t -> string -> unit
	    = fn self => fn title => set_title_ (repr self) title
	val get_title_ : cptr -> string = app1 (symb"mgtk_gtk_menu_get_title")
	val get_title : 'a t -> string = fn self => get_title_ (repr self)
	val reorder_child_ : cptr -> cptr -> int -> unit
	    = app3 (symb"mgtk_gtk_menu_reorder_child")
	val reorder_child : 'a t -> 'b Widget.t -> int -> unit
	    = fn self => fn child => fn position =>
		 reorder_child_ (repr self) (repr child) position
	val bar_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_menu_bar_get_type")
	val bar_get_type : unit -> GType.t = fn dummy => bar_get_type_ dummy
	val item_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_menu_item_get_type")
	val item_get_type : unit -> GType.t = fn dummy => item_get_type_ dummy
	val shell_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_menu_shell_get_type")
	val shell_get_type : unit -> GType.t
	    = fn dummy => shell_get_type_ dummy
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a menubar_t = unit
	type 'a t = 'a menubar_t MenuShell.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in MenuShell.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toMenuBar obj = inherit () (fn () => repr obj)
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_menu_bar_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure List :>
      sig
	type base
	type 'a list_t
	type 'a t = 'a list_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toList : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val clear_items : 'a t -> int -> int -> unit
	val select_item : 'a t -> int -> unit
	val unselect_item : 'a t -> int -> unit
	val select_child : 'a t -> 'b Widget.t -> unit
	val unselect_child : 'a t -> 'b Widget.t -> unit
	val child_position : 'a t -> 'b Widget.t -> int
	val set_selection_mode : 'a t -> selection_mode -> unit
	val extend_selection : 'a t -> scrolltype -> real -> bool -> unit
	val start_selection : 'a t -> unit
	val end_selection : 'a t -> unit
	val select_all : 'a t -> unit
	val unselect_all : 'a t -> unit
	val scroll_horizontal : 'a t -> scrolltype -> real -> unit
	val scroll_vertical : 'a t -> scrolltype -> real -> unit
	val toggle_add_mode : 'a t -> unit
	val toggle_focus_row : 'a t -> unit
	val toggle_row : 'a t -> 'b Widget.t -> unit
	val undo_selection : 'a t -> unit
	val end_drag_selection : 'a t -> unit
	val item_get_type : unit -> GType.t
	val store_get_type : unit -> GType.t
	val selection_changed_sig : (unit -> unit) -> 'a t Signal.signal
	val select_child_sig : (unit -> unit) -> 'a t Signal.signal
	val unselect_child_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a list_t = unit
	type 'a t = 'a list_t Container.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Container.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toList obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_list_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_list_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val clear_items_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_list_clear_items")
	val clear_items : 'a t -> int -> int -> unit
	    = fn self => fn start => fn en => clear_items_ (repr self) start en
	val select_item_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_list_select_item")
	val select_item : 'a t -> int -> unit
	    = fn self => fn item => select_item_ (repr self) item
	val unselect_item_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_list_unselect_item")
	val unselect_item : 'a t -> int -> unit
	    = fn self => fn item => unselect_item_ (repr self) item
	val select_child_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_list_select_child")
	val select_child : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child => select_child_ (repr self) (repr child)
	val unselect_child_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_list_unselect_child")
	val unselect_child : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child => unselect_child_ (repr self) (repr child)
	val child_position_ : cptr -> cptr -> int
	    = app2 (symb"mgtk_gtk_list_child_position")
	val child_position : 'a t -> 'b Widget.t -> int
	    = fn self => fn child => child_position_ (repr self) (repr child)
	val set_selection_mode_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_list_set_selection_mode")
	val set_selection_mode : 'a t -> selection_mode -> unit
	    = fn self => fn mode => set_selection_mode_ (repr self) mode
	val extend_selection_ : cptr -> int -> real -> bool -> unit
	    = app4 (symb"mgtk_gtk_list_extend_selection")
	val extend_selection : 'a t -> scrolltype -> real -> bool -> unit
	    = fn self => fn scroll_type => fn position => 
	      fn auto_start_selection =>
		 extend_selection_ (repr self) scroll_type position
				   auto_start_selection
	val start_selection_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_list_start_selection")
	val start_selection : 'a t -> unit
	    = fn self => start_selection_ (repr self)
	val end_selection_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_list_end_selection")
	val end_selection : 'a t -> unit
	    = fn self => end_selection_ (repr self)
	val select_all_ : cptr -> unit = app1 (symb"mgtk_gtk_list_select_all")
	val select_all : 'a t -> unit = fn self => select_all_ (repr self)
	val unselect_all_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_list_unselect_all")
	val unselect_all : 'a t -> unit = fn self => unselect_all_ (repr self)
	val scroll_horizontal_ : cptr -> int -> real -> unit
	    = app3 (symb"mgtk_gtk_list_scroll_horizontal")
	val scroll_horizontal : 'a t -> scrolltype -> real -> unit
	    = fn self => fn scroll_type => fn position =>
		 scroll_horizontal_ (repr self) scroll_type position
	val scroll_vertical_ : cptr -> int -> real -> unit
	    = app3 (symb"mgtk_gtk_list_scroll_vertical")
	val scroll_vertical : 'a t -> scrolltype -> real -> unit
	    = fn self => fn scroll_type => fn position =>
		 scroll_vertical_ (repr self) scroll_type position
	val toggle_add_mode_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_list_toggle_add_mode")
	val toggle_add_mode : 'a t -> unit
	    = fn self => toggle_add_mode_ (repr self)
	val toggle_focus_row_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_list_toggle_focus_row")
	val toggle_focus_row : 'a t -> unit
	    = fn self => toggle_focus_row_ (repr self)
	val toggle_row_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_list_toggle_row")
	val toggle_row : 'a t -> 'b Widget.t -> unit
	    = fn self => fn item => toggle_row_ (repr self) (repr item)
	val undo_selection_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_list_undo_selection")
	val undo_selection : 'a t -> unit
	    = fn self => undo_selection_ (repr self)
	val end_drag_selection_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_list_end_drag_selection")
	val end_drag_selection : 'a t -> unit
	    = fn self => end_drag_selection_ (repr self)
	val item_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_list_item_get_type")
	val item_get_type : unit -> GType.t = fn dummy => item_get_type_ dummy
	val store_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_list_store_get_type")
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
    structure Layout :>
      sig
	type base
	type 'a layout_t
	type 'a t = 'a layout_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toLayout : 'a t -> base t
	val get_type : unit -> GType.t
	val new : 'a Adjustment.t option -> 'b Adjustment.t option -> base t
	val new' : unit -> base t
	val put : 'a t -> 'b Widget.t -> int -> int -> unit
	val move : 'a t -> 'b Widget.t -> int -> int -> unit
	val set_size : 'a t -> int -> int -> unit
	val set_hadjustment : 'a t -> 'b Adjustment.t option -> unit
	val set_hadjustment' : 'a t -> unit
	val set_vadjustment : 'a t -> 'b Adjustment.t option -> unit
	val set_vadjustment' : 'a t -> unit
	val freeze : 'a t -> unit
	val thaw : 'a t -> unit
	val set_scroll_adjustments_sig
	  : (unit -> unit -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a layout_t = unit
	type 'a t = 'a layout_t Container.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Container.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toLayout obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_layout_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr -> cptr -> cptr = app2 (symb"mgtk_gtk_layout_new")
	val new : 'a Adjustment.t option -> 'b Adjustment.t option -> base t
	    = fn hadjustment => fn vadjustment =>
		 make
		   (new_ (getOpt (Option.map repr hadjustment, GObject.null))
			 (getOpt (Option.map repr vadjustment, GObject.null)))
	val new' : unit -> base t
	    = fn dummy => make (new_ GObject.null GObject.null)
	val put_ : cptr -> cptr -> int -> int -> unit
	    = app4 (symb"mgtk_gtk_layout_put")
	val put : 'a t -> 'b Widget.t -> int -> int -> unit
	    = fn self => fn child_widget => fn x => fn y =>
		 put_ (repr self) (repr child_widget) x y
	val move_ : cptr -> cptr -> int -> int -> unit
	    = app4 (symb"mgtk_gtk_layout_move")
	val move : 'a t -> 'b Widget.t -> int -> int -> unit
	    = fn self => fn child_widget => fn x => fn y =>
		 move_ (repr self) (repr child_widget) x y
	val set_size_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_layout_set_size")
	val set_size : 'a t -> int -> int -> unit
	    = fn self => fn width => fn height =>
		 set_size_ (repr self) width height
	val set_hadjustment_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_layout_set_hadjustment")
	val set_hadjustment : 'a t -> 'b Adjustment.t option -> unit
	    = fn self => fn adjustment =>
		 set_hadjustment_
		   (repr self)
		   (getOpt (Option.map repr adjustment, GObject.null))
	val set_hadjustment' : 'a t -> unit
	    = fn self => set_hadjustment_ (repr self) GObject.null
	val set_vadjustment_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_layout_set_vadjustment")
	val set_vadjustment : 'a t -> 'b Adjustment.t option -> unit
	    = fn self => fn adjustment =>
		 set_vadjustment_
		   (repr self)
		   (getOpt (Option.map repr adjustment, GObject.null))
	val set_vadjustment' : 'a t -> unit
	    = fn self => set_vadjustment_ (repr self) GObject.null
	val freeze_ : cptr -> unit = app1 (symb"mgtk_gtk_layout_freeze")
	val freeze : 'a t -> unit = fn self => freeze_ (repr self)
	val thaw_ : cptr -> unit = app1 (symb"mgtk_gtk_layout_thaw")
	val thaw : 'a t -> unit = fn self => thaw_ (repr self)
	local open Signal
	      infixr -->
	in val set_scroll_adjustments_sig
	     : (unit -> unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "set-scroll-adjustments" false
			        (unit --> unit --> return_void) f
	end
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a fixed_t = unit
	type 'a t = 'a fixed_t Container.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Container.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toFixed obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_fixed_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_fixed_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val put_ : cptr -> cptr -> int -> int -> unit
	    = app4 (symb"mgtk_gtk_fixed_put")
	val put : 'a t -> 'b Widget.t -> int -> int -> unit
	    = fn self => fn widget => fn x => fn y =>
		 put_ (repr self) (repr widget) x y
	val move_ : cptr -> cptr -> int -> int -> unit
	    = app4 (symb"mgtk_gtk_fixed_move")
	val move : 'a t -> 'b Widget.t -> int -> int -> unit
	    = fn self => fn widget => fn x => fn y =>
		 move_ (repr self) (repr widget) x y
	val set_has_window_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_fixed_set_has_window")
	val set_has_window : 'a t -> bool -> unit
	    = fn self => fn has_window =>
		 set_has_window_ (repr self) has_window
	val get_has_window_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_fixed_get_has_window")
	val get_has_window : 'a t -> bool
	    = fn self => get_has_window_ (repr self)
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a bin_t = unit
	type 'a t = 'a bin_t Container.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Container.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toBin obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_bin_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val get_child_ : cptr -> cptr = app1 (symb"mgtk_gtk_bin_get_child")
	val get_child : 'a t -> base Widget.t
	    = fn self => Widget.inherit () (fn () => get_child_ (repr self))
    end
    structure Viewport :>
      sig
	type base
	type 'a viewport_t
	type 'a t = 'a viewport_t Bin.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toViewport : 'a t -> base t
	val get_type : unit -> GType.t
	val new : 'a Adjustment.t option -> 'b Adjustment.t option -> base t
	val new' : unit -> base t
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a viewport_t = unit
	type 'a t = 'a viewport_t Bin.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Bin.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toViewport obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_viewport_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr -> cptr -> cptr = app2 (symb"mgtk_gtk_viewport_new")
	val new : 'a Adjustment.t option -> 'b Adjustment.t option -> base t
	    = fn hadjustment => fn vadjustment =>
		 make
		   (new_ (getOpt (Option.map repr hadjustment, GObject.null))
			 (getOpt (Option.map repr vadjustment, GObject.null)))
	val new' : unit -> base t
	    = fn dummy => make (new_ GObject.null GObject.null)
	val get_hadjustment_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_viewport_get_hadjustment")
	val get_hadjustment : 'a t -> base Adjustment.t
	    = fn self => Adjustment.inherit
			   () (fn () => get_hadjustment_ (repr self))
	val get_vadjustment_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_viewport_get_vadjustment")
	val get_vadjustment : 'a t -> base Adjustment.t
	    = fn self => Adjustment.inherit
			   () (fn () => get_vadjustment_ (repr self))
	val set_hadjustment_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_viewport_set_hadjustment")
	val set_hadjustment : 'a t -> 'b Adjustment.t option -> unit
	    = fn self => fn adjustment =>
		 set_hadjustment_
		   (repr self)
		   (getOpt (Option.map repr adjustment, GObject.null))
	val set_hadjustment' : 'a t -> unit
	    = fn self => set_hadjustment_ (repr self) GObject.null
	val set_vadjustment_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_viewport_set_vadjustment")
	val set_vadjustment : 'a t -> 'b Adjustment.t option -> unit
	    = fn self => fn adjustment =>
		 set_vadjustment_
		   (repr self)
		   (getOpt (Option.map repr adjustment, GObject.null))
	val set_vadjustment' : 'a t -> unit
	    = fn self => set_vadjustment_ (repr self) GObject.null
	val set_shadowtype_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_viewport_set_shadowtype")
	val set_shadowtype : 'a t -> shadowtype -> unit
	    = fn self => fn typ => set_shadowtype_ (repr self) typ
	val get_shadowtype_ : cptr -> int
	    = app1 (symb"mgtk_gtk_viewport_get_shadowtype")
	val get_shadowtype : 'a t -> shadowtype
	    = fn self => get_shadowtype_ (repr self)
	local open Signal
	      infixr -->
	in val set_scroll_adjustments_sig
	     : (unit -> unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "set-scroll-adjustments" false
			        (unit --> unit --> return_void) f
	end
    end
    structure ScrolledWindow :>
      sig
	type base
	type 'a scrolledwindow_t
	type 'a t = 'a scrolledwindow_t Bin.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toScrolledWindow : 'a t -> base t
	val get_type : unit -> GType.t
	val new : 'a Adjustment.t option -> 'b Adjustment.t option -> base t
	val new' : unit -> base t
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a scrolledwindow_t = unit
	type 'a t = 'a scrolledwindow_t Bin.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Bin.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toScrolledWindow obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_scrolled_window_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : cptr -> cptr -> cptr
	    = app2 (symb"mgtk_gtk_scrolled_window_new")
	val new : 'a Adjustment.t option -> 'b Adjustment.t option -> base t
	    = fn hadjustment => fn vadjustment =>
		 make
		   (new_ (getOpt (Option.map repr hadjustment, GObject.null))
			 (getOpt (Option.map repr vadjustment, GObject.null)))
	val new' : unit -> base t
	    = fn dummy => make (new_ GObject.null GObject.null)
	val set_hadjustment_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_scrolled_window_set_hadjustment")
	val set_hadjustment : 'a t -> 'b Adjustment.t -> unit
	    = fn self => fn hadjustment =>
		 set_hadjustment_ (repr self) (repr hadjustment)
	val set_vadjustment_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_scrolled_window_set_vadjustment")
	val set_vadjustment : 'a t -> 'b Adjustment.t -> unit
	    = fn self => fn hadjustment =>
		 set_vadjustment_ (repr self) (repr hadjustment)
	val get_hadjustment_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_scrolled_window_get_hadjustment")
	val get_hadjustment : 'a t -> base Adjustment.t
	    = fn self => Adjustment.inherit
			   () (fn () => get_hadjustment_ (repr self))
	val get_vadjustment_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_scrolled_window_get_vadjustment")
	val get_vadjustment : 'a t -> base Adjustment.t
	    = fn self => Adjustment.inherit
			   () (fn () => get_vadjustment_ (repr self))
	val set_policy_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_scrolled_window_set_policy")
	val set_policy : 'a t -> policytype -> policytype -> unit
	    = fn self => fn hscrollbar_policy => fn vscrollbar_policy =>
		 set_policy_ (repr self) hscrollbar_policy vscrollbar_policy
	val get_policy_ : cptr -> int ref -> int ref -> unit
	    = app3 (symb"mgtk_gtk_scrolled_window_get_policy")
	val get_policy : 'a t -> policytype * policytype
	    = fn self =>
		 let val (hscrollbar_policy, vscrollbar_policy)
			 = (ref 0, ref 0)
		     val ret = get_policy_ (repr self) hscrollbar_policy
					   vscrollbar_policy
		 in (!hscrollbar_policy, !vscrollbar_policy) end
	val set_placement_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_scrolled_window_set_placement")
	val set_placement : 'a t -> cornertype -> unit
	    = fn self => fn window_placement =>
		 set_placement_ (repr self) window_placement
	val get_placement_ : cptr -> int
	    = app1 (symb"mgtk_gtk_scrolled_window_get_placement")
	val get_placement : 'a t -> cornertype
	    = fn self => get_placement_ (repr self)
	val set_shadowtype_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_scrolled_window_set_shadowtype")
	val set_shadowtype : 'a t -> shadowtype -> unit
	    = fn self => fn typ => set_shadowtype_ (repr self) typ
	val get_shadowtype_ : cptr -> int
	    = app1 (symb"mgtk_gtk_scrolled_window_get_shadowtype")
	val get_shadowtype : 'a t -> shadowtype
	    = fn self => get_shadowtype_ (repr self)
	val add_with_viewport_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_scrolled_window_add_with_viewport")
	val add_with_viewport : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child =>
		 add_with_viewport_ (repr self) (repr child)
	local open Signal
	      infixr -->
	in val scroll_child_sig : (unit -> bool -> unit) -> 'a t Signal.signal
	       = fn f => signal "scroll-child" false
			        (unit --> bool --> return_void) f
	   val move_focus_out_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "move-focus-out" false (unit --> return_void) f
	end
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
	val factories_path_delete : string -> string -> unit
	val select_sig : (unit -> unit) -> 'a t Signal.signal
	val deselect_sig : (unit -> unit) -> 'a t Signal.signal
	val toggle_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a item_t = unit
	type 'a t = 'a item_t Bin.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Bin.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toItem obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_item_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val select_ : cptr -> unit = app1 (symb"mgtk_gtk_item_select")
	val select : 'a t -> unit = fn self => select_ (repr self)
	val deselect_ : cptr -> unit = app1 (symb"mgtk_gtk_item_deselect")
	val deselect : 'a t -> unit = fn self => deselect_ (repr self)
	val toggle_ : cptr -> unit = app1 (symb"mgtk_gtk_item_toggle")
	val toggle : 'a t -> unit = fn self => toggle_ (repr self)
	val factory_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_item_factory_get_type")
	val factory_get_type : unit -> GType.t
	    = fn dummy => factory_get_type_ dummy
	val factory_path_from_widget_ : cptr -> string
	    = app1 (symb"mgtk_gtk_item_factory_path_from_widget")
	val factory_path_from_widget : 'a Widget.t -> string
	    = fn widget => factory_path_from_widget_ (repr widget)
	val factory_popup_data_from_widget_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_item_factory_popup_data_from_widget")
	val factory_popup_data_from_widget : 'a Widget.t -> cptr
	    = fn widget => factory_popup_data_from_widget_ (repr widget)
	val factories_path_delete_ : string -> string -> unit
	    = app2 (symb"mgtk_gtk_item_factories_path_delete")
	val factories_path_delete : string -> string -> unit
	    = fn ifactory_path => fn path =>
		 factories_path_delete_ ifactory_path path
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
	val right_justify : 'a t -> unit
	val activate_sig : (unit -> unit) -> 'a t Signal.signal
	val activate_item_sig : (unit -> unit) -> 'a t Signal.signal
	val toggle_size_request_sig : (int -> unit) -> 'a t Signal.signal
	val toggle_size_allocate_sig : (int -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a menuitem_t = unit
	type 'a t = 'a menuitem_t Item.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Item.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toMenuItem obj = inherit () (fn () => repr obj)
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_menu_item_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_label_ : string -> cptr
	    = app1 (symb"mgtk_gtk_menu_item_new_with_label")
	val new_with_label : string -> base t
	    = fn label => make (new_with_label_ label)
	val new_with_mnemonic_ : string -> cptr
	    = app1 (symb"mgtk_gtk_menu_item_new_with_mnemonic")
	val new_with_mnemonic : string -> base t
	    = fn label => make (new_with_mnemonic_ label)
	val set_submenu_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_menu_item_set_submenu")
	val set_submenu : 'a t -> 'b Widget.t -> unit
	    = fn self => fn submenu => set_submenu_ (repr self) (repr submenu)
	val get_submenu_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_menu_item_get_submenu")
	val get_submenu : 'a t -> base Widget.t
	    = fn self => Widget.inherit () (fn () => get_submenu_ (repr self))
	val remove_submenu_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_menu_item_remove_submenu")
	val remove_submenu : 'a t -> unit
	    = fn self => remove_submenu_ (repr self)
	val select_ : cptr -> unit = app1 (symb"mgtk_gtk_menu_item_select")
	val select : 'a t -> unit = fn self => select_ (repr self)
	val deselect_ : cptr -> unit = app1 (symb"mgtk_gtk_menu_item_deselect")
	val deselect : 'a t -> unit = fn self => deselect_ (repr self)
	val activate_ : cptr -> unit = app1 (symb"mgtk_gtk_menu_item_activate")
	val activate : 'a t -> unit = fn self => activate_ (repr self)
	val toggle_size_allocate_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_menu_item_toggle_size_allocate")
	val toggle_size_allocate : 'a t -> int -> unit
	    = fn self => fn allocation =>
		 toggle_size_allocate_ (repr self) allocation
	val set_right_justified_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_menu_item_set_right_justified")
	val set_right_justified : 'a t -> bool -> unit
	    = fn self => fn right_justified =>
		 set_right_justified_ (repr self) right_justified
	val get_right_justified_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_menu_item_get_right_justified")
	val get_right_justified : 'a t -> bool
	    = fn self => get_right_justified_ (repr self)
	val set_accel_path_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_menu_item_set_accel_path")
	val set_accel_path : 'a t -> string -> unit
	    = fn self => fn accel_path =>
		 set_accel_path_ (repr self) accel_path
	val right_justify_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_menu_item_right_justify")
	val right_justify : 'a t -> unit
	    = fn self => right_justify_ (repr self)
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a tearoffmenuitem_t = unit
	type 'a t = 'a tearoffmenuitem_t MenuItem.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in MenuItem.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toTearoffMenuItem obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_tearoff_menu_item_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_tearoff_menu_item_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a separatormenuitem_t = unit
	type 'a t = 'a separatormenuitem_t MenuItem.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in MenuItem.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toSeparatorMenuItem obj = inherit () (fn () => repr obj)
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_separator_menu_item_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
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
	val set_show_toggle : 'a t -> bool -> unit
	val set_state : 'a t -> bool -> unit
	val toggled_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a checkmenuitem_t = unit
	type 'a t = 'a checkmenuitem_t MenuItem.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in MenuItem.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toCheckMenuItem obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_check_menu_item_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_check_menu_item_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_label_ : string -> cptr
	    = app1 (symb"mgtk_gtk_check_menu_item_new_with_label")
	val new_with_label : string -> base t
	    = fn label => make (new_with_label_ label)
	val new_with_mnemonic_ : string -> cptr
	    = app1 (symb"mgtk_gtk_check_menu_item_new_with_mnemonic")
	val new_with_mnemonic : string -> base t
	    = fn label => make (new_with_mnemonic_ label)
	val set_active_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_check_menu_item_set_active")
	val set_active : 'a t -> bool -> unit
	    = fn self => fn is_active => set_active_ (repr self) is_active
	val get_active_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_check_menu_item_get_active")
	val get_active : 'a t -> bool = fn self => get_active_ (repr self)
	val toggled_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_check_menu_item_toggled")
	val toggled : 'a t -> unit = fn self => toggled_ (repr self)
	val set_inconsistent_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_check_menu_item_set_inconsistent")
	val set_inconsistent : 'a t -> bool -> unit
	    = fn self => fn setting => set_inconsistent_ (repr self) setting
	val get_inconsistent_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_check_menu_item_get_inconsistent")
	val get_inconsistent : 'a t -> bool
	    = fn self => get_inconsistent_ (repr self)
	val set_show_toggle_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_check_menu_item_set_show_toggle")
	val set_show_toggle : 'a t -> bool -> unit
	    = fn self => fn always => set_show_toggle_ (repr self) always
	val set_state_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_check_menu_item_set_state")
	val set_state : 'a t -> bool -> unit
	    = fn self => fn is_active => set_state_ (repr self) is_active
	local open Signal
	      infixr -->
	in val toggled_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "toggled" false (void --> return_void) f
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
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a radiomenuitem_t = unit
	type 'a t = 'a radiomenuitem_t CheckMenuItem.t
	fun inherit w con
	  = let val con = let val ptr = con () in fn () => ptr end
		val witness = ()
	    in CheckMenuItem.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toRadioMenuItem obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_radio_menu_item_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a imagemenuitem_t = unit
	type 'a t = 'a imagemenuitem_t MenuItem.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in MenuItem.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toImageMenuItem obj = inherit () (fn () => repr obj)
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_image_menu_item_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_label_ : string -> cptr
	    = app1 (symb"mgtk_gtk_image_menu_item_new_with_label")
	val new_with_label : string -> base t
	    = fn label => make (new_with_label_ label)
	val new_with_mnemonic_ : string -> cptr
	    = app1 (symb"mgtk_gtk_image_menu_item_new_with_mnemonic")
	val new_with_mnemonic : string -> base t
	    = fn label => make (new_with_mnemonic_ label)
	val new_from_stock_ : string -> cptr -> cptr
	    = app2 (symb"mgtk_gtk_image_menu_item_new_from_stock")
	val new_from_stock : string -> 'a AccelGroup.t -> base t
	    = fn stock_id => fn accel_group =>
		 make (new_from_stock_ stock_id (repr accel_group))
	val set_image_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_image_menu_item_set_image")
	val set_image : 'a t -> 'b Widget.t option -> unit
	    = fn self => fn image =>
		 set_image_ (repr self)
			    (getOpt (Option.map repr image, GObject.null))
	val set_image' : 'a t -> unit
	    = fn self => set_image_ (repr self) GObject.null
    end
    structure ListItem :>
      sig
	type base
	type 'a listitem_t
	type 'a t = 'a listitem_t Item.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toListItem : 'a t -> base t
	val new : unit -> base t
	val new_with_label : string -> base t
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a listitem_t = unit
	type 'a t = 'a listitem_t Item.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Item.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toListItem obj = inherit () (fn () => repr obj)
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_list_item_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_label_ : string -> cptr
	    = app1 (symb"mgtk_gtk_list_item_new_with_label")
	val new_with_label : string -> base t
	    = fn label => make (new_with_label_ label)
	val select_ : cptr -> unit = app1 (symb"mgtk_gtk_list_item_select")
	val select : 'a t -> unit = fn self => select_ (repr self)
	val deselect_ : cptr -> unit = app1 (symb"mgtk_gtk_list_item_deselect")
	val deselect : 'a t -> unit = fn self => deselect_ (repr self)
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a handlebox_t = unit
	type 'a t = 'a handlebox_t Bin.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Bin.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toHandleBox obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_handle_box_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_handle_box_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val set_shadowtype_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_handle_box_set_shadowtype")
	val set_shadowtype : 'a t -> shadowtype -> unit
	    = fn self => fn typ => set_shadowtype_ (repr self) typ
	val get_shadowtype_ : cptr -> int
	    = app1 (symb"mgtk_gtk_handle_box_get_shadowtype")
	val get_shadowtype : 'a t -> shadowtype
	    = fn self => get_shadowtype_ (repr self)
	val set_handle_position_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_handle_box_set_handle_position")
	val set_handle_position : 'a t -> positiontype -> unit
	    = fn self => fn position =>
		 set_handle_position_ (repr self) position
	val get_handle_position_ : cptr -> int
	    = app1 (symb"mgtk_gtk_handle_box_get_handle_position")
	val get_handle_position : 'a t -> positiontype
	    = fn self => get_handle_position_ (repr self)
	val set_snap_edge_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_handle_box_set_snap_edge")
	val set_snap_edge : 'a t -> positiontype -> unit
	    = fn self => fn edge => set_snap_edge_ (repr self) edge
	val get_snap_edge_ : cptr -> int
	    = app1 (symb"mgtk_gtk_handle_box_get_snap_edge")
	val get_snap_edge : 'a t -> positiontype
	    = fn self => get_snap_edge_ (repr self)
	local open Signal
	      infixr -->
	in val child_attached_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "child-attached" false (unit --> return_void) f
	   val child_detached_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "child-detached" false (unit --> return_void) f
	end
    end
    structure Frame :>
      sig
	type base
	type 'a frame_t
	type 'a t = 'a frame_t Bin.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toFrame : 'a t -> base t
	val get_type : unit -> GType.t
	val new : string option -> base t
	val new' : unit -> base t
	val set_label : 'a t -> string option -> unit
	val set_label' : 'a t -> unit
	val get_label : 'a t -> string
	val set_label_widget : 'a t -> 'b Widget.t -> unit
	val get_label_widget : 'a t -> base Widget.t
	val set_label_align : 'a t -> real -> real -> unit
	val set_shadowtype : 'a t -> shadowtype -> unit
	val get_shadowtype : 'a t -> shadowtype
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a frame_t = unit
	type 'a t = 'a frame_t Bin.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Bin.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toFrame obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_frame_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : string -> cptr = app1 (symb"mgtk_gtk_frame_new")
	val new : string option -> base t
	    = fn label => make (new_ (getOpt (label, "")))
	val new' : unit -> base t = fn dummy => make (new_ "")
	val set_label_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_frame_set_label")
	val set_label : 'a t -> string option -> unit
	    = fn self => fn label =>
		 set_label_ (repr self) (getOpt (label, ""))
	val set_label' : 'a t -> unit = fn self => set_label_ (repr self) ""
	val get_label_ : cptr -> string = app1 (symb"mgtk_gtk_frame_get_label")
	val get_label : 'a t -> string = fn self => get_label_ (repr self)
	val set_label_widget_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_frame_set_label_widget")
	val set_label_widget : 'a t -> 'b Widget.t -> unit
	    = fn self => fn label_widget =>
		 set_label_widget_ (repr self) (repr label_widget)
	val get_label_widget_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_frame_get_label_widget")
	val get_label_widget : 'a t -> base Widget.t
	    = fn self => Widget.inherit
			   () (fn () => get_label_widget_ (repr self))
	val set_label_align_ : cptr -> real -> real -> unit
	    = app3 (symb"mgtk_gtk_frame_set_label_align")
	val set_label_align : 'a t -> real -> real -> unit
	    = fn self => fn xalign => fn yalign =>
		 set_label_align_ (repr self) xalign yalign
	val set_shadowtype_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_frame_set_shadowtype")
	val set_shadowtype : 'a t -> shadowtype -> unit
	    = fn self => fn typ => set_shadowtype_ (repr self) typ
	val get_shadowtype_ : cptr -> int
	    = app1 (symb"mgtk_gtk_frame_get_shadowtype")
	val get_shadowtype : 'a t -> shadowtype
	    = fn self => get_shadowtype_ (repr self)
    end
    structure AspectFrame :>
      sig
	type base
	type 'a aspectframe_t
	type 'a t = 'a aspectframe_t Frame.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toAspectFrame : 'a t -> base t
	val get_type : unit -> GType.t
	val new : string option -> real option -> real option -> real option 
	       -> bool option
		  -> base t
	val new' : unit -> base t
	val set : 'a t -> real option -> real option -> real option 
	       -> bool option
		  -> unit
	val set' : 'a t -> unit
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a aspectframe_t = unit
	type 'a t = 'a aspectframe_t Frame.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Frame.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toAspectFrame obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_aspect_frame_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : string -> real -> real -> real -> bool -> cptr
	    = app5 (symb"mgtk_gtk_aspect_frame_new")
	val new : string option -> real option -> real option -> real option 
	       -> bool option
		  -> base t
	    = fn label => fn xalign => fn yalign => fn ratio => 
	      fn obey_child =>
		 make (new_ (getOpt (label, "")) (getOpt (xalign, 0.5))
			    (getOpt (yalign, 0.5)) (getOpt (ratio, 1.0))
			    (getOpt (obey_child, true)))
	val new' : unit -> base t = fn dummy => make (new_ "" 0.5 0.5 1.0 true)
	val set_ : cptr -> real -> real -> real -> bool -> unit
	    = app5 (symb"mgtk_gtk_aspect_frame_set")
	val set : 'a t -> real option -> real option -> real option 
	       -> bool option
		  -> unit
	    = fn self => fn xalign => fn yalign => fn ratio => fn obey_child =>
		 set_ (repr self) (getOpt (xalign, 0.0)) (getOpt (yalign, 0.0))
		      (getOpt (ratio, 1.0)) (getOpt (obey_child, true))
	val set' : 'a t -> unit = fn self => set_ (repr self) 0.0 0.0 1.0 true
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
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a eventbox_t = unit
	type 'a t = 'a eventbox_t Bin.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Bin.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toEventBox obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_event_box_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_event_box_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure Alignment :>
      sig
	type base
	type 'a alignment_t
	type 'a t = 'a alignment_t Bin.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toAlignment : 'a t -> base t
	val get_type : unit -> GType.t
	val new : real option -> real option -> real option -> real option
		  -> base t
	val new' : unit -> base t
	val set : 'a t -> real -> real -> real -> real -> unit
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a alignment_t = unit
	type 'a t = 'a alignment_t Bin.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Bin.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toAlignment obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_alignment_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : real -> real -> real -> real -> cptr
	    = app4 (symb"mgtk_gtk_alignment_new")
	val new : real option -> real option -> real option -> real option
		  -> base t
	    = fn xalign => fn yalign => fn xscale => fn yscale =>
		 make (new_ (getOpt (xalign, 0.0)) (getOpt (yalign, 0.0))
			    (getOpt (xscale, 0.0)) (getOpt (yscale, 0.0)))
	val new' : unit -> base t = fn dummy => make (new_ 0.0 0.0 0.0 0.0)
	val set_ : cptr -> real -> real -> real -> real -> unit
	    = app5 (symb"mgtk_gtk_alignment_set")
	val set : 'a t -> real -> real -> real -> real -> unit
	    = fn self => fn xalign => fn yalign => fn xscale => fn yscale =>
		 set_ (repr self) xalign yalign xscale yscale
    end
    structure Button :>
      sig
	type base
	type 'a button_t
	type 'a t = 'a button_t Bin.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toButton : 'a t -> base t
	type action
	val IGNORED : action
	val SELECTS : action
	val DRAGS : action
	val EXPANDS : action
	type box_style
	val BUTTONBOX_DEFAULT_STYLE : box_style
	val BUTTONBOX_SPREAD : box_style
	val BUTTONBOX_EDGE : box_style
	val BUTTONBOX_START : box_style
	val BUTTONBOX_END : box_style
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
	val clicked_sig : (unit -> unit) -> 'a t Signal.signal
	val pressed_sig : (unit -> unit) -> 'a t Signal.signal
	val released_sig : (unit -> unit) -> 'a t Signal.signal
	val enter_sig : (unit -> unit) -> 'a t Signal.signal
	val leave_sig : (unit -> unit) -> 'a t Signal.signal
	val activate_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a button_t = unit
	type 'a t = 'a button_t Bin.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Bin.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toButton obj = inherit () (fn () => repr obj)
	type action = int
	val get_action_ : unit -> int * int * int * int
	    = app1 (symb"mgtk_get_gtk_button_action")
	val (IGNORED, SELECTS, DRAGS, EXPANDS) = get_action_ ()
	type box_style = int
	val get_box_style_ : unit -> int * int * int * int * int
	    = app1 (symb"mgtk_get_gtk_button_box_style")
	val (BUTTONBOX_DEFAULT_STYLE, BUTTONBOX_SPREAD, BUTTONBOX_EDGE, 
	     BUTTONBOX_START, BUTTONBOX_END)
	    = get_box_style_ ()
	val box_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_button_box_get_type")
	val box_get_type : unit -> GType.t = fn dummy => box_get_type_ dummy
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_button_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_button_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_label_ : string -> cptr
	    = app1 (symb"mgtk_gtk_button_new_with_label")
	val new_with_label : string -> base t
	    = fn label => make (new_with_label_ label)
	val new_from_stock_ : string -> cptr
	    = app1 (symb"mgtk_gtk_button_new_from_stock")
	val new_from_stock : string -> base t
	    = fn stock_id => make (new_from_stock_ stock_id)
	val new_with_mnemonic_ : string -> cptr
	    = app1 (symb"mgtk_gtk_button_new_with_mnemonic")
	val new_with_mnemonic : string -> base t
	    = fn label => make (new_with_mnemonic_ label)
	val pressed_ : cptr -> unit = app1 (symb"mgtk_gtk_button_pressed")
	val pressed : 'a t -> unit = fn self => pressed_ (repr self)
	val released_ : cptr -> unit = app1 (symb"mgtk_gtk_button_released")
	val released : 'a t -> unit = fn self => released_ (repr self)
	val clicked_ : cptr -> unit = app1 (symb"mgtk_gtk_button_clicked")
	val clicked : 'a t -> unit = fn self => clicked_ (repr self)
	val enter_ : cptr -> unit = app1 (symb"mgtk_gtk_button_enter")
	val enter : 'a t -> unit = fn self => enter_ (repr self)
	val leave_ : cptr -> unit = app1 (symb"mgtk_gtk_button_leave")
	val leave : 'a t -> unit = fn self => leave_ (repr self)
	val set_relief_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_button_set_relief")
	val set_relief : 'a t -> relief_style -> unit
	    = fn self => fn newstyle => set_relief_ (repr self) newstyle
	val get_relief_ : cptr -> int = app1 (symb"mgtk_gtk_button_get_relief")
	val get_relief : 'a t -> relief_style
	    = fn self => get_relief_ (repr self)
	val set_label_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_button_set_label")
	val set_label : 'a t -> string -> unit
	    = fn self => fn label => set_label_ (repr self) label
	val get_label_ : cptr -> string
	    = app1 (symb"mgtk_gtk_button_get_label")
	val get_label : 'a t -> string = fn self => get_label_ (repr self)
	val set_use_underline_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_button_set_use_underline")
	val set_use_underline : 'a t -> bool -> unit
	    = fn self => fn use_underline =>
		 set_use_underline_ (repr self) use_underline
	val get_use_underline_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_button_get_use_underline")
	val get_use_underline : 'a t -> bool
	    = fn self => get_use_underline_ (repr self)
	val set_use_stock_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_button_set_use_stock")
	val set_use_stock : 'a t -> bool -> unit
	    = fn self => fn use_stock => set_use_stock_ (repr self) use_stock
	val get_use_stock_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_button_get_use_stock")
	val get_use_stock : 'a t -> bool
	    = fn self => get_use_stock_ (repr self)
	local open Signal
	      infixr -->
	in val clicked_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "clicked" false (void --> return_void) f
	   val pressed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "pressed" false (void --> return_void) f
	   val released_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "released" false (void --> return_void) f
	   val enter_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "enter" false (void --> return_void) f
	   val leave_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "leave" false (void --> return_void) f
	   val activate_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "activate" false (void --> return_void) f
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
	val set_state : 'a t -> bool -> unit
	val toggled_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a togglebutton_t = unit
	type 'a t = 'a togglebutton_t Button.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Button.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toToggleButton obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_toggle_button_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_toggle_button_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_label_ : string -> cptr
	    = app1 (symb"mgtk_gtk_toggle_button_new_with_label")
	val new_with_label : string -> base t
	    = fn label => make (new_with_label_ label)
	val new_with_mnemonic_ : string -> cptr
	    = app1 (symb"mgtk_gtk_toggle_button_new_with_mnemonic")
	val new_with_mnemonic : string -> base t
	    = fn label => make (new_with_mnemonic_ label)
	val set_mode_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_toggle_button_set_mode")
	val set_mode : 'a t -> bool -> unit
	    = fn self => fn draw_indicator =>
		 set_mode_ (repr self) draw_indicator
	val get_mode_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_toggle_button_get_mode")
	val get_mode : 'a t -> bool = fn self => get_mode_ (repr self)
	val set_active_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_toggle_button_set_active")
	val set_active : 'a t -> bool -> unit
	    = fn self => fn is_active => set_active_ (repr self) is_active
	val get_active_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_toggle_button_get_active")
	val get_active : 'a t -> bool = fn self => get_active_ (repr self)
	val toggled_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_toggle_button_toggled")
	val toggled : 'a t -> unit = fn self => toggled_ (repr self)
	val set_inconsistent_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_toggle_button_set_inconsistent")
	val set_inconsistent : 'a t -> bool -> unit
	    = fn self => fn setting => set_inconsistent_ (repr self) setting
	val get_inconsistent_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_toggle_button_get_inconsistent")
	val get_inconsistent : 'a t -> bool
	    = fn self => get_inconsistent_ (repr self)
	val set_state_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_toggle_button_set_state")
	val set_state : 'a t -> bool -> unit
	    = fn self => fn is_active => set_state_ (repr self) is_active
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a checkbutton_t = unit
	type 'a t = 'a checkbutton_t ToggleButton.t
	fun inherit w con
	  = let val con = let val ptr = con () in fn () => ptr end
		val witness = ()
	    in ToggleButton.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toCheckButton obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_check_button_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_check_button_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_label_ : string -> cptr
	    = app1 (symb"mgtk_gtk_check_button_new_with_label")
	val new_with_label : string -> base t
	    = fn label => make (new_with_label_ label)
	val new_with_mnemonic_ : string -> cptr
	    = app1 (symb"mgtk_gtk_check_button_new_with_mnemonic")
	val new_with_mnemonic : string -> base t
	    = fn label => make (new_with_mnemonic_ label)
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
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a radiobutton_t = unit
	type 'a t = 'a radiobutton_t CheckButton.t
	fun inherit w con
	  = let val con = let val ptr = con () in fn () => ptr end
		val witness = ()
	    in CheckButton.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toRadioButton obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_radio_button_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_from_widget_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_radio_button_new_from_widget")
	val new_from_widget : 'a t -> base t
	    = fn group => make (new_from_widget_ (repr group))
	val new_with_label_from_widget_ : cptr -> string -> cptr
	    = app2 (symb"mgtk_gtk_radio_button_new_with_label_from_widget")
	val new_with_label_from_widget : 'a t -> string -> base t
	    = fn group => fn label =>
		 make (new_with_label_from_widget_ (repr group) label)
	val new_with_mnemonic_from_widget_ : cptr -> string -> cptr
	    = app2 (symb"mgtk_gtk_radio_button_new_with_mnemonic_from_widget")
	val new_with_mnemonic_from_widget : 'a t -> string -> base t
	    = fn group => fn label =>
		 make (new_with_mnemonic_from_widget_ (repr group) label)
    end
    structure OptionMenu :>
      sig
	type base
	type 'a optionmenu_t
	type 'a t = 'a optionmenu_t Button.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toOptionMenu : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val get_menu : 'a t -> base Widget.t
	val set_menu : 'a t -> 'b Widget.t -> unit
	val remove_menu : 'a t -> unit
	val get_history : 'a t -> int
	val set_history : 'a t -> int -> unit
	val changed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a optionmenu_t = unit
	type 'a t = 'a optionmenu_t Button.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Button.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toOptionMenu obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_option_menu_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_option_menu_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_menu_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_option_menu_get_menu")
	val get_menu : 'a t -> base Widget.t
	    = fn self => Widget.inherit () (fn () => get_menu_ (repr self))
	val set_menu_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_option_menu_set_menu")
	val set_menu : 'a t -> 'b Widget.t -> unit
	    = fn self => fn menu => set_menu_ (repr self) (repr menu)
	val remove_menu_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_option_menu_remove_menu")
	val remove_menu : 'a t -> unit = fn self => remove_menu_ (repr self)
	val get_history_ : cptr -> int
	    = app1 (symb"mgtk_gtk_option_menu_get_history")
	val get_history : 'a t -> int = fn self => get_history_ (repr self)
	val set_history_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_option_menu_set_history")
	val set_history : 'a t -> int -> unit
	    = fn self => fn index => set_history_ (repr self) index
	local open Signal
	      infixr -->
	in val changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "changed" false (void --> return_void) f
	end
    end
    structure Box :>
      sig
	type base
	type 'a box_t
	type 'a t = 'a box_t Container.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toBox : 'a t -> base t
	val get_type : unit -> GType.t
	val pack_start
	  : 'a t -> 'b Widget.t -> bool option -> bool option -> int option
	    -> unit
	val pack_start' : 'a t -> 'b Widget.t -> unit
	val pack_end : 'a t -> 'b Widget.t -> bool option -> bool option 
		    -> int option
		       -> unit
	val pack_end' : 'a t -> 'b Widget.t -> unit
	val pack_start_defaults : 'a t -> 'b Widget.t -> unit
	val pack_end_defaults : 'a t -> 'b Widget.t -> unit
	val set_homogeneous : 'a t -> bool -> unit
	val get_homogeneous : 'a t -> bool
	val set_spacing : 'a t -> int -> unit
	val get_spacing : 'a t -> int
	val reorder_child : 'a t -> 'b Widget.t -> int -> unit
	val set_child_packing
	  : 'a t -> 'b Widget.t -> bool -> bool -> int -> packtype -> unit
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a box_t = unit
	type 'a t = 'a box_t Container.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Container.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toBox obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_box_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val pack_start_ : cptr -> cptr -> bool -> bool -> int -> unit
	    = app5 (symb"mgtk_gtk_box_pack_start")
	val pack_start
	  : 'a t -> 'b Widget.t -> bool option -> bool option -> int option
	    -> unit
	    = fn self => fn child => fn expand => fn fill => fn padding =>
		 pack_start_ (repr self) (repr child) (getOpt (expand, true))
			     (getOpt (fill, true)) (getOpt (padding, 0))
	val pack_start' : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child =>
		 pack_start_ (repr self) (repr child) true true 0
	val pack_end_ : cptr -> cptr -> bool -> bool -> int -> unit
	    = app5 (symb"mgtk_gtk_box_pack_end")
	val pack_end : 'a t -> 'b Widget.t -> bool option -> bool option 
		    -> int option
		       -> unit
	    = fn self => fn child => fn expand => fn fill => fn padding =>
		 pack_end_ (repr self) (repr child) (getOpt (expand, true))
			   (getOpt (fill, true)) (getOpt (padding, 0))
	val pack_end' : 'a t -> 'b Widget.t -> unit
	    = fn self => fn child =>
		 pack_end_ (repr self) (repr child) true true 0
	val pack_start_defaults_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_box_pack_start_defaults")
	val pack_start_defaults : 'a t -> 'b Widget.t -> unit
	    = fn self => fn widget =>
		 pack_start_defaults_ (repr self) (repr widget)
	val pack_end_defaults_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_box_pack_end_defaults")
	val pack_end_defaults : 'a t -> 'b Widget.t -> unit
	    = fn self => fn widget =>
		 pack_end_defaults_ (repr self) (repr widget)
	val set_homogeneous_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_box_set_homogeneous")
	val set_homogeneous : 'a t -> bool -> unit
	    = fn self => fn homogeneous =>
		 set_homogeneous_ (repr self) homogeneous
	val get_homogeneous_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_box_get_homogeneous")
	val get_homogeneous : 'a t -> bool
	    = fn self => get_homogeneous_ (repr self)
	val set_spacing_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_box_set_spacing")
	val set_spacing : 'a t -> int -> unit
	    = fn self => fn spacing => set_spacing_ (repr self) spacing
	val get_spacing_ : cptr -> int = app1 (symb"mgtk_gtk_box_get_spacing")
	val get_spacing : 'a t -> int = fn self => get_spacing_ (repr self)
	val reorder_child_ : cptr -> cptr -> int -> unit
	    = app3 (symb"mgtk_gtk_box_reorder_child")
	val reorder_child : 'a t -> 'b Widget.t -> int -> unit
	    = fn self => fn child => fn position =>
		 reorder_child_ (repr self) (repr child) position
	val set_child_packing_ : cptr * cptr * bool * bool * int * int -> unit
	    = app1 (symb"mgtk_gtk_box_set_child_packing")
	val set_child_packing
	  : 'a t -> 'b Widget.t -> bool -> bool -> int -> packtype -> unit
	    = fn self => fn child => fn expand => fn fill => fn padding => 
	      fn pack_type =>
		 set_child_packing_ (repr self, repr child, expand, fill, 
				     padding, pack_type)
    end
    structure VBox :>
      sig
	type base
	type 'a vbox_t
	type 'a t = 'a vbox_t Box.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toVBox : 'a t -> base t
	val get_type : unit -> GType.t
	val new : bool option -> int option -> base t
	val new' : unit -> base t
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a vbox_t = unit
	type 'a t = 'a vbox_t Box.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Box.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toVBox obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_vbox_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : bool -> int -> cptr = app2 (symb"mgtk_gtk_vbox_new")
	val new : bool option -> int option -> base t
	    = fn homogeneous => fn spacing =>
		 make (new_ (getOpt (homogeneous, false))
			    (getOpt (spacing, 0)))
	val new' : unit -> base t = fn dummy => make (new_ false 0)
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
	val set_update_policy : 'a t -> updatetype -> unit
	val dialog_get_type : unit -> GType.t
	val color_changed_sig : (unit -> unit) -> 'a t Signal.signal
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a colorselection_t = unit
	type 'a t = 'a colorselection_t VBox.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in VBox.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toColorSelection obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_color_selection_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_color_selection_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_has_opacity_control_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_color_selection_get_has_opacity_control")
	val get_has_opacity_control : 'a t -> bool
	    = fn self => get_has_opacity_control_ (repr self)
	val set_has_opacity_control_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_color_selection_set_has_opacity_control")
	val set_has_opacity_control : 'a t -> bool -> unit
	    = fn self => fn has_opacity =>
		 set_has_opacity_control_ (repr self) has_opacity
	val get_has_palette_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_color_selection_get_has_palette")
	val get_has_palette : 'a t -> bool
	    = fn self => get_has_palette_ (repr self)
	val set_has_palette_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_color_selection_set_has_palette")
	val set_has_palette : 'a t -> bool -> unit
	    = fn self => fn has_palette =>
		 set_has_palette_ (repr self) has_palette
	val set_current_alpha_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_color_selection_set_current_alpha")
	val set_current_alpha : 'a t -> int -> unit
	    = fn self => fn alpha => set_current_alpha_ (repr self) alpha
	val get_current_alpha_ : cptr -> int
	    = app1 (symb"mgtk_gtk_color_selection_get_current_alpha")
	val get_current_alpha : 'a t -> int
	    = fn self => get_current_alpha_ (repr self)
	val set_previous_alpha_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_color_selection_set_previous_alpha")
	val set_previous_alpha : 'a t -> int -> unit
	    = fn self => fn alpha => set_previous_alpha_ (repr self) alpha
	val get_previous_alpha_ : cptr -> int
	    = app1 (symb"mgtk_gtk_color_selection_get_previous_alpha")
	val get_previous_alpha : 'a t -> int
	    = fn self => get_previous_alpha_ (repr self)
	val is_adjusting_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_color_selection_is_adjusting")
	val is_adjusting : 'a t -> bool = fn self => is_adjusting_ (repr self)
	val set_update_policy_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_color_selection_set_update_policy")
	val set_update_policy : 'a t -> updatetype -> unit
	    = fn self => fn policy => set_update_policy_ (repr self) policy
	val dialog_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_color_selection_dialog_get_type")
	val dialog_get_type : unit -> GType.t
	    = fn dummy => dialog_get_type_ dummy
	local open Signal
	      infixr -->
	in val color_changed_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "color-changed" false (void --> return_void) f
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a fontselection_t = unit
	type 'a t = 'a fontselection_t VBox.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in VBox.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toFontSelection obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_font_selection_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_font_selection_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_font_name_ : cptr -> string
	    = app1 (symb"mgtk_gtk_font_selection_get_font_name")
	val get_font_name : 'a t -> string
	    = fn self => get_font_name_ (repr self)
	val set_font_name_ : cptr -> string -> bool
	    = app2 (symb"mgtk_gtk_font_selection_set_font_name")
	val set_font_name : 'a t -> string -> bool
	    = fn self => fn fontname => set_font_name_ (repr self) fontname
	val get_preview_text_ : cptr -> string
	    = app1 (symb"mgtk_gtk_font_selection_get_preview_text")
	val get_preview_text : 'a t -> string
	    = fn self => get_preview_text_ (repr self)
	val set_preview_text_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_font_selection_set_preview_text")
	val set_preview_text : 'a t -> string -> unit
	    = fn self => fn text => set_preview_text_ (repr self) text
	val dialog_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_font_selection_dialog_get_type")
	val dialog_get_type : unit -> GType.t
	    = fn dummy => dialog_get_type_ dummy
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a gammacurve_t = unit
	type 'a t = 'a gammacurve_t VBox.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in VBox.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toGammaCurve obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_gamma_curve_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_gamma_curve_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
    end
    structure HBox :>
      sig
	type base
	type 'a hbox_t
	type 'a t = 'a hbox_t Box.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toHBox : 'a t -> base t
	val get_type : unit -> GType.t
	val new : bool option -> int option -> base t
	val new' : unit -> base t
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a hbox_t = unit
	type 'a t = 'a hbox_t Box.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Box.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toHBox obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_hbox_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : bool -> int -> cptr = app2 (symb"mgtk_gtk_hbox_new")
	val new : bool option -> int option -> base t
	    = fn homogeneous => fn spacing =>
		 make (new_ (getOpt (homogeneous, false))
			    (getOpt (spacing, 0)))
	val new' : unit -> base t = fn dummy => make (new_ false 0)
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a statusbar_t = unit
	type 'a t = 'a statusbar_t HBox.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in HBox.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toStatusbar obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_statusbar_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_statusbar_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_context_id_ : cptr -> string -> int
	    = app2 (symb"mgtk_gtk_statusbar_get_context_id")
	val get_context_id : 'a t -> string -> int
	    = fn self => fn context_description =>
		 get_context_id_ (repr self) context_description
	val push_ : cptr -> int -> string -> int
	    = app3 (symb"mgtk_gtk_statusbar_push")
	val push : 'a t -> int -> string -> int
	    = fn self => fn context_id => fn text =>
		 push_ (repr self) context_id text
	val pop_ : cptr -> int -> unit = app2 (symb"mgtk_gtk_statusbar_pop")
	val pop : 'a t -> int -> unit
	    = fn self => fn context_id => pop_ (repr self) context_id
	val remove_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_statusbar_remove")
	val remove : 'a t -> int -> int -> unit
	    = fn self => fn context_id => fn message_id =>
		 remove_ (repr self) context_id message_id
	val set_has_resize_grip_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_statusbar_set_has_resize_grip")
	val set_has_resize_grip : 'a t -> bool -> unit
	    = fn self => fn setting => set_has_resize_grip_ (repr self) setting
	val get_has_resize_grip_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_statusbar_get_has_resize_grip")
	val get_has_resize_grip : 'a t -> bool
	    = fn self => get_has_resize_grip_ (repr self)
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
    structure Combo :>
      sig
	type base
	type 'a combo_t
	type 'a t = 'a combo_t HBox.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toCombo : 'a t -> base t
	val get_type : unit -> GType.t
	val new : unit -> base t
	val set_value_in_list : 'a t -> bool -> bool -> unit
	val set_use_arrows : 'a t -> bool -> unit
	val set_use_arrows_always : 'a t -> bool -> unit
	val set_case_sensitive : 'a t -> bool -> unit
	val set_item_string : 'a t -> 'b Item.t -> string -> unit
	val disable_activate : 'a t -> unit
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a combo_t = unit
	type 'a t = 'a combo_t HBox.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in HBox.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toCombo obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_combo_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_combo_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val set_value_in_list_ : cptr -> bool -> bool -> unit
	    = app3 (symb"mgtk_gtk_combo_set_value_in_list")
	val set_value_in_list : 'a t -> bool -> bool -> unit
	    = fn self => fn valu => fn ok_if_empty =>
		 set_value_in_list_ (repr self) valu ok_if_empty
	val set_use_arrows_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_combo_set_use_arrows")
	val set_use_arrows : 'a t -> bool -> unit
	    = fn self => fn valu => set_use_arrows_ (repr self) valu
	val set_use_arrows_always_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_combo_set_use_arrows_always")
	val set_use_arrows_always : 'a t -> bool -> unit
	    = fn self => fn valu => set_use_arrows_always_ (repr self) valu
	val set_case_sensitive_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_combo_set_case_sensitive")
	val set_case_sensitive : 'a t -> bool -> unit
	    = fn self => fn valu => set_case_sensitive_ (repr self) valu
	val set_item_string_ : cptr -> cptr -> string -> unit
	    = app3 (symb"mgtk_gtk_combo_set_item_string")
	val set_item_string : 'a t -> 'b Item.t -> string -> unit
	    = fn self => fn item => fn item_value =>
		 set_item_string_ (repr self) (repr item) item_value
	val disable_activate_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_combo_disable_activate")
	val disable_activate : 'a t -> unit
	    = fn self => disable_activate_ (repr self)
    end
    structure ButtonBox :>
      sig
	type base
	type 'a buttonbox_t
	type 'a t = 'a buttonbox_t Box.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toButtonBox : 'a t -> base t
	val set_child_secondary : 'a t -> 'b Widget.t -> bool -> unit
	val set_child_size : 'a t -> int -> int -> unit
	val set_child_ipadding : 'a t -> int -> int -> unit
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a buttonbox_t = unit
	type 'a t = 'a buttonbox_t Box.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Box.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toButtonBox obj = inherit () (fn () => repr obj)
	val set_child_secondary_ : cptr -> cptr -> bool -> unit
	    = app3 (symb"mgtk_gtk_button_box_set_child_secondary")
	val set_child_secondary : 'a t -> 'b Widget.t -> bool -> unit
	    = fn self => fn child => fn is_secondary =>
		 set_child_secondary_ (repr self) (repr child) is_secondary
	val set_child_size_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_button_box_set_child_size")
	val set_child_size : 'a t -> int -> int -> unit
	    = fn self => fn min_width => fn min_height =>
		 set_child_size_ (repr self) min_width min_height
	val set_child_ipadding_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_button_box_set_child_ipadding")
	val set_child_ipadding : 'a t -> int -> int -> unit
	    = fn self => fn ipad_x => fn ipad_y =>
		 set_child_ipadding_ (repr self) ipad_x ipad_y
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
	val get_spacing_default : unit -> int
	val set_spacing_default : int -> unit
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a vbuttonbox_t = unit
	type 'a t = 'a vbuttonbox_t ButtonBox.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in ButtonBox.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toVButtonBox obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_vbutton_box_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_vbutton_box_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_spacing_default_ : unit -> int
	    = app1 (symb"mgtk_gtk_vbutton_box_get_spacing_default")
	val get_spacing_default : unit -> int
	    = fn dummy => get_spacing_default_ dummy
	val set_spacing_default_ : int -> unit
	    = app1 (symb"mgtk_gtk_vbutton_box_set_spacing_default")
	val set_spacing_default : int -> unit
	    = fn spacing => set_spacing_default_ spacing
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
	val get_spacing_default : unit -> int
	val set_spacing_default : int -> unit
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a hbuttonbox_t = unit
	type 'a t = 'a hbuttonbox_t ButtonBox.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in ButtonBox.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toHButtonBox obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_hbutton_box_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_hbutton_box_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val get_spacing_default_ : unit -> int
	    = app1 (symb"mgtk_gtk_hbutton_box_get_spacing_default")
	val get_spacing_default : unit -> int
	    = fn dummy => get_spacing_default_ dummy
	val set_spacing_default_ : int -> unit
	    = app1 (symb"mgtk_gtk_hbutton_box_set_spacing_default")
	val set_spacing_default : int -> unit
	    = fn spacing => set_spacing_default_ spacing
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a calendar_t = unit
	type 'a t = 'a calendar_t Widget.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Widget.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toCalendar obj = inherit () (fn () => repr obj)
	type display_options = int
	val get_display_options_ : unit -> int * int * int * int * int
	    = app1 (symb"mgtk_get_gtk_calendar_display_options")
	val (SHOW_HEADING, SHOW_DAY_NAMES, NO_MONTH_CHANGE, SHOW_WEEK_NUMBERS, 
	     WEEK_START_MONDAY)
	    = get_display_options_ ()
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_calendar_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_calendar_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val select_month_ : cptr -> int -> int -> bool
	    = app3 (symb"mgtk_gtk_calendar_select_month")
	val select_month : 'a t -> int -> int -> bool
	    = fn self => fn month => fn year =>
		 select_month_ (repr self) month year
	val select_day_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_calendar_select_day")
	val select_day : 'a t -> int -> unit
	    = fn self => fn day => select_day_ (repr self) day
	val mark_day_ : cptr -> int -> bool
	    = app2 (symb"mgtk_gtk_calendar_mark_day")
	val mark_day : 'a t -> int -> bool
	    = fn self => fn day => mark_day_ (repr self) day
	val unmark_day_ : cptr -> int -> bool
	    = app2 (symb"mgtk_gtk_calendar_unmark_day")
	val unmark_day : 'a t -> int -> bool
	    = fn self => fn day => unmark_day_ (repr self) day
	val clear_marks_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_calendar_clear_marks")
	val clear_marks : 'a t -> unit = fn self => clear_marks_ (repr self)
	val display_options_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_calendar_display_options")
	val display_options : 'a t -> display_options list -> unit
	    = fn self => fn flags =>
		 display_options_ (repr self) (Flags.set flags)
	val freeze_ : cptr -> unit = app1 (symb"mgtk_gtk_calendar_freeze")
	val freeze : 'a t -> unit = fn self => freeze_ (repr self)
	val thaw_ : cptr -> unit = app1 (symb"mgtk_gtk_calendar_thaw")
	val thaw : 'a t -> unit = fn self => thaw_ (repr self)
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
	val get_type : unit -> GType.t
	val new : type_t option -> base t
	val new' : unit -> base t
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
	val set_destroy_with_parent : 'a t -> bool -> unit
	val get_destroy_with_parent : 'a t -> bool
	val set_resizable : 'a t -> bool -> unit
	val get_resizable : 'a t -> bool
	val set_has_frame : 'a t -> bool -> unit
	val get_has_frame : 'a t -> bool
	val set_frame_dimensions : 'a t -> int -> int -> int -> int -> unit
	val set_decorated : 'a t -> bool -> unit
	val get_decorated : 'a t -> bool
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
	val begin_move_drag : 'a t -> int -> int -> int -> int -> unit
	val set_policy : 'a t -> int -> int -> int -> unit
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a window_t = unit
	type 'a t = 'a window_t Bin.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Bin.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toWindow obj = inherit () (fn () => repr obj)
	type position = int
	val get_position_ : unit -> int * int * int * int * int
	    = app1 (symb"mgtk_get_gtk_window_position")
	val (WIN_POS_NONE, WIN_POS_CENTER, WIN_POS_MOUSE, 
	     WIN_POS_CENTER_ALWAYS, WIN_POS_CENTER_ON_PARENT)
	    = get_position_ ()
	type type_t = int
	val get_type_t_ : unit -> int * int
	    = app1 (symb"mgtk_get_gtk_window_type")
	val (TOPLEVEL, POPUP) = get_type_t_ ()
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_window_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : int -> cptr = app1 (symb"mgtk_gtk_window_new")
	val new : type_t option -> base t
	    = fn typ => make (new_ (getOpt (typ, TOPLEVEL)))
	val new' : unit -> base t = fn dummy => make (new_ TOPLEVEL)
	val set_title_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_window_set_title")
	val set_title : 'a t -> string -> unit
	    = fn self => fn title => set_title_ (repr self) title
	val get_title_ : cptr -> string
	    = app1 (symb"mgtk_gtk_window_get_title")
	val get_title : 'a t -> string = fn self => get_title_ (repr self)
	val set_wmclass_ : cptr -> string -> string -> unit
	    = app3 (symb"mgtk_gtk_window_set_wmclass")
	val set_wmclass : 'a t -> string -> string -> unit
	    = fn self => fn wmclass_name => fn wmclass_class =>
		 set_wmclass_ (repr self) wmclass_name wmclass_class
	val set_role_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_window_set_role")
	val set_role : 'a t -> string -> unit
	    = fn self => fn role => set_role_ (repr self) role
	val get_role_ : cptr -> string = app1 (symb"mgtk_gtk_window_get_role")
	val get_role : 'a t -> string = fn self => get_role_ (repr self)
	val add_accelgroup_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_window_add_accelgroup")
	val add_accelgroup : 'a t -> 'b AccelGroup.t -> unit
	    = fn self => fn accel_group =>
		 add_accelgroup_ (repr self) (repr accel_group)
	val remove_accelgroup_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_window_remove_accelgroup")
	val remove_accelgroup : 'a t -> 'b AccelGroup.t -> unit
	    = fn self => fn accel_group =>
		 remove_accelgroup_ (repr self) (repr accel_group)
	val set_position_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_window_set_position")
	val set_position : 'a t -> position -> unit
	    = fn self => fn position => set_position_ (repr self) position
	val activate_focus_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_window_activate_focus")
	val activate_focus : 'a t -> bool
	    = fn self => activate_focus_ (repr self)
	val set_focus_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_window_set_focus")
	val set_focus : 'a t -> 'b Widget.t option -> unit
	    = fn self => fn focus =>
		 set_focus_ (repr self)
			    (getOpt (Option.map repr focus, GObject.null))
	val set_focus' : 'a t -> unit
	    = fn self => set_focus_ (repr self) GObject.null
	val get_focus_ : cptr -> cptr = app1 (symb"mgtk_gtk_window_get_focus")
	val get_focus : 'a t -> base Widget.t
	    = fn self => Widget.inherit () (fn () => get_focus_ (repr self))
	val set_default_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_window_set_default")
	val set_default : 'a t -> 'b Widget.t option -> unit
	    = fn self => fn default_widget =>
		 set_default_ (repr self)
			      (getOpt (Option.map repr default_widget, 
				       GObject.null))
	val set_default' : 'a t -> unit
	    = fn self => set_default_ (repr self) GObject.null
	val activate_default_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_window_activate_default")
	val activate_default : 'a t -> bool
	    = fn self => activate_default_ (repr self)
	val set_transient_for_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_window_set_transient_for")
	val set_transient_for : 'a t -> 'b t option -> unit
	    = fn self => fn parent =>
		 set_transient_for_
		   (repr self) (getOpt (Option.map repr parent, GObject.null))
	val set_transient_for' : 'a t -> unit
	    = fn self => set_transient_for_ (repr self) GObject.null
	val get_transient_for_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_window_get_transient_for")
	val get_transient_for : 'a t -> base t
	    = fn self => make (get_transient_for_ (repr self))
	val set_destroy_with_parent_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_window_set_destroy_with_parent")
	val set_destroy_with_parent : 'a t -> bool -> unit
	    = fn self => fn setting =>
		 set_destroy_with_parent_ (repr self) setting
	val get_destroy_with_parent_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_window_get_destroy_with_parent")
	val get_destroy_with_parent : 'a t -> bool
	    = fn self => get_destroy_with_parent_ (repr self)
	val set_resizable_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_window_set_resizable")
	val set_resizable : 'a t -> bool -> unit
	    = fn self => fn resizable => set_resizable_ (repr self) resizable
	val get_resizable_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_window_get_resizable")
	val get_resizable : 'a t -> bool
	    = fn self => get_resizable_ (repr self)
	val set_has_frame_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_window_set_has_frame")
	val set_has_frame : 'a t -> bool -> unit
	    = fn self => fn setting => set_has_frame_ (repr self) setting
	val get_has_frame_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_window_get_has_frame")
	val get_has_frame : 'a t -> bool
	    = fn self => get_has_frame_ (repr self)
	val set_frame_dimensions_ : cptr -> int -> int -> int -> int -> unit
	    = app5 (symb"mgtk_gtk_window_set_frame_dimensions")
	val set_frame_dimensions : 'a t -> int -> int -> int -> int -> unit
	    = fn self => fn left => fn top => fn right => fn bottom =>
		 set_frame_dimensions_ (repr self) left top right bottom
	val set_decorated_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_window_set_decorated")
	val set_decorated : 'a t -> bool -> unit
	    = fn self => fn setting => set_decorated_ (repr self) setting
	val get_decorated_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_window_get_decorated")
	val get_decorated : 'a t -> bool
	    = fn self => get_decorated_ (repr self)
	val set_modal_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_window_set_modal")
	val set_modal : 'a t -> bool -> unit
	    = fn self => fn modal => set_modal_ (repr self) modal
	val get_modal_ : cptr -> bool = app1 (symb"mgtk_gtk_window_get_modal")
	val get_modal : 'a t -> bool = fn self => get_modal_ (repr self)
	val add_mnemonic_ : cptr -> int -> cptr -> unit
	    = app3 (symb"mgtk_gtk_window_add_mnemonic")
	val add_mnemonic : 'a t -> int -> 'b Widget.t -> unit
	    = fn self => fn keyval => fn target =>
		 add_mnemonic_ (repr self) keyval (repr target)
	val remove_mnemonic_ : cptr -> int -> cptr -> unit
	    = app3 (symb"mgtk_gtk_window_remove_mnemonic")
	val remove_mnemonic : 'a t -> int -> 'b Widget.t -> unit
	    = fn self => fn keyval => fn target =>
		 remove_mnemonic_ (repr self) keyval (repr target)
	val present_ : cptr -> unit = app1 (symb"mgtk_gtk_window_present")
	val present : 'a t -> unit = fn self => present_ (repr self)
	val iconify_ : cptr -> unit = app1 (symb"mgtk_gtk_window_iconify")
	val iconify : 'a t -> unit = fn self => iconify_ (repr self)
	val deiconify_ : cptr -> unit = app1 (symb"mgtk_gtk_window_deiconify")
	val deiconify : 'a t -> unit = fn self => deiconify_ (repr self)
	val stick_ : cptr -> unit = app1 (symb"mgtk_gtk_window_stick")
	val stick : 'a t -> unit = fn self => stick_ (repr self)
	val unstick_ : cptr -> unit = app1 (symb"mgtk_gtk_window_unstick")
	val unstick : 'a t -> unit = fn self => unstick_ (repr self)
	val maximize_ : cptr -> unit = app1 (symb"mgtk_gtk_window_maximize")
	val maximize : 'a t -> unit = fn self => maximize_ (repr self)
	val unmaximize_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_window_unmaximize")
	val unmaximize : 'a t -> unit = fn self => unmaximize_ (repr self)
	val begin_move_drag_ : cptr -> int -> int -> int -> int -> unit
	    = app5 (symb"mgtk_gtk_window_begin_move_drag")
	val begin_move_drag : 'a t -> int -> int -> int -> int -> unit
	    = fn self => fn button => fn root_x => fn root_y => fn timestamp =>
		 begin_move_drag_ (repr self) button root_x root_y timestamp
	val set_policy_ : cptr -> int -> int -> int -> unit
	    = app4 (symb"mgtk_gtk_window_set_policy")
	val set_policy : 'a t -> int -> int -> int -> unit
	    = fn self => fn allow_shrink => fn allow_grow => fn auto_shrink =>
		 set_policy_ (repr self) allow_shrink allow_grow auto_shrink
	val set_default_size_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_window_set_default_size")
	val set_default_size : 'a t -> int -> int -> unit
	    = fn self => fn width => fn height =>
		 set_default_size_ (repr self) width height
	val get_default_size_ : cptr -> int ref -> int ref -> unit
	    = app3 (symb"mgtk_gtk_window_get_default_size")
	val get_default_size : 'a t -> int * int
	    = fn self => let val (width, height) = (ref 0, ref 0)
			     val ret = get_default_size_
					 (repr self) width height
			 in (!width, !height) end
	val resize_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_window_resize")
	val resize : 'a t -> int -> int -> unit
	    = fn self => fn width => fn height =>
		 resize_ (repr self) width height
	val get_size_ : cptr -> int ref -> int ref -> unit
	    = app3 (symb"mgtk_gtk_window_get_size")
	val get_size : 'a t -> int * int
	    = fn self => let val (width, height) = (ref 0, ref 0)
			     val ret = get_size_ (repr self) width height
			 in (!width, !height) end
	val move_ : cptr -> int -> int -> unit
	    = app3 (symb"mgtk_gtk_window_move")
	val move : 'a t -> int -> int -> unit
	    = fn self => fn x => fn y => move_ (repr self) x y
	val get_position_ : cptr -> int ref -> int ref -> unit
	    = app3 (symb"mgtk_gtk_window_get_position")
	val get_position : 'a t -> int * int
	    = fn self => let val (root_x, root_y) = (ref 0, ref 0)
			     val ret = get_position_ (repr self) root_x root_y
			 in (!root_x, !root_y) end
	val parse_geometry_ : cptr -> string -> bool
	    = app2 (symb"mgtk_gtk_window_parse_geometry")
	val parse_geometry : 'a t -> string -> bool
	    = fn self => fn geometry => parse_geometry_ (repr self) geometry
	val reshow_with_initial_size_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_window_reshow_with_initial_size")
	val reshow_with_initial_size : 'a t -> unit
	    = fn self => reshow_with_initial_size_ (repr self)
	val group_get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_window_group_get_type")
	val group_get_type : unit -> GType.t
	    = fn dummy => group_get_type_ dummy
	val remove_embedded_xid_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_window_remove_embedded_xid")
	val remove_embedded_xid : 'a t -> int -> unit
	    = fn self => fn xid => remove_embedded_xid_ (repr self) xid
	val add_embedded_xid_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_window_add_embedded_xid")
	val add_embedded_xid : 'a t -> int -> unit
	    = fn self => fn xid => add_embedded_xid_ (repr self) xid
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a plug_t = unit
	type 'a t = 'a plug_t Window.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Window.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toPlug obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_plug_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	local open Signal
	      infixr -->
	in val embedded_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "embedded" false (void --> return_void) f
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
			    -> flags list option -> string option
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a dialog_t = unit
	type 'a t = 'a dialog_t Window.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Window.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toDialog obj = inherit () (fn () => repr obj)
	type flags = int
	val get_flags_ : unit -> int * int * int
	    = app1 (symb"mgtk_get_gtk_dialog_flags")
	val (MODAL, DESTROY_WITH_PARENT, NO_SEPARATOR) = get_flags_ ()
	val get_type_ : unit -> GType.t = app1 (symb"mgtk_gtk_dialog_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_dialog_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val new_with_buttons_ : string -> cptr -> int -> string -> cptr
	    = app4 (symb"mgtk_gtk_dialog_new_with_buttons")
	val new_with_buttons : string option -> 'a Window.t option 
			    -> flags list option -> string option
			       -> base t
	    = fn title => fn parent => fn flags => fn first_button_text =>
		 make (new_with_buttons_
			 (getOpt (title, ""))
			 (getOpt (Option.map repr parent, GObject.null))
			 (getOpt (Option.map Flags.set flags, 0))
			 (getOpt (first_button_text, "")))
	val new_with_buttons' : unit -> base t
	    = fn dummy => make (new_with_buttons_ "" GObject.null 0 "")
	val add_action_widget_ : cptr -> cptr -> int -> unit
	    = app3 (symb"mgtk_gtk_dialog_add_action_widget")
	val add_action_widget : 'a t -> 'b Widget.t -> int -> unit
	    = fn self => fn child => fn response_id =>
		 add_action_widget_ (repr self) (repr child) response_id
	val add_button_ : cptr -> string -> int -> cptr
	    = app3 (symb"mgtk_gtk_dialog_add_button")
	val add_button : 'a t -> string -> int -> base Widget.t
	    = fn self => fn button_text => fn response_id =>
		 Widget.inherit
		   ()
		   (fn () => add_button_ (repr self) button_text response_id)
	val add_buttons_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_dialog_add_buttons")
	val add_buttons : 'a t -> string -> unit
	    = fn self => fn first_button_text =>
		 add_buttons_ (repr self) first_button_text
	val set_response_sensitive_ : cptr -> int -> bool -> unit
	    = app3 (symb"mgtk_gtk_dialog_set_response_sensitive")
	val set_response_sensitive : 'a t -> int -> bool -> unit
	    = fn self => fn response_id => fn setting =>
		 set_response_sensitive_ (repr self) response_id setting
	val set_default_response_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_dialog_set_default_response")
	val set_default_response : 'a t -> int -> unit
	    = fn self => fn response_id =>
		 set_default_response_ (repr self) response_id
	val set_has_separator_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_dialog_set_has_separator")
	val set_has_separator : 'a t -> bool -> unit
	    = fn self => fn setting => set_has_separator_ (repr self) setting
	val get_has_separator_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_dialog_get_has_separator")
	val get_has_separator : 'a t -> bool
	    = fn self => get_has_separator_ (repr self)
	val response_ : cptr -> int -> unit
	    = app2 (symb"mgtk_gtk_dialog_response")
	val response : 'a t -> int -> unit
	    = fn self => fn response_id => response_ (repr self) response_id
	val run_ : cptr -> int = app1 (symb"mgtk_gtk_dialog_run")
	val run : 'a t -> int = fn self => run_ (repr self)
	local open Signal
	      infixr -->
	in val response_sig : (int -> unit) -> 'a t Signal.signal
	       = fn f => signal "response" false (int --> return_void) f
	   val close_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "close" false (void --> return_void) f
	end
    end
    structure MessageDialog :>
      sig
	type base
	type 'a messagedialog_t
	type 'a t = 'a messagedialog_t Dialog.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toMessageDialog : 'a t -> base t
	val get_type : unit -> GType.t
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a messagedialog_t = unit
	type 'a t = 'a messagedialog_t Dialog.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Dialog.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toMessageDialog obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_message_dialog_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a inputdialog_t = unit
	type 'a t = 'a inputdialog_t Dialog.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Dialog.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toInputDialog obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_input_dialog_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_input_dialog_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	local open Signal
	      infixr -->
	in val enable_device_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "enable-device" false (unit --> return_void) f
	   val disable_device_sig : (unit -> unit) -> 'a t Signal.signal
	       = fn f => signal "disable-device" false (unit --> return_void) f
	end
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a fontselectiondialog_t = unit
	type 'a t = 'a fontselectiondialog_t Dialog.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Dialog.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toFontSelectionDialog obj = inherit () (fn () => repr obj)
	val new_ : string -> cptr
	    = app1 (symb"mgtk_gtk_font_selection_dialog_new")
	val new : string -> base t = fn title => make (new_ title)
	val get_font_name_ : cptr -> string
	    = app1 (symb"mgtk_gtk_font_selection_dialog_get_font_name")
	val get_font_name : 'a t -> string
	    = fn self => get_font_name_ (repr self)
	val set_font_name_ : cptr -> string -> bool
	    = app2 (symb"mgtk_gtk_font_selection_dialog_set_font_name")
	val set_font_name : 'a t -> string -> bool
	    = fn self => fn fontname => set_font_name_ (repr self) fontname
	val get_preview_text_ : cptr -> string
	    = app1 (symb"mgtk_gtk_font_selection_dialog_get_preview_text")
	val get_preview_text : 'a t -> string
	    = fn self => get_preview_text_ (repr self)
	val set_preview_text_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_font_selection_dialog_set_preview_text")
	val set_preview_text : 'a t -> string -> unit
	    = fn self => fn text => set_preview_text_ (repr self) text
    end
    structure FileSelection :>
      sig
	type base
	type 'a fileselection_t
	type 'a t = 'a fileselection_t Dialog.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toFileSelection : 'a t -> base t
	val get_type : unit -> GType.t
	val new : string option -> base t
	val new' : unit -> base t
	val set_filename : 'a t -> string -> unit
	val get_filename : 'a t -> string
	val complete : 'a t -> string -> unit
	val show_fileop_buttons : 'a t -> unit
	val hide_fileop_buttons : 'a t -> unit
	val set_select_multiple : 'a t -> bool -> unit
	val get_select_multiple : 'a t -> bool
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a fileselection_t = unit
	type 'a t = 'a fileselection_t Dialog.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Dialog.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toFileSelection obj = inherit () (fn () => repr obj)
	val get_type_ : unit -> GType.t
	    = app1 (symb"mgtk_gtk_file_selection_get_type")
	val get_type : unit -> GType.t = fn dummy => get_type_ dummy
	val new_ : string -> cptr = app1 (symb"mgtk_gtk_file_selection_new")
	val new : string option -> base t
	    = fn title => make (new_ (getOpt (title, "")))
	val new' : unit -> base t = fn dummy => make (new_ "")
	val set_filename_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_file_selection_set_filename")
	val set_filename : 'a t -> string -> unit
	    = fn self => fn filename => set_filename_ (repr self) filename
	val get_filename_ : cptr -> string
	    = app1 (symb"mgtk_gtk_file_selection_get_filename")
	val get_filename : 'a t -> string
	    = fn self => get_filename_ (repr self)
	val complete_ : cptr -> string -> unit
	    = app2 (symb"mgtk_gtk_file_selection_complete")
	val complete : 'a t -> string -> unit
	    = fn self => fn pattern => complete_ (repr self) pattern
	val show_fileop_buttons_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_file_selection_show_fileop_buttons")
	val show_fileop_buttons : 'a t -> unit
	    = fn self => show_fileop_buttons_ (repr self)
	val hide_fileop_buttons_ : cptr -> unit
	    = app1 (symb"mgtk_gtk_file_selection_hide_fileop_buttons")
	val hide_fileop_buttons : 'a t -> unit
	    = fn self => hide_fileop_buttons_ (repr self)
	val set_select_multiple_ : cptr -> bool -> unit
	    = app2 (symb"mgtk_gtk_file_selection_set_select_multiple")
	val set_select_multiple : 'a t -> bool -> unit
	    = fn self => fn select_multiple =>
		 set_select_multiple_ (repr self) select_multiple
	val get_select_multiple_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_file_selection_get_select_multiple")
	val get_select_multiple : 'a t -> bool
	    = fn self => get_select_multiple_ (repr self)
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a colorselectiondialog_t = unit
	type 'a t = 'a colorselectiondialog_t Dialog.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in Dialog.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toColorSelectionDialog obj = inherit () (fn () => repr obj)
	val new_ : string -> cptr
	    = app1 (symb"mgtk_gtk_color_selection_dialog_new")
	val new : string -> base t = fn title => make (new_ title)
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
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a windowgroup_t = unit
	type 'a t = 'a windowgroup_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toWindowGroup obj = inherit () (fn () => repr obj)
	val new_ : unit -> cptr = app1 (symb"mgtk_gtk_window_group_new")
	val new : unit -> base t = fn dummy => make (new_ dummy)
	val add_window_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_window_group_add_window")
	val add_window : 'a t -> 'b t -> unit
	    = fn self => fn window => add_window_ (repr self) (repr window)
	val remove_window_ : cptr -> cptr -> unit
	    = app2 (symb"mgtk_gtk_window_group_remove_window")
	val remove_window : 'a t -> 'b t -> unit
	    = fn self => fn window => remove_window_ (repr self) (repr window)
    end
    structure Clipboard :>
      sig
	type base
	type 'a clipboard_t
	type 'a t = 'a clipboard_t GObject.t
	val inherit : 'a -> GObject.constructor -> 'a t
	val toClipboard : 'a t -> base t
	val get_owner : 'a t -> base GObject.t
	val clear : 'a t -> unit
	val set_text : 'a t -> string -> int -> unit
	val wait_for_text : 'a t -> string
	val wait_is_text_available : 'a t -> bool
      end = struct
	open Dynlib
	type cptr = GObject.cptr
	val repr = GObject.repr
	val symb = GtkBasis.symb
	type base = unit
	type 'a clipboard_t = unit
	type 'a t = 'a clipboard_t GObject.t
	fun inherit w con = let val con = let val ptr = con ()
					  in fn () => ptr end
				val witness = ()
			    in GObject.inherit witness con end
	fun make ptr = inherit () (fn () => ptr)
	fun toClipboard obj = inherit () (fn () => repr obj)
	val get_owner_ : cptr -> cptr
	    = app1 (symb"mgtk_gtk_clipboard_get_owner")
	val get_owner : 'a t -> base GObject.t
	    = fn self => GObject.inherit () (fn () => get_owner_ (repr self))
	val clear_ : cptr -> unit = app1 (symb"mgtk_gtk_clipboard_clear")
	val clear : 'a t -> unit = fn self => clear_ (repr self)
	val set_text_ : cptr -> string -> int -> unit
	    = app3 (symb"mgtk_gtk_clipboard_set_text")
	val set_text : 'a t -> string -> int -> unit
	    = fn self => fn text => fn len => set_text_ (repr self) text len
	val wait_for_text_ : cptr -> string
	    = app1 (symb"mgtk_gtk_clipboard_wait_for_text")
	val wait_for_text : 'a t -> string
	    = fn self => wait_for_text_ (repr self)
	val wait_is_text_available_ : cptr -> bool
	    = app1 (symb"mgtk_gtk_clipboard_wait_is_text_available")
	val wait_is_text_available : 'a t -> bool
	    = fn self => wait_is_text_available_ (repr self)
    end
end
