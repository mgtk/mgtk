structure Gtk :> Gtk =
struct

    prim_type gtkobj

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
    fun init args = init_(vector args)
    val main : unit -> unit = app1(symb "mgtk_main")
    val main_quit : unit -> unit = app1(symb "mgtk_main_quit")
    fun main_quit_with e = (main_quit(); raise e)

    (* A litle type cleverness *)
    datatype 'a GtkObject = OBJ of gtkobj

    (* Typecast function *)
    fun super (OBJ obj) = OBJ obj

    fun unwrap (OBJ obj) = obj


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
		SOME f => (f data handle e => main_quit_with e)
	      | NONE   => 
		   main_quit_with(Fail("mgtk: Unknown callback function (id: "^
				       Int.toString id^")"))

	val dummy = Callback.register "mgtk_callback_dispatch" dispatch

	val set_bool_pos : GtkArgs -> int -> bool -> unit 
                         = app3(symb "mgtk_set_pos_bool")

    in
	fun register f = localId(fn id => (add (id, f); id))
	fun reg_unit f = register(fn _ => f())
	fun reg_bool f = register(fn (_,args,pos) => 
				  set_bool_pos args pos (f()))

	val signal_connect : gtkobj -> string -> int -> bool -> int
	                   = app4(symb"mgtk_signal_connect")

    end	

    fun unit_connect (OBJ wid) sign cb =
	let val id = reg_unit cb
	in  ignore(signal_connect wid sign id false)
	end

    
    type base = unit


    type 'a widget_t = unit
    type 'a GtkWidget = 'a widget_t GtkObject

    val show_ : gtkobj -> unit = app1(symb "mgtk_widget_show")
    val show : 'a GtkWidget -> unit = fn OBJ wid => show_ wid

    val hide_ : gtkobj -> unit = app1(symb "mgtk_widget_hide")
    val hide : 'a GtkWidget -> unit = fn OBJ wid => hide_ wid

    fun connect_destroy wid cb = unit_connect wid "destroy" cb

    val toWidget = super


    type 'a misc_t = unit
    type 'a GtkMisc = 'a misc_t GtkWidget


    type 'a label_t = unit
    type 'a GtkLabel = 'a label_t GtkMisc

    type justification = int
    val get_justifications : unit -> int * int * int * int 
	                   = app1(symb"mgtk_get_justifications")
    val (JUSTIFY_LEFT, JUSTIFY_RIGHT, JUSTIFY_CENTER, JUSTIFY_FILL)
                           = get_justifications();

    val label_new_ : string -> gtkobj = app1(symb"mgtk_label_new")
    val label_new  : string -> base GtkLabel 
                   = fn s => OBJ(label_new_ s)

    val label_set_text_ : gtkobj -> string -> unit
                        = app2(symb"mgtk_label_set_text")
    val label_set_text  : 'a GtkLabel -> string -> unit
	                = fn OBJ lab => fn s => label_set_text_ lab s

    val label_get_ : gtkobj -> string
                   = app1(symb"mgtk_label_get")
    val label_get  : 'a GtkLabel -> string
	           = fn OBJ lab => label_get_ lab 

    val label_set_justify_ : gtkobj -> justification -> unit
                        = app2(symb"mgtk_label_set_justify")
    val label_set_justify  : 'a GtkLabel -> justification -> unit
	                = fn OBJ lab => fn s => label_set_justify_ lab s

    val label_set_pattern_ : gtkobj -> string -> unit
                        = app2(symb"mgtk_label_set_pattern")
    val label_set_pattern  : 'a GtkLabel -> string -> unit
	                = fn OBJ lab => fn s => label_set_pattern_ lab s

    val label_set_line_wrap_ : gtkobj -> bool -> unit
                        = app2(symb"mgtk_label_set_line_wrap")
    val label_set_line_wrap  : 'a GtkLabel -> bool -> unit
	                = fn OBJ lab => fn s => label_set_line_wrap_ lab s


    type 'a container_t = unit
    type 'a GtkContainer = 'a container_t GtkWidget

    val container_set_border_width_ : gtkobj -> int -> unit
	= app2(symb "mgtk_container_set_border_width")
    val container_set_border_width : 'a GtkContainer -> int -> unit
	= fn OBJ cont => fn w => container_set_border_width_ cont w

    val container_add_ : gtkobj -> gtkobj -> unit
	               = app2(symb"mgtk_container_add")
    val container_add : 'a GtkContainer -> 'b GtkWidget -> unit
                      = fn OBJ c => fn OBJ w => container_add_ c w


    type 'a box_t = unit
    type 'a GtkBox = 'a box_t GtkContainer

    val pack_start_ : gtkobj -> gtkobj -> bool -> bool -> int -> unit
                    = app5(symb "mgtk_box_pack_start")
    fun pack_start (OBJ box) (OBJ child) expand fill padding = 
	pack_start_ box child expand fill padding

    val pack_end_ : gtkobj -> gtkobj -> bool -> bool -> int -> unit
                  = app5(symb "mgtk_box_pack_end")
    fun pack_end (OBJ box) (OBJ child) expand fill padding = 
	pack_end_ box child expand fill padding
              

    type 'a hbox_t = unit
    type 'a GtkHBox = 'a hbox_t GtkBox

    val hbox_new_ : bool -> int -> gtkobj
                  = app2(symb"mgtk_hbox_new")
    fun hbox_new homogeneous spacing = OBJ(hbox_new_ homogeneous spacing)

    type 'a vbox_t = unit
    type 'a GtkVBox = 'a vbox_t GtkBox

    val vbox_new_ : bool -> int -> gtkobj
                  = app2(symb"mgtk_vbox_new")
    fun vbox_new homogeneous spacing = OBJ(vbox_new_ homogeneous spacing)


    type 'a bin_t = unit
    type 'a GtkBin = 'a bin_t GtkContainer


    type 'a scrolledwindow_t = unit
    type 'a GtkScrolledWindow = 'a scrolledwindow_t GtkBin

    type policy = int
    val get_policies : unit -> int * int * int
	             = app1(symb"mgtk_get_policies")
    val (ALWAYS, AUTOMATIC, NEVER) = get_policies()
   
    val scrolled_window_new_ : unit -> gtkobj
                             = app1(symb"mgtk_scrolled_window_new")
    val scrolled_window_new : unit -> base GtkScrolledWindow
                            = fn _ => OBJ(scrolled_window_new_())

    val scrolled_window_set_policy_ : gtkobj -> policy -> policy -> unit
                                 = app3(symb"mgtk_scrolled_window_set_policy")
    val scrolled_window_set_policy : 'a GtkScrolledWindow ->
	                             policy -> policy -> unit
             = fn OBJ sc => fn h => fn v => scrolled_window_set_policy_ sc h v

    val scrolled_window_add_with_viewport_ : gtkobj -> gtkobj -> unit
                                         = app2(symb"mgtk_scrolled_window_add_with_viewport")
    val scrolled_window_add_with_viewport: 'a GtkScrolledWindow -> 'b GtkWidget -> unit
                                         = fn OBJ sc => fn OBJ w => 
                                           scrolled_window_add_with_viewport_ sc w
    type 'a button_t = unit
    type 'a GtkButton = 'a button_t GtkBin

    val button_new_with_label_ : string -> gtkobj 
                               = app1(symb "mgtk_button_new_with_label")
    val button_new_with_label : string -> base GtkButton 
                              = fn s => OBJ(button_new_with_label_ s)

    fun connect_clicked but cb = unit_connect but "clicked" cb
    fun connect_pressed but cb = unit_connect but "pressed" cb
    fun connect_released but cb = unit_connect but "released" cb
    fun connect_enter but cb = unit_connect but "enter" cb
    fun connect_leave but cb = unit_connect but "leave" cb

    val button_clicked_ : gtkobj -> unit = app1(symb "mgtk_button_clicked")
    val button_clicked  : 'a GtkButton -> unit 
	                =  fn obj => button_clicked_(unwrap obj) 

    val button_pressed_ : gtkobj -> unit = app1(symb "mgtk_button_pressed")
    val button_pressed  : 'a GtkButton -> unit 
	                =  fn obj => button_pressed_(unwrap obj) 

    val button_released_ : gtkobj -> unit = app1(symb "mgtk_button_released")
    val button_released  : 'a GtkButton -> unit 
	                 =  fn obj => button_released_(unwrap obj) 

    val button_enter_ : gtkobj -> unit = app1(symb "mgtk_button_enter")
    val button_enter  : 'a GtkButton -> unit 
	              =  fn obj => button_enter_(unwrap obj) 

    val button_leave_ : gtkobj -> unit = app1(symb "mgtk_button_leave")
    val button_leave  : 'a GtkButton -> unit 
	              =  fn obj => button_leave_(unwrap obj) 

    type 'a window_t = unit
    type 'a GtkWindow = 'a window_t GtkBin


    type window_kind = int

    val window_kinds : unit -> int = app1(symb "mgtk_window_kinds")
    val WINDOW_TOPLEVEL = window_kinds()

    val window_new_ : int -> gtkobj = app1(symb "mgtk_window_new")
    val window_new : window_kind -> base GtkWindow = OBJ o window_new_

    fun connect_delete_event win cb =
	let val id = reg_bool cb
	in  ignore(signal_connect (unwrap win) "delete_event" id true)
	end
	

    type 'a editable_t = unit
    type 'a GtkEditable = 'a editable_t GtkWidget

    val editable_get_chars_ : gtkobj -> int -> int -> string
                            = app3(symb"mgtk_editable_get_chars")
    val editable_get_chars  : 'a GtkEditable -> int -> int -> string
                = fn OBJ ed => fn s => fn e => editable_get_chars_ ed s e

    val editable_delete_text_ : gtkobj -> int -> int -> unit
                              = app3(symb"mgtk_editable_delete_text")
    val editable_delete_text : 'a GtkEditable -> int -> int -> unit
                = fn OBJ ed => fn s => fn e => editable_delete_text_ ed s e


    type 'a entry_t = unit
    type 'a GtkEntry = 'a entry_t GtkEditable

    val entry_new_                : unit -> gtkobj
	                          = app1(symb"mgtk_entry_new")
    val entry_new_with_max_length : int -> gtkobj
                                  = app1(symb"mgtk_entry_new_with_max_length")
    val entry_new : int option -> base GtkEntry
                  = fn SOME len => OBJ(entry_new_with_max_length len)
		     | NONE     => OBJ(entry_new_())
 
    val entry_set_text_ : gtkobj -> string -> unit
                        = app2(symb"mgtk_entry_set_text")
    val entry_set_text  : 'a GtkEntry -> string -> unit
                        = fn OBJ ent => fn s => entry_set_text_ ent s

    val entry_append_text_ : gtkobj -> string -> unit
                        = app2(symb"mgtk_entry_append_text")
    val entry_append_text  : 'a GtkEntry -> string -> unit
                        = fn OBJ ent => fn s => entry_append_text_ ent s

    val entry_prepend_text_ : gtkobj -> string -> unit
                        = app2(symb"mgtk_entry_prepend_text")
    val entry_prepend_text  : 'a GtkEntry -> string -> unit
                        = fn OBJ ent => fn s => entry_prepend_text_ ent s

    val entry_set_position_ : gtkobj -> int -> unit
                        = app2(symb"mgtk_entry_set_position")
    val entry_set_position  : 'a GtkEntry -> int -> unit
                        = fn OBJ ent => fn s => entry_set_position_ ent s

    val entry_get_text_ : gtkobj -> string
                        = app1(symb"mgtk_entry_get_text")
    val entry_get_text  : 'a GtkEntry -> string
                        = fn OBJ ent => entry_get_text_ ent

    val entry_select_region_ : gtkobj -> int -> int -> unit
                        = app3(symb"mgtk_entry_select_region")
    val entry_select_region  : 'a GtkEntry -> int -> int -> unit
                  = fn OBJ ent => fn s => fn e => entry_select_region_ ent s e

    val entry_set_visibility_ : gtkobj -> bool -> unit
                        = app2(symb"mgtk_entry_set_visibility")
    val entry_set_visibility  : 'a GtkEntry -> bool -> unit
                        = fn OBJ ent => fn s => entry_set_visibility_ ent s

    val entry_set_editable_ : gtkobj -> bool -> unit
                        = app2(symb"mgtk_entry_set_editable")
    val entry_set_editable  : 'a GtkEntry -> bool -> unit
                        = fn OBJ ent => fn s => entry_set_editable_ ent s

    val entry_set_max_length_ : gtkobj -> int -> unit
                        = app2(symb"mgtk_entry_set_max_length")
    val entry_set_max_length  : 'a GtkEntry -> int -> unit
                        = fn OBJ ent => fn s => entry_set_max_length_ ent s

    fun connect_activate ent cb = unit_connect ent "activate" cb 
    fun connect_changed ent cb  = unit_connect ent "changed" cb 

    type 'a text_t = unit
    type 'a GtkText = 'a text_t GtkEditable
    
    val text_new_ : unit -> gtkobj = app1(symb"mgtk_text_new")
    val text_new : unit -> base GtkText = fn _ => OBJ(text_new_())

    val text_set_editable_ : gtkobj -> bool -> unit
                           = app2(symb"mgtk_text_set_editable")
    val text_set_editable : 'a GtkText -> bool -> unit
                          = fn OBJ t => fn b => text_set_editable_ t b

    val text_set_word_wrap_ : gtkobj -> bool -> unit
                           = app2(symb"mgtk_text_set_word_wrap")
    val text_set_word_wrap : 'a GtkText -> bool -> unit
                          = fn OBJ t => fn b => text_set_word_wrap_ t b

    val text_set_line_wrap_ : gtkobj -> bool -> unit
                           = app2(symb"mgtk_text_set_line_wrap")
    val text_set_line_wrap : 'a GtkText -> bool -> unit
                          = fn OBJ t => fn b => text_set_line_wrap_ t b

    val text_set_point_ : gtkobj -> int -> unit
                           = app2(symb"mgtk_text_set_point")
    val text_set_point : 'a GtkText -> int -> unit
                          = fn OBJ t => fn b => text_set_point_ t b

    val text_get_point_ : gtkobj -> int
                           = app1(symb"mgtk_text_get_point")
    val text_get_point : 'a GtkText -> int
                          = fn OBJ t => text_get_point_ t

    val text_get_length_ : gtkobj -> int
                           = app1(symb"mgtk_text_get_length")
    val text_get_length : 'a GtkText -> int
                          = fn OBJ t => text_get_length_ t

    val text_freeze_ : gtkobj -> unit
                           = app1(symb"mgtk_text_freeze")
    val text_freeze : 'a GtkText -> unit
                          = fn OBJ t => text_freeze_ t

    val text_thaw_ : gtkobj -> unit
                           = app1(symb"mgtk_text_thaw")
    val text_thaw : 'a GtkText -> unit
                          = fn OBJ t => text_thaw_ t

    val text_insert_ : gtkobj -> string -> int -> unit
                           = app3(symb"mgtk_text_insert")
    val text_insert : 'a GtkText -> string -> int option -> unit
                          = fn OBJ t => fn b => fn i => 
	                    text_insert_ t b (case i of SOME i => i | _ => ~1)

    val text_backward_delete_ : gtkobj -> int -> bool
                           = app2(symb"mgtk_text_backward_delete")
    val text_backward_delete : 'a GtkText -> int -> bool
                          = fn OBJ t => fn b => text_backward_delete_ t b

    val text_forward_delete_ : gtkobj -> int -> bool
                           = app2(symb"mgtk_text_forward_delete")
    val text_forward_delete : 'a GtkText -> int -> bool
                          = fn OBJ t => fn b => text_forward_delete_ t b


    type 'a arrow_t = unit
    type 'a GtkArrow = 'a arrow_t GtkMisc

    type arrow_type = int
    type shadow_type = int

    val get_arrow_types_ : unit -> int * int * int * int
                       = app1(symb"mgtk_get_arrow_types")
    val (ARROW_UP, ARROW_DOWN, ARROW_LEFT, ARROW_RIGHT) = get_arrow_types_ ()

    val get_shadow_types_ : unit -> int * int * int * int * int
                        = app1(symb"mgtk_get_arrow_types")
    val (SHADOW_NONE, SHADOW_IN, SHADOW_OUT, SHADOW_ETCHED_IN,
         SHADOW_ETCHED_OUT) = get_shadow_types_ ()

    val arrow_new_ : arrow_type -> shadow_type -> gtkobj
                 = app2(symb"mgtk_arrow_new")
    val arrow_new : arrow_type -> shadow_type -> base GtkArrow
                = fn at => fn st => OBJ(arrow_new_ at st)

    val arrow_set_ : gtkobj -> arrow_type -> shadow_type -> unit
                 = app3(symb"mgtk_arrow_set")
    val arrow_set : 'a GtkArrow -> arrow_type -> shadow_type -> unit
                = fn OBJ a => fn at => fn st => arrow_set_ a at st


    type 'a item_t = unit
    type 'a GtkItem = 'a item_t GtkBin

    val item_select_ :  gtkobj -> unit
                   = app1(symb"mgtk_item_select")
    val item_select : 'a GtkItem -> unit
                  = fn OBJ i => item_select_ i

    val item_deselect_ : gtkobj -> unit
                     = app1(symb"mgtk_item_deselect")
    val item_deselect : 'a GtkItem -> unit
                  = fn OBJ i => item_deselect_ i

    val item_toggle_ : gtkobj -> unit
                   = app1(symb"mgtk_item_toggle")
    val item_toggle : 'a GtkItem -> unit
                  = fn OBJ i => item_toggle_ i
 
    fun connect_select item cb = unit_connect item "select" cb
    fun connect_deselect item cb = unit_connect item "deselect" cb
    fun connect_toggle item cb = unit_connect item "toggle" cb


    type 'a listitem_t = unit
    type 'a GtkListItem = 'a listitem_t GtkItem

    val list_item_new_ : unit -> gtkobj
                     = app1(symb"mgtk_list_item_new")
    val list_item_new : unit -> base GtkListItem 
                    = fn _ => OBJ(list_item_new_())

    val list_item_new_with_label_ : string -> gtkobj
                                = app1(symb"mgtk_list_item_new_with_label")
    val list_item_new_with_label : string -> base GtkListItem
                               = fn s => OBJ(list_item_new_with_label_ s)

    val list_item_select_ : gtkobj -> unit
                        = app1(symb"mgtk_list_item_select")
    val list_item_select : 'a GtkListItem -> unit
                       = fn OBJ ti => list_item_select_ ti

    val list_item_deselect_ : gtkobj -> unit
                          = app1(symb"mgtk_list_item_deselect")
    val list_item_deselect : 'a GtkListItem -> unit
                         = fn OBJ ti => list_item_deselect_ ti


    type 'a treeitem_t = unit
    type 'a GtkTreeItem = 'a treeitem_t GtkItem

    val tree_item_new_ : unit -> gtkobj
                     = app1(symb"mgtk_tree_item_new")
    val tree_item_new : unit -> base GtkTreeItem 
                    = fn _ => OBJ(tree_item_new_())

    val tree_item_new_with_label_ : string -> gtkobj
                                = app1(symb"mgtk_tree_item_new_with_label")
    val tree_item_new_with_label : string -> base GtkTreeItem
                               = fn s => OBJ(tree_item_new_with_label_ s)

    val tree_item_set_subtree_ : gtkobj -> gtkobj -> unit
                             = app2(symb"mgtk_tree_item_set_subtree")
    val tree_item_set_subtree : 'a GtkTreeItem -> 'b GtkWidget -> unit
                            = fn OBJ ti => fn OBJ t => 
                              tree_item_set_subtree_ ti t

    val tree_item_remove_subtree_ : gtkobj -> unit
                                = app1(symb"mgtk_tree_item_remove_subtree")
    val tree_item_remove_subtree : 'a GtkTreeItem -> unit
                               = fn OBJ ti => tree_item_remove_subtree_ ti

    val tree_item_select_ : gtkobj -> unit
                        = app1(symb"mgtk_tree_item_select")
    val tree_item_select : 'a GtkTreeItem -> unit
                       = fn OBJ ti => tree_item_select_ ti

    val tree_item_deselect_ : gtkobj -> unit
                          = app1(symb"mgtk_tree_item_deselect")
    val tree_item_deselect : 'a GtkTreeItem -> unit
                         = fn OBJ ti => tree_item_deselect_ ti

    val tree_item_collapse_ : gtkobj -> unit
                          = app1(symb"mgtk_tree_item_collapse")
    val tree_item_collapse : 'a GtkTreeItem -> unit
                         = fn OBJ ti => tree_item_collapse_ ti

    val tree_item_expand_ : gtkobj -> unit
                        = app1(symb"mgtk_tree_item_expand")
    val tree_item_expand : 'a GtkTreeItem -> unit
                       = fn OBJ ti => tree_item_expand_ ti

    fun connect_collapse treeitem cb = unit_connect treeitem "collapse" cb
    fun connect_expand treeitem cb = unit_connect treeitem "expand" cb


    type 'a list_t = unit
    type 'a GtkList = 'a list_t GtkContainer

    (* fill in the holes *)


    type 'a tree_t = unit
    type 'a GtkTree = 'a tree_t GtkBin

    val tree_new_ : unit -> gtkobj = app1(symb"mgtk_tree_new")
    val tree_new : unit -> base GtkTree = fn _ => OBJ(tree_new_())

    val tree_append_ : gtkobj -> gtkobj -> unit
                   = app2(symb"mgtk_tree_append")
    val tree_append : 'a GtkTree -> 'b GtkWidget -> unit
                  = fn OBJ t => fn OBJ ti => tree_append_ t ti

    val tree_prepend_ : gtkobj -> gtkobj -> unit
                    = app2(symb"mgtk_tree_prepend")
    val tree_prepend : 'a GtkTree -> 'b GtkWidget -> unit
                   = fn OBJ t => fn OBJ ti => tree_prepend_ t ti

    val tree_insert_ : gtkobj -> gtkobj -> int -> unit
                   = app3(symb"mgtk_tree_insert")
    val tree_insert : 'a GtkTree -> 'b GtkWidget -> int -> unit
                  = fn OBJ t => fn OBJ w => fn p =>
                    tree_insert_ t w p

    val tree_clear_items_ : gtkobj -> int -> int -> unit
                        = app3(symb"mgtk_tree_clear_items")
    val tree_clear_items : 'a GtkTree -> int -> int -> unit
                       = fn OBJ t => fn s => fn e =>
                         tree_clear_items_ t s e

    val tree_select_item_ : gtkobj -> int -> unit
                        = app2(symb"mgtk_tree_select_item")
    val tree_select_item : 'a GtkTree -> int -> unit
                       = fn OBJ t => fn p => tree_select_item_ t p

    val tree_unselect_item_ : gtkobj -> int -> unit
                          = app2(symb"mgtk_tree_unselect_item")
    val tree_unselect_item : 'a GtkTree -> int -> unit
                         = fn OBJ t => fn p => tree_unselect_item_ t p

    val tree_select_child_ : gtkobj -> gtkobj -> unit
                         = app2(symb"mgtk_tree_select_child") 
    val tree_select_child : 'a GtkTree -> 'b GtkWidget -> unit
                        = fn OBJ t => fn OBJ c => tree_select_child_ t c

    val tree_unselect_child_ : gtkobj -> gtkobj -> unit
                           = app2(symb"mgtk_tree_unselect_child") 
    val tree_unselect_child : 'a GtkTree -> 'b GtkWidget -> unit
                          = fn OBJ t => fn OBJ c => tree_unselect_child_ t c

    val tree_child_position_ : gtkobj -> gtkobj -> int
                           = app2(symb"mgtk_tree_child_position")
    val tree_child_position : 'a GtkTree -> 'b GtkWidget -> int
                          = fn OBJ t => fn OBJ c => tree_child_position_ t c

    val tree_remove_item_ : gtkobj -> gtkobj -> unit
                        = app2(symb"mgtk_tree_remove_item") 
    val tree_remove_item : 'a GtkTree -> 'b GtkWidget -> unit
                       = fn OBJ t => fn OBJ c => tree_remove_item_ t c


    type selection_mode = int
    val get_selection_modes : unit -> int * int * int * int
                            = app1(symb"mgtk_get_selection_modes")
    val (SELECTION_SINGLE, SELECTION_BROWSE, SELECTION_MULTIPLE,
        SELECTION_EXTENDED) = get_selection_modes()

    val tree_set_selection_mode_ : gtkobj -> selection_mode -> unit
                               = app2(symb"mgtk_tree_set_selection_mode")
    val tree_set_selection_mode : 'a GtkTree -> selection_mode -> unit
                              = fn OBJ t => fn sm => 
	                        tree_set_selection_mode_ t sm

    type view_mode = int
    val get_view_modes : unit -> int * int
                       = app1(symb"mgtk_get_view_modes")
    val (TREE_VIEW_LINE,TREE_VIEW_ITEM) = get_view_modes()

    val tree_set_view_mode_ : gtkobj -> view_mode -> unit
                          = app2(symb"mgtk_tree_set_view_mode")
    val tree_set_view_mode : 'a GtkTree -> view_mode -> unit
                         = fn OBJ t => fn wm => tree_set_view_mode_ t wm

    fun connect_selection_changed tree cb = unit_connect tree "selection-changed" cb
    fun connect_select_child tree cb = unit_connect tree "select-child" cb
    fun connect_unselect_child tree cb = unit_connect tree "unselect-child" cb


    prim_type glist
    val glist_nil : unit -> glist = app1(symb"mgtk_glist_nil")
    val glist_append_string : glist -> string -> unit
                            = app2(symb"mgtk_glist_append_string")

    fun toStingGlist sl = 
	let val res = glist_nil()
	in  app (glist_append_string res) sl
          ; res
	end


    type 'a combo_t = unit
    type 'a GtkCombo = 'a combo_t GtkHBox

    val combo_new_ : unit -> gtkobj = app1(symb"mgtk_combo_new")
    val combo_new : unit -> base GtkCombo = fn _ => OBJ(combo_new_())

    val combo_get_list_ : gtkobj -> gtkobj
                      = app1(symb"mgtk_combo_get_list")
    val combo_get_list : 'a GtkCombo -> base GtkList
                     = fn OBJ cbo => OBJ(combo_get_list_ cbo)

    val combo_get_entry_ : gtkobj -> gtkobj
                      = app1(symb"mgtk_combo_get_entry")
    val combo_get_entry : 'a GtkCombo -> base GtkList
                     = fn OBJ cbo => OBJ(combo_get_entry_ cbo)

    val combo_set_popdown_strings_ : gtkobj -> glist -> unit
                                 = app2(symb"mgtk_combo_set_popdown_strings")
    val combo_set_popdown_strings : 'a GtkCombo -> string list -> unit
      = fn OBJ cbo => fn strs =>
			 combo_set_popdown_strings_ cbo (toStingGlist strs)

    val combo_set_value_in_list_ : gtkobj -> bool -> bool -> unit
                               = app3(symb"mgtk_combo_set_value_in_list")
    val combo_set_value_in_list : 'a GtkCombo -> bool -> bool -> unit
                              = fn OBJ cbo => fn inlist => fn emptyok =>
                                combo_set_value_in_list_ cbo inlist emptyok

    val combo_set_use_arrows_ : gtkobj -> bool -> unit
                            = app2(symb"mgtk_combo_set_use_arrows")
    val combo_set_use_arrows : 'a GtkCombo -> bool -> unit
                           = fn OBJ cbo => fn ua =>
                             combo_set_use_arrows_ cbo ua

    val combo_set_use_arrows_always_ : gtkobj -> bool -> unit
                                   = app2(symb"mgtk_combo_set_use_arrows_always")
    val combo_set_use_arrows_always : 'a GtkCombo -> bool -> unit
                                  = fn OBJ cbo => fn ua =>
                                    combo_set_use_arrows_always_ cbo ua

    val combo_set_case_sensitive_ : gtkobj -> bool -> unit
                                = app2(symb"mgtk_combo_set_case_sensitive")
    val combo_set_case_sensitive : 'a GtkCombo -> bool -> unit
                                = fn OBJ cbo => fn cs =>
                                  combo_set_case_sensitive_ cbo cs

    val combo_set_item_string_ : gtkobj -> gtkobj -> string -> unit
                             = app3(symb"mgtk_combo_set_item_string")
    val combo_set_item_string : 'a GtkCombo -> 'b GtkItem -> string -> unit
                            = fn OBJ cbo => fn OBJ i => fn s => 
                              combo_set_item_string_ cbo i s

    val combo_disable_activate_ : gtkobj -> unit
                              = app1(symb"mgtk_combo_disable_activate")
    val combo_disable_activate : 'a GtkCombo -> unit
                             = fn OBJ cbo => combo_disable_activate_ cbo




end
