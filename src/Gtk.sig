(* Interface to the GTK library, created by Ken Friis Larsen 1999-06-01. *
 * Modified: 1999-10-22 (kfl)                                            *)
signature Gtk =
sig

    type 'a GtkObject

    val init : string list -> unit
    val main : unit -> unit
    val main_quit : unit -> unit
    val main_quit_with: exn -> 'a
    
    type base
    type 'a widget_t
    type 'a GtkWidget = 'a widget_t GtkObject

    val show : 'a GtkWidget -> unit
    val hide : 'a GtkWidget -> unit
    val connect_destroy : 'a GtkWidget -> (unit -> unit) -> unit
    val toWidget : 'a GtkWidget -> base GtkWidget


    type 'a misc_t
    type 'a GtkMisc = 'a misc_t GtkWidget


    type 'a label_t
    type 'a GtkLabel = 'a label_t GtkMisc

    type justification
    val JUSTIFY_LEFT   : justification
    val JUSTIFY_RIGHT  : justification
    val JUSTIFY_CENTER : justification
    val JUSTIFY_FILL   : justification
  
    val label_new           : string -> base GtkLabel
    val label_set_text      : 'a GtkLabel -> string -> unit
    val label_get           : 'a GtkLabel -> string
    val label_set_justify   : 'a GtkLabel -> justification -> unit
    val label_set_pattern   : 'a GtkLabel -> string -> unit
    val label_set_line_wrap : 'a GtkLabel -> bool -> unit

    type 'a container_t
    type 'a GtkContainer = 'a container_t GtkWidget

    val container_set_border_width : 'a GtkContainer -> int -> unit
    val container_add : 'a GtkContainer -> 'b GtkWidget -> unit


    type 'a box_t
    type 'a GtkBox = 'a box_t GtkContainer

    val pack_start : 'a GtkBox -> 'b GtkWidget -> bool -> bool -> int -> unit
    val pack_end   : 'a GtkBox -> 'b GtkWidget -> bool -> bool -> int -> unit


    type 'a hbox_t
    type 'a GtkHBox = 'a hbox_t GtkBox

    val hbox_new : bool -> int -> base GtkHBox


    type 'a vbox_t
    type 'a GtkVBox = 'a vbox_t GtkBox

    val vbox_new : bool -> int -> base GtkVBox


    type 'a bin_t
    type 'a GtkBin = 'a bin_t GtkContainer


    type 'a scrolledwindow_t 
    type 'a GtkScrolledWindow = 'a scrolledwindow_t GtkBin

    type policy
    val ALWAYS    : policy
    val AUTOMATIC : policy
    val NEVER     : policy
   
    val scrolled_window_new : unit -> base GtkScrolledWindow
    val scrolled_window_set_policy : 'a GtkScrolledWindow ->
	                             policy -> policy -> unit

    val scrolled_window_add_with_viewport : 'a GtkScrolledWindow ->
                                            'b GtkWidget -> unit

    type 'a button_t
    type 'a GtkButton = 'a button_t GtkBin

    val button_new_with_label : string -> base GtkButton

    val connect_clicked  : 'a GtkButton -> (unit -> unit) -> unit
    val connect_pressed  : 'a GtkButton -> (unit -> unit) -> unit
    val connect_released : 'a GtkButton -> (unit -> unit) -> unit
    val connect_enter    : 'a GtkButton -> (unit -> unit) -> unit
    val connect_leave    : 'a GtkButton -> (unit -> unit) -> unit

    val button_clicked  : 'a GtkButton -> unit
    val button_pressed  : 'a GtkButton -> unit
    val button_released : 'a GtkButton -> unit
    val button_enter    : 'a GtkButton -> unit
    val button_leave    : 'a GtkButton -> unit


    type 'a window_t
    type 'a GtkWindow = 'a window_t GtkBin

    type window_kind	
    val WINDOW_TOPLEVEL : window_kind

    val window_new : window_kind -> base GtkWindow 
    val connect_delete_event : 'a GtkWindow -> (unit -> bool) -> unit


    type 'a editable_t
    type 'a GtkEditable = 'a editable_t GtkWidget

    val editable_get_chars : 'a GtkEditable -> int -> int -> string
    val editable_delete_text : 'a GtkEditable -> int -> int -> unit

    type 'a entry_t
    type 'a GtkEntry = 'a entry_t GtkEditable

    val entry_new            : int option -> base GtkEntry
    val entry_set_text       : 'a GtkEntry -> string -> unit
    val entry_append_text    : 'a GtkEntry -> string -> unit
    val entry_prepend_text   : 'a GtkEntry -> string -> unit
    val entry_set_position   : 'a GtkEntry -> int -> unit
    val entry_get_text       : 'a GtkEntry -> string
    val entry_select_region  : 'a GtkEntry -> int -> int -> unit
    val entry_set_visibility : 'a GtkEntry -> bool -> unit
    val entry_set_editable   : 'a GtkEntry -> bool -> unit
    val entry_set_max_length : 'a GtkEntry -> int -> unit

    val connect_activate : 'a GtkEntry -> (unit -> unit) -> unit
    val connect_changed  : 'a GtkEntry -> (unit -> unit) -> unit


    type 'a text_t
    type 'a GtkText = 'a text_t GtkEditable
    
(*    val text_new : 'a GtkAdjustment option -> 'b GtkAdjustment option
                                                    -> base GtkText
*)
    val text_new : unit -> base GtkText
    val text_set_editable : 'a GtkText -> bool -> unit
    val text_set_word_wrap : 'a GtkText -> bool -> unit
    val text_set_line_wrap : 'a GtkText -> bool -> unit
(*    val text_set_adjustments : 'a GtkText -> 
                               'b GtkAdjustment option -> 
                               'c GtkAdjustment option ->
			       unit
*)
    val text_set_point : 'a GtkText -> int -> unit
    val text_get_point : 'a GtkText -> int
    val text_get_length : 'a GtkText -> int
    val text_freeze : 'a GtkText -> unit
    val text_thaw   : 'a GtkText -> unit
(*    val text_insert : 'a GtkText -> 'b Gdk.GdkFont -> 'c Gdk.GdkFont ->
                      'd Gdk.GdkColor -> string -> int option -> unit
*)
    val text_insert : 'a GtkText -> string -> int option -> unit
    val text_backward_delete : 'a GtkText -> int -> bool
    val text_forward_delete : 'a GtkText -> int -> bool


    type 'a arrow_t
    type 'a GtkArrow = 'a arrow_t GtkMisc

    type arrow_type
    type shadow_type
    val ARROW_UP : arrow_type
    val ARROW_DOWN : arrow_type
    val ARROW_LEFT : arrow_type
    val ARROW_RIGHT : arrow_type

    val SHADOW_NONE : shadow_type
    val SHADOW_IN : shadow_type
    val SHADOW_OUT : shadow_type
    val SHADOW_ETCHED_IN : shadow_type
    val SHADOW_ETCHED_OUT : shadow_type

    val arrow_new : arrow_type -> shadow_type -> base GtkArrow
    val arrow_set : 'a GtkArrow -> arrow_type -> shadow_type -> unit


    type 'a item_t
    type 'a GtkItem = 'a item_t GtkBin

    val item_select : 'a GtkItem -> unit
    val item_deselect : 'a GtkItem -> unit
    val item_toggle : 'a GtkItem -> unit
 
    val connect_select : 'a GtkItem -> (unit -> unit) -> unit
    val connect_deselect : 'a GtkItem -> (unit -> unit) -> unit
    val connect_toggle : 'a GtkItem -> (unit -> unit) -> unit


    type 'a listitem_t
    type 'a GtkListItem = 'a listitem_t GtkItem

    val list_item_new : unit -> base GtkListItem
    val list_item_new_with_label : string -> base GtkListItem
    val list_item_select : 'a GtkListItem -> unit
    val list_item_deselect : 'a GtkListItem -> unit


    type 'a treeitem_t
    type 'a GtkTreeItem = 'a treeitem_t GtkItem

    val tree_item_new : unit -> base GtkTreeItem
    val tree_item_new_with_label : string -> base GtkTreeItem
    val tree_item_set_subtree : 'a GtkTreeItem -> 'b GtkWidget -> unit
    val tree_item_remove_subtree : 'a GtkTreeItem -> unit
    val tree_item_select : 'a GtkTreeItem -> unit
    val tree_item_deselect : 'a GtkTreeItem -> unit
    val tree_item_expand : 'a GtkTreeItem -> unit
    val tree_item_collapse : 'a GtkTreeItem -> unit

    val connect_collapse : 'a GtkTreeItem -> (unit -> unit) -> unit
    val connect_expand : 'a GtkTreeItem -> (unit -> unit) -> unit


    type 'a list_t
    type 'a GtkList = 'a list_t GtkContainer

    (* fill in the holes *)


    type 'a tree_t 
    type 'a GtkTree = 'a tree_t GtkBin

    val tree_new : unit -> base GtkTree
    val tree_append : 'a GtkTree -> 'b GtkWidget -> unit
    val tree_prepend : 'a GtkTree -> 'b GtkWidget -> unit
    val tree_insert : 'a GtkTree -> 'b GtkWidget -> int -> unit
    val tree_clear_items : 'a GtkTree -> int -> int -> unit
    val tree_select_item : 'a GtkTree -> int -> unit
    val tree_unselect_item : 'a GtkTree -> int -> unit
    val tree_select_child : 'a GtkTree -> 'b GtkWidget -> unit
    val tree_unselect_child : 'a GtkTree -> 'b GtkWidget -> unit
    val tree_child_position : 'a GtkTree -> 'b GtkWidget -> int
    val tree_remove_item : 'a GtkTree -> 'b GtkWidget -> unit

    type selection_mode
    val SELECTION_SINGLE : selection_mode
    val SELECTION_BROWSE : selection_mode
    val SELECTION_MULTIPLE : selection_mode
    val SELECTION_EXTENDED : selection_mode

    val tree_set_selection_mode : 'a GtkTree -> selection_mode -> unit

    type view_mode
    val TREE_VIEW_LINE : view_mode
    val TREE_VIEW_ITEM : view_mode

    val tree_set_view_mode : 'a GtkTree -> view_mode -> unit

    val connect_selection_changed : 'a GtkTree -> (unit -> unit) -> unit
    val connect_select_child : 'a GtkTree -> (unit -> unit) -> unit
    val connect_unselect_child : 'a GtkTree -> (unit -> unit) -> unit


    type 'a combo_t 
    type 'a GtkCombo = 'a combo_t GtkHBox

    val combo_new : unit -> base GtkCombo
    val combo_get_list : 'a GtkCombo -> base GtkList
    val combo_get_entry : 'a GtkCombo -> base GtkEntry
    val combo_set_popdown_strings : 'a GtkCombo -> string list -> unit
    val combo_set_value_in_list : 'a GtkCombo -> bool -> bool -> unit
    val combo_set_use_arrows : 'a GtkCombo -> bool -> unit
    val combo_set_use_arrows_always : 'a GtkCombo -> bool -> unit
    val combo_set_case_sensitive : 'a GtkCombo -> bool -> unit
    val combo_set_item_string : 'a GtkCombo -> 'b GtkItem -> string -> unit
    val combo_disable_activate : 'a GtkCombo -> unit


end 

(*

   [combo_get_list combo] returns the list associated with the
   combo box. This is not a GTK function --- it corresponds to the C idom
   (GTK_COMBO (combo)) -> list 

   [combo_get_entry combo] returns the entry associated with the
   combo box. This is not a GTK function --- it corresponds to the C idom
   (GTK_COMBO (combo)) -> entry 

*)

