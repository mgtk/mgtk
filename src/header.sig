signature Gtk =
sig

    type 'a GtkObject

    val init : string list -> unit
    val main : unit -> unit
    val main_quit : unit -> unit
(*    val main_quit_with: exn -> 'a
*)    
    type base
    type 'a widget_t
    type 'a GtkWidget = 'a widget_t GtkObject
    val toWidget: 'a GtkWidget -> base GtkWidget
    val toObject: 'a GtkWidget -> base GtkObject

    type gtk_type
