(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001.                   *)

(*
app load ["Dynlib", "Polyhash", "Callback"];
*)
signature Gtk =
sig
    val init : string list -> unit
    val main : unit -> unit
    val main_quit : unit -> unit

    type base
    type 'a GtkObject
    val toObject : 'a GtkObject -> base GtkObject


    (* *** Signal stuff *** *)
    type state

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
                                                  'a GtkObject signal
    val signalConnect : 'a GtkObject -> 'a GtkObject signal -> signal_id


    (* *** Widget stuff *** *)
    type 'a widget_t
    type 'a GtkWidget = 'a widget_t GtkObject
    val toWidget: 'a GtkWidget -> base GtkWidget

    val widget_destroy: 'a GtkWidget -> unit
    val widget_show: 'a GtkWidget -> unit
    val widget_show_all: 'a GtkWidget -> unit

    (* *** Container *** *)
    type 'a container_t
    type 'a GtkContainer = 'a container_t GtkWidget


    val container_set_border_width: 'a GtkContainer -> int -> unit
    val container_add: 'a GtkContainer -> 'b GtkWidget -> unit
    val container_remove: 'a GtkContainer -> 'b GtkWidget -> unit
 
    (* *** Button *** *)

    type 'a button_t
    type 'a GtkButton = 'a button_t GtkContainer


    val button_new: unit -> base GtkButton
    val button_new_with_label: string -> base GtkButton

    (* The window stuff is only for demo purposes *)
    (* *** Window *** *)
    type 'a window_t
    type 'a GtkWindow = 'a window_t GtkContainer

    val window_new: unit -> base GtkWindow

    (* *** Signals *** *)
    val connect_destroy: 'a GtkObject -> (unit -> unit) -> unit
    val connect_delete_event: 'a GtkWidget -> (unit -> bool) -> unit
    val connect_clicked: 'a GtkButton -> (unit -> unit) -> unit
end

