signature Gtk =
sig

    type 'a GtkObject

    val init : string list -> unit
    val main : unit -> unit
    val main_quit : unit -> unit

    (* *** Signal stuff *** *)
    type state

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
                                                  'a GtkObject signal
    val signalConnect : 'a GtkObject -> 'a GtkObject signal -> signal_id

    val bool_connect : 'a GtkObject -> string -> (unit -> bool) -> unit
    val unit_connect : 'a GtkObject -> string -> (unit -> unit) -> unit   

    type base
    type 'a widget_t
    type 'a GtkWidget = 'a widget_t GtkObject
    val toWidget: 'a GtkWidget -> base GtkWidget
    val toObject: 'a GtkWidget -> base GtkObject

    type gtk_type
