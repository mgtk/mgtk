(* An attempt to functionalize the event handling *)

fun setLabel lab i = Gtk.label_set_text lab (Int.toString i)

fun delete_event _ = false
fun destroy _      = Gtk.main_quit()


datatype event = UP | DOWN | SQ

fun count UP i   = i+1
  | count DOWN i = i-1
  | count SQ i   = i*i 

fun report lab f inp state =
    let val res = f inp state
    in  setLabel lab res
      ; res
    end

fun main () =
    let val _       = Gtk.init(CommandLine.name()::CommandLine.arguments())
	val window  = Gtk.window_new Gtk.WINDOW_TOPLEVEL
	val but1    = Gtk.button_new_with_label "Up"
	val but2    = Gtk.button_new_with_label "Down"
	val but3    = Gtk.button_new_with_label "Square"
	val lab     = Gtk.label_new "None"
	val clicked = Gtk.connect_clicked
	val con     = [(clicked, but1, UP), (clicked, but2, DOWN), 
		       (clicked, but3, SQ)]
	val statef  = FunInput.setupInput (report lab count) 0 con
	val box1    = Gtk.vbox_new false 0
	fun pack w  = (Gtk.box_pack_start box1 w true true 0; Gtk.widget_show w)
	val tW      = Gtk.toWidget
    in 	Gtk.connect_delete_event window delete_event 
      ; Gtk.connect_destroy window destroy
      ; Gtk.container_set_border_width window 10
      ; pack lab
      ; app pack [but1,but2,but3] 
      ; Gtk.container_add window box1
      ; app Gtk.widget_show [tW box1, tW window]
      ; Gtk.main() 
    end
    
