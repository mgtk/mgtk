(* Illustrates the use of boxes, labels, and toWidget *)

fun hello l s _ = Gtk.label_set_text l ("Hello "^s)

fun delete_event _ = ( print "delete event occurred\n"
		     ; false)

fun destroy _ = Gtk.main_quit()

fun main () =
    let val _      = Gtk.init(CommandLine.name()::CommandLine.arguments())
	val window = Gtk.window_new Gtk.WINDOW_TOPLEVEL
	val but1   = Gtk.button_new_with_label "Button1"
	val but2   = Gtk.button_new_with_label "Button2"
	val lab    = Gtk.label_new "None"
	val say    = hello lab
	val box1   = Gtk.vbox_new false 0
	fun pack w = Gtk.box_pack_start box1 w true true 0
	val tW     = Gtk.toWidget
    in  Gtk.connect_delete_event window delete_event 
      ; Gtk.connect_destroy window destroy
      ; Gtk.container_set_border_width window 10
      ; Gtk.connect_clicked but1 (say "button 1")
      ; Gtk.connect_clicked but2 (say "but2")
      ; pack lab
      ; app pack [but1,but2] 
      ; Gtk.container_add window box1
      ; app Gtk.widget_show [tW but1, tW box1, tW but2, tW window, tW lab]
      ; Gtk.main() 
    end
    
