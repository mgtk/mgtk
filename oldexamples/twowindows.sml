
fun click i  _ = app print ["Hello Window ",Int.toString i,"\n"]

fun delete_event _ = ( print "delete event occurred\n"
		     ; false)

fun simpwin i =
    let val window = Gtk.window_new Gtk.WINDOW_TOPLEVEL
	val button = Gtk.button_new_with_label "Click here"

    in  Gtk.connect_delete_event window delete_event
      ; Gtk.container_set_border_width window 10
      ; Gtk.connect_clicked button (click i)
      ; Gtk.container_add window button
      ; Gtk.widget_show button
      ; Gtk.widget_show window
    end


fun destroy _ = Gtk.main_quit()

fun main () =
    let val _      = Gtk.init(CommandLine.name()::CommandLine.arguments())
	val window = Gtk.window_new Gtk.WINDOW_TOPLEVEL
	val button = Gtk.button_new_with_label "New Window"
	val count = ref 1
	fun clicked _ = simpwin (!count) before count := !count + 1

    in  Gtk.connect_delete_event window delete_event 
      ; Gtk.connect_destroy window destroy
      ; Gtk.container_set_border_width window 10
      ; Gtk.connect_clicked button clicked
      ; Gtk.container_add window button
      ; Gtk.widget_show button
      ; Gtk.widget_show window
      ; Gtk.main() 
    end
    
