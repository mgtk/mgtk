fun hello _ = print "Hello World\n"

fun delete_event _ = ( print "delete event occurred\n"
		     ; false)

fun destroy _ = Gtk.main_quit()

fun main _ =
    let val _      = Gtk.init(CommandLine.name()::CommandLine.arguments())
	val window = Gtk.window_new ()
	val button = Gtk.button_new_with_label "Hello World"
    in  Gtk.connect_delete_event window delete_event 
      ; Gtk.connect_destroy window destroy
      ; Gtk.container_set_border_width window 10
      ; Gtk.connect_clicked button hello
      ; Gtk.container_add window button
      ; Gtk.widget_show button
      ; Gtk.widget_show window
      ; Gtk.main() 
    end

val _ = main()
