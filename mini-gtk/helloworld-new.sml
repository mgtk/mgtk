fun hello _ = print "Hello World\n"

fun delete_event _ = ( print "delete event occurred\n"
		     ; false)

fun destroy _ = GtkBasis.main_quit()

fun main () =
    let val _      = GtkBasis.init(CommandLine.name()::CommandLine.arguments())
	val window = Window.new Window.WINDOW_TOPLEVEL
	val button = Button.new_with_label "Hello World"
    in  Signal.connect window (Widget.delete_event_sig delete_event) 
      ; Signal.connect window (Widget.destroy_sig destroy) 
      ; Container.set_border_width window 10
      ; Signal.connect button (Button.clicked_sig hello)
      ; Container.add window button
      ; Widget.show_all window
      ; GtkBasis.main() 
    end


val _ = main()
