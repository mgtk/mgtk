fun hello _ = print "Hello World\n"

fun delete_event _ = ( print "delete event occurred\n"
		     ; false)

fun destroy _ = GtkBasis.main_quit()

fun add_button box ent _ =
    let val s = Entry.get_text ent
        val but = Button.new_with_label s
    in  Box.pack_start box but
      ; Widget.show but
    end

fun main () =
    let val _      = GtkBasis.init(CommandLine.name()::CommandLine.arguments())
	val window = Window.new ()
        val box    = VBox.new ()
	val button = Button.new_with_label "Hello World"
        val entry  = Entry.new()
    in  Signal.connect window (Widget.delete_event_sig delete_event) 
      ; Signal.connect window (Widget.destroy_sig destroy) 
      ; Container.set_border_width window 10
      ; Signal.connect button (Button.clicked_sig hello)
      ; Signal.connect entry (Entry.activate_sig (add_button box entry))
      ; Container.add window box
      ; Box.pack_start box entry
      ; Box.pack_start box button
      ; Widget.show_all window
      ; GtkBasis.main() 
    end


val _ = main()
