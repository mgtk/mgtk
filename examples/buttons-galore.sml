open Gtk

fun say s _ = print (s^"\n")

fun delete_event _ = ( say "delete event occurred" ()
		     ; false)

fun destroy _ = GtkBasis.main_quit()

fun add_button box ent _ =
    let val s = Entry.get_text ent
        val but = Button.new_with_label s
    in  Box.pack_start' box but
      ; Signal.connect but (Button.clicked_sig (say s))
      ; Widget.show but
    end

fun main () =
    let val _      = GtkBasis.init(CommandLine.name()::CommandLine.arguments())
	val window = Window.new Window.TOPLEVEL
        val box    = VBox.new false 0
        val entry  = Entry.new ()
    in  Signal.connect window (Widget.delete_event_sig delete_event) 
      ; Signal.connect window (Object.destroy_sig destroy)
      ; Container.set_border_width window 10
      ; Signal.connect entry (Entry.activate_sig (add_button box entry))
      ; Container.add window box
      ; Box.pack_start' box entry
      ; Widget.show_all window
      ; GtkBasis.main() 
    end


val _ = main()
