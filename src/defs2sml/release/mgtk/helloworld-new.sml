fun hello _ = print "Hello World\n"

fun main () =
    let val _      = GtkBasis.init(CommandLine.name()::CommandLine.arguments())
	val window = Window.new (SOME Window.TOPLEVEL)
	val button = Button.new_with_label "Hello World"
    in  Signal.connect window (Widget.delete_event_sig (fn _ => false))
      ; Signal.connect window (Object.destroy_sig GtkBasis.main_quit)
      ; Signal.connect button (Button.clicked_sig hello)
      ; Container.add window button
      ; Widget.show_all window
      ; GtkBasis.main() 
    end


val _ = main()
