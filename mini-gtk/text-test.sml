
fun say s _ = print (s^"\n")

fun delete_event _ = ( say "delete event occurred" ()
		     ; false)

fun destroy _ = GtkBasis.main_quit()

fun add ent buffer get _ =
    let val s = Entry.get_text ent
        val iter = get buffer
    in  TextBuffer.insert buffer iter (s^"\n")
    end

fun main () =
    let val _      = GtkBasis.init(CommandLine.name()::CommandLine.arguments())
	val window = Window.new ()
        val view   = TextView.new ()
        val buffer = TextView.get_buffer view
        val box    = VBox.new ()
        val entry1  = Entry.new()
        val entry2  = Entry.new()

        val start = TextBuffer.get_start_iter
        val getEnd = TextBuffer.get_end_iter
    in  Signal.connect window (Widget.delete_event_sig delete_event) 
      ; Signal.connect window (Widget.destroy_sig destroy) 
      ; Container.set_border_width window 10
      ; Signal.connect entry1 (Entry.activate_sig (add entry1 buffer start))
      ; Signal.connect entry2 (Entry.activate_sig (add entry2 buffer getEnd))
      ; Container.add window box
      ; Box.pack_start box entry1
      ; Box.pack_start box entry2
      ; Box.pack_start box view
      ; Widget.show_all window
      ; GtkBasis.main() 
    end


val _ = main()
