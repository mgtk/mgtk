(* An example adopted from "Mono, A Developers Notebook" *)

fun delete_event _ = ( GtkBasis.main_quit()
		     ; true
                     )

fun setUpGui() = 
    let val w = Window.new Window.WINDOW_TOPLEVEL

        val firstname_entry = Entry.new ()
	val lastname_entry = Entry.new ()
        val email_entry = Entry.new ()

        val outerv = let val outerv = VBox.new ()
                     in	 Container.set_border_width outerv 12
		       ; Box.set_spacing outerv 12
		       ; Container.add w outerv
                       ; outerv
                     end

        val l = let val l = Label.new ("<span weight=\"bold\" size=\"larger\">" ^
			   "Enter your name and preferred address</span>")
                in   Misc.set_alignment l 0.0 0.5 
                   ; Label.set_usemarkup l true
                   ; Box.pack_start outerv l false false 0
                   ; l
                end

        val h = let val h = HBox.new ()
                in  Box.set_spacing h 6
                  ; Container.add outerv h
                  ; h
                end

        val v = let val v = VBox.new ()
                in  Box.set_spacing v 6
                  ; Box.pack_start h v false false 0
                  ; v
                end

        fun makeEntryLabel text entry =
            let val l = Label.new text
            in  Misc.set_alignment l 0.0 0.5 
              ; Box.pack_start v l true false 0
              ; Label.set_mnemonic_widget l entry
            end

        val _ = ( makeEntryLabel "_First name:" firstname_entry
                ; makeEntryLabel "_Last name:" lastname_entry
                ; makeEntryLabel "_Email address:" email_entry
                )

        val v = let val v = VBox.new ()
                in  Box.set_spacing v 6
                  ; Box.pack_start h v true true 0
                  ; v
                end
                    
        fun nameChanged () =
            let val e = Entry.get_text firstname_entry ^
                        Entry.get_text lastname_entry ^
                        "@example.net"
                fun emailize c = if Char.isSpace c then #"_" 
                                 else Char.toLower c
            in  Entry.set_text email_entry (String.map emailize e)
            end

        val nameChanged_cb = Entry.changed_sig nameChange

    in  Box.pack_start v firstname_entry true true 0
      ; Box.pack_start v lastname_entry true true 0
      ; Box.pack_start v email_entry true true 0
      ; Signal.connect firstname_entry nameChange
      ; Signal.connect lastname_entry nameChange
      ; Signal.connect w (Widget.delete_event_sig delete_event) 
      ; Widget.show_all w
    end

fun main () = ( GtkBasis.init(CommandLine.name()::CommandLine.arguments())
              ; setUpGui()
              ; GtkBasis.main()
              )

val _ = main()
