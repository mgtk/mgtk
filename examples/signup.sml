(* An example adopted from "Mono: A Developers Notebook" (04-gtk/03-signup) *)

open Gtk

fun leftAlign lab = 
    Misc.set_alignment lab 0.0 0.5 (* Left align X and center Y *)

fun delete_event _ = ( GtkBasis.main_quit()
		     ; true
                     )

(* Create an hbox containing two vboxes: the left containing labels
   and the right containing entries.
*)
fun labelsAndEntries ls = 
    let val hbox = HBox.new false 0
        val left = VBox.new false 0
        val right = VBox.new false 0
        fun makeEntryLabel (text, entry) =
            let val lab = Label.new_with_mnemonic text
            in  leftAlign lab
              ; Label.set_mnemonic_widget lab entry
              ; Box.pack_start left lab true false 0
              ; Box.pack_start' right entry
            end
    in  Box.set_spacing hbox 6
      ; Box.set_spacing left 6
      ; Box.pack_start hbox left false false 0
      ; Box.set_spacing right 6
      ; Box.pack_start' hbox right
      ; app makeEntryLabel ls
      ; hbox
    end

fun setUpGui() = 
    let val w = Window.new Window.TOPLEVEL

        val firstname_entry = Entry.new ()
	val lastname_entry = Entry.new ()
        val email_entry = Entry.new ()

        fun nameChanged () =
            let val e = Entry.get_text firstname_entry ^ "." ^
                        Entry.get_text lastname_entry ^ "@example.net"
                fun emailize c = if Char.isSpace c then #"_" 
                                 else Char.toLower c
            in  Entry.set_text email_entry (String.map emailize e)
            end
        val nameChanged_cb = Editable.changed_sig nameChanged
        val outerv = VBox.new false 0
        val _ = ( Container.set_border_width outerv 12
                ; Box.set_spacing outerv 12
                ; Container.add w outerv
                ) 
                 
        val topLabel = 
            Label.new ("<span weight=\"bold\" size=\"larger\">" ^
		       "Enter your name and preferred address</span>")

    in  Label.set_use_markup topLabel true
      ; Box.pack_start outerv topLabel false false 0
                  
      ; Container.add outerv (labelsAndEntries 
                                  [ ("_First name:",    firstname_entry)
                                  , ("_Last name:",     lastname_entry)
                                  , ("_Email address:", email_entry)])
      ; Signal.connect (Entry.asEditable firstname_entry) nameChanged_cb
      ; Signal.connect (Entry.asEditable lastname_entry)  nameChanged_cb
      ; Signal.connect w (Widget.delete_event_sig delete_event) 

      ; Window.set_title w "Sign up"
      ; Widget.show_all w
    end

fun main () = ( GtkBasis.init(CommandLine.name()::CommandLine.arguments())
              ; setUpGui()
              ; GtkBasis.main()
              )

val _ = main()
