local open Gtk in

fun leftAlign lab = 
    Misc.set_alignment lab 0.0 0.5 (* Left align X and center Y *)

fun delete_event _ = ( GtkBasis.main_quit()
		     ; true
                     )

fun say message () = TextIO.print (message^"\n")
val openActivated = fn say => fn _ => ignore(say "Open")
val openActivated2 = say "Open"
val rotateActivated = say "Rotate" 

fun makeMenubar agrp say = 
    let val mb = MenuBar.new()

        (* file menu *)
        val file_menu = Menu.new()
        val file_item = let val item = MenuItem.new_with_mnemonic ("_File")
		        in  MenuItem.set_submenu item file_menu
                          ; MenuShell.append mb item
                          ; item
                        end

        val openItem = let val item = ImageMenuItem.new_from_stock "gtk-open" agrp
                       in  MenuShell.append file_menu item
                         ; item
                       end

        val closeItem = let val item = ImageMenuItem.new_from_stock "gtk-close" agrp
                        in  MenuShell.append file_menu item
                          ; item
                        end
		
	val _ = MenuShell.append file_menu (SeparatorMenuItem.new())
		
	val quitItem = let val item = ImageMenuItem.new_from_stock "gtk-quit" agrp
                       in  Signal.connect item (MenuItem.activate_sig GtkBasis.main_quit)
                         ; MenuShell.append file_menu item
                         ; item
                       end


        (* edit menu *)
	val edit_menu = Menu.new ()
        val editItem = let val item = MenuItem.new_with_mnemonic "_Edit"
                       in  MenuItem.set_submenu item edit_menu
                         ; MenuShell.append mb item
                         ; item
                       end

    in  (mb, Signal.connect openItem, Signal.connect closeItem)
    end

fun getFile () =
    let val fileSelector = FileSelection.new (SOME "Select a file for editing")
    in  Widget.show fileSelector
      ; SOME "foobar.txt"
    end

fun setUpGui() = 
    let val w = let val w = Window.new' ()
                in  Window.set_title w "Editor"
                  ; Window.set_default_size w 260 150
                  ; Signal.connect w (Widget.delete_event_sig delete_event)
                  ; w
                end

        val agrp = let val argp = AccelGroup.new ()
                   in  Window.add_accelgroup w argp
                     ; argp
                   end

        val statusbar = Statusbar.new()
        val menu_context = Statusbar.get_context_id statusbar "Menu bar"
        fun say msg = ignore(Statusbar.push statusbar menu_context msg)

        val (menubar, connectOpen, connectClose) = makeMenubar agrp say

        val textView = TextView.new()
        val scrolled = let val sw = ScrolledWindow.new'()
                       in  ScrolledWindow.set_policy sw POLICY_AUTOMATIC POLICY_AUTOMATIC
                         ; Container.add sw textView
                         ; sw
                       end

        val vbox = VBox.new' ()
    in  Box.pack_start vbox menubar (SOME false) (SOME false) (SOME 0)
      ; Box.pack_start' vbox scrolled
      ; Box.pack_start vbox statusbar (SOME false) (SOME false) (SOME 0)
      ; Container.add w vbox
      ; Widget.show_all w
      ; connectOpen (MenuItem.activate_sig (fn () => 
                                               (say "Open"; 
                                                case getFile() of
                                                    NONE => say "No file selected"
                                                  | SOME s => say ("File :"^s^" selected"); ())))
      ; connectClose (MenuItem.activate_sig (fn () => ignore(say "Close 4")))
    end

fun main () = ( GtkBasis.init(CommandLine.name()::CommandLine.arguments())
              ; setUpGui()
              ; GtkBasis.main()
              )

val _ = main()

end (* local *)
