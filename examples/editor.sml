local open Gtk in

fun uncurry f (x,y) = f x y

fun delete_event _ = ( GtkBasis.main_quit()
		     ; true
                     )

fun makeMenubar agrp = 
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

datatype chooser_kind = OPEN | SAVE 

fun getFile kind =
    let val (title, action, stock) = 
            case kind of 
                OPEN => ("Open File", FILE_CHOOSER_ACTION_OPEN, "gtk-open")
              | SAVE => ("Save As", FILE_CHOOSER_ACTION_SAVE, "gtk-save-as")

        val dialog = FileChooserDialog.new title NONE action NONE
        val _ = map (uncurry (Dialog.add_button dialog))
                    [ ("gtk-cancel", RESPONSE_CANCEL)
                    , (stock       , RESPONSE_ACCEPT)
                    ]

        val result = if Dialog.run dialog =  RESPONSE_ACCEPT then 
                         let val chooser = FileChooserDialog.asFileChooser dialog
                         in  SOME(FileChooser.get_filename chooser)
                         end
                     else NONE
    in  Widget.destroy dialog
      ; result
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

        val (menubar, connectOpen, connectClose) = makeMenubar agrp

        val textView = TextView.new()

        fun openAction () =
            let val filename = getFile OPEN
            in   case filename of
                     NONE => say "No file selected"
                   | SOME filename => 
                     let (* FIXME: check permissions and stuff *)
                         val dev = TextIO.openIn filename
                         val content = TextIO.inputAll dev before TextIO.closeIn dev
                         val buffer = TextView.get_buffer textView
                     in  TextBuffer.set_text buffer content ~1
                       ; say (OS.Path.file filename ^ " has " ^ Int.toString (TextBuffer.get_line_count buffer) ^ " lines")
                     end
            end

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
      ; connectOpen (MenuItem.activate_sig openAction)
      ; connectClose (MenuItem.activate_sig (fn () => ignore(say "Close 4")))
    end

fun main () = ( GtkBasis.init(CommandLine.name()::CommandLine.arguments())
              ; setUpGui()
              ; GtkBasis.main()
              )

val _ = main()

end (* local *)
