local 
    structure BasisList = List
    open Gtk 
in

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

        fun makeStockItem menu stock =
            let val item = ImageMenuItem.new_from_stock stock agrp
            in  MenuShell.append menu item
              ; item
            end

        val openItem = makeStockItem file_menu "gtk-open"

        val closeItem = makeStockItem file_menu "gtk-close"

        val _ = MenuShell.append file_menu (SeparatorMenuItem.new())

        val saveAsItem = makeStockItem file_menu "gtk-save-as"
		
	val _ = MenuShell.append file_menu (SeparatorMenuItem.new())
		
        val quitItem = let val item = makeStockItem file_menu "gtk-quit"
                       in  Signal.connect item (MenuItem.activate_sig GtkBasis.main_quit)
                         ; item
                       end

        (* edit menu *)
	val edit_menu = Menu.new ()
        val editItem = let val item = MenuItem.new_with_mnemonic "_Edit"
                       in  MenuItem.set_submenu item edit_menu
                         ; MenuShell.append mb item
                         ; item
                       end

        fun activateConnect item action = 
            Signal.connect item (MenuItem.activate_sig action)

    in  (mb, activateConnect openItem
           , activateConnect closeItem
           , activateConnect saveAsItem)
    end

datatype chooser_kind = OPEN | SAVE 

fun getFile kind =
    let val (title, action, stock) = 
            case kind of 
                OPEN => ("Open File", FileChooser.ACTION_OPEN, "gtk-open")
              | SAVE => ("Save As", FileChooser.ACTION_SAVE, "gtk-save-as")

        val dialog = FileChooserDialog.new NONE NONE action
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
    let val w = let val w = Window.new Window.TOPLEVEL
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

        val (menubar, connectOpen, connectClose, connectSaveAs) = 
            makeMenubar agrp

        val notebook = Notebook.new()
        val buffers = ref[]


        fun openAction () =
            let val filename = getFile OPEN
            in   case filename of
                     NONE => say "No file selected"
                   | SOME path => 
                     let (* FIXME: check permissions and stuff *)
                         val {dir, file} = OS.Path.splitDirFile path
                         val _ = OS.FileSys.chDir dir
                         val dev = TextIO.openIn file
                         val content = TextIO.inputAll dev 
                                       before TextIO.closeIn dev
                         
                         val textView = TextView.new()
                         val buffer = TextView.get_buffer textView
                         val scrolled = ScrolledWindow.new'()
                         val lab = Label.new file
                     in  ScrolledWindow.set_policy scrolled POLICY_AUTOMATIC 
                                                            POLICY_AUTOMATIC
                       ; Container.add scrolled textView  
                       ; TextBuffer.set_text buffer content ~1
                       ; Widget.show_all scrolled
                       ; Widget.show lab
                       ; Notebook.prepend_page notebook scrolled (SOME lab)
                       ; Notebook.set_current_page notebook 0
                       ; buffers := buffer :: !buffers
                       ; say (file ^ " has " ^ 
                              Int.toString (TextBuffer.get_line_count buffer) ^
                              " lines")
                     end
            end

        fun closeAction () =
            let val n = Notebook.get_current_page notebook
            in  if n >= 0 then
                    let val first = BasisList.take(!buffers, n)
                        val last  = BasisList.drop(!buffers, n+1)
                    in  Notebook.remove_page notebook n
                      ; buffers := first @ last
                      ; say ("Closing tab number "^Int.toString n)
                    end
                else ()
            end


        fun saveAsAction () =
            case getFile SAVE of
                NONE => say "The buffer was not saved"
              | SOME path =>
                let val n = Notebook.get_current_page notebook
                in  if n < 0 then say "Nothing to save"
                    else let (* FIXME: check permissions and stuff *)
                            val {dir, file} = OS.Path.splitDirFile path
                            val _ = OS.FileSys.chDir dir
                            val dev = TextIO.openOut file
                                      
                            val buffer = BasisList.nth(!buffers, n)
                            val (startIter, endIter) = TextBuffer.get_bounds buffer
                            val content = TextBuffer.get_text buffer startIter endIter false
                        in  TextIO.output(dev, content)
                          ; TextIO.closeOut dev 
                          ; say ("Buffer saved to file "^file)
                        end
                end
            

        val vbox = VBox.new false 0
    in  Box.pack_start vbox menubar false false 0
      ; Box.pack_start vbox notebook true true 0
      ; Box.pack_start vbox statusbar false false 0
      ; Container.add w vbox
      ; Widget.show_all w
      ; connectOpen openAction
      ; connectClose closeAction
      ; connectSaveAs saveAsAction
    end

fun main () = ( GtkBasis.init(CommandLine.name()::CommandLine.arguments())
              ; setUpGui()
              ; GtkBasis.main()
              )

val _ = main()

end (* local *)
