(* An example adopted from "Mono, A Developers Notebook" (04-gtk/06-menus) *)

local open Gtk in

fun leftAlign lab = 
    Misc.set_alignment lab 0.0 0.5 (* Left align X and center Y *)

fun delete_event _ = ( GtkBasis.main_quit()
                     ; print("back in mosml\n")
		     ; true
                     )
fun say message () = TextIO.print (message^"\n")
val openActivated = say "Open"
val rotateActivated = say "Rotate" 

fun setUpGui() = 
    let val w = let val w = Window.new' ()
                in  Window.set_title w "Menus Example"
                  ; Window.set_default_size w 260 150
                  ; Signal.connect w (Widget.delete_event_sig delete_event)
                  ; w
                end

        val mb = MenuBar.new()

        val agrp = let val argp = AccelGroup.new ()
                   in  Window.add_accelgroup w argp
                     ; argp
                   end

        (* file menu *)
        val file_menu = Menu.new()
        val file_item = let val item = MenuItem.new_with_mnemonic ("_File")
		        in  MenuItem.set_submenu item file_menu
                          ; MenuShell.append mb item
                          ; item
                        end

        val openItem = let val item = ImageMenuItem.new_from_stock "gtk-open" agrp
                       in  Signal.connect item (MenuItem.activate_sig openActivated)
                         ; MenuShell.append file_menu item
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

        val transformItem = MenuItem.new_with_mnemonic "_Transform"
        val transform_menu = let val transform_menu = Menu.new()
                             in  MenuItem.set_submenu transformItem transform_menu 
                               ; MenuShell.append edit_menu transformItem
                               ; transform_menu
                           end

	val rotateItem = let val item = MenuItem.new_with_mnemonic "_Rotate"
                         in  (* custom accelerator *)
		             (* Widget.add_accelerator item "activate" agrp
			                               (AccelKey.new Gdk.Key.R Gdk.ModifierType.ControlMask AccelFlags.Visible)
                              ; *)
                             Signal.connect item (MenuItem.activate_sig rotateActivated)
                           ; MenuShell.append transform_menu item
                           ; item
		         end

        val _ = MenuShell.append transform_menu (MenuItem.new_with_mnemonic "_Flip")

        val vbox = VBox.new' ()
    in  Box.pack_start vbox mb (SOME false) (SOME false) (SOME 0)                 
      ; Container.add w vbox
      ; Widget.show_all w
    end

fun main () = ( GtkBasis.init(CommandLine.name()::CommandLine.arguments())
              ; setUpGui()
              ; GtkBasis.main()
              ; print "done in main\n"
              )

val _ = main()

end (* local *)
