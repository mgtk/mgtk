(* An example adopted from "Mono, A Developers Notebook" (04-gtk/07-treeview) *)

local open Gtk in

fun delete_event _ = ( GtkBasis.main_quit()
		     ; true
                     )

fun setUpGui() = 
    let val w = let val w = Window.new Window.TOPLEVEL
                in  Window.set_title w "Tree"
                  ; Signal.connect w (Widget.delete_event_sig delete_event)
                  ; w
                end

        val vbox = let val vbox = VBox.new false 0
                   in  Container.set_border_width vbox 6
                     ; Container.add w vbox
                     ; vbox
                   end
                       
        val tv = let val tv = TreeView.new()
                 in  TreeView.set_headers_visible tv true
                   ; Container.add vbox tv
                   ; tv
                 end

        val col1 = let val col  = TreeViewColumn.new ()
                       val colr = CellRendererText.new ()
                   in  TreeViewColumn.set_title col "Column 1"
                     ; TreeViewColumn.pack_start col colr true
                     ; TreeViewColumn.add_attribute col colr "text" 0
                     ; TreeView.append_column tv col
                     ; col
                   end
                                   
        val store = let val store = TreeStore.newv 1 [GType.string]
                    in  TreeView.set_model tv (SOME (TreeStore.asTreeModel store))
                      ; store
                    end

        fun populate n = 
            let fun loop i = 
                    if i < n then 
                        let val parent = TreeStore.append' store
                            fun inner j = 
                                if j >= 0 then 
                                    let val child = TreeStore.append store (SOME parent)
                                    in  TreeStore.set_value store child 0 (GValue.string("Visited "^Int.toString j))
                                      ; inner (j-1)
                                    end
                                else ()   
                        in  TreeStore.set_value store parent 0 (GValue.string("Point "^Int.toString i))
                          ; inner (i-1)
                          ; loop (i+1)
                        end
                    else ()
            in loop 0
            end

    in  populate 4
      ; Widget.show_all w
    end

fun main () = ( GtkBasis.init(CommandLine.name()::CommandLine.arguments())
              ; setUpGui()
              ; GtkBasis.main()
              )

val _ = main()

end (* local *)
