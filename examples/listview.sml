(* An example adopted from "Mono, A Developers Notebook" (04-gtk/07-listview) *)

local open Gtk in

fun delete_event _ = ( GtkBasis.main_quit()
		     ; true
                     )

fun setUpGui() = 
    let val w = let val w = Window.new Window.TOPLEVEL
                in  Window.set_title w "List"
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
                       
        val col2 = let val col  = TreeViewColumn.new ()
                       val colr = CellRendererText.new ()
                   in  TreeViewColumn.set_title col "Column 2"
                     ; TreeViewColumn.pack_start col colr true
                     ; TreeViewColumn.add_attribute col colr "text" 1
                     ; TreeView.append_column tv col
                     ; col
                   end
            
        val store = let val store = ListStore.newv 2 [GType.string, GType.string]
                    in  TreeView.set_model tv (SOME (ListStore.asTreeModel store))
                      ; store
                    end

        fun populate n = 
            let fun loop i = 
                    if i < n then 
                        let val iter = ListStore.append store 
                        in  ListStore.set_value store iter 0 (GValue.string("Point "^Int.toString i))
                          ; ListStore.set_value store iter 1 (GValue.string("Distance "^Int.toString(4-i)))
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
