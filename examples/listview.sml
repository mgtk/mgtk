(* An example adopted from "Mono, A Developers Notebook" (04-gtk/07-listview) *)

local open Gtk in

fun delete_event _ = ( GtkBasis.main_quit()
		     ; true
                     )

fun setUpGui() = 
    let val w = let val w = Window.new' ()
                in  Window.set_title w "List"
                  ; Signal.connect w (Widget.delete_event_sig delete_event)
                  ; w
                end

        val vbox = let val vbox = VBox.new' ()
                   in  Box.set_border_width vbox 6
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
                     ; TreeView.add_column tv col
                     ; col
                   end
                       
        val col2 = let val col  = TreeViewColumn.new ()
                       val colr = CellRendererText.new ()
                   in  TreeViewColumn.set_title col "Column 2"
                     ; TreeViewColumn.pack_start col colr true
                     ; TreeViewColumn.add_attribute col colr "text" 1
                     ; TreeView.add_column tv col
                     ; col
                   end
            
        (*** DEFECTS CITY STARTS HERE ***)
                       
        val store = let val store = ListStore.new [GType.string, GType.string]
                    in  TreeView.set_model tv (SOME store)
                      ; store
                    end

        fun populate n = 
            let fun loop i = 
                    if i < n then 
                        let val iter = ListStore.append store 
                        in  ListStore.set_value store iter 
                                                [ "Point "^Int.toString i
                                                , "Distance "^Int.toString(4-i)]
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
