(* An example adopted from "Mono, A Developers Notebook" (04-gtk/08-dragdrop) *)

local open Gtk in

fun delete_event _ = ( GtkBasis.main_quit()
		     ; true
                     )

(* Defects 
val monkey = Gdk.Pixbuf.new_from_file "monkey.png"
             handle GError msg => raise Fail("GError: " ^ msg)
 *)


(* the media types we'll accept *)
val target_table = let val target_list = [ TargetEntry.new "text/uri-list" 0 0
                                         , TargetEntry.new "application/x-monkey" 0 1
                                         ]
                   in  TargetList.new target_list
                   end

(* the media types we'll send *)
val source_table = let val source_list = [ TargetEntry.new "application/x-monkey" 0 0 ]
                   in  TargetList.new source_list
                   end

fun dataReceived drag_context x y data info time =
    let val data = Gtk.SelectionData.toString data  
        val succes = 
            case info of
                0 => (* uri-list *) 
                     let val uri_list = String.tokens (fn c => c = #"\n") data 
                     in  List.app (fn u => TextIO.print ("Got URI "^u^"\n")) uri_list
                       ; true
                     end
              | 1 => (* monkey *) 
                     ( TextIO.print ("Monkey '"^data^"' was dropped\n")
                     ; true
                     )
              | _ => false 
    in  Widget.drag_finish drag_context success false time 
    end

fun dataBegin drag_context = 
(* Defect
    Widget.drag_set_icon_pixbuf drag_context monkey 0 0
*)
    let val monkeyImage = Image.new_from_file "monkey.png"
        (* The following might be wrong *)
    in  Widget.drag_set_icon_widget drag_context monkeyImage 0 0
    end

fun dataGet drag_context data info time =
    let val tagets = #targets drag_context
    in  data.set (Glist.nth_data tagets 0) 8 "Rupert"
    end  



fun setUpGui() = 
    let val w = let val w = Window.new' ()
                in  Window.set_title w "Drag & drop"
                  ; Signal.connect w (Widget.delete_event_sig delete_event)
                  ; w
                end
                    
        val hbox = let val hbox = HBox.new' ()
                   in  Container.set_border_width hbox 6
                     ; Box.set_spacing hbox 6
                     ; Container.add w hbox
                     ; hbox
                   end
                     
        val image = let val image = EventBox.new()
                        (* Defects
                         val monkeyImage = Image.new_from_pixbuf monkey
                         *)
                        val monkeyImage = Image.new_from_file "monkey.png"
                    in  Container.add image monkeyImage
                      ; Container.add h image
                      ; image
                    end

        val label = let val label = Label.new (SOME "Drop stuff here")
                    in  Container.add h image
                      ; label
                    end
    in  Widget.show_all w
      ; (* set up label as a drop target *)
        Widget.drag_dest_set label Widget.DEST_DEFAULT_ALL target_table Gdk.Drag.COPY
      ; Signal.connect label (Widget.drag_data_received_sig dataReceived)

      ; (* set up image as a drag source *)
        Widget.drag_source_set image Gdk.Windows.BUTTON1MASK source_table Gdk.Drag.COPY
      ; Signal.connect image (Widget.drag_data_get_sig dataGet)
      ; Signal.connect image (Widget.drag_data_begin_sig dataBegin)
    end

fun main () = ( GtkBasis.init(CommandLine.name()::CommandLine.arguments())
              ; setUpGui()
              ; GtkBasis.main()
              )

val _ = main()

end (* local *)
