fun hello _ = print "Hello World\n"

fun itemsignal n () = print ("itemsignal on item: "^n^"\n")
fun newItem name = 
    let val item = Gtk.menu_item_new_with_label name
    in  Gtk.connect_select item (itemsignal name)
      ; item
    end

fun makeMenu names =
    let val menu = Gtk.menu_new()
	val addItem = (Gtk.menu_append menu) o newItem
    in  app addItem names
      ; menu
    end


fun delete_event _ = ( print "delete event occurred\n"
		     ; false)
fun destroy _ = Gtk.main_quit()


fun main () =
    let val _       = Gtk.init(CommandLine.name() :: CommandLine.arguments())
	val window  = Gtk.window_new Gtk.WINDOW_TOPLEVEL
	val menubar = Gtk.menu_bar_new()
	val topmenu = makeMenu ["Foo", "Bar", "Zap"]
	val helpmenu= makeMenu ["About"]
	val top     = newItem "Top"
	val help    = newItem "Help"
	val box1    = Gtk.vbox_new false 0
	fun pack w  = Gtk.box_pack_start box1 w true true 0
        val button  = Gtk.button_new_with_label "Button"
    in  Gtk.connect_delete_event window delete_event 
      ; Gtk.connect_destroy window destroy
      ; Gtk.container_add window box1
      ; Gtk.menu_item_set_submenu top topmenu
      ; Gtk.menu_item_set_submenu help helpmenu
      ; Gtk.menu_bar_append menubar top
      ; Gtk.menu_bar_append menubar help
      ; Gtk.menu_item_right_justify help
      ; pack menubar
      ; pack button
      ; Gtk.widget_show_all window
      ; Gtk.main() 
    end
    
