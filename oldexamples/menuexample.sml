

fun newItem say name = 
    let val item = Gtk.menu_item_new_with_label name
    in  Gtk.connect_menu_item_activate item (fn() => say (name^"\n"))
      ; item
    end

fun makeMenu say names =
    let val menu = Gtk.menu_new()
	val addItem = (Gtk.menu_append menu) o (newItem say)
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
        val text    = Gtk.text_new'()
	fun say s   = Gtk.text_insert' text s ~1
	val topmenu = makeMenu say ["Foo", "Bar", "Zap"]
	val helpmenu= makeMenu say ["About"]
	val top     = newItem say "Top"
	val help    = newItem say "Help"
	val box1    = Gtk.vbox_new false 0
	fun pack w  = Gtk.box_pack_start box1 w true true 0

    in  Gtk.connect_delete_event window delete_event 
      ; Gtk.connect_destroy window destroy
      ; Gtk.container_add window box1
      ; Gtk.menu_item_set_submenu top topmenu
      ; Gtk.menu_item_set_submenu help helpmenu
      ; Gtk.menu_bar_append menubar top
      ; Gtk.menu_bar_append menubar help
      ; Gtk.menu_item_right_justify help
      ; Gtk.text_set_editable text false
      ; pack menubar
      ; pack text
      ; Gtk.widget_show_all window
      ; Gtk.main() 
    end
    
