fun delete_event _ = false
fun destroy _ = Gtk.main_quit()

fun mkArrowLabel combo label string =
    let val item  = Gtk.list_item_new ()
	val hbox  = Gtk.hbox_new false 3
	val arrow = Gtk.arrow_new Gtk.ARROW_RIGHT Gtk.SHADOW_OUT
	val label = Gtk.label_new label
    in  Gtk.container_add item hbox
      ; Gtk.box_pack_start hbox arrow false false 0
      ; Gtk.box_pack_start hbox label false false 0
      ; Gtk.combo_set_item_string combo item string
      ; Gtk.widget_show hbox
      ; Gtk.widget_show item
      ; Gtk.widget_show arrow
      ; Gtk.widget_show label
      ; item
    end

fun mkComplexCombo parent =
    let val combo = Gtk.combo_new ()
	val item1 = mkArrowLabel combo "First Item" "1st item"
	val item2 = mkArrowLabel combo "Second Item" "2nd item"
    in  Gtk.container_add (Gtk.combo_get_list combo) item1
      ; Gtk.container_add (Gtk.combo_get_list combo) item2
      ; Gtk.widget_show item1
      ; Gtk.widget_show item2
      ; combo
    end

fun main () =
    let val _      = Gtk.init(CommandLine.name()::CommandLine.arguments())
	val window = Gtk.window_new Gtk.WINDOW_TOPLEVEL
	val combo = mkComplexCombo window

    in  Gtk.connect_delete_event window delete_event 
      ; Gtk.connect_destroy window destroy
      ; Gtk.container_set_border_width window 10
      ; Gtk.container_add window combo
      ; Gtk.widget_show combo
      ; Gtk.widget_show window
      ; Gtk.main() 
    end
    
