fun modlab lab get_text _ = Gtk.label_set_text lab (Gtk.label_get lab^
						    get_text())
fun clear ent _ = Gtk.entry_set_text ent ""

fun delete_event _ = ( print "delete event occurred\n"
		     ; false)

fun destroy _ = Gtk.main_quit()

fun main () =
    let val _      = Gtk.init(CommandLine.arguments())
	val window = Gtk.window_new Gtk.WINDOW_TOPLEVEL
	val ent1   = Gtk.entry_new ()
	val ent2   = Gtk.entry_new ()
	val lab1   = Gtk.label_new ""
	val lab2   = Gtk.label_new ""
	val mod1   = modlab lab1 (fn _ => Gtk.entry_get_text ent1)
	val mod2   = modlab lab2 (fn _ => Gtk.entry_get_text ent2)
	val box1   = Gtk.vbox_new false 0
	val tW     = Gtk.toWidget
	val ws     = [tW ent1, tW lab1, tW ent2, tW lab2]
	fun pack w = (Gtk.box_pack_start box1 w true true 0;
		      Gtk.widget_show w)
    in  Gtk.connect_delete_event window delete_event 
      ; Gtk.connect_destroy window destroy
      ; Gtk.container_set_border_width window 5
      ; Gtk.connect_activate ent1 mod1
      ; Gtk.connect_changed ent2 mod2
      ; app pack ws
      ; Gtk.container_add window box1
      ; app Gtk.widget_show [tW window, tW box1]
      ; Gtk.main() 
    end
    
