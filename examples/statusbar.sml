fun clear ed = Gtk.editable_delete_text ed 0 ~1 

fun insertFile t fname =
    let val dev = TextIO.openIn fname
	val s   = TextIO.inputAll dev
    in  TextIO.closeIn dev
      ; clear t
      ; Gtk.text_insert' t s ~1
    end

fun loadFile ent t s id _ = 
    let val file = Gtk.entry_get_text ent
    in  insertFile t file
      ; Gtk.statusbar_pop s id
      ; Gtk.statusbar_push s id file
      ; ()
    end
			  
fun text_changed t s id _ =
    let val pos = Word.toInt(Gtk.text_get_point t)
	val posStr = String.concat ["pos: ", Int.toString pos]
    in  Gtk.statusbar_pop s id
      ; Gtk.statusbar_push s id posStr
      ; ()
    end


fun delete_event _ = ( print "delete event occurred\n"
		     ; false)

fun destroy _ = Gtk.main_quit()

fun main () =
    let val _      = Gtk.init(CommandLine.name()::CommandLine.arguments())
	val window = Gtk.window_new Gtk.WINDOW_TOPLEVEL
	val ent1   = Gtk.entry_new ()
	val text   = Gtk.text_new' ()
	val load   = Gtk.button_new_with_label "Load"
	val main   = Gtk.vbox_new false 1
	val box1   = Gtk.vbox_new false 0
	val box2   = Gtk.hbox_new false 2
	val scw    = Gtk.scrolled_window_new' ()
	val stat1  = Gtk.statusbar_new ()
	val stat2  = Gtk.statusbar_new ()
	val id1    = Gtk.statusbar_get_context_id stat1 "File Name"
	val id2    = Gtk.statusbar_get_context_id stat2 "Position"
	val statb  = Gtk.hbox_new false 1
	val tW     = Gtk.toWidget
	val panel  = [(tW ent1,true), (tW load,false)]
	val view   = [(tW box2,false), (tW scw,true)]
	fun pack box (w,e) = (Gtk.box_pack_start box w e true 0;
			      Gtk.widget_show w)
    in  Gtk.connect_delete_event window delete_event 
      ; Gtk.connect_destroy window destroy
      ; Gtk.connect_button_press_event text (text_changed text stat2 id2)
      ; Gtk.connect_key_press_event text (text_changed text stat2 id2)
      ; Gtk.container_set_border_width window 5
      ; Gtk.connect_clicked load (loadFile ent1 text stat1 id1)
      ; Gtk.text_set_editable text true
      ; Gtk.scrolled_window_set_policy scw Gtk.POLICY_NEVER Gtk.POLICY_AUTOMATIC
      ; Gtk.container_add scw text

      ; Gtk.statusbar_push stat1 id1 "Status bar example"

      ; Gtk.statusbar_push stat2 id2 "<empty>"

      ; Gtk.widget_show text

      ; app (pack box2) panel
      ; app (pack box1) view
      ; app (pack main) [(tW box1,true), (tW statb,false)]
      ; app (pack statb) [(tW stat1,true), (tW stat2,true)]

      ; Gtk.container_add window main
      ; app Gtk.widget_show [tW window, tW main]
      ; Gtk.main() 
    end
    
