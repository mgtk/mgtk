(* A small editor shows how ScrolledWindow works *)

fun clear ed = Gtk.editable_delete_text ed 0 ~1 

fun insertFile t fname =
    let val dev = TextIO.openIn fname
	val s   = TextIO.inputAll dev
    in  TextIO.closeIn dev
      ; clear t
      ; Gtk.text_insert' t s ~1
    end

fun loadFile ent t _ = insertFile t (Gtk.entry_get_text ent)

fun saveText t filename =
    let val s = Gtk.editable_get_chars t 0 ~1
	val dev = TextIO.openOut filename
    in  TextIO.output(dev,s)
      ; TextIO.closeOut dev
    end

fun saveFile ent t _ = saveText t (Gtk.entry_get_text ent)


fun delete_event _ = ( print "delete event occurred\n"
		     ; false)

fun destroy _ = Gtk.main_quit()

fun main () =
    let val _      = Gtk.init(CommandLine.name()::CommandLine.arguments())
	val window = Gtk.window_new Gtk.WINDOW_TOPLEVEL
	val ent1   = Gtk.entry_new ()
	val text   = Gtk.text_new' ()
	val load   = Gtk.button_new_with_label "Load"
	val save   = Gtk.button_new_with_label "Save"
	val box1   = Gtk.vbox_new false 0
	val box2   = Gtk.hbox_new false 2
	val scw    = Gtk.scrolled_window_new' ()
	val tW     = Gtk.toWidget
	val panel  = [tW ent1, tW load, tW save]
	val view   = [tW box2, tW scw]
	fun pack box w = (Gtk.box_pack_start box w true true 0;
			  Gtk.widget_show w)
    in  Gtk.connect_delete_event window delete_event 
      ; Gtk.connect_destroy window destroy
      ; Gtk.container_set_border_width window 5
      ; Gtk.connect_clicked load (loadFile ent1 text)
      ; Gtk.connect_clicked save (saveFile ent1 text)
      ; Gtk.text_set_editable text true
      ; Gtk.scrolled_window_set_policy scw Gtk.POLICY_NEVER Gtk.POLICY_AUTOMATIC
      ; Gtk.container_add scw text
      ; Gtk.widget_show text
      ; app (pack box2) panel
      ; app (pack box1) view
      ; Gtk.container_add window box1
      ; app Gtk.widget_show [tW window, tW box1]
      ; Gtk.main() 
    end
    
