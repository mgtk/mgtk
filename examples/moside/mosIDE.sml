(* A small IDE for Moscow ML *)

fun clear ed = Gtk.editable_delete_text ed 0 ~1 
fun getAll ed = Gtk.editable_get_chars ed 0 ~1

fun cinsert text font string = 
    ( clear text
    ; Gtk.text_insert text font NONE NONE string ~1
    )

fun error text = cinsert text NONE "An error happened!\n"

fun insertFile text font fname =
    let val dev = TextIO.openIn fname
	val s   = TextIO.inputAll dev
    in  TextIO.closeIn dev
      ; cinsert text (SOME font) s
    end

fun loadFile ent text font _ = insertFile text font (Gtk.entry_get_text ent)
    handle _ => error text

fun saveText text filename =
    let val s = getAll text
	val dev = TextIO.openOut filename
    in  TextIO.output(dev,s)
      ; TextIO.closeOut dev
    end 

fun saveFile ent text _ = saveText text (Gtk.entry_get_text ent)
    handle _ => error text

fun compile font send out inp _ =
    let val s = getAll inp
	val res = send s
    in  Gtk.text_insert out (SOME font) NONE NONE res ~1
    end

fun pack box w = 
    ( Gtk.box_pack_start box w true true 0
    ; Gtk.widget_show w)

fun addscw (scw, t) = 
    ( Gtk.scrolled_window_set_policy scw Gtk.POLICY_NEVER Gtk.POLICY_AUTOMATIC
    ; Gtk.container_add scw t
    ; Gtk.widget_show t
    )

fun delete_event _ = false
fun destroy quit _ = (quit(); Gtk.main_quit())

fun main () =
    let val _      = Gtk.init(CommandLine.arguments())
	val window = Gtk.window_new Gtk.WINDOW_TOPLEVEL
	val ent1   = Gtk.entry_new ()
	val ent2   = Gtk.entry_new ()
	val edit   = Gtk.text_new' ()
	val outp   = Gtk.text_new' ()
	val load   = Gtk.button_new_with_label "Load"
	val save   = Gtk.button_new_with_label "Save"
	val comp   = Gtk.button_new_with_label "Eval"
	val box1   = Gtk.vbox_new false 0
	val box2   = Gtk.hbox_new false 2
	val scwe   = Gtk.scrolled_window_new' ()
	val scwo   = Gtk.scrolled_window_new' ()
	val tW     = Gtk.toWidget
	val panel  = [tW ent1, tW load, tW save, tW comp]
	val view   = [tW box2, tW scwe, tW scwo, tW ent2]
	val (pre,mosml) = MosmlToplevel.new["-P full"]

	val upright = Gtk.gdk_font_load "-*-courier-medium-r-*-*-*-100-*-*-*-*-*-*"
	val italics = Gtk.gdk_font_load "-*-courier-medium-o-*-*-*-100-*-*-*-*-*-*"
	fun eval inp = compile italics (#send mosml) outp inp

    in  Gtk.connect_delete_event window delete_event 
      ; Gtk.connect_destroy window (destroy (#quit mosml))
      ; Gtk.window_set_title window "mosIDE"
      ; Gtk.window_set_default_size window 400 320
      ; Gtk.container_set_border_width window 5
      ; app (fn (b, f) => Gtk.connect_clicked b f) 
	[(load,loadFile ent1 edit upright), (save, saveFile ent1 edit),
	 (comp, eval edit)]
      ; Gtk.connect_activate ent2 (eval ent2)
      ; Gtk.text_set_editable edit true
      ; Gtk.text_set_editable outp false
      ; cinsert outp (SOME italics) pre
      ; app addscw [(scwe,edit), (scwo,outp)]
      ; app (pack box2) panel
      ; Gtk.box_pack_start box1 box2 false false 0
      ; Gtk.box_pack_start box1 scwe true true 0
      ; Gtk.box_pack_start box1 scwo true true 0
      ; Gtk.box_pack_start box1 ent2 false false 0
      ; app Gtk.widget_show view
      ; Gtk.container_add window box1
      ; app Gtk.widget_show [tW window, tW box1]
      ; Gtk.main() 
    end
    
val _ = main ()