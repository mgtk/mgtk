(* Peter S. valuta example, shows that exceptions in cb are not handled *)

local 
    fun delete_event _ = false
    fun destroy _ = Gtk.main_quit()
in  
    fun window () = 
	let val win = Gtk.window_new Gtk.WINDOW_TOPLEVEL
	in  Gtk.connect_delete_event win delete_event 
	  ; Gtk.connect_destroy win destroy
	  ; Gtk.container_set_border_width win 5
	  ; win
	end
end

fun labent s editable =
    let val box = Gtk.hbox_new false 1
	val lab = Gtk.label_new s
	val ent = Gtk.entry_new ()
	fun pack w = (Gtk.box_pack_start box w true true 0;
		      Gtk.widget_show w)
    in  pack lab; pack ent
      ; Gtk.entry_set_editable ent editable
      ; Gtk.widget_show box
      ; (fn _ => Gtk.entry_get_text ent, Gtk.entry_set_text ent, box)
    end

fun main () =
    let val _      = Gtk.init(CommandLine.name()::CommandLine.arguments())
	val window = window()
	val (getarg, _, box1) = labent "S. Francs" true
	val (_, setres, box2) = labent "Kroner" false
	val box3   = Gtk.vbox_new true 2
	val but    = Gtk.button_new_with_label "Beregn"
	fun action _ = 
	    setres(case Int.fromString(getarg()) of
		       NONE   => "Fejl i indput"
		     | SOME i => Real.toString(real i * 4.65))

	val tW     = Gtk.toWidget
	val ws     = [tW box1, tW but, tW box2]
	fun pack w = (Gtk.box_pack_start box3 w true true 0;
		      Gtk.widget_show w)
    in  Gtk.connect_clicked but action
      ; app pack ws
      ; Gtk.container_add window box3
      ; app Gtk.widget_show [tW window, tW box3]
      ; Gtk.main() 
    end
    
