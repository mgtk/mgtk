
fun main () =
    let val _      = Gtk.init(CommandLine.name()::CommandLine.arguments())
	val window = Gtk.window_new Gtk.WINDOW_TOPLEVEL
	val clist  = Gtk.clist_new 3
    in  Gtk.connect_delete_event window (fn _ => false)
      ; Gtk.connect_destroy window (fn _ => Gtk.main_quit())
      ; Gtk.container_set_border_width window 10
      ; Gtk.container_add window clist
      ; Gtk.clist_set_column_auto_resize clist 0 true
      ; Gtk.clist_set_column_auto_resize clist 1 true
      ; Gtk.clist_set_column_auto_resize clist 2 true
      ; Gtk.clist_set_column_justification clist 1 Gtk.JUSTIFY_RIGHT
      ; Gtk.widget_show_all window
      ; map (Gtk.clist_append clist) [["foo", "bar", "zap"], 
                                      ["FOO", "BARBAR", "ZAP"]]
      ; Gtk.main() 
    end
    
