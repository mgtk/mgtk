fun topwindow() =
    let val window = Gtk.window_new Gtk.WINDOW_TOPLEVEL
    in  Gtk.connect_delete_event window (fn _ => false)
      ; Gtk.connect_destroy window (fn _ => Gtk.main_quit())
      ; window
    end


fun main () =
    let val _      = Gtk.init(CommandLine.name()::CommandLine.arguments())
	val window = topwindow()
	val label  = Gtk.label_new ""

        val counter = ref 0
        fun idle_up() = ( counter := !counter + 1
                        ; Gtk.label_set_text label (Int.toString(!counter))
                        ; if !counter = 10000 then ( Gtk.idle_add idle_down
                                                   ; false
                                                   )
                          else true
                        )
        and idle_down() = ( counter := !counter - 1
                        ; Gtk.label_set_text label (Int.toString(!counter))
                        ; if !counter = 0 then ( Gtk.idle_add idle_up
                                               ; false
                                               )
                          else true
                        )

    in  Gtk.container_set_border_width window 10
      ; Gtk.idle_add idle_up
      ; Gtk.container_add window label
      ; Gtk.widget_show_all window
      ; Gtk.main() 
    end
