fun print_sel cal _ = 
    let val (setYear, setMonth, setDay) = Gtk.calendar_get_date cal
    in  print (concat["you requested: ", 
		      Int.toString (Word.toInt setYear),", ", 
		      Int.toString (Word.toInt setMonth+1), ", ", 
		      Int.toString (Word.toInt setDay), "\n"])
    end

fun delete_event _ = ( print "delete event occurred\n"
		     ; false)

fun destroy _ = Gtk.main_quit()

fun main () =
    let val _      = Gtk.init(CommandLine.name()::CommandLine.arguments())
	val window = Gtk.window_new Gtk.WINDOW_TOPLEVEL
	val cal    = Gtk.calendar_new ()
    in  Gtk.connect_delete_event window delete_event 
      ; Gtk.connect_destroy window destroy
      ; Gtk.container_set_border_width window 10
      ; Gtk.container_add window cal
      ; Gtk.connect_day_selected cal (print_sel cal)
      ; Gtk.window_set_title window "Calendar Example"
      ; Gtk.widget_show cal
      ; Gtk.widget_show window
      ; Gtk.main() 
    end
    
