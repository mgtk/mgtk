fun makeDate (y, m, d) =
    let open Date
	val month = case m of 
		       0w0 => Jan | 0w1 => Feb | 0w2 => Mar | 0w3 => Apr 
		     | 0w4 => May | 0w5 => Jun | 0w6 => Jul | 0w7 => Aug 
		     | 0w8 => Sep | 0w9 => Oct | 0w10 => Nov | _ => Dec
    in  date{year=Word.toInt y, month=month, day=Word.toInt d,
	     hour=0, minute=0, second=0,
	     offset=NONE}
    end

fun print_sel cal _ = 
    let val date = makeDate (Gtk.calendar_get_date cal)
    in   print (Date.fmt "You requested: %B %d, %Y.  Which is a %A\n" date)
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
      ; Gtk.connect_day_selected_double_click cal (print_sel cal)
      ; Gtk.window_set_title window "Calendar Example"
      ; Gtk.widget_show cal
      ; Gtk.widget_show window
      ; Gtk.main() 
    end
    
