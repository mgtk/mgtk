
fun month_changed cal _ = 
    let val (setYear, setMonth, setDay) = Gtk.calendar_get_date cal
	val toDay = Date.fromTimeLocal (Time.now ())
	val day = Word.fromInt(Date.day toDay)
	val year = Word.fromInt(Date.year toDay)
	val month = Word.fromInt(
			case Date.month toDay of
			  Date.Jan => 1
                        | Date.Feb => 2
                        | Date.Mar => 3
			| Date.Apr => 4
			| Date.May => 5
			| Date.Jun => 6
			| Date.Jul => 7
			| Date.Aug => 8
			| Date.Sep => 9
			| Date.Oct => 10
			| Date.Nov => 11
			| Date.Dec => 12)
    in  if setYear = year andalso setMonth = month andalso setDay = day
        then ( print ("Doing nothing!\n") )
        else ( print ("Changed month; reseting to current date\n")
             ; print (concat["  (you requested: ", 
			     Word.toString setYear,", ", 
			     Word.toString setMonth, ", ", 
			     Word.toString setDay, ")\n"])
             ; Gtk.calendar_select_day cal day
             ; Gtk.calendar_select_month cal month year
             ; ())
    end

fun delete_event _ = ( print "delete event occurred\n"
		     ; false)

fun destroy _ = Gtk.main_quit()

fun main () =
    let val _      = Gtk.init(CommandLine.arguments())
	val window = Gtk.window_new Gtk.WINDOW_TOPLEVEL
	val cal    = Gtk.calendar_new ()

    in  Gtk.connect_delete_event window delete_event 
      ; Gtk.connect_destroy window destroy
      ; Gtk.container_set_border_width window 10
      ; Gtk.connect_month_changed cal (month_changed cal)
      ; Gtk.container_add window cal
      ; Gtk.window_set_title window "Calendar Example"
      ; Gtk.widget_show cal
      ; Gtk.widget_show window
      ; Gtk.main() 
    end
    
