(* Illustrates the use of timeouts *)
(* load"Gtk"; *)

fun topwindow() =
    let val window = Gtk.window_new Gtk.WINDOW_TOPLEVEL
    in  Gtk.connect_delete_event window (fn _ => false)
      ; Gtk.connect_destroy window (fn _ => Gtk.main_quit())
      ; window
    end

(* some time utilities *)

type time = int * int * int (* hours * minutes * seconds *)

val initTime = (0,0,0)

fun incSec (h, m, s) =
    if s = 59 then if m = 59 then (h+1,0,0)
                   else           (h,m+1,0)
    else                          (h,m,s+1)

fun zpad n = StringCvt.padLeft #"0" 2 (Int.toString n)
fun timeToString (h,m,s) = 
    String.concatWith ":" [Int.toString h, zpad m, zpad s]

fun makeTimer () =
    let val label   = Gtk.label_new "0:00:00"
        val running = ref false
        val timer   = ref initTime
        fun update() = 
            if !running then ( timer := incSec(!timer)
                             ; Gtk.label_set_text label (timeToString(!timer))
                             ; true
                             )
            else true
        fun start () = running := true
        fun stop ()  = running := false
        val id      = Gtk.timeout_add 1000 update (* update once per sec *)
    in  (label, id, start, stop) end
                                     
fun main () =
    let val _      = Gtk.init(CommandLine.name()::CommandLine.arguments())
	val window = topwindow()
	val startB = Gtk.button_new_with_label "Start"
	val stopB  = Gtk.button_new_with_label "Stop"
        val (timer, id, start, stop) = makeTimer ()
	val box1   = Gtk.vbox_new false 0
	fun pack w = Gtk.box_pack_start box1 w true true 0
	val tW     = Gtk.toWidget
    in  Gtk.container_set_border_width window 10
      ; Gtk.connect_clicked stopB stop
      ; Gtk.connect_clicked startB start
      ; app pack [tW timer, tW startB, tW stopB] 
      ; Gtk.container_add window box1
      ; Gtk.widget_show_all window
      ; Gtk.main()
      ; Gtk.timeout_remove id (*only for convenience in the interactive loop*)
    end
    
