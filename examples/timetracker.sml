
fun topwindow() =
    let val window = Gtk.window_new Gtk.WINDOW_TOPLEVEL
    in  Gtk.connect_delete_event window (fn _ => false)
      ; Gtk.connect_destroy window Gtk.main_quit
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

fun overview () =
    let val clist  = Gtk.clist_new_with_titles 2 ["Project", "Time"] 
    in  Gtk.clist_set_column_auto_resize clist 0 true
      ; Gtk.clist_set_column_auto_resize clist 1 true
      ; Gtk.clist_set_column_justification clist 1 Gtk.JUSTIFY_RIGHT
      ; clist
    end

local infixr --> val op--> = Gtk.--> in
val select_row = 
    Gtk.signal "select_row" false 
    (Gtk.int --> Gtk.int --> Gtk.unit --> Gtk.return_unit)
val unselect_row = 
    Gtk.signal "unselect_row" false 
    (Gtk.int --> Gtk.int --> Gtk.unit --> Gtk.return_unit)
end

fun setTime clist t row = Gtk.clist_set_text clist row 1 (timeToString t)

fun update(xs, i, x) = List.take(xs, i) @ x :: List.drop(xs, i+1)

fun main () =
    let val _        = Gtk.init(CommandLine.name()::CommandLine.arguments())
	val window   = topwindow()
        val overview = overview()
        val proj = Gtk.entry_new()


        val projects = ref []
        val current  = ref NONE

        fun sec () = case !current of 
                         SOME(t, row) => let val t = incSec t
                                         in  current := SOME(t, row)
                                           ; setTime overview t row
                                           ; true
                                         end
                       | _            => true

                                         
        fun select row _ _   = let val t = List.nth(!projects, row)
                               in  current := SOME(t, row) end

        fun unselect _ _ _ = let val (t, row) = valOf(!current)
                             in  current  := NONE
                               ; projects := update(!projects, row, t) 
                             end 

        fun new () = 
            let val name = Gtk.entry_get_text proj
            in  if name <> "" then (Gtk.clist_append overview [name, "0:00:00"]
                                   ;projects := !projects @ [initTime]
                                   )
                else ()
            end

        val connect = Gtk.signalConnect overview
        val id = Gtk.timeout_add 1000 sec

	val box1   = Gtk.vbox_new false 0
	fun pack w = Gtk.box_pack_start box1 w true true 0
	val tW     = Gtk.toWidget

    in  Gtk.container_set_border_width window 10
      ; Gtk.box_pack_start box1 proj true true 0
      ; Gtk.box_pack_start box1 overview true true 0
      ; Gtk.container_add window box1
      ; connect (select_row select)
      ; connect (unselect_row unselect)
      ; Gtk.connect_activate proj new
      ; Gtk.widget_show_all window
      ; Gtk.main()
      ; Gtk.timeout_remove id 
    end
    
