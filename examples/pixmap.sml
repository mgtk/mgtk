val _ = Gtk.init(CommandLine.name()::CommandLine.arguments())

fun mkColor color =
    let val (parsed, color) = Gtk.gdk_color_parse color
	val colormap = Gtk.gdk_colormap_get_system ()
    in  if parsed 
	then if Gtk.gdk_colormap_alloc_color colormap color false true
	     then SOME color
	     else NONE
	else NONE
    end
val SOME red =  mkColor "red"
val SOME yellow =  mkColor "yellow"

val font = Gtk.gdk_font_load "-*-courier-medium-r-normal--10-*"

fun delete_event _ = false
fun destroy _ = Gtk.main_quit()

fun attach table (widget,no) =
    let val left  = no mod 2
	val right = left+1
	val bot   = no div 2
	val top   = bot+1
    in  Gtk.table_attach_defaults table widget left right bot top
    end

fun pixmap widget =
    let	val gdkwindow = Gtk.widget_get_window widget
	val gc = Gtk.widget_get_style_fg_gc widget (Gtk.widget_get_state widget)
	val pix = Gtk.gdk_pixmap_create_from_xpm' gdkwindow "wheel.xpm"
    in  

        Gtk.gdk_gc_set_foreground gc yellow
      ; Gtk.gdk_draw_arc pix gc true 25 35 10 10 (64 * 0) (64 * 360)
      ; Gtk.gdk_gc_set_foreground gc red
      ; Gtk.gdk_draw_string pix font gc 2 46 "mGtk!"
      ; pix
    end

fun main () =
    let val window  = Gtk.window_new Gtk.WINDOW_TOPLEVEL
	val _       = Gtk.widget_show window

	val pix     = pixmap (window)
	val pixs     = map (fn _ => Gtk.pixmap_new' pix) ["1", "2", "3", "4"]
	val table   = Gtk.table_new 2 2 true

    in  Gtk.connect_delete_event window delete_event 
    ;   Gtk.connect_destroy window destroy
    ;   Gtk.container_set_border_width window 10
    ;   Gtk.container_add window table
    ;   app (attach table) (ListPair.zip (pixs,[0,1,2,3]))
    ;   app Gtk.widget_show pixs
    ;   Gtk.widget_show table
    ;   Gtk.main() 
    end
    
