val _ = Gtk.init(CommandLine.arguments())

fun mkColor color =
    let val (parsed, color) = Gtk.gdk_color_parse color
	val colormap = Gtk.gdk_colormap_get_system ()
    in  if parsed 
	then if Gtk.gdk_colormap_alloc_color colormap color false true
	     then SOME color
	     else NONE
	else NONE
    end
val SOME blue =  mkColor "blue"
val SOME yellow =  mkColor "yellow"

fun delete_event _ = false
fun destroy _ = Gtk.main_quit()


fun repaint widget =
    let val drawable = Gtk.widget_get_drawable widget
	val window = Gtk.widget_get_window widget
	val style  = Gtk.widget_get_style widget
	val state  = Gtk.widget_get_state widget
	val gc = Gtk.widget_get_style_fg_gc widget state
	val size as (width, height, _, _) = Gtk.widget_get_allocation widget
	val centx = width div 2
	val centy = height div 2
    in  Gtk.gdk_window_clear_area drawable 0 0 width height
    ;   Gtk.gdk_gc_set_foreground gc yellow
    ;   Gtk.gdk_draw_arc drawable gc true centx centy (width div 3) (height div 3) (64 * 0) (64 * 360)
    ;   Gtk.gdk_gc_set_foreground gc blue
    ;   Gtk.draw_string style window state (centx - 45) centy "mGtk rules!"
    end

fun expose widget _ = repaint widget

fun main () =
    let val window  = Gtk.window_new Gtk.WINDOW_TOPLEVEL
	val drawing = Gtk.drawing_area_new ()

    in  Gtk.connect_delete_event window delete_event 
    ;   Gtk.connect_destroy window destroy
    ;   Gtk.connect_expose_event drawing (expose drawing)

    ;   Gtk.widget_set_usize window 210 210
    ;   Gtk.drawing_area_size drawing 200 200
    ;   Gtk.container_set_border_width window 10
    ;   Gtk.container_add window drawing
    ;   Gtk.widget_show drawing
    ;   Gtk.widget_show window
    ;   Gtk.main() 
    end
    
