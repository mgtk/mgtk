fun itemsignal n () = print ("itemsignal on item: "^n^"\n")
fun unselect_child () = print "unselect child\n"
fun select_child () = print "select child\n"
fun selection_changed () = print "select changed\n"

local 
    fun delete_event _ = false
    fun destroy _ = Gtk.main_quit()
in  
    fun window () = 
	let val win = Gtk.window_new Gtk.WINDOW_TOPLEVEL
	    val swin = Gtk.scrolled_window_new NONE NONE
	    val _ = (Gtk.scrolled_window_set_policy swin Gtk.POLICY_AUTOMATIC Gtk.POLICY_AUTOMATIC
                   ; Gtk.container_add win swin
		   ; Gtk.widget_show swin
                   )
	in  Gtk.connect_delete_event win delete_event 
	  ; Gtk.connect_destroy win destroy
	  ; Gtk.container_set_border_width win 5
	  ; (win, swin)
	end
end

fun tree parent =
    let val tree = Gtk.tree_new ()
	(* Signals *)
	val _ = (Gtk.connect_select_child tree select_child
               ; Gtk.connect_unselect_child tree unselect_child
               ; Gtk.connect_selection_changed tree selection_changed)
    in  Gtk.scrolled_window_add_with_viewport parent tree
      ; Gtk.tree_set_selection_mode tree Gtk.SELECTION_SINGLE
      ; Gtk.widget_show tree
      ; tree
    end

local val idgen = ref 0
      fun newId () = !idgen before idgen := !idgen + 1
in
fun newItem name = 
    let val item   = Gtk.tree_item_new_with_label name
	val signal = itemsignal name
    in  (item,signal)
    end
end

fun add_items tree itemnames =
    let fun add_one parent name =
	    let val (item, itemsignal) = newItem name
		val _ = (Gtk.connect_select item itemsignal
                       ; Gtk.connect_deselect item itemsignal
                       ; Gtk.connect_toggle item itemsignal
                       ; Gtk.connect_expand item itemsignal
                       ; Gtk.connect_collapse item itemsignal)
	    in  Gtk.tree_append parent item
              ; Gtk.widget_show item
	      ; item
	    end
	fun add_sub name =
	    let val item = add_one tree name
		val subtree = Gtk.tree_new ()
		val _ = Gtk.tree_item_set_subtree item subtree
	    in  app (fn item => (add_one subtree item; ())) itemnames
	    end
    in  app add_sub itemnames
    end

fun main(args) =
    let val _ = Gtk.init(args)
	val (window,scrolled) = window()
        val tree = tree scrolled
	val _ = add_items tree ["Foo", "Bar", "Baz", "Quux", "Maurice"]

    in  Gtk.widget_show window
      ; Gtk.main ()
    end
(*
val _ = main(CommandLine.arguments())
*)
