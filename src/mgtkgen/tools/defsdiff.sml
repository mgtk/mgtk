
val _ = Gtk.init(CommandLine.arguments())
val upright = Gtk.gdk_font_load "-misc-fixed-medium-r-*-*-*-100-*-*-*-*-*-*"
fun mkColor color =
    let val (parsed, color) = Gtk.gdk_color_parse color
	val colormap = Gtk.gdk_colormap_get_system ()
    in  if parsed 
	then if Gtk.gdk_colormap_alloc_color colormap color false true
	     then SOME color
	     else NONE
	else (print "Couldn't find color\n"; NONE)
    end
val red = mkColor "red"

val error_color = red

    fun order (d1, d2) = String.compare (AST.nameOf d1, AST.nameOf d2)
    fun equal (d1, d2) = (AST.nameOf d1 = AST.nameOf d2) andalso (AST.equal(d1, d2))
    fun less d1 d2 = order (d1,d2) = LESS

    fun member x xs =
	let fun loop [] = false
              | loop (x'::xs) = x = x' orelse loop xs
	in  loop xs
	end
    fun splitWhile p xs =
	let fun loop [] acc = (rev acc, [])
	      | loop (x::xs) acc = if p x then loop xs (x :: acc)
				   else (rev acc, x::xs)
	in  loop xs []
	end

    fun extract file NONE = ""
      | extract file (SOME (p1,p2)) =
	let val str = Util.extractSource file (p1,p2+20)
	in  str
	end
	
    fun parse file =
	case Parse.parse' Parser.decls file of
	    (_, SOME list) => list
          | (p, NONE) => ( TextIO.print ("Error parsing " ^ file ^ "\n")
                         ; TextIO.flushOut TextIO.stdOut
			 ; raise Fail ("Error parsing " ^ file ^ ":\n" ^ extract file p))

    fun diff textWidget (file1, file2) =
	let 

	    fun insert text = Gtk.text_insert' textWidget text ~1

	    fun notIn file list =
		List.app (fn d => insert ("Declaration " ^ AST.nameOf d ^ " not in " ^ file ^ "\n")) list
	    fun showDiff (d1, d2) =
		insert (AST.nameOf d1 ^ " and " ^ AST.nameOf d2 ^ " differs!\n")


	    val _ = TextIO.print ("Parsing " ^ file1 ^ " ...\n")
	    val list1 = parse file1
	    val _ = TextIO.print ("Parsing " ^ file2 ^ " ...\n")
	    val list2 = parse file2
	
	    val _ = TextIO.print ("Sorting " ^ file1 ^ " ...\n")
	    val list1 = Listsort.sort order list1
	    val _ = TextIO.print ("Sorting " ^ file2 ^ " ...\n")
	    val list2 = Listsort.sort order list2
		
	    fun loop' ([], []) = ()
              | loop' ([], list2 as (_::_)) = notIn file1 list2
	      | loop' (list1 as (_::_), []) = notIn file2 list1
	      | loop' (decl1::list1, decl2::list2) =
		  (case order (decl1, decl2) of
		       EQUAL => 
			   if equal (decl1, decl2) 
			   then (TextIO.print "They're equal!\n"; loop (list1, list2))
			   else (* show differences *)
			        (showDiff (decl1, decl2); loop (list1, list2))
                     | GREATER =>
			   if member decl1 list2 
			   then let val (less,more) = splitWhile (less decl1) list2
				in  ( notIn file1 less
                                    ; loop (list1, more))
				end
			   else ( notIn file1 [decl2]
				; notIn file2 [decl1]
				; loop (list1, list2))
                     | LESS => 
			   if member decl2 list1 
			   then let val (less,more) = splitWhile (less decl2) list1
				in  ( notIn file2 less
                                    ; loop (more, list2))
				end
			   else ( notIn file2 [decl1]
                                ; notIn file1 [decl2]
                                ; loop (list1, list2))
                  )
	    and loop (d1::list1, d2::list2) =
		( TextIO.print(concat["Comparing ", AST.nameOf d1, " (", Int.toString(List.length list1+1), ") and ",
				      AST.nameOf d2, " (", Int.toString(List.length list2+1), ")\n\n"])
		; loop' (d1::list1, d2::list2)
                )
	      | loop pair = loop' pair
	in  loop (list1, list2)
	end
        handle Fail msg => Gtk.text_insert textWidget NONE error_color NONE msg ~1

    fun fileEntry label =
	let val frame = Gtk.frame_new (SOME label)
	    val entry = Gtk.entry_new ()
	in  Gtk.container_add frame entry
	  ; Gtk.widget_show entry
          ; (frame, entry)
	end

    fun fileView () =
	let val window = Gtk.scrolled_window_new' ()
	    val text = Gtk.text_new' ()
	in  Gtk.scrolled_window_set_policy window Gtk.POLICY_NEVER Gtk.POLICY_AUTOMATIC
	  ; Gtk.container_add window text
          ; Gtk.text_set_editable text false
	  ; Gtk.widget_show text
          ; (window, text)
	end

    fun pack box w expand fill = 
        ( Gtk.box_pack_start box w expand true 0
        ; Gtk.widget_show w)

    fun paned_add paned (w1, w2) =
	( Gtk.paned_add1 paned w1
        ; Gtk.paned_add2 paned w2
        ; Gtk.widget_show w1
        ; Gtk.widget_show w2
        )

    val toWidget = Gtk.toWidget

    fun delete_event _ = false
    fun destroy _ = Gtk.main_quit()

    fun clear ed = Gtk.editable_delete_text ed 0 ~1 

    fun insertFile text font fname =
	let val dev = TextIO.openIn fname
	    val s   = TextIO.inputAll dev
	in  TextIO.closeIn dev
          ; clear text
          ; Gtk.text_freeze text
	  ; Gtk.text_insert text (SOME font) NONE NONE s ~1
	  ; Gtk.text_thaw text
	end

    fun showFile text entry font _ = insertFile text font (Gtk.entry_get_text entry)

    fun compareFiles textWidget entry1 entry2 _ =
	let val file1 = Gtk.entry_get_text entry1
	    val file2 = Gtk.entry_get_text entry2
	in  clear textWidget
        ;   diff textWidget (file1, file2)
	end

    fun mkGUI () =
	let 
	    val window = Gtk.window_new Gtk.WINDOW_TOPLEVEL

	    val vbox = Gtk.vbox_new false 2
	    val vpaned = Gtk.vpaned_new ()

	    (* An hbox with entryfields for the files and the various
	       buttons of the application *)
	    val hbox = Gtk.hbox_new false 2
	    val hbox1 = Gtk.hbox_new true 2
	    val hbox2 = Gtk.hbox_new true 2
	    val compare = Gtk.button_new_with_label "compare"
	    val exit = Gtk.button_new_with_label "exit"
	    val panel = [toWidget hbox1, toWidget hbox2]
	    val title = Gtk.label_new "Compare .defs files\n(c) Henning Niss"
	    (* a window with the messages *)
	    val (messages,msgtext) = fileView()

	    (* a hpaned to show the two files *)
	    val hpaned = Gtk.hpaned_new ()
	    val vbox1 = Gtk.vbox_new false 2
	    val vbox2 = Gtk.vbox_new false 2
	    val (win1, msg1) = fileView ()
	    val (win2, msg2) = fileView ()
	    val (file1, file1entry) = fileEntry "File A"
	    val (file2, file2entry) = fileEntry "File B"


	    fun setFile file msg entry =
		(Gtk.entry_set_text entry file; showFile msg entry upright ())
	    val _ = case CommandLine.arguments () of
		      [file] => setFile file msg1 file1entry
		    | [file1, file2] => (setFile file1 msg1 file1entry;
					 setFile file2 msg2 file2entry)
                    | _ => ()

	in  Gtk.connect_delete_event window delete_event 
	  ; Gtk.connect_destroy window destroy

	  ; Gtk.window_set_title window "Compare .defs files"
          ; Gtk.window_set_default_size window 400 320
          ; Gtk.container_set_border_width window 5

	  ; Gtk.scrolled_window_set_policy messages Gtk.POLICY_NEVER Gtk.POLICY_AUTOMATIC
          (* add callbacks *)
          ; Gtk.connect_clicked exit destroy

	  ; Gtk.connect_clicked compare (compareFiles msgtext file1entry file2entry)

	  ; Gtk.connect_activate file1entry (showFile msg1 file1entry upright)
	  ; Gtk.connect_activate file2entry (showFile msg2 file2entry upright)

          (* add the widgets to the right containers *)
          ; pack hbox2 compare false false
          ; pack hbox2 exit false false

	  ; Gtk.label_set_justify title Gtk.JUSTIFY_LEFT
	  ; Gtk.misc_set_alignment title 0.1 0.5
          ; pack hbox1 title true true

          ; pack hbox  hbox1 true true
          ; pack hbox  hbox2 false false

	  ; pack vbox1 file1 false false
          ; pack vbox1 win1  true  true
          ; pack vbox2 file2 false false
          ; pack vbox2 win2  true  true

          ; paned_add hpaned (vbox1, vbox2)
          ; paned_add vpaned (messages, hpaned)

	  ; pack vbox hbox false false 
          ; pack vbox vpaned true true 

          ; Gtk.container_add window vbox
  
          (* and lift off! *)
          ; app Gtk.widget_show [toWidget window, toWidget vbox]
          ; Gtk.main() 
	end

    fun main () = mkGUI ()

(*
val _ = (diff ("gtk-label.defs", "gtk-label2.defs")
        handle exn => TextIO.print ("Caught exception!\n"))
*)
val _ = main()