(*
	hjælpe-system
	Lars Szwalski
	Efteråret 2002
*)

(*	gtk funktionerne,
	kernen i mosml hjælpe-systemet,
	adgang til miljø-variable,
	bygge filstier
*)
app load ["gtk","database","process","path"];


fun main () =
	let
		val _ = Gtk.init(CommandLine.name()::CommandLine.arguments())
		
		(* program tilstand *)
		val show_seek = ref true

		fun stdpath () = (case Process.getEnv "mosmllib" of
								 SOME s => s
								|NONE => "")
		
		(* stien til hjælpe-bibliotek og sig-filer *)
		val help_path = ref (stdpath())
		
		fun read_db () = Database.readbase (Path.concat(!help_path,"helpsigs.val"))
						handle SysErr _ => Database.Empty
		
		(* hjælp databasen *)
		val help_db = ref (read_db())

		(* hovedvinduet *)
		val window = Gtk.window_new Gtk.WINDOW_TOPLEVEL
		
		(* hovedvindue indhold i vertikal orden *)
		val vbox = Gtk.vbox_new false 0

		(* text feltet *)
		val text = Gtk.text_new'()
		
		(* aktive font *)
		val font = ref NONE
		
		(* kopieret fra eksempel: "pixmap.sml" *)
		fun mkColor color =
		    let val (parsed, color) = Gtk.gdk_color_parse color
				val colormap = Gtk.gdk_colormap_get_system ()
		    in  if parsed 
			then if Gtk.gdk_colormap_alloc_color colormap color false true
			     then SOME color
			     else NONE
			else NONE
		    end
    		
		val blue = mkColor "blue"
		
		val white = mkColor "white"
		
		(* fremhæv et ord i tekst-feltet *)
		fun highlight term =
			let val len  = size term
				val s    = SubString.all (Gtk.editable_get_chars text 0 ~1)
				val slen = SubString.size s
			
				fun color (s1,s2) =
					let val len_s2 = SubString.size s2
					in  if len_s2 > 0 then
							let val off = slen - len_s2
							in Gtk.editable_delete_text text off (off + len)
							;  Gtk.text_insert text (!font) white blue term ~1
							;  color (SubString.position term (SubString.triml len s2))
							end
						else ()
					end
					
			in	Gtk.text_freeze text
			;	color (SubString.position term s)
			;	Gtk.text_thaw text
			end

		(* fil-historie *)
		val history = ref []
		
		val hist_loc = ref 0
		
		fun history_add file =
			( history := file :: List.drop(!history,!hist_loc)
			; hist_loc := 0)
		
		(* indlæs og vis en fil i "text" *)
		fun set_file f =
			let val fd  = TextIO.openIn f
			in    Gtk.editable_delete_text text 0 ~1 
				; Gtk.text_insert text (!font) NONE NONE (TextIO.inputAll fd) ~1
				; TextIO.closeIn fd				
			end
			handle Io _ => print "IO Error\n"
		
		(* vis "hjemmesiden" (readme) *)
		fun set_home () =
				set_file (Path.concat(!help_path,"README"))

		(* notebook *)
		val note =
			let val note = Gtk.notebook_new ()
				val indh_scrl = Gtk.scrolled_window_new'()
				val indh_list = Gtk.clist_new 1
				val indx_scrl = Gtk.scrolled_window_new'()
				val indx_list = Gtk.clist_new 1
				
				val seek_vbox = Gtk.vbox_new false 10
				val seek_labl = Gtk.label_new "Seek string:"
				val seek_entr = Gtk.entry_new ()
				val seek_seek = Gtk.button_new_with_label "Seek"
				val seek_list = Gtk.clist_new 1
				val seek_scrl = Gtk.scrolled_window_new'()
				
				val seek_res  = ref []
				
				(* valg i søge-listen *)
				fun seek_select () =
					let val entry = List.nth (!seek_res,(Gtk.clist_get_focus_row seek_list))
						val ({file,line,...}) = entry
						val fname = Path.concat(!help_path,file ^ ".sig")
					in    set_file fname
						; highlight (Database.getname entry)
						; history_add fname
					end
				
				(* valg i indholds-listen *)
				fun indh_select () =
					let val (_,s) = Gtk.clist_get_text indh_list (Gtk.clist_get_focus_row indh_list) 0
						val fname = Path.concat(!help_path,s ^ ".sig")
					in  set_file fname
					;   history_add fname
					end

				fun list_add str cp =
					let
						fun add_str s =
							Gtk.clist_set_text seek_list (Gtk.clist_append seek_list) 0 (String.concat s)
					
						fun add_comp f  Database.Str = add_str ["structure ",f]
						   |add_comp f (Database.Exc s) = add_str ["exn  ", f,".",s]
						   |add_comp f (Database.Typ s) = add_str ["type ", f,".",s]
						   |add_comp f (Database.Val s) = add_str ["val  ", f,".",s]
						   |add_comp f (Database.Con s) = add_str ["con  ", f,".",s]
						   |add_comp f (Database.Term (s,NONE)) = add_str [s," (", f,")"]
						   |add_comp f (Database.Term (s,SOME kind)) = add_str [kind," ",s," (", f,")"]
	      			in
						app (fn {comp,file,...}:Database.entry => add_comp file comp) cp
					end
					
				fun do_seek term = (
					  seek_res := Database.lookup (!help_db,term)
					; Gtk.clist_clear seek_list
					; list_add term (!seek_res))
					
				fun seek_entry () = do_seek (Gtk.entry_get_text seek_entr)
				
				(* valg i index-listen *)
				fun index_select () = 
					let val str = #2(Gtk.clist_get_text indx_list (Gtk.clist_get_focus_row indx_list) 0)
					in	  Gtk.entry_set_text seek_entr str
						; Gtk.notebook_set_page note 2
						; do_seek str
					end

				fun get_str ({comp,file,...}) =
					case comp of
			   			 Database.Str => Gtk.clist_set_text indh_list (Gtk.clist_append indh_list) 0 file
			   			|_ => ()
				
				(* fyld index-listen & fyld indholds-listen *)
				fun fill_indx Database.Empty = ()
				   |fill_indx (Database.Node(s,c,n1,n2)) =
				   		(fill_indx n1; Gtk.clist_set_text indx_list (Gtk.clist_append indx_list) 0 s;
				   		app get_str c; fill_indx n2)
				
			in Gtk.label_set_justify seek_labl Gtk.JUSTIFY_LEFT
			
			;  Gtk.connect_expose_event seek_vbox (fn ()=> Gtk.widget_grab_focus seek_entr)
			
			;  Gtk.scrolled_window_add_with_viewport seek_scrl seek_list
			
			;  Gtk.box_pack_start seek_vbox seek_labl false false 0
			;  Gtk.box_pack_start seek_vbox seek_entr false false 0
			;  Gtk.box_pack_start seek_vbox seek_seek false false 0
			;  Gtk.box_pack_start seek_vbox seek_scrl true true 0
			
			;  Gtk.scrolled_window_add_with_viewport indh_scrl indh_list
			;  Gtk.scrolled_window_add_with_viewport indx_scrl indx_list

			;  Gtk.notebook_append_page note indh_scrl (Gtk.label_new "Content")
			;  Gtk.notebook_append_page note indx_scrl (Gtk.label_new "Index")
			;  Gtk.notebook_append_page note seek_vbox (Gtk.label_new "Seek")
			
			;  Gtk.connect_select_row indh_list indh_select
			;  Gtk.connect_select_row indx_list index_select			
			;  Gtk.connect_select_row seek_list seek_select
			
			;  Gtk.connect_activate seek_entr seek_entry
			;  Gtk.connect_clicked seek_seek seek_entry

			;  fill_indx (!help_db)

			;  note
			end

		(* split pane *)
		val split_pane =
			let val pane = Gtk.hpaned_new ()
				val hbox = Gtk.hbox_new false 0
				(* scroll til tekstfeltet *)
				val text_scrl = Gtk.vscrollbar_new (SOME (Gtk.text_get_vadj text))
				
			in    Gtk.paned_add1 pane note

				; Gtk.box_pack_start hbox text true true 0
				; Gtk.box_pack_start hbox text_scrl false false 0
			
				; Gtk.paned_add2 pane hbox
				
				; pane
			end
		
		(* toolbar søge-knappen *)
		val seek_but = Gtk.button_new_with_label "Seek"
		
		(* vis/skjul søgeknappen *)
		fun toggle_seek () = (
			if !show_seek then Gtk.widget_hide seek_but
			else Gtk.widget_show seek_but;
			show_seek := not (!show_seek))
			
		(* værktøjs linjen (toolbar) *)
		val toolbar =
			let val bar = Gtk.toolbar_new Gtk.ORIENTATION_HORIZONTAL Gtk.TOOLBAR_BOTH
			
				fun add_button button tip handler =
					(  Gtk.toolbar_append_widget bar button tip tip
					;  Gtk.connect_clicked button handler
					;  Gtk.button_set_relief button Gtk.RELIEF_NONE)
					
				fun go_back () =
					if List.length (!history) > 1 + !hist_loc then
						( hist_loc := !hist_loc + 1
						; set_file (List.nth (!history,!hist_loc)))
					else ()
				
				fun go_forward () =
					if !hist_loc > 0 then
						( hist_loc := !hist_loc - 1
						; set_file (List.nth (!history,!hist_loc)))
					else ()
					
			in    add_button (Gtk.button_new_with_label "Back") "Back" go_back
				; add_button (Gtk.button_new_with_label "Forward") "Forward" go_forward
				; add_button (Gtk.button_new_with_label "Home") "Home" set_home
				; Gtk.toolbar_append_space bar
				; add_button seek_but "Seek" (fn ()=> Gtk.notebook_set_page note 2)
			
				; bar
			end
		
		(* about dialogboks *)
		fun help_dialog () =
			let val dlg = Gtk.dialog_new ()
				val act = Gtk.dialog_get_action_area dlg
				val vbx = Gtk.dialog_get_vbox dlg
				val ok  = Gtk.button_new_with_label "Ok"
				
			in Gtk.window_set_modal dlg true
			;  Gtk.window_set_position dlg Gtk.WIN_POS_CENTER
			;  Gtk.window_set_title dlg "About"
			
			;  Gtk.connect_clicked ok (fn () => Gtk.widget_destroy dlg)
			
			;  Gtk.container_add vbx (Gtk.label_new "-= GUI help =-\nlsz@it-c.dk\n(c)2002")
			;  Gtk.container_add act ok
			
			;  Gtk.widget_show_all dlg
			end
		
		(* options dialogboks *)
		fun option_dialog () =
			let val dlg = Gtk.dialog_new ()
				val act = Gtk.dialog_get_action_area dlg
				val vbx = Gtk.dialog_get_vbox dlg
				
				val ok  = Gtk.button_new_with_label "Ok"
				val ccl = Gtk.button_new_with_label "Cancel"
				
				val gui_frm  = Gtk.frame_new (SOME "Gui options")
				val gui_vbox = Gtk.vbox_new false 0
				val shw_seek = Gtk.check_button_new_with_label "Show seek"
				
				val file_frm  = Gtk.frame_new (SOME "File options")
				val file_vbox = Gtk.vbox_new false 0
				val file_loc  = Gtk.entry_new ()
				val file_r1   = Gtk.radio_button_new_with_label_from_widget NONE "Standard"
				val file_r2   = Gtk.radio_button_new_with_label_from_widget (SOME file_r1) "User defined location:"
				
				val font_frm  = Gtk.frame_new (SOME "Font options")
				val font_selc = Gtk.font_selection_new()
				
				(* klik på ok-knappen *)
				fun clk_ok () =
					( if Gtk.toggle_button_get_active shw_seek <> !show_seek then toggle_seek () else ()
					; if Gtk.toggle_button_get_active file_r1 then
						help_path := stdpath()
					  else
					  	help_path := Gtk.entry_get_text file_loc
					; help_db := read_db()
					; font := SOME (Gtk.font_selection_get_font font_selc)
					; Gtk.widget_destroy dlg)
				
			in Gtk.window_set_modal dlg true
			;  Gtk.window_set_position dlg Gtk.WIN_POS_CENTER
			;  Gtk.window_set_title dlg "Options"
			
			;  Gtk.connect_clicked ok clk_ok
			;  Gtk.connect_clicked ccl (fn () => Gtk.widget_destroy dlg)
			;  Gtk.container_add act ok
			;  Gtk.container_add act ccl
			
			;  Gtk.toggle_button_set_active shw_seek (!show_seek)
			
			;  Gtk.box_pack_start gui_vbox shw_seek false false 0

			;  Gtk.box_pack_start file_vbox file_r1 false false 0
			;  Gtk.box_pack_start file_vbox file_r2 false false 0
			;  Gtk.box_pack_start file_vbox file_loc false false 0

			;  Gtk.container_add gui_frm gui_vbox
			;  Gtk.container_add file_frm file_vbox
			;  Gtk.container_add font_frm font_selc

			;  Gtk.container_add vbx gui_frm
			;  Gtk.container_add vbx file_frm
			;  Gtk.container_add vbx font_frm
						
			;  Gtk.widget_show_all dlg
			end
		
		(* menu-linjen *)
		val menubar =
			let val bar = Gtk.menu_bar_new ()

				(* almindeligt menupunkt *)
				fun add_item menu label handler =
					let val item = Gtk.menu_item_new_with_label label
					in Gtk.menu_append menu item
					;  Gtk.connect_menu_item_activate item handler
					end

				(* afkrydsnings-menupunkt *)
				fun add_check_item menu label handler =
					let val item = Gtk.check_menu_item_new_with_label label
					in Gtk.menu_append menu item
					;  Gtk.check_menu_item_set_show_toggle item true
					;  Gtk.connect_check_menu_item_toggled item handler
					;  item
					end
				
				(* tilføj punkt til menu-bar *)
				fun add_to_bar label menu =
					let val item = Gtk.menu_item_new_with_label label
					in Gtk.menu_item_set_submenu item menu
					;  Gtk.menu_bar_append bar item
					;  item
					end
						
				val file_menu   = Gtk.menu_new ()
				val option_menu = Gtk.menu_new ()
				val help_menu   = Gtk.menu_new ()
				
				(* for at undgå set_check_state-event *)
				val user_event = ref false
				
				val chk_seek = add_check_item option_menu "Seek button" (fn ()=> if !user_event then toggle_seek() else ())

				fun set_check_state () = (
					  user_event := false
					; Gtk.check_menu_item_set_active chk_seek (!show_seek)
					; user_event := true)
				
			in    add_item file_menu "Exit" (fn () => Gtk.widget_destroy window)
				; add_to_bar "File" file_menu
				
				; add_item option_menu "Configure..." option_dialog
				; Gtk.connect_menu_item_activate (add_to_bar "Options" option_menu) set_check_state
				
				; add_item help_menu "About" help_dialog
				; Gtk.menu_item_right_justify (add_to_bar "Help" help_menu)
				
				; bar
			end
			
	in    Gtk.connect_delete_event window (fn _=> false)	(* bare afslut *)
		; Gtk.connect_destroy window Gtk.main_quit			(* Bed Gtk nedlægge app. *)

		; Gtk.window_set_default_size window 500 400
		; Gtk.window_set_title window "GUI mosml help"
		
		; Gtk.box_pack_start vbox menubar false false 0
		; Gtk.box_pack_start vbox toolbar false false 0
		; Gtk.box_pack_start vbox split_pane true true 0
		; Gtk.container_add window vbox
		
		; Gtk.widget_show_all window
		
		(* indlæs "readme" og vis den som start *)
		; set_home()
		
		; Gtk.main()
	end
	
(* auto-run *)
val _ = main ()

(* forlad mosml *)
val _ = quit()
