/* GTK stuff */
#include <gtk/gtk.h>

/* Mosml stuff */
#include <mlvalues.h> 
#include <fail.h>
#include <alloc.h>
#include <memory.h>
#include <callback.h>


/* TODO: 

   . Don't use void* so extensive.  Use the cast functions provided by gtk

   . Fix mgtk_init
*/


/* A nice macro to have */
#define GtkObj_val(x) ((void*) Field(x, 1))


static void ml_finalize_gtkobject (value val) {
  gtk_object_unref (GtkObj_val(val)); 
}

value Val_GtkObject (void* obj) { 
  value res; 
  gtk_object_ref(obj); 
  res = alloc_final (2, ml_finalize_gtkobject, 0, 1);
  GtkObj_val(res) = (value) obj;  
  return res; 
}



/* *** Basic stuff *** */

/* FIXME */
/* ML type: string vector -> unit */
value mgtk_init(value args) { /* ML */
  int argc = 1;
  char** argv;

  argv = stat_alloc(4);
  argv[0] = "dummy";
  gtk_init(&argc, &argv);
  stat_free(argv);
  return Val_unit;
}

/* ML type: unit -> unit */
value mgtk_main(value dummy) { /* ML */
  gtk_main ();
  return Val_unit;
}

/* ML type: unit -> unit */
value mgtk_main_quit(value dummy) { /* ML */
  gtk_main_quit ();
  return Val_unit;
}


/* *** Signal stuff *** */

void mgtk_callback_dispatch (GtkObject *object, gpointer data, guint nargs, 
			     GtkArg *args) {
  value res;
  valueptr mvp;

  res = alloc_tuple(3);
  Field(res,1) = (value) args;
  Field(res,2) = Val_int(nargs);
  Field(res,0) = Val_GtkObject(object); // last because it allocates

  mvp = get_valueptr("mgtk_callback_dispatch"); 
  if(mvp == NULL)
    failwith("Cannot find mgtk_callback_dispatch");

  //printf("callback id = %i\n",(int) data);

  res = callbackptr2(mvp, (value) data, res);

  /*
  switch (GTK_FUNDAMENTAL_TYPE(args[nargs].type)) {
  case GTK_TYPE_NONE:
    return;
  case GTK_TYPE_BOOL:
    *GTK_RETLOC_BOOL(*args) = Bool_val(res);
    return;
  }
  */
}


/* FIXME */
void mgtk_callback_destroy (gpointer data) {

}


/* ML type: gtkobj -> string -> clb -> bool -> int */
value mgtk_signal_connect (value object, value name, value clb, value after){
  int res;

  //  printf("register id = %i\n",(int) clb);

  res = gtk_signal_connect_full (GtkObj_val(object), 
				 String_val(name), 
				 NULL,
				 mgtk_callback_dispatch, 
				 (void*) clb,
				 mgtk_callback_destroy, 
				 FALSE, 
				 Bool_val(after));
  return Val_long(res);
}

/* *** GtkArg stuff *** */

/* ML type : GtkArgs -> int -> bool -> unit */
value mgtk_set_pos_bool (GtkArg *args, value pos, value val) { /* ML */
  int p = Int_val(pos);

  if (GTK_FUNDAMENTAL_TYPE(args[p].type) != GTK_TYPE_BOOL)
    failwith (" mgtk_set_pos_bool: argument type mismatch");
  
  *GTK_RETLOC_BOOL(args[p]) = Bool_val(val);
  return Val_unit;
}


/* *** Glib stuff *** */

/* *** glist stuff *** */

#define Glist_val(x) ((GList *) Field(x, 1))


static void mgtk_finalize_glist (value val) {
  g_list_free(Glist_val(val));
}


/* ML type: unit -> glist */
value mgtk_glist_nil (value dummy) { /* ML */
  value res; 
  res = alloc_final (2, mgtk_finalize_glist, 0, 1);
  Glist_val(res) = (value) NULL;  
  return res; 
}

/* ML type: glist -> string -> unit */
value mgtk_glist_append_string(value ls, value s) { /* ML */
  Glist_val(ls) = g_list_append (Glist_val(ls), String_val (s));
  return Val_unit;
}



/* *** Widget stuff *** */

/* ML type: gtkobj -> unit */
value mgtk_widget_show(value obj) { /* ML */
  gtk_widget_show(GtkObj_val(obj));
  return Val_unit;
}

/* ML type: gtkobj -> unit */
value mgtk_widget_hide(value obj) { /* ML */
  gtk_widget_hide(GtkObj_val(obj));
  return Val_unit;
}


/* *** Label *** */

/* ML type : string -> gtkobj */
value mgtk_label_new(value s) { /* ML */
  return Val_GtkObject(gtk_label_new(String_val(s)));
}

/* ML type : unit -> int * int * int * int  */
value mgtk_get_justifications(value dummy) {
  value res = alloc_tuple(4);
  Field(res,0) = Val_int(GTK_JUSTIFY_LEFT); 
  Field(res,1) = Val_int(GTK_JUSTIFY_RIGHT);
  Field(res,2) = Val_int(GTK_JUSTIFY_CENTER);
  Field(res,3) = Val_int(GTK_JUSTIFY_FILL);

  return res;
}

/* ML type : gtkobj -> string -> unit */
value mgtk_label_set_text(value lab, value s) { /* ML */
  gtk_label_set_text(GtkObj_val(lab),String_val(s));
  return Val_unit;
}

/* ML type : gtkobj -> string */
value mgtk_label_get(value lab) { /* ML */
  char* res;  
  gtk_label_get(GtkObj_val(lab), &res);
  return copy_string(res);
}

/* ML type : gtkobj -> justification -> unit */
value mgtk_label_set_justify(value lab, value s) { /* ML */
  gtk_label_set_justify(GtkObj_val(lab),Int_val(s));
  return Val_unit;
}

/* ML type : gtkobj -> string -> unit */
value mgtk_label_set_pattern(value lab, value s) { /* ML */
  gtk_label_set_pattern(GtkObj_val(lab),String_val(s));
  return Val_unit;
}

/* ML type : gtkobj -> bool -> unit */
value mgtk_label_set_line_wrap(value lab, value s) { /* ML */
  gtk_label_set_line_wrap(GtkObj_val(lab),Bool_val(s));
  return Val_unit;
}





/* *** Container stuff *** */

/* ML type : gtkobj -> int -> unit */
value mgtk_container_set_border_width(value obj, value width) { /* ML */
  gtk_container_set_border_width(GtkObj_val(obj), Int_val(width));
  return Val_unit;
}

/* ML type : gtkobj -> gtkobj -> unit */
value mgtk_container_add(value cont, value wid) { /* ML */
  gtk_container_add(GtkObj_val(cont),GtkObj_val(wid));
  return Val_unit;
}


/* *** Box stuff *** */

/* ML type : gtkobj -> gtkobj -> bool -> bool -> int -> unit */
value mgtk_box_pack_start(value b, value w, value e, value f, value p){/* ML */
  gtk_box_pack_start(GtkObj_val(b),GtkObj_val(w),Bool_val(e),Bool_val(f),
		     Int_val(p));
  return Val_unit;
}

/* ML type : gtkobj -> gtkobj -> bool -> bool -> int -> unit */
value mgtk_box_pack_end(value b, value w, value e, value f, value p){/* ML */
  gtk_box_pack_end(GtkObj_val(b),GtkObj_val(w),Bool_val(e),Bool_val(f),
		     Int_val(p));
  return Val_unit;
}

/* ML type : bool -> int -> gtkobj */
value mgtk_hbox_new(value hom, value sp) { /* ML */
  return Val_GtkObject(gtk_hbox_new(Bool_val(hom), Int_val(sp)));
}

/* ML type : bool -> int -> gtkobj */
value mgtk_vbox_new(value hom, value sp) { /* ML */
  return Val_GtkObject(gtk_vbox_new(Bool_val(hom), Int_val(sp)));
}


/* *** ScrolledWindow stuff *** */

/* ML type : unit -> gtkobj */
value mgtk_scrolled_window_new(value s) { /* ML */
  return Val_GtkObject(gtk_scrolled_window_new(NULL,NULL));
}

/* ML type : unit -> int * int * int  */
value mgtk_get_policies(value dummy) { /* ML */
  value res = alloc_tuple(3);
  Field(res,0) = Val_int(GTK_POLICY_ALWAYS);
  Field(res,1) = Val_int(GTK_POLICY_AUTOMATIC);
  Field(res,2) = Val_int(GTK_POLICY_NEVER);

  return res;
}

/* ML type : gtkobj -> policy -> policy -> unit */
value mgtk_scrolled_window_set_policy(value sc, value h, value v) { /* ML */
  gtk_scrolled_window_set_policy(GtkObj_val(sc), Int_val(h), Int_val(v));
  return Val_unit;
}

/* ML type : gtkobj -> gtkobj -> unit */
value mgtk_scrolled_window_add_with_viewport(value sc, value w) { /* ML */
  gtk_scrolled_window_add_with_viewport(GtkObj_val(sc), GtkObj_val(w));
  return Val_unit;
}

/* *** Button stuff *** */

/* ML type: string -> gtkobj */
value mgtk_button_new_with_label(value lab) { /* ML */
  return Val_GtkObject(gtk_button_new_with_label(String_val(lab)));
}

/* ML type: gtkobj -> unit */
value mgtk_button_clicked(value button) { /* ML */
  gtk_button_clicked(GtkObj_val(button));
  return Val_unit;
}

/* ML type: gtkobj -> unit */
value mgtk_button_pressed(value button) { /* ML */
  gtk_button_pressed(GtkObj_val(button));
  return Val_unit;
}

/* ML type: gtkobj -> unit */
value mgtk_button_released(value button) { /* ML */
  gtk_button_released(GtkObj_val(button));
  return Val_unit;
}

/* ML type: gtkobj -> unit */
value mgtk_button_enter(value button) { /* ML */
  gtk_button_enter(GtkObj_val(button));
  return Val_unit;
}

/* ML type: gtkobj -> unit */
value mgtk_button_leave(value button) { /* ML */
  gtk_button_leave(GtkObj_val(button));
  return Val_unit;
}


/* *** Window stuff *** */

/* ML type: int -> gtkobj */
value mgtk_window_new(value kind) { /* ML */
  return Val_GtkObject(gtk_window_new(Int_val(kind))); 
}

/* ML type: unit -> int */
value mgtk_window_kinds(value dummy) { /* ML */
  return Val_long(GTK_WINDOW_TOPLEVEL);
}


/* *** Editable stuff *** */

/* ML type : gtkobj -> int -> int -> string */
value mgtk_editable_get_chars(value ed, value s, value e) { /* ML */
  value res;
  char* tmp = gtk_editable_get_chars(GtkObj_val(ed), Int_val(s),
				     Int_val(e));
  res = copy_string(tmp);
  free(tmp);

  return res;
}

/* ML type : gtkobj -> int -> int -> string */
value mgtk_editable_delete_text(value ed, value s, value e) { /* ML */
  gtk_editable_delete_text(GtkObj_val(ed), Int_val(s),Int_val(e));
  return Val_unit;
}


/* *** Entry stuff *** */

/* ML type : string -> gtkobj */
value mgtk_entry_new(value dummy) { /* ML */
  return Val_GtkObject(gtk_entry_new());
}

/* ML type : string -> gtkobj */
value mgtk_entry_new_with_max_length(value len) { /* ML */
  return Val_GtkObject(gtk_entry_new_with_max_length(Int_val(len)));
}

/* ML type : gtkobj -> string -> unit */
value mgtk_entry_set_text(value ent, value s) { /* ML */
  gtk_entry_set_text(GtkObj_val(ent), String_val(s));
  return Val_unit;
}

/* ML type : gtkobj -> string -> unit */
value mgtk_entry_append_text(value ent, value s) { /* ML */
  gtk_entry_append_text(GtkObj_val(ent), String_val(s));
  return Val_unit;
}

/* ML type : gtkobj -> string -> unit */
value mgtk_entry_prepend_text(value ent, value s) { /* ML */
  gtk_entry_prepend_text(GtkObj_val(ent), String_val(s));
  return Val_unit;
}

/* ML type : gtkobj -> int -> unit */
value mgtk_entry_set_position(value ent, value s) { /* ML */
  gtk_entry_set_position(GtkObj_val(ent), Int_val(s));
  return Val_unit;
}

/* ML type : gtkobj -> string */
value mgtk_entry_get_text(value ent) { /* ML */
  return copy_string(gtk_entry_get_text(GtkObj_val(ent)));
}

/* ML type : gtkobj -> int -> int -> unit */
value mgtk_entry_select_region(value ent, value s, value e) { /* ML */
  gtk_entry_select_region(GtkObj_val(ent), Int_val(s), Int_val(e));
  return Val_unit;
}

/* ML type : gtkobj -> bool -> unit */
value mgtk_entry_set_visibility(value ent, value s) { /* ML */
  gtk_entry_set_visibility(GtkObj_val(ent), Bool_val(s));
  return Val_unit;
}

/* ML type : gtkobj -> bool -> unit */
value mgtk_entry_set_editable(value ent, value s) { /* ML */
  gtk_entry_set_editable(GtkObj_val(ent), Bool_val(s));
  return Val_unit;
}

/* ML type : gtkobj -> int -> unit */
value mgtk_entry_set_max_length(value ent, value s) { /* ML */
  gtk_entry_set_max_length(GtkObj_val(ent), Int_val(s));
  return Val_unit;
}


/* *** Text stuff *** */

/* ML type : unit -> gtkobj */
value mgtk_text_new(value dummy) { /* ML */
  return Val_GtkObject(gtk_text_new(NULL, NULL));
}

/* ML type : gtkobj -> bool -> unit */
value mgtk_text_set_editable(value t, value b) { /* ML */
  gtk_text_set_editable(GtkObj_val(t), Bool_val(b));
  return Val_unit;
}

/* ML type : gtkobj -> bool -> unit */
value mgtk_text_set_word_wrap(value t, value b) { /* ML */
  gtk_text_set_word_wrap(GtkObj_val(t), Bool_val(b));
  return Val_unit;
}

/* ML type : gtkobj -> bool -> unit */
value mgtk_text_set_line_wrap(value t, value b) { /* ML */
  gtk_text_set_line_wrap(GtkObj_val(t), Bool_val(b));
  return Val_unit;
}

/* ML type : gtkobj -> int -> unit */
value mgtk_text_set_point(value t, value b) { /* ML */
  gtk_text_set_point(GtkObj_val(t), Int_val(b));
  return Val_unit;
}

/* ML type : gtkobj -> int -> bool */
value mgtk_text_backward_delete(value t, value b) { /* ML */
  return Val_bool(gtk_text_backward_delete(GtkObj_val(t), Int_val(b)));
}

/* ML type : gtkobj -> int -> bool */
value mgtk_text_forward_delete(value t, value b) { /* ML */
  return Val_bool(gtk_text_forward_delete(GtkObj_val(t), Int_val(b)));
}

/* ML type : gtkobj -> int */
value mgtk_text_get_point(value t) { /* ML */
  return Val_int(gtk_text_get_point(GtkObj_val(t)));
}

/* ML type : gtkobj -> int */
value mgtk_text_get_length(value t) { /* ML */
  return Val_int(gtk_text_get_length(GtkObj_val(t)));
}

/* ML type : gtkobj -> unit */
value mgtk_text_freeze(value t) { /* ML */
  gtk_text_freeze(GtkObj_val(t));
  return Val_unit;
}

/* ML type : gtkobj -> unit */
value mgtk_text_thaw(value t) { /* ML */
  gtk_text_thaw(GtkObj_val(t));
  return Val_unit;
}

/* ML type : gtkobj -> string -> int -> unit */
value mgtk_text_insert(value t, value s, value i) { /* ML */
  gtk_text_insert(GtkObj_val(t), NULL, NULL, NULL, String_val(s), Int_val(i));
  return Val_unit;
}

/* *** Arrow stuff *** */

/* ML type : unit -> int * int * int * int  */
value mgtk_get_arrow_types(value dummy) {
  value res = alloc_tuple(4);
  Field(res,0) = Val_int(GTK_ARROW_UP); 
  Field(res,1) = Val_int(GTK_ARROW_DOWN);
  Field(res,2) = Val_int(GTK_ARROW_LEFT);
  Field(res,3) = Val_int(GTK_ARROW_RIGHT);

  return res;
}

/* ML type : unit -> int * int * int * int * int  */
value mgtk_get_shadow_types(value dummy) {
  value res = alloc_tuple(5);
  Field(res,0) = Val_int(GTK_SHADOW_NONE); 
  Field(res,1) = Val_int(GTK_SHADOW_IN);
  Field(res,2) = Val_int(GTK_SHADOW_OUT);
  Field(res,3) = Val_int(GTK_SHADOW_ETCHED_IN);
  Field(res,4) = Val_int(GTK_SHADOW_ETCHED_OUT);

  return res;
}

/* ML type : arrow_type -> shadow_type -> gtkobj */
value mgtk_arrow_new(value at, value st) { /* ML */
  return Val_GtkObject(gtk_arrow_new(Int_val(at), Int_val(st)));
}

/* ML type : gtkobj -> arrow_type -> shadow_type -> gtkobj */
value mgtk_arrow_set(value a, value at, value st) { /* ML */
  gtk_arrow_set(GtkObj_val(a), Int_val(at), Int_val(st));
  return Val_unit;
}


/* *** Item stuff *** */

/* ML type : gtkobj -> unit */
value mgtk_item_select(value i) { /* ML */
  gtk_item_select(GtkObj_val(i));
  return Val_unit;
}

/* ML type : gtkobj -> unit */
value mgtk_item_deselect(value i) { /* ML */
  gtk_item_deselect(GtkObj_val(i));
  return Val_unit;
}

/* ML type : gtkobj -> gtkobj -> unit */
value mgtk_item_toggle(value i) { /* ML */
  gtk_item_toggle(GtkObj_val(i));
  return Val_unit;
}

/* *** ListItem stuff *** */

/* ML type : unit -> gtkobj */
value mgtk_list_item_new(value dummy) { /* ML */
  return Val_GtkObject(gtk_list_item_new());
}

/* ML type : string -> gtkobj */
value mgtk_list_item_new_with_label(value s) { /* ML */
  return Val_GtkObject(gtk_list_item_new_with_label(String_val(s)));
}

/* ML type : gtkobj -> unit */
value mgtk_list_item_select(value ti) { /* ML */
  gtk_list_item_select(GtkObj_val(ti));
  return Val_unit;
}

/* ML type : gtkobj -> unit */
value mgtk_list_item_deselect(value ti) { /* ML */
  gtk_list_item_deselect(GtkObj_val(ti));
  return Val_unit;
}

/* *** TreeItem stuff *** */

/* ML type : unit -> gtkobj */
value mgtk_tree_item_new(value dummy) { /* ML */
  return Val_GtkObject(gtk_tree_item_new());
}

/* ML type : string -> gtkobj */
value mgtk_tree_item_new_with_label(value s) { /* ML */
  return Val_GtkObject(gtk_tree_item_new_with_label(String_val(s)));
}

/* ML type : gtkobj -> unit */
value mgtk_tree_item_select(value ti) { /* ML */
  gtk_tree_item_select(GtkObj_val(ti));
  return Val_unit;
}

/* ML type : gtkobj -> unit */
value mgtk_tree_item_deselect(value ti) { /* ML */
  gtk_tree_item_deselect(GtkObj_val(ti));
  return Val_unit;
}

/* ML type : gtkobj -> gtkobj -> unit */
value mgtk_tree_item_set_subtree(value ti, value st) { /* ML */
  gtk_tree_item_set_subtree(GtkObj_val(ti), GtkObj_val(st));
  return Val_unit;
}

/* ML type : gtkobj -> unit */
value mgtk_tree_item_remove_subtree(value ti) { /* ML */
  gtk_tree_item_remove_subtree(GtkObj_val(ti));
  return Val_unit;
}

/* ML type : gtkobj -> unit */
value mgtk_tree_item_expand(value ti) { /* ML */
  gtk_tree_item_expand(GtkObj_val(ti));
  return Val_unit;
}

/* ML type : gtkobj -> unit */
value mgtk_tree_item_collapse(value ti) { /* ML */
  gtk_tree_item_collapse(GtkObj_val(ti));
  return Val_unit;
}


/* *** Tree stuff *** */

/* ML type : unit -> gtkobj */
value mgtk_tree_new(value dummy) { /* ML */
  return Val_GtkObject(gtk_tree_new());
}

/* ML type : unit -> int * int * int * int  */
value mgtk_get_selection_modes(value dummy) {
  value res = alloc_tuple(4);
  Field(res,0) = Val_int(GTK_SELECTION_SINGLE); 
  Field(res,1) = Val_int(GTK_SELECTION_BROWSE);
  Field(res,2) = Val_int(GTK_SELECTION_MULTIPLE);
  Field(res,3) = Val_int(GTK_SELECTION_EXTENDED);

  return res;
}

/* ML type : unit -> int * int */
value mgtk_get_view_modes(value dummy) {
  value res = alloc_tuple(2);
  Field(res,0) = Val_int(GTK_TREE_VIEW_LINE);
  Field(res,1) = Val_int(GTK_TREE_VIEW_ITEM);

  return res;
}

/* ML type : gtkobj -> gtkobj -> unit */
value mgtk_tree_append(value t, value ti) { /* ML */
  gtk_tree_append(GtkObj_val(t), GtkObj_val(ti));
  return Val_unit;
}

/* ML type : gtkobj -> gtkobj -> unit */
value mgtk_tree_prepend(value t, value ti) { /* ML */
  gtk_tree_prepend(GtkObj_val(t), GtkObj_val(ti));
  return Val_unit;
}

/* ML type : gtkobj -> gtkobj -> int -> unit */
value mgtk_tree_insert(value t, value ti, value p) { /* ML */
  gtk_tree_insert(GtkObj_val(t), GtkObj_val(ti), Int_val(p));
  return Val_unit;
}

/* ML type : gtkobj -> int -> int -> unit */
value mgtk_tree_clear_items(value t, value s, value e) { /* ML */
  gtk_tree_clear_items(GtkObj_val(t), Int_val(s), Int_val(e));
  return Val_unit;
}

/* ML type : gtkobj -> int -> unit */
value mgtk_tree_select_item(value t, value i) { /* ML */
  gtk_tree_select_item(GtkObj_val(t), Int_val(i));
  return Val_unit;
}

/* ML type : gtkobj -> int -> unit */
value mgtk_tree_unselect_item(value t, value i) { /* ML */
  gtk_tree_unselect_item(GtkObj_val(t), Int_val(i));
  return Val_unit;
}

/* ML type : gtkobj -> gtkobj -> unit */
value mgtk_tree_select_child(value t, value ti) { /* ML */
  gtk_tree_select_child(GtkObj_val(t), GtkObj_val(ti));
  return Val_unit;
}

/* ML type : gtkobj -> gtkobj -> unit */
value mgtk_tree_unselect_child(value t, value ti) { /* ML */
  gtk_tree_unselect_child(GtkObj_val(t), GtkObj_val(ti));
  return Val_unit;
}

/* ML type : gtkobj -> gtkobj -> int */
value mgtk_tree_child_position(value t, value ti) { /* ML */
  return Int_val(gtk_tree_child_position(GtkObj_val(t),GtkObj_val(ti)));
}

/* ML type : gtkobj -> gtkobj -> unit */
value mgtk_tree_remove_item(value t, value c) { /* ML */
  gtk_tree_remove_item(GtkObj_val(t), GtkObj_val(t));
  return Val_unit;
}

/* ML type : gtkobj -> int -> unit */
value mgtk_tree_set_selection_mode(value t, value m) { /* ML */
  gtk_tree_set_selection_mode(GtkObj_val(t), Int_val(m));
  return Val_unit;
}

/* ML type : gtkobj -> int -> unit */
value mgtk_tree_set_view_mode(value t, value m) { /* ML */
  gtk_tree_set_view_mode(GtkObj_val(t), Int_val(m));
  return Val_unit;
}

/* ML type : gtkobj -> bool -> unit */
value mgtk_tree_set_view_lines(value t, value f) { /* ML */
  gtk_tree_set_view_lines(GtkObj_val(t), Bool_val(f));
  return Val_unit;
}

/* *** Combo stuff *** */

/* ML type : unit -> gtkobj */
value mgtk_combo_new(value dummy) { /* ML */
  return Val_GtkObject(gtk_combo_new());
}

/* [mgtk_combo_get_list(cbo)] returns combo->list

   This is NOT a gtk function, however, we need access to
   the internal list used in a combo box if we want to
   insert arbitrary widgets in the combo box.
*/
/* ML type : gtkobj -> gtkobj */
value mgtk_combo_get_list(value cbo) { /* ML */
  GtkCombo *combo = GTK_COMBO(GtkObj_val(cbo));
  return Val_GtkObject(combo -> list);
}

/* ML type : gtkobj -> gtkobj */
value mgtk_combo_get_entry(value cbo) { /* ML */
  GtkCombo *combo = GTK_COMBO(GtkObj_val(cbo));
  return Val_GtkObject(combo -> entry);
}


/* ML type : gtkobj -> glist -> unit */
value mgtk_combo_set_popdown_strings(value cbo, value lst) { /* ML */
  gtk_combo_set_popdown_strings(GtkObj_val(cbo), Glist_val(lst));
  return Val_unit;
}

/* ML type : gtkobj -> bool -> bool -> unit */
value mgtk_combo_set_value_in_list(value cbo, value il, value eok) { /* ML */
  gtk_combo_set_value_in_list(GtkObj_val(cbo), Bool_val(il), Bool_val(eok));
  return Val_unit;
}

/* ML type : gtkobj -> bool -> unit */
value mgtk_combo_set_use_arrows(value cbo, value ua) { /* ML */
  gtk_combo_set_use_arrows(GtkObj_val(cbo), Bool_val(ua));
  return Val_unit;
}

/* ML type : gtkobj -> bool -> unit */
value mgtk_combo_set_use_arrows_always(value cbo, value ua) { /* ML */
  gtk_combo_set_use_arrows_always(GtkObj_val(cbo), Bool_val(ua));
  return Val_unit;
}

/* ML type : gtkobj -> bool -> unit */
value mgtk_combo_set_case_sensitive(value cbo, value cs) { /* ML */
  gtk_combo_set_case_sensitive(GtkObj_val(cbo), Bool_val(cs));
  return Val_unit;
}

/* ML type : gtkobj -> gtkobj -> string -> unit */
value mgtk_combo_set_item_string(value cbo, value i, value s) { /* ML */
  gtk_combo_set_item_string(GtkObj_val(cbo), GtkObj_val(i), String_val(s));
  return Val_unit;
}

/* ML type : gtkobj -> unit */
value mgtk_combo_disable_activate(value cbo) { /* ML */
  gtk_combo_disable_activate(GtkObj_val(cbo));
  return Val_unit;
}



