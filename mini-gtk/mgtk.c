/* mgtk --- an SML binding for GTK.                                          */
/* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                         */
/*                                                                           */

/* GTK stuff */
#include <gtk/gtk.h>

/* Mosml stuff */
#include <mlvalues.h> 
#include <fail.h>
#include <alloc.h>
#include <memory.h>
#include <callback.h>

#ifdef WIN32
#define EXTERNML __declspec(dllexport)
#else
#define EXTERNML
#endif


/* TODO: 

   . Don't use void* so extensively.  Use the cast functions provided by gtk.

*/


/* A nice macro to have */
#define GtkObj_val(x) (((void*) Field(x, 1)))

static void ml_finalize_gtkobject (value val) {
  gtk_object_unref (GtkObj_val(val)); 
}

value Val_GtkObj (void* obj) { 
  value res; 
  gtk_object_ref(obj); 
  res = alloc_final (2, ml_finalize_gtkobject, 0, 1);
  GtkObj_val(res) = obj;  
  return res; 
}



/* *** Basic stuff *** */

/* ML type: string vector -> unit */
EXTERNML value mgtk_init(value args) { /* ML */
  /* PRECONDITION: Wosize_val(args) > 0 */
  int argc, i;
  char** argv;

  argc = Wosize_val(args);
  argv = (char**) stat_alloc(sizeof(char*) * argc);

  /* Assumes that gtk_init don't changes the strings; if it does we should use
     toCstring instead of String_val */
  for (i=0; i<argc; i++) {
    argv[i] = String_val(Field(args, i)); 
  }

  gtk_init(&argc, &argv);

  /*
  value result;
  result = alloc(argc, 0);
  for (i=0; i<argc; i++) {
    Field(result, i) = copy_string(argv[i]);
  }
  */

  stat_free((char *) argv);

  return Val_unit;
}


/* ML type: unit -> unit */
EXTERNML value mgtk_main(value dummy) { /* ML */
  gtk_main ();
  return Val_unit;
}

/* ML type: unit -> unit */
EXTERNML value mgtk_main_quit(value dummy) { /* ML */
  gtk_main_quit ();
  return Val_unit;
}

/* *** GtkArg stuff *** */

#define GtkArg_val(arg) ( Field(arg, 0) )

value wrap_GtkArg(GtkArg *args) {
  value res = alloc(1, Abstract_tag);
  Field(res, 0) = (value) args;
  return res;
}


#define MGTK_MakeSetter(name, retloc, mlconv)                   \
EXTERNML value name (value a, value pos, value val) {  /* ML */ \
  long p = Long_val(pos);                                       \
  GtkArg* args = (GtkArg*) GtkArg_val(a);                       \
  *retloc(args[p]) = mlconv(val);                               \
  return Val_unit;                                              \
}

#define MGTK_MakeGetter(name, gtkvalue, convml)         \
EXTERNML value name (value vargs, value pos) { /* ML */ \
  long p = Long_val(pos);                               \
  GtkArg* args = (GtkArg*) GtkArg_val(vargs);           \
  return convml(gtkvalue(args[p]));                     \
}



MGTK_MakeSetter(mgtk_set_retpos_bool, GTK_RETLOC_BOOL, Bool_val)
MGTK_MakeSetter(mgtk_set_retpos_long, GTK_RETLOC_LONG, Long_val)
MGTK_MakeSetter(mgtk_set_retpos_int, GTK_RETLOC_INT, Long_val)
MGTK_MakeSetter(mgtk_set_retpos_uint, GTK_RETLOC_UINT, Long_val)
MGTK_MakeSetter(mgtk_set_retpos_double, GTK_RETLOC_DOUBLE, Double_val)


MGTK_MakeGetter(mgtk_get_pos_char, GTK_VALUE_CHAR, Val_long)
MGTK_MakeGetter(mgtk_get_pos_uchar, GTK_VALUE_UCHAR, Val_long)
MGTK_MakeGetter(mgtk_get_pos_bool, GTK_VALUE_BOOL, Val_bool)
MGTK_MakeGetter(mgtk_get_pos_int, GTK_VALUE_INT, Val_long)
MGTK_MakeGetter(mgtk_get_pos_uint, GTK_VALUE_UINT, Val_long)
MGTK_MakeGetter(mgtk_get_pos_long, GTK_VALUE_LONG, Val_long)
MGTK_MakeGetter(mgtk_get_pos_float, GTK_VALUE_FLOAT, copy_double)
MGTK_MakeGetter(mgtk_get_pos_double, GTK_VALUE_DOUBLE, copy_double)
MGTK_MakeGetter(mgtk_get_pos_string, GTK_VALUE_STRING, copy_string)

/* *** Signal stuff *** */
void mgtk_callback_dispatch (GtkObject *object, gpointer data, guint nargs, 
			     GtkArg *args) {
  value res;
  valueptr mvp;

  Push_roots(r, 2);  // because both alloc_tuple and Val_GtkObj allocates
    r[0] = Val_GtkObj(object);
    r[1] = wrap_GtkArg(args);
    res  = alloc_tuple(3);
    Field(res,0) = r[0]; 
    Field(res,1) = r[1];
    Field(res,2) = Val_int(nargs);   
  Pop_roots();

  mvp = get_valueptr("mgtk_callback_dispatch"); 
  if(mvp == (valueptr) NULL)
    failwith("Cannot find mgtk_callback_dispatch");

  /* printf("callback id = %i\n",(int) data); */

  /* data is really a SML int */
  res = callbackptr2(mvp, (value) data, res);
}

void mgtk_callback_destroy (gpointer data) {
  valueptr mvp = get_valueptr("mgtk_callback_destroy"); 
  if(mvp == (valueptr) NULL)
    failwith("Cannot find mgtk_callback_destroy");

  /* printf("callback id = %i\n",(int) data);
   */
  callbackptr(mvp, (value) data);
}


/* ML type: gtkobj -> string -> clb -> bool -> int */
EXTERNML value mgtk_signal_connect (value object, value name, value clb, value after){
  int res;

  /*  printf("register id = %i\n",(int) clb); */

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

/* *** Widget stuff *** */

/* ML type: gtkobj -> unit */
EXTERNML value mgtk_gtk_widget_destroy(value widget) { /* ML */
  gtk_widget_destroy(GtkObj_val(widget));
  return Val_unit;
}

/* ML type: gtkobj -> unit */
EXTERNML value mgtk_gtk_widget_show(value widget) { /* ML */
  gtk_widget_show(GtkObj_val(widget));
  return Val_unit;
}


/* ML type: gtkobj -> unit */
EXTERNML value mgtk_gtk_widget_show_all(value widget) { /* ML */
  gtk_widget_show_all(GtkObj_val(widget));
  return Val_unit;
}


/* *** Container stuff *** */

/* ML type: gtkobj -> int -> unit */
EXTERNML value mgtk_gtk_container_set_border_width(value container, value border_width) { /* ML */
  gtk_container_set_border_width(GtkObj_val(container), Int_val(border_width));
  return Val_unit;
}

/* ML type: gtkobj -> gtkobj -> unit */
EXTERNML value mgtk_gtk_container_add(value container, value widget) { /* ML */
  gtk_container_add(GtkObj_val(container), GtkObj_val(widget));
  return Val_unit;
}

/* ML type: gtkobj -> gtkobj -> unit */
EXTERNML value mgtk_gtk_container_remove(value container, value widget) { /* ML */
  gtk_container_remove(GtkObj_val(container), GtkObj_val(widget));
  return Val_unit;
}


/* *** Button stuff *** */

/* ML type: unit -> gtkobj */
EXTERNML value mgtk_gtk_button_new(value dummy) { /* ML */
  return Val_GtkObj(gtk_button_new());
}

/* ML type: string -> gtkobj */
EXTERNML value mgtk_gtk_button_new_with_label(value label) { /* ML */
  return Val_GtkObj(gtk_button_new_with_label(String_val(label)));
}

/* *** Window stuff *** */

/* ML type: int -> gtkobj */
EXTERNML value mgtk_gtk_window_new(value typ) { /* ML */
  return Val_GtkObj(gtk_window_new(Int_val(typ)));
}
