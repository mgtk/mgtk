/* mgtk --- an SML binding for GTK.                                          */
/* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       */
/*                                                                           */

#include <string.h>

/* GTK stuff */
#include <gtk/gtk.h>

/* Mosml stuff */
#include <mlvalues.h> 
#include <fail.h>
#include <alloc.h>
#include <memory.h>
#include <callback.h>
#include <str.h>

#ifdef WIN32
#define EXTERNML __declspec(dllexport)
#else
#define EXTERNML
#endif


#define GetRefVal(r) (Field(r, 0))
#define SetRefVal(r, v) (modify(&Field(r,0), v))

/* TODO: 

   . Don't use void* so extensively.  Use the cast functions provided by gtk.
     For instance, use #define GtkObj_val(x) ((G_OBJECT(FIELD(x,1))))
     instead of the definition below. We might even want to declare widget
     specific _val macros a la: 
     #define GtkContainer_val(x) ((GTK_CONTAINER(Field(x,1))))

   . Perhaps rename Val_GtkObj to make_GtkObj or something similar.

*/


/* A nice macro to have */
#define GtkObj_val(x) (((void*) Field(x, 1)))

static void ml_finalize_gtkobject (value val) {
  gtk_object_unref (GtkObj_val(val)); 
}

static inline value Val_GtkObj (void* obj) { 
  value res; 
  gtk_object_ref(obj); 
  res = alloc_final (2, ml_finalize_gtkobject, 0, 1);
  GtkObj_val(res) = obj;  
  return res; 
}



/* *** Basic stuff *** */

/* ** Working with SML lists ** */
#define IsCons(x) (Tag_val(x) != 0)  
#define Nil_list  (Atom(0))
#define Head(xs)  (Field(xs, 0))
#define Tail(xs)  (Field(xs, 1))

static inline value make_cons(value elem, value tail) {
  value result;
  Push_roots(tmp, 2);
   tmp[0] = elem;
   tmp[1] = tail;
   result       = alloc(2, 1);     /* Allocate a cons cell, tag == 1 */
   Head(result) = tmp[0];          /* result is just allocated, thus */
   Tail(result) = tmp[1];          /* we don't need to use modify    */
  Pop_roots();
  return result;
}


/* Copy an SML string from the SML heap to the C heap 
 */
static inline char* copy_sml_string(value s) {
  mlsize_t len = string_length(s);    /* Much faster than strlen */
  char* mlstr  = String_val(s);       /* \0-terminated string    */
  char* result = stat_alloc(len+1);   /* +1 for the trailing \0  */
  memcpy(result, mlstr, len);         /* Copy the ML string      */
  return result;
}


/* Construct an SML string list from a C char* array
 * TODO: Generalise so that the type becomes:
        value array_to_list(int n, void** arr, value conv(void*))
 */
static inline value string_array_to_list(int n, char** arr) {
  value result;
  Push_roots(tmp, 2);
  tmp[0] = Nil_list;
  for( ; n != 0; n--) {
    value ml_str     = copy_string(arr[n-1]);
    tmp[1]           = make_cons(ml_str, tmp[0]);
    tmp[0]           = tmp[1];     
  }
  result = tmp[0];  
  Pop_roots();
  return result;
}    


/* ML type: string vector -> string list */
EXTERNML value mgtk_init(value args) { /* ML */
  /* PRECONDITION: Wosize_val(args) > 0 */
  int argc, i;
  char** argv;
  value result;

  argc = Wosize_val(args);
  argv = (char**) stat_alloc(sizeof(char*) * argc);

  for (i=0; i<argc; i++) {
    argv[i] = copy_sml_string(Field(args, i)); 
  }

  gtk_init(&argc, &argv);

  result = string_array_to_list(argc, argv);

  for(; argc > 0; argc--){
    stat_free(argv[argc-1]);
  }

  stat_free((char *) argv);

  return result;
}


static inline GList* GList_val (value list, gpointer (*conv_val)(value)) {
  if (!IsCons(list)) {
    return NULL;
  } else {
    GList* res = NULL, * last;
    Push_roots(tmp, 1)// We need this in case conv_val allocates in the ML heap
      tmp[0] = list;
      res = last = g_list_append(res, conv_val(Head(tmp[0])));
      tmp[0] = Tail(tmp[0]);
      for (; IsCons(tmp[0]); tmp[0] = Tail(tmp[0])) {
        GList* elem = g_list_alloc();
        elem->prev = last;
        elem->next = NULL;
        elem->data = conv_val(Head(tmp[0]));
        last->next = elem;
        last = elem;
      }
    Pop_roots();
    return res;
  }
}

static inline value val_GList(GList* glist, value (*val_conv)(gpointer)) {
  if (glist == NULL) {
    return Nil_list;
  } else {
    value result, elem;
    Push_roots(tmp, 3);
      elem   = val_conv(glist->data);              /* The first element */
      tmp[2] = tmp[0] = make_cons(elem, Nil_list); /* tmp[2] is the result */
      glist  = g_list_next(glist);
      for(; glist != NULL; glist = g_list_next(glist)) {
        elem   = val_conv(glist->data);
        tmp[1] = make_cons(elem, Nil_list);
        modify(&Tail(tmp[0]), tmp[1]); /* tmp[0] is older than tmp[1] */
        tmp[0] = tmp[1];     
      }
      result = tmp[2];  
    Pop_roots();
    return result;
  }
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



/* *** GValue stuff *** */

/* We wrap both pointers to a single GValue and arrays to GValues in
   abtract values.
*/

#define GValue_val(arg) ( Field(arg, 0) )

static inline value make_GValue(const GValue* val) {
  value res = alloc(1, Abstract_tag);
  GValue_val(res) = (value) val;
  return res;
}


#define MGTK_MakeSetter(name, gval_setter, mlconv)              \
EXTERNML value name (value gvalue, value mlvalue) {  /* ML */   \
  GValue* val = (GValue*) GValue_val(gvalue);                   \
  gval_setter(val, mlconv(mlvalue));                            \
  return Val_unit;                                              \
}


#define MGTK_MakeGetter(name, gval_getter, convml)      \
EXTERNML value name (value vargs, value pos) { /* ML */ \
  long p = Long_val(pos);                               \
  GValue* args = (GValue*) GValue_val(vargs);           \
  return convml(gval_getter(&args[p]));                 \
}



MGTK_MakeSetter(mgtk_set_bool, g_value_set_boolean, Bool_val)
MGTK_MakeSetter(mgtk_set_long, g_value_set_long, Long_val)
MGTK_MakeSetter(mgtk_set_int, g_value_set_int, Long_val)

/*
MGTK_MakeGetter(mgtk_get_pos_char, GTK_VALUE_CHAR, Val_long)
MGTK_MakeGetter(mgtk_get_pos_uchar, GTK_VALUE_UCHAR, Val_long)
*/
MGTK_MakeGetter(mgtk_get_pos_bool, g_value_get_boolean, Val_bool)
MGTK_MakeGetter(mgtk_get_pos_int, g_value_get_int, Val_long)
/*
MGTK_MakeGetter(mgtk_get_pos_uint, GTK_VALUE_UINT, Val_long)
MGTK_MakeGetter(mgtk_get_pos_long, GTK_VALUE_LONG, Val_long)
MGTK_MakeGetter(mgtk_get_pos_float, GTK_VALUE_FLOAT, copy_double)
MGTK_MakeGetter(mgtk_get_pos_double, GTK_VALUE_DOUBLE, copy_double)
MGTK_MakeGetter(mgtk_get_pos_string, GTK_VALUE_STRING, copy_string)
*/


/* *** Signal stuff *** */
static void mgtk_callback_dispatch (GClosure *closure,
                                    GValue *return_value,
                                    guint n_param_values,
                                    const GValue *param_values,
                                    gpointer invocation_hint,
                                    gpointer marshal_data) {
  value tup;
  valueptr mvp;

  Push_roots(r, 2);  
    r[0] = make_GValue(return_value);
    r[1] = make_GValue(param_values);
    tup  = alloc_tuple(3);
    Field(tup, 0) = r[0];
    Field(tup, 1) = r[1];
  Pop_roots();
    Field(tup, 2) = Val_int(n_param_values);


  mvp = get_valueptr("mgtk_callback_dispatch"); 
  if(mvp == (valueptr) NULL)
    failwith("Cannot find mgtk_callback_dispatch");

  /*  printf("callback dispatch id = %i\n",(int) closure->data); 
   */
  tup = callbackptr2(mvp, (value) closure->data, tup);
}


static void mgtk_callback_destroy (gpointer data,
                                   GClosure *closure) {
  valueptr mvp = get_valueptr("mgtk_callback_destroy"); 
  if(mvp == (valueptr) NULL)
    failwith("Cannot find mgtk_callback_destroy");

  /*  printf("callback destroy id = %i\n",(int) data);
   */
  callbackptr(mvp, (value) data);
}


static inline GClosure* mgtk_closure_new(gpointer callback_id) {
  GClosure *closure;

  /*    printf("register id = %i\n",(int) callback_id);
   */
  closure = g_closure_new_simple(sizeof(GClosure), 
                                 callback_id);

  g_closure_set_marshal (closure, mgtk_callback_dispatch);

  g_closure_add_finalize_notifier (closure, 
                                   callback_id, 
                                   mgtk_callback_destroy);

  return closure;
}


/* Comment (HN, 2003/02/19): I kinda think that the return value from
   g_signal_connect_closure is not the signal id, but rather the tag
   (refer to the section "More on Signal Handlers" in the Gtk+ 2.0
   tutorial.
*/

/* ML type: cptr -> string -> clb -> bool -> int */
EXTERNML value mgtk_signal_connect (value object, value name, value clb, value after){  /* ML */
  int res;
  GClosure *closure;


  /*  printf("register id = %i\n",(int) clb); */

  closure = mgtk_closure_new((gpointer) clb);

  res = g_signal_connect_closure (GtkObj_val(object), 
                                  String_val(name), 
                                  closure,
                                  Bool_val(after));

  /*  g_closure_unref(closure);
   */
  return Val_long(res);
}

/* *** Widget stuff *** */

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_destroy(value widget) { /* ML */
  gtk_widget_destroy(GtkObj_val(widget));
  return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_show(value widget) { /* ML */
  gtk_widget_show(GtkObj_val(widget));
  return Val_unit;
}


/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_show_all(value widget) { /* ML */
  gtk_widget_show_all(GtkObj_val(widget));
  return Val_unit;
}


/* *** Container stuff *** */

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_container_set_border_width(value container, value border_width) { /* ML */
  gtk_container_set_border_width(GtkObj_val(container), Int_val(border_width));
  return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_container_add(value container, value widget) { /* ML */
  gtk_container_add(GtkObj_val(container), GtkObj_val(widget));
  return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_container_remove(value container, value widget) { /* ML */
  gtk_container_remove(GtkObj_val(container), GtkObj_val(widget));
  return Val_unit;
}


/* *** Button stuff *** */

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_button_new(value dummy) { /* ML */
  return Val_GtkObj(gtk_button_new());
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_button_new_with_label(value label) { /* ML */
  return Val_GtkObj(gtk_button_new_with_label(String_val(label)));
}

/* ML type: gtkobj -> string -> unit */
EXTERNML value mgtk_gtk_button_set_label(value button, value label) { /* ML */
  gtk_button_set_label(GtkObj_val(button), String_val(label));
  return Val_unit;
}

/* *** Window stuff *** */

/* ML type: unit -> int * int  */
EXTERNML value mgtk_get_window_type (value dummy) { /* ML */
  value res = alloc_tuple(2);
  Field(res,0) = Val_int(GTK_WINDOW_TOPLEVEL);
  Field(res,1) = Val_int(GTK_WINDOW_POPUP);
  return res;
}


/* ML type: int -> cptr */
EXTERNML value mgtk_gtk_window_new(value typ) { /* ML */
  return Val_GtkObj(gtk_window_new(Int_val(typ)));
}


/* ML type: cptr -> int ref -> int ref -> unit */
EXTERNML value mgtk_gtk_window_get_size(value self, value wdr, value htr) { /* ML */
  int wd = Int_val(GetRefVal(wdr));
  int ht = Int_val(GetRefVal(htr));
  gtk_window_get_size(GtkObj_val(self), &wd, &ht);
  SetRefVal(wdr, Val_int(wd));
  SetRefVal(htr, Val_int(ht));
  return Val_unit;
}

/* ML type: int -> cptr */
EXTERNML value mgtk_gtk_entry_new(value unit) { /* ML */
  return Val_GtkObj(gtk_entry_new());
}

/* ML type: gtkobj -> string */
EXTERNML value mgtk_gtk_entry_get_text(value entry) { /* ML */
  return copy_string(gtk_entry_get_text(GtkObj_val(entry)));
}


/* ML type: gtkobj -> gtkobj -> unit */
EXTERNML value mgtk_gtk_box_pack_start(value box, value child) { /* ML */
  gtk_box_pack_start_defaults(GtkObj_val(box), GtkObj_val(child));
  return Val_unit;
}

/* ML type: bool -> int -> gtkobj */
EXTERNML value mgtk_gtk_vbox_new(value homogeneous, value spacing) { /* ML */
  return Val_GtkObj(gtk_vbox_new(Bool_val(homogeneous), Int_val(spacing)));
}



/* *** Text stuff *** */

#define GtkTextIter_val(x) (((void*) Field(x, 1)))

static void ml_finalize_gtktextiter (value val) {
  gtk_text_iter_free (GtkTextIter_val(val)); 
}

static inline value Val_GtkTextIter (void* obj) { 
  value res; 
  res = alloc_final (2, ml_finalize_gtktextiter, 0, 1);
  GtkObj_val(res) = gtk_text_iter_copy(obj);  
  return res; 
}

/* ML type : cptr -> cptr */
EXTERNML value mgtk_gtk_text_iter_copy(value iter) { /* ML */
  return Val_GtkTextIter(GtkTextIter_val(iter));
}

/* ML type : cptr -> TextIter.t -> string -> unit */
EXTERNML value mgtk_gtk_text_buffer_insert(value b, value i, value t){ /* ML */
  gtk_text_buffer_insert(GtkObj_val(b), GtkTextIter_val(i), String_val(t), -1);
  return Val_unit;
}

/* ML type : cptr -> TextIter.t */
EXTERNML value mgtk_gtk_text_buffer_get_start_iter (value buffer) { /* ML */
  GtkTextIter iter;
  gtk_text_buffer_get_start_iter(GtkObj_val(buffer), &iter);
  return Val_GtkTextIter(&iter);
}

/* ML type : cptr -> TextIter.t */
EXTERNML value mgtk_gtk_text_buffer_get_end_iter (value buffer) { /* ML */
  GtkTextIter iter;
  gtk_text_buffer_get_end_iter(GtkObj_val(buffer), &iter);
  return Val_GtkTextIter(&iter);
}

EXTERNML value mgtk_gtk_text_view_new(value dummy) { /* ML */
  return Val_GtkObj(gtk_text_view_new());
}


EXTERNML value mgtk_gtk_text_view_get_buffer(value buffer) { /* ML */
  return Val_GtkObj(gtk_text_view_get_buffer(GtkObj_val(buffer)));
}
