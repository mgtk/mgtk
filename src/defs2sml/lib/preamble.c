#include <string.h>

/* GTK stuff */
#include <gtk/gtk.h>
#include <string.h>

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

/* Get rid of this stuff somehow! */
EXTERNML value my_copy_string(const char *s) {
  return copy_string((char*)s);
}

/* From Ken's note on interfacing mosml to C */
#define Char_val(c)	((char) Long_val(c))
#define Val_char(c)	(Val_long((long) c))

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
#define GtkObj_val_nocast(x) (Field(x, 1))

static void ml_finalize_gtkobject (value val) {
  gtk_object_unref (GtkObj_val(val)); 
}

static inline value Val_GtkObj (void* obj) { 
  value res; 
  gtk_object_ref(obj); 
  res = alloc_final (2, ml_finalize_gtkobject, 0, 1);
  GtkObj_val_nocast(res) = (value)obj;  
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

/* A char * -> value version of the same
   FIXME: Is this correct?
 */
static inline value copy_sml_string_to_value(const char *s) {
  return (value) copy_sml_string((value)s);
}


/* Construct an SML string list from a C char* array
 * TODO: Generalise so that the type becomes:
        value array_to_list(int n, void** arr, value conv(void*))
 */
static inline value string_array_to_list(int n, char** arr) {
  value result;
  Push_roots(tmp, 2);
  tmp[0] = Nil_list;
  for( ; n > 0; n--) {
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

/* ML type: unit -> unit */
EXTERNML value mgtk_get_null(value dummy) { /* ML */
  return (value) NULL;
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
MGTK_MakeSetter(mgtk_set_char, g_value_set_char, Char_val)
MGTK_MakeSetter(mgtk_set_double, g_value_set_double, Double_val)
MGTK_MakeSetter(mgtk_set_string, g_value_set_string, copy_sml_string)

/*
MGTK_MakeGetter(mgtk_get_pos_char, GTK_VALUE_CHAR, Val_long)
MGTK_MakeGetter(mgtk_get_pos_uchar, GTK_VALUE_UCHAR, Val_long)
*/
MGTK_MakeGetter(mgtk_get_pos_bool, g_value_get_boolean, Val_bool)
MGTK_MakeGetter(mgtk_get_pos_int, g_value_get_int, Val_long)
MGTK_MakeGetter(mgtk_get_pos_char, g_value_get_char, Val_char)
MGTK_MakeGetter(mgtk_get_pos_double, g_value_get_double, copy_double)
MGTK_MakeGetter(mgtk_get_pos_string, g_value_get_string, copy_sml_string_to_value)
/*
MGTK_MakeGetter(mgtk_get_pos_uint, GTK_VALUE_UINT, Val_long)
MGTK_MakeGetter(mgtk_get_pos_long, GTK_VALUE_LONG, Val_long)
MGTK_MakeGetter(mgtk_get_pos_float, GTK_VALUE_FLOAT, copy_double)
MGTK_MakeGetter(mgtk_get_pos_double, GTK_VALUE_DOUBLE, copy_double)
MGTK_MakeGetter(mgtk_get_pos_string, GTK_VALUE_STRING, copy_string)
*/

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

