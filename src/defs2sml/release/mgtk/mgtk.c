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
#define GtkObj_val_nocast(x) (Field(x, 1))

static void ml_finalize_gtkobject (value val) {
  g_object_unref (GtkObj_val(val)); 
}

static inline value Val_GtkObj (void* obj) { 
  value res; 
  g_object_ref(obj); 
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

static inline long length(value xs) {
  long sum = 0;
  while (IsCons(xs)) { 	           /* While non-Nil                      */
    sum++;
    xs = Tail(xs);                 /* The list tail = second arg of Cons */
  }
  return sum;  
}

#define list_to_array(T, result, conv_val, list) {                        \
  if (!IsCons(list)) {                                                    \
    (result) = NULL;                                                      \
  } else {                                                                \
    int _i_ = 0;                                                          \
    T* _res_ = (T*) malloc(sizeof(T) * length(list));                     \
    Push_roots(_tmp_, 1); /* if conv_val allocates in the ML heap */      \
      _tmp_[0] = (list);                                                  \
      while (IsCons(_tmp_[0])) {                                          \
	_res_[_i_++] = conv_val(Head(_tmp_[0]));                          \
	_tmp_[0] = Tail(_tmp_[0]);                                        \
      }                                                                   \
    Pop_roots();                                                          \
    (result) = _res_;                                                     \
  }                                                                       \
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
  value res = alloc(2, Abstract_tag);
  Field(res,1) = (value)NULL;
  return res;
}


/* *** GValue stuff *** */

/* We wrap both pointers to a single GValue and arrays to GValues in
   abtract values.
*/

#define GValue_val(arg) ( (GValue*) Field(arg, 1) )
#define GValue_val_nocast(arg) ( Field(arg, 1) )

static void ml_finalize_gvalue_old (value val) {
  g_value_unset ((GValue*) Field(val,1)); 
}

static void ml_finalize_gvalue (value val) {
  g_value_unset ((GValue*) &Field(val,1)); 
}

static inline value create_GValue (GType type) {
  value res = alloc_final (2, ml_finalize_gvalue, 0, 1);
  GValue_val_nocast(res) = (value) malloc(sizeof(GValue));
  memset(GValue_val(res), 0, sizeof(GValue));
  g_value_init(GValue_val(res), type);
  return res;
}

static inline value Val_GValue (GValue* val) {
  value res = alloc_final (2, ml_finalize_gvalue, 0, 1);
  GValue_val_nocast(res) = (value)val;
  return res;
}

EXTERNML value mgtk_g_value_set_int (value i){
  value res = create_GValue(G_TYPE_INT);
  g_value_set_int(GValue_val(res), Int_val(i));
  return res;
}

EXTERNML value mgtk_g_value_set_real (value r){
  value res = create_GValue(G_TYPE_DOUBLE);
  g_value_set_double(GValue_val(res), Double_val(r));
  return res;
}

EXTERNML value mgtk_g_value_set_string (value s){
  value res = create_GValue(G_TYPE_STRING);
  g_value_set_string(GValue_val(res), String_val(s));
  return res;
}

static inline value make_GValue(const GValue* val) {
  value res = alloc(2, Abstract_tag); // Hack: assume that C owns the GValue 
  GValue_val_nocast(res) = (value) val;
  return res;
}


#define MGTK_MakeSetter(name, gval_setter, mlconv)              \
EXTERNML value name (value gvalue, value mlvalue) {  /* ML */   \
  GValue* val = GValue_val(gvalue);                             \
  gval_setter(val, mlconv(mlvalue));                            \
  return Val_unit;                                              \
}


#define MGTK_MakeGetter(name, gval_getter, convml)      \
EXTERNML value name (value vargs, value pos) { /* ML */ \
  long p = Long_val(pos);                               \
  GValue* args = GValue_val(vargs);                     \
  return convml(gval_getter(&args[p]));                 \
}



MGTK_MakeSetter(mgtk_set_bool, g_value_set_boolean, Bool_val)
MGTK_MakeSetter(mgtk_set_long, g_value_set_long, Long_val)
MGTK_MakeSetter(mgtk_set_int, g_value_set_int, Long_val)
MGTK_MakeSetter(mgtk_set_char, g_value_set_char, Char_val)
MGTK_MakeSetter(mgtk_set_real, g_value_set_double, Double_val)
MGTK_MakeSetter(mgtk_set_string, g_value_set_string, copy_sml_string)

/*
MGTK_MakeGetter(mgtk_get_pos_char, GTK_VALUE_CHAR, Val_long)
MGTK_MakeGetter(mgtk_get_pos_uchar, GTK_VALUE_UCHAR, Val_long)
*/
MGTK_MakeGetter(mgtk_get_pos_bool, g_value_get_boolean, Val_bool)
MGTK_MakeGetter(mgtk_get_pos_int, g_value_get_int, Val_long)
MGTK_MakeGetter(mgtk_get_pos_char, g_value_get_char, Val_char)
MGTK_MakeGetter(mgtk_get_pos_real, g_value_get_double, copy_double)
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


/* GType's */

/* ML type: cptr -> int -> string */
EXTERNML value mgtk_g_type_name (value typ) { /* ML */
  return my_copy_string(g_type_name(Int_val(typ)));
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_g_type_int (value dummy) { /* ML */
  return Val_int (G_TYPE_INT);
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_g_type_real (value dummy) { /* ML */
  return Val_int (G_TYPE_DOUBLE);
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_g_type_string (value dummy) { /* ML */
  return Val_int (G_TYPE_STRING);
}


/* *** Gtk *** */
#define GtkRequisition_val(x) (((void*) Field(x, 1)))

#define GtkRequisition_val_nocast(x) (Field(x, 1))

static void ml_finalize_GtkRequisition(value val) { /* Empty */
}

value Val_GtkRequisition(void* obj) {
  value res;
  res = alloc_final(2, ml_finalize_GtkRequisition, 0, 1);
  GtkRequisition_val_nocast(res) = (value) obj;
  return res;
}

#define GtkCTreeNode_val(x) (((void*) Field(x, 1)))

#define GtkCTreeNode_val_nocast(x) (Field(x, 1))

static void ml_finalize_GtkCTreeNode(value val) { /* Empty */
}

value Val_GtkCTreeNode(void* obj) {
  value res;
  res = alloc_final(2, ml_finalize_GtkCTreeNode, 0, 1);
  GtkCTreeNode_val_nocast(res) = (value) obj;
  return res;
}

#define GtkIconSet_val(x) (((void*) Field(x, 1)))

#define GtkIconSet_val_nocast(x) (Field(x, 1))

static void ml_finalize_GtkIconSet(value val) { /* Empty */
}

value Val_GtkIconSet(void* obj) {
  value res;
  res = alloc_final(2, ml_finalize_GtkIconSet, 0, 1);
  GtkIconSet_val_nocast(res) = (value) obj;
  return res;
}

#define GtkSelectionData_val(x) (((void*) Field(x, 1)))

#define GtkSelectionData_val_nocast(x) (Field(x, 1))

static void ml_finalize_GtkSelectionData(value val) {
  gtk_selection_data_free(GtkSelectionData_val(val));
}

value Val_GtkSelectionData(void* obj) {
  value res;
  res = alloc_final(2, ml_finalize_GtkSelectionData, 0, 1);
  GtkSelectionData_val_nocast(res) = (value) gtk_selection_data_copy(obj);
  return res;
}

#define GtkTextAttributes_val(x) (((void*) Field(x, 1)))

#define GtkTextAttributes_val_nocast(x) (Field(x, 1))

static void ml_finalize_GtkTextAttributes(value val) { /* Empty */
}

value Val_GtkTextAttributes(void* obj) {
  value res;
  res = alloc_final(2, ml_finalize_GtkTextAttributes, 0, 1);
  GtkTextAttributes_val_nocast(res) = (value) obj;
  return res;
}

#define GtkTextIter_val(x) (((void*) Field(x, 1)))

#define GtkTextIter_val_nocast(x) (Field(x, 1))

static void ml_finalize_GtkTextIter(value val) {
  gtk_text_iter_free(GtkTextIter_val(val));
}

value Val_GtkTextIter(void* obj) {
  value res;
  res = alloc_final(2, ml_finalize_GtkTextIter, 0, 1);
  GtkTextIter_val_nocast(res) = (value) gtk_text_iter_copy(obj);
  return res;
}

#define GtkTreeIter_val(x) (((void*) Field(x, 1)))

#define GtkTreeIter_val_nocast(x) (Field(x, 1))

static void ml_finalize_GtkTreeIter(value val) {
  gtk_tree_iter_free(GtkTreeIter_val(val));
}

value Val_GtkTreeIter(void* obj) {
  value res;
  res = alloc_final(2, ml_finalize_GtkTreeIter, 0, 1);
  GtkTreeIter_val_nocast(res) = (value) gtk_tree_iter_copy(obj);
  return res;
}

/* ML type: unit -> int * int * int */
EXTERNML value mgtk_get_gtk_accel_flags(value dummy) { /* ML */
  value res = alloc_tuple(3);
  Field(res, 0) = Val_int(GTK_ACCEL_VISIBLE);
  Field(res, 1) = Val_int(GTK_ACCEL_LOCKED);
  Field(res, 2) = Val_int(GTK_ACCEL_MASK);
  return res;
}

/* ML type: unit -> int * int * int * int * int */
EXTERNML value mgtk_get_gtk_celltype(value dummy) { /* ML */
  value res = alloc_tuple(5);
  Field(res, 0) = Val_int(GTK_CELL_EMPTY);
  Field(res, 1) = Val_int(GTK_CELL_TEXT);
  Field(res, 2) = Val_int(GTK_CELL_PIXMAP);
  Field(res, 3) = Val_int(GTK_CELL_PIXTEXT);
  Field(res, 4) = Val_int(GTK_CELL_WIDGET);
  return res;
}

/* ML type: unit -> int * int * int * int * int */
EXTERNML value mgtk_get_gtk_debug_flag(value dummy) { /* ML */
  value res = alloc_tuple(5);
  Field(res, 0) = Val_int(GTK_DEBUG_MISC);
  Field(res, 1) = Val_int(GTK_DEBUG_PLUGSOCKET);
  Field(res, 2) = Val_int(GTK_DEBUG_TEXT);
  Field(res, 3) = Val_int(GTK_DEBUG_TREE);
  Field(res, 4) = Val_int(GTK_DEBUG_UPDATES);
  return res;
}

/* ML type: unit -> int * int * int * int * int * int * int * int * int * int * int */
EXTERNML value mgtk_get_gtk_responsetype(value dummy) { /* ML */
  value res = alloc_tuple(11);
  Field(res, 0) = Val_int(GTK_RESPONSE_NONE);
  Field(res, 1) = Val_int(GTK_RESPONSE_REJECT);
  Field(res, 2) = Val_int(GTK_RESPONSE_ACCEPT);
  Field(res, 3) = Val_int(GTK_RESPONSE_DELETE_EVENT);
  Field(res, 4) = Val_int(GTK_RESPONSE_OK);
  Field(res, 5) = Val_int(GTK_RESPONSE_CANCEL);
  Field(res, 6) = Val_int(GTK_RESPONSE_CLOSE);
  Field(res, 7) = Val_int(GTK_RESPONSE_YES);
  Field(res, 8) = Val_int(GTK_RESPONSE_NO);
  Field(res, 9) = Val_int(GTK_RESPONSE_APPLY);
  Field(res, 10) = Val_int(GTK_RESPONSE_HELP);
  return res;
}

/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_dest_defaults(value dummy) { /* ML */
  value res = alloc_tuple(4);
  Field(res, 0) = Val_int(GTK_DEST_DEFAULT_MOTION);
  Field(res, 1) = Val_int(GTK_DEST_DEFAULT_HIGHLIGHT);
  Field(res, 2) = Val_int(GTK_DEST_DEFAULT_DROP);
  Field(res, 3) = Val_int(GTK_DEST_DEFAULT_ALL);
  return res;
}

/* ML type: unit -> int * int */
EXTERNML value mgtk_get_gtk_target_flags(value dummy) { /* ML */
  value res = alloc_tuple(2);
  Field(res, 0) = Val_int(GTK_TARGET_SAME_APP);
  Field(res, 1) = Val_int(GTK_TARGET_SAME_WIDGET);
  return res;
}

/* ML type: unit -> int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int */
EXTERNML value mgtk_get_gtk_anchortype(value dummy) { /* ML */
  value res = alloc_tuple(17);
  Field(res, 0) = Val_int(GTK_ANCHOR_CENTER);
  Field(res, 1) = Val_int(GTK_ANCHOR_NORTH);
  Field(res, 2) = Val_int(GTK_ANCHOR_NORTH_WEST);
  Field(res, 3) = Val_int(GTK_ANCHOR_NORTH_EAST);
  Field(res, 4) = Val_int(GTK_ANCHOR_SOUTH);
  Field(res, 5) = Val_int(GTK_ANCHOR_SOUTH_WEST);
  Field(res, 6) = Val_int(GTK_ANCHOR_SOUTH_EAST);
  Field(res, 7) = Val_int(GTK_ANCHOR_WEST);
  Field(res, 8) = Val_int(GTK_ANCHOR_EAST);
  Field(res, 9) = Val_int(GTK_ANCHOR_N);
  Field(res, 10) = Val_int(GTK_ANCHOR_NW);
  Field(res, 11) = Val_int(GTK_ANCHOR_NE);
  Field(res, 12) = Val_int(GTK_ANCHOR_S);
  Field(res, 13) = Val_int(GTK_ANCHOR_SW);
  Field(res, 14) = Val_int(GTK_ANCHOR_SE);
  Field(res, 15) = Val_int(GTK_ANCHOR_W);
  Field(res, 16) = Val_int(GTK_ANCHOR_E);
  return res;
}

/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_arrowtype(value dummy) { /* ML */
  value res = alloc_tuple(4);
  Field(res, 0) = Val_int(GTK_ARROW_UP);
  Field(res, 1) = Val_int(GTK_ARROW_DOWN);
  Field(res, 2) = Val_int(GTK_ARROW_LEFT);
  Field(res, 3) = Val_int(GTK_ARROW_RIGHT);
  return res;
}

/* ML type: unit -> int * int * int */
EXTERNML value mgtk_get_gtk_attach_options(value dummy) { /* ML */
  value res = alloc_tuple(3);
  Field(res, 0) = Val_int(GTK_EXPAND);
  Field(res, 1) = Val_int(GTK_SHRINK);
  Field(res, 2) = Val_int(GTK_FILL);
  return res;
}

/* ML type: unit -> int * int * int */
EXTERNML value mgtk_get_gtk_curvetype(value dummy) { /* ML */
  value res = alloc_tuple(3);
  Field(res, 0) = Val_int(GTK_CURVE_TYPE_LINEAR);
  Field(res, 1) = Val_int(GTK_CURVE_TYPE_SPLINE);
  Field(res, 2) = Val_int(GTK_CURVE_TYPE_FREE);
  return res;
}

/* ML type: unit -> int * int * int * int * int * int * int * int */
EXTERNML value mgtk_get_gtk_deletetype(value dummy) { /* ML */
  value res = alloc_tuple(8);
  Field(res, 0) = Val_int(GTK_DELETE_CHARS);
  Field(res, 1) = Val_int(GTK_DELETE_WORD_ENDS);
  Field(res, 2) = Val_int(GTK_DELETE_WORDS);
  Field(res, 3) = Val_int(GTK_DELETE_DISPLAY_LINES);
  Field(res, 4) = Val_int(GTK_DELETE_DISPLAY_LINE_ENDS);
  Field(res, 5) = Val_int(GTK_DELETE_PARAGRAPH_ENDS);
  Field(res, 6) = Val_int(GTK_DELETE_PARAGRAPHS);
  Field(res, 7) = Val_int(GTK_DELETE_WHITESPACE);
  return res;
}

/* ML type: unit -> int * int * int * int * int * int */
EXTERNML value mgtk_get_gtk_directiontype(value dummy) { /* ML */
  value res = alloc_tuple(6);
  Field(res, 0) = Val_int(GTK_DIR_TAB_FORWARD);
  Field(res, 1) = Val_int(GTK_DIR_TAB_BACKWARD);
  Field(res, 2) = Val_int(GTK_DIR_UP);
  Field(res, 3) = Val_int(GTK_DIR_DOWN);
  Field(res, 4) = Val_int(GTK_DIR_LEFT);
  Field(res, 5) = Val_int(GTK_DIR_RIGHT);
  return res;
}

/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_expander_style(value dummy) { /* ML */
  value res = alloc_tuple(4);
  Field(res, 0) = Val_int(GTK_EXPANDER_COLLAPSED);
  Field(res, 1) = Val_int(GTK_EXPANDER_SEMI_COLLAPSED);
  Field(res, 2) = Val_int(GTK_EXPANDER_SEMI_EXPANDED);
  Field(res, 3) = Val_int(GTK_EXPANDER_EXPANDED);
  return res;
}

/* ML type: unit -> int * int * int * int * int * int * int */
EXTERNML value mgtk_get_gtk_icon_size(value dummy) { /* ML */
  value res = alloc_tuple(7);
  Field(res, 0) = Val_int(GTK_ICON_SIZE_INVALID);
  Field(res, 1) = Val_int(GTK_ICON_SIZE_MENU);
  Field(res, 2) = Val_int(GTK_ICON_SIZE_SMALL_TOOLBAR);
  Field(res, 3) = Val_int(GTK_ICON_SIZE_LARGE_TOOLBAR);
  Field(res, 4) = Val_int(GTK_ICON_SIZE_BUTTON);
  Field(res, 5) = Val_int(GTK_ICON_SIZE_DND);
  Field(res, 6) = Val_int(GTK_ICON_SIZE_DIALOG);
  return res;
}

/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_sidetype(value dummy) { /* ML */
  value res = alloc_tuple(4);
  Field(res, 0) = Val_int(GTK_SIDE_TOP);
  Field(res, 1) = Val_int(GTK_SIDE_BOTTOM);
  Field(res, 2) = Val_int(GTK_SIDE_LEFT);
  Field(res, 3) = Val_int(GTK_SIDE_RIGHT);
  return res;
}

/* ML type: unit -> int * int * int */
EXTERNML value mgtk_get_gtk_text_direction(value dummy) { /* ML */
  value res = alloc_tuple(3);
  Field(res, 0) = Val_int(GTK_TEXT_DIR_NONE);
  Field(res, 1) = Val_int(GTK_TEXT_DIR_LTR);
  Field(res, 2) = Val_int(GTK_TEXT_DIR_RTL);
  return res;
}

/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_justification(value dummy) { /* ML */
  value res = alloc_tuple(4);
  Field(res, 0) = Val_int(GTK_JUSTIFY_LEFT);
  Field(res, 1) = Val_int(GTK_JUSTIFY_RIGHT);
  Field(res, 2) = Val_int(GTK_JUSTIFY_CENTER);
  Field(res, 3) = Val_int(GTK_JUSTIFY_FILL);
  return res;
}

/* ML type: unit -> int * int * int * int * int * int */
EXTERNML value mgtk_get_gtk_matchtype(value dummy) { /* ML */
  value res = alloc_tuple(6);
  Field(res, 0) = Val_int(GTK_MATCH_ALL);
  Field(res, 1) = Val_int(GTK_MATCH_ALL_TAIL);
  Field(res, 2) = Val_int(GTK_MATCH_HEAD);
  Field(res, 3) = Val_int(GTK_MATCH_TAIL);
  Field(res, 4) = Val_int(GTK_MATCH_EXACT);
  Field(res, 5) = Val_int(GTK_MATCH_LAST);
  return res;
}

/* ML type: unit -> int * int * int */
EXTERNML value mgtk_get_gtk_metrictype(value dummy) { /* ML */
  value res = alloc_tuple(3);
  Field(res, 0) = Val_int(GTK_PIXELS);
  Field(res, 1) = Val_int(GTK_INCHES);
  Field(res, 2) = Val_int(GTK_CENTIMETERS);
  return res;
}

/* ML type: unit -> int * int * int * int * int * int * int * int * int */
EXTERNML value mgtk_get_gtk_movement_step(value dummy) { /* ML */
  value res = alloc_tuple(9);
  Field(res, 0) = Val_int(GTK_MOVEMENT_LOGICAL_POSITIONS);
  Field(res, 1) = Val_int(GTK_MOVEMENT_VISUAL_POSITIONS);
  Field(res, 2) = Val_int(GTK_MOVEMENT_WORDS);
  Field(res, 3) = Val_int(GTK_MOVEMENT_DISPLAY_LINES);
  Field(res, 4) = Val_int(GTK_MOVEMENT_DISPLAY_LINE_ENDS);
  Field(res, 5) = Val_int(GTK_MOVEMENT_PARAGRAPHS);
  Field(res, 6) = Val_int(GTK_MOVEMENT_PARAGRAPH_ENDS);
  Field(res, 7) = Val_int(GTK_MOVEMENT_PAGES);
  Field(res, 8) = Val_int(GTK_MOVEMENT_BUFFER_ENDS);
  return res;
}

/* ML type: unit -> int * int */
EXTERNML value mgtk_get_gtk_orientation(value dummy) { /* ML */
  value res = alloc_tuple(2);
  Field(res, 0) = Val_int(GTK_ORIENTATION_HORIZONTAL);
  Field(res, 1) = Val_int(GTK_ORIENTATION_VERTICAL);
  return res;
}

/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_cornertype(value dummy) { /* ML */
  value res = alloc_tuple(4);
  Field(res, 0) = Val_int(GTK_CORNER_TOP_LEFT);
  Field(res, 1) = Val_int(GTK_CORNER_BOTTOM_LEFT);
  Field(res, 2) = Val_int(GTK_CORNER_TOP_RIGHT);
  Field(res, 3) = Val_int(GTK_CORNER_BOTTOM_RIGHT);
  return res;
}

/* ML type: unit -> int * int */
EXTERNML value mgtk_get_gtk_packtype(value dummy) { /* ML */
  value res = alloc_tuple(2);
  Field(res, 0) = Val_int(GTK_PACK_START);
  Field(res, 1) = Val_int(GTK_PACK_END);
  return res;
}

/* ML type: unit -> int * int * int * int * int * int */
EXTERNML value mgtk_get_gtk_path_prioritytype(value dummy) { /* ML */
  value res = alloc_tuple(6);
  Field(res, 0) = Val_int(GTK_PATH_PRIO_LOWEST);
  Field(res, 1) = Val_int(GTK_PATH_PRIO_GTK);
  Field(res, 2) = Val_int(GTK_PATH_PRIO_APPLICATION);
  Field(res, 3) = Val_int(GTK_PATH_PRIO_THEME);
  Field(res, 4) = Val_int(GTK_PATH_PRIO_RC);
  Field(res, 5) = Val_int(GTK_PATH_PRIO_HIGHEST);
  return res;
}

/* ML type: unit -> int * int * int */
EXTERNML value mgtk_get_gtk_pathtype(value dummy) { /* ML */
  value res = alloc_tuple(3);
  Field(res, 0) = Val_int(GTK_PATH_WIDGET);
  Field(res, 1) = Val_int(GTK_PATH_WIDGET_CLASS);
  Field(res, 2) = Val_int(GTK_PATH_CLASS);
  return res;
}

/* ML type: unit -> int * int * int */
EXTERNML value mgtk_get_gtk_policytype(value dummy) { /* ML */
  value res = alloc_tuple(3);
  Field(res, 0) = Val_int(GTK_POLICY_ALWAYS);
  Field(res, 1) = Val_int(GTK_POLICY_AUTOMATIC);
  Field(res, 2) = Val_int(GTK_POLICY_NEVER);
  return res;
}

/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_positiontype(value dummy) { /* ML */
  value res = alloc_tuple(4);
  Field(res, 0) = Val_int(GTK_POS_LEFT);
  Field(res, 1) = Val_int(GTK_POS_RIGHT);
  Field(res, 2) = Val_int(GTK_POS_TOP);
  Field(res, 3) = Val_int(GTK_POS_BOTTOM);
  return res;
}

/* ML type: unit -> int * int */
EXTERNML value mgtk_get_gtk_previewtype(value dummy) { /* ML */
  value res = alloc_tuple(2);
  Field(res, 0) = Val_int(GTK_PREVIEW_COLOR);
  Field(res, 1) = Val_int(GTK_PREVIEW_GRAYSCALE);
  return res;
}

/* ML type: unit -> int * int * int */
EXTERNML value mgtk_get_gtk_relief_style(value dummy) { /* ML */
  value res = alloc_tuple(3);
  Field(res, 0) = Val_int(GTK_RELIEF_NORMAL);
  Field(res, 1) = Val_int(GTK_RELIEF_HALF);
  Field(res, 2) = Val_int(GTK_RELIEF_NONE);
  return res;
}

/* ML type: unit -> int * int * int */
EXTERNML value mgtk_get_gtk_resize_mode(value dummy) { /* ML */
  value res = alloc_tuple(3);
  Field(res, 0) = Val_int(GTK_RESIZE_PARENT);
  Field(res, 1) = Val_int(GTK_RESIZE_QUEUE);
  Field(res, 2) = Val_int(GTK_RESIZE_IMMEDIATE);
  return res;
}

/* ML type: unit -> int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int */
EXTERNML value mgtk_get_gtk_scrolltype(value dummy) { /* ML */
  value res = alloc_tuple(16);
  Field(res, 0) = Val_int(GTK_SCROLL_NONE);
  Field(res, 1) = Val_int(GTK_SCROLL_JUMP);
  Field(res, 2) = Val_int(GTK_SCROLL_STEP_BACKWARD);
  Field(res, 3) = Val_int(GTK_SCROLL_STEP_FORWARD);
  Field(res, 4) = Val_int(GTK_SCROLL_PAGE_BACKWARD);
  Field(res, 5) = Val_int(GTK_SCROLL_PAGE_FORWARD);
  Field(res, 6) = Val_int(GTK_SCROLL_STEP_UP);
  Field(res, 7) = Val_int(GTK_SCROLL_STEP_DOWN);
  Field(res, 8) = Val_int(GTK_SCROLL_PAGE_UP);
  Field(res, 9) = Val_int(GTK_SCROLL_PAGE_DOWN);
  Field(res, 10) = Val_int(GTK_SCROLL_STEP_LEFT);
  Field(res, 11) = Val_int(GTK_SCROLL_STEP_RIGHT);
  Field(res, 12) = Val_int(GTK_SCROLL_PAGE_LEFT);
  Field(res, 13) = Val_int(GTK_SCROLL_PAGE_RIGHT);
  Field(res, 14) = Val_int(GTK_SCROLL_START);
  Field(res, 15) = Val_int(GTK_SCROLL_END);
  return res;
}

/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_selection_mode(value dummy) { /* ML */
  value res = alloc_tuple(4);
  Field(res, 0) = Val_int(GTK_SELECTION_NONE);
  Field(res, 1) = Val_int(GTK_SELECTION_SINGLE);
  Field(res, 2) = Val_int(GTK_SELECTION_BROWSE);
  Field(res, 3) = Val_int(GTK_SELECTION_MULTIPLE);
  return res;
}

/* ML type: unit -> int * int * int * int * int */
EXTERNML value mgtk_get_gtk_shadowtype(value dummy) { /* ML */
  value res = alloc_tuple(5);
  Field(res, 0) = Val_int(GTK_SHADOW_NONE);
  Field(res, 1) = Val_int(GTK_SHADOW_IN);
  Field(res, 2) = Val_int(GTK_SHADOW_OUT);
  Field(res, 3) = Val_int(GTK_SHADOW_ETCHED_IN);
  Field(res, 4) = Val_int(GTK_SHADOW_ETCHED_OUT);
  return res;
}

/* ML type: unit -> int * int * int * int * int */
EXTERNML value mgtk_get_gtk_statetype(value dummy) { /* ML */
  value res = alloc_tuple(5);
  Field(res, 0) = Val_int(GTK_STATE_NORMAL);
  Field(res, 1) = Val_int(GTK_STATE_ACTIVE);
  Field(res, 2) = Val_int(GTK_STATE_PRELIGHT);
  Field(res, 3) = Val_int(GTK_STATE_SELECTED);
  Field(res, 4) = Val_int(GTK_STATE_INSENSITIVE);
  return res;
}

/* ML type: unit -> int * int */
EXTERNML value mgtk_get_gtk_submenu_direction(value dummy) { /* ML */
  value res = alloc_tuple(2);
  Field(res, 0) = Val_int(GTK_DIRECTION_LEFT);
  Field(res, 1) = Val_int(GTK_DIRECTION_RIGHT);
  return res;
}

/* ML type: unit -> int * int */
EXTERNML value mgtk_get_gtk_submenu_placement(value dummy) { /* ML */
  value res = alloc_tuple(2);
  Field(res, 0) = Val_int(GTK_TOP_BOTTOM);
  Field(res, 1) = Val_int(GTK_LEFT_RIGHT);
  return res;
}

/* ML type: unit -> int * int * int */
EXTERNML value mgtk_get_gtk_updatetype(value dummy) { /* ML */
  value res = alloc_tuple(3);
  Field(res, 0) = Val_int(GTK_UPDATE_CONTINUOUS);
  Field(res, 1) = Val_int(GTK_UPDATE_DISCONTINUOUS);
  Field(res, 2) = Val_int(GTK_UPDATE_DELAYED);
  return res;
}

/* ML type: unit -> int * int * int */
EXTERNML value mgtk_get_gtk_visibility(value dummy) { /* ML */
  value res = alloc_tuple(3);
  Field(res, 0) = Val_int(GTK_VISIBILITY_NONE);
  Field(res, 1) = Val_int(GTK_VISIBILITY_PARTIAL);
  Field(res, 2) = Val_int(GTK_VISIBILITY_FULL);
  return res;
}

/* ML type: unit -> int * int * int */
EXTERNML value mgtk_get_gtk_wrap_mode(value dummy) { /* ML */
  value res = alloc_tuple(3);
  Field(res, 0) = Val_int(GTK_WRAP_NONE);
  Field(res, 1) = Val_int(GTK_WRAP_CHAR);
  Field(res, 2) = Val_int(GTK_WRAP_WORD);
  return res;
}

/* ML type: unit -> int * int */
EXTERNML value mgtk_get_gtk_sorttype(value dummy) { /* ML */
  value res = alloc_tuple(2);
  Field(res, 0) = Val_int(GTK_SORT_ASCENDING);
  Field(res, 1) = Val_int(GTK_SORT_DESCENDING);
  return res;
}

/* ML type: unit -> int * int * int * int * int * int * int */
EXTERNML value mgtk_get_gtk_imagetype(value dummy) { /* ML */
  value res = alloc_tuple(7);
  Field(res, 0) = Val_int(GTK_IMAGE_EMPTY);
  Field(res, 1) = Val_int(GTK_IMAGE_PIXMAP);
  Field(res, 2) = Val_int(GTK_IMAGE_IMAGE);
  Field(res, 3) = Val_int(GTK_IMAGE_PIXBUF);
  Field(res, 4) = Val_int(GTK_IMAGE_STOCK);
  Field(res, 5) = Val_int(GTK_IMAGE_ICON_SET);
  Field(res, 6) = Val_int(GTK_IMAGE_ANIMATION);
  return res;
}

/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_messagetype(value dummy) { /* ML */
  value res = alloc_tuple(4);
  Field(res, 0) = Val_int(GTK_MESSAGE_INFO);
  Field(res, 1) = Val_int(GTK_MESSAGE_WARNING);
  Field(res, 2) = Val_int(GTK_MESSAGE_QUESTION);
  Field(res, 3) = Val_int(GTK_MESSAGE_ERROR);
  return res;
}

/* ML type: unit -> int * int * int * int * int * int */
EXTERNML value mgtk_get_gtk_buttonstype(value dummy) { /* ML */
  value res = alloc_tuple(6);
  Field(res, 0) = Val_int(GTK_BUTTONS_NONE);
  Field(res, 1) = Val_int(GTK_BUTTONS_OK);
  Field(res, 2) = Val_int(GTK_BUTTONS_CLOSE);
  Field(res, 3) = Val_int(GTK_BUTTONS_CANCEL);
  Field(res, 4) = Val_int(GTK_BUTTONS_YES_NO);
  Field(res, 5) = Val_int(GTK_BUTTONS_OK_CANCEL);
  return res;
}

/* ML type: unit -> int * int * int * int * int */
EXTERNML value mgtk_get_gtk_arg_flags(value dummy) { /* ML */
  value res = alloc_tuple(5);
  Field(res, 0) = Val_int(GTK_ARG_READABLE);
  Field(res, 1) = Val_int(GTK_ARG_WRITABLE);
  Field(res, 2) = Val_int(GTK_ARG_CONSTRUCT);
  Field(res, 3) = Val_int(GTK_ARG_CONSTRUCT_ONLY);
  Field(res, 4) = Val_int(GTK_ARG_CHILD_ARG);
  return res;
}

/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_rc_flags(value dummy) { /* ML */
  value res = alloc_tuple(4);
  Field(res, 0) = Val_int(GTK_RC_FG);
  Field(res, 1) = Val_int(GTK_RC_BG);
  Field(res, 2) = Val_int(GTK_RC_TEXT);
  Field(res, 3) = Val_int(GTK_RC_BASE);
  return res;
}

/* ML type: unit -> int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int */
EXTERNML value mgtk_get_gtk_rc_tokentype(value dummy) { /* ML */
  value res = alloc_tuple(38);
  Field(res, 0) = Val_int(GTK_RC_TOKEN_INVALID);
  Field(res, 1) = Val_int(GTK_RC_TOKEN_INCLUDE);
  Field(res, 2) = Val_int(GTK_RC_TOKEN_NORMAL);
  Field(res, 3) = Val_int(GTK_RC_TOKEN_ACTIVE);
  Field(res, 4) = Val_int(GTK_RC_TOKEN_PRELIGHT);
  Field(res, 5) = Val_int(GTK_RC_TOKEN_SELECTED);
  Field(res, 6) = Val_int(GTK_RC_TOKEN_INSENSITIVE);
  Field(res, 7) = Val_int(GTK_RC_TOKEN_FG);
  Field(res, 8) = Val_int(GTK_RC_TOKEN_BG);
  Field(res, 9) = Val_int(GTK_RC_TOKEN_TEXT);
  Field(res, 10) = Val_int(GTK_RC_TOKEN_BASE);
  Field(res, 11) = Val_int(GTK_RC_TOKEN_XTHICKNESS);
  Field(res, 12) = Val_int(GTK_RC_TOKEN_YTHICKNESS);
  Field(res, 13) = Val_int(GTK_RC_TOKEN_FONT);
  Field(res, 14) = Val_int(GTK_RC_TOKEN_FONTSET);
  Field(res, 15) = Val_int(GTK_RC_TOKEN_FONT_NAME);
  Field(res, 16) = Val_int(GTK_RC_TOKEN_BG_PIXMAP);
  Field(res, 17) = Val_int(GTK_RC_TOKEN_PIXMAP_PATH);
  Field(res, 18) = Val_int(GTK_RC_TOKEN_STYLE);
  Field(res, 19) = Val_int(GTK_RC_TOKEN_BINDING);
  Field(res, 20) = Val_int(GTK_RC_TOKEN_BIND);
  Field(res, 21) = Val_int(GTK_RC_TOKEN_WIDGET);
  Field(res, 22) = Val_int(GTK_RC_TOKEN_WIDGET_CLASS);
  Field(res, 23) = Val_int(GTK_RC_TOKEN_CLASS);
  Field(res, 24) = Val_int(GTK_RC_TOKEN_LOWEST);
  Field(res, 25) = Val_int(GTK_RC_TOKEN_GTK);
  Field(res, 26) = Val_int(GTK_RC_TOKEN_APPLICATION);
  Field(res, 27) = Val_int(GTK_RC_TOKEN_THEME);
  Field(res, 28) = Val_int(GTK_RC_TOKEN_RC);
  Field(res, 29) = Val_int(GTK_RC_TOKEN_HIGHEST);
  Field(res, 30) = Val_int(GTK_RC_TOKEN_ENGINE);
  Field(res, 31) = Val_int(GTK_RC_TOKEN_MODULE_PATH);
  Field(res, 32) = Val_int(GTK_RC_TOKEN_IM_MODULE_PATH);
  Field(res, 33) = Val_int(GTK_RC_TOKEN_IM_MODULE_FILE);
  Field(res, 34) = Val_int(GTK_RC_TOKEN_STOCK);
  Field(res, 35) = Val_int(GTK_RC_TOKEN_LTR);
  Field(res, 36) = Val_int(GTK_RC_TOKEN_RTL);
  Field(res, 37) = Val_int(GTK_RC_TOKEN_LAST);
  return res;
}

/* ML type: unit -> int * int * int * int * int * int * int */
EXTERNML value mgtk_get_gtk_spintype(value dummy) { /* ML */
  value res = alloc_tuple(7);
  Field(res, 0) = Val_int(GTK_SPIN_STEP_FORWARD);
  Field(res, 1) = Val_int(GTK_SPIN_STEP_BACKWARD);
  Field(res, 2) = Val_int(GTK_SPIN_PAGE_FORWARD);
  Field(res, 3) = Val_int(GTK_SPIN_PAGE_BACKWARD);
  Field(res, 4) = Val_int(GTK_SPIN_HOME);
  Field(res, 5) = Val_int(GTK_SPIN_END);
  Field(res, 6) = Val_int(GTK_SPIN_USER_DEFINED);
  return res;
}

/* ML type: unit -> int * int */
EXTERNML value mgtk_get_gtk_text_search_flags(value dummy) { /* ML */
  value res = alloc_tuple(2);
  Field(res, 0) = Val_int(GTK_TEXT_SEARCH_VISIBLE_ONLY);
  Field(res, 1) = Val_int(GTK_TEXT_SEARCH_TEXT_ONLY);
  return res;
}

/* ML type: unit -> int * int * int * int * int * int * int */
EXTERNML value mgtk_get_gtk_text_window_type(value dummy) { /* ML */
  value res = alloc_tuple(7);
  Field(res, 0) = Val_int(GTK_TEXT_WINDOW_PRIVATE);
  Field(res, 1) = Val_int(GTK_TEXT_WINDOW_WIDGET);
  Field(res, 2) = Val_int(GTK_TEXT_WINDOW_TEXT);
  Field(res, 3) = Val_int(GTK_TEXT_WINDOW_LEFT);
  Field(res, 4) = Val_int(GTK_TEXT_WINDOW_RIGHT);
  Field(res, 5) = Val_int(GTK_TEXT_WINDOW_TOP);
  Field(res, 6) = Val_int(GTK_TEXT_WINDOW_BOTTOM);
  return res;
}

#define GtkTreePath_val(x) (((void*) Field(x, 1)))

#define GtkTreePath_val_nocast(x) (Field(x, 1))

static void ml_finalize_GtkTreePath(value val) { /* Empty */
}

value Val_GtkTreePath(void* obj) {
  value res;
  res = alloc_final(2, ml_finalize_GtkTreePath, 0, 1);
  GtkTreePath_val_nocast(res) = (value) obj;
  return res;
}

#define GtkFunction_val(x) (((void*) Field(x, 1)))

#define GtkFunction_val_nocast(x) (Field(x, 1))

static void ml_finalize_GtkFunction(value val) { /* Empty */
}

value Val_GtkFunction(void* obj) {
  value res;
  res = alloc_final(2, ml_finalize_GtkFunction, 0, 1);
  GtkFunction_val_nocast(res) = (value) obj;
  return res;
}

/* ML type: unit -> int */
EXTERNML value mgtk_gtk_accelerator_get_default_mod_mask(value dummy) { /* ML */
  return Val_int(gtk_accelerator_get_default_mod_mask());
}

/* ML type: string -> unit */
EXTERNML value mgtk_gtk_accel_map_load(value file_name) { /* ML */
  gtk_accel_map_load(String_val(file_name));
  return Val_unit;
}

/* ML type: string -> unit */
EXTERNML value mgtk_gtk_accel_map_save(value file_name) { /* ML */
  gtk_accel_map_save(String_val(file_name));
  return Val_unit;
}

/* ML type: int -> unit */
EXTERNML value mgtk_gtk_accel_map_load_fd(value fd) { /* ML */
  gtk_accel_map_load_fd(Int_val(fd));
  return Val_unit;
}

/* ML type: int -> unit */
EXTERNML value mgtk_gtk_accel_map_save_fd(value fd) { /* ML */
  gtk_accel_map_save_fd(Int_val(fd));
  return Val_unit;
}

/* ML type: string -> unit */
EXTERNML value mgtk_gtk_accel_map_add_filter(value filter_pattern) { /* ML */
  gtk_accel_map_add_filter(String_val(filter_pattern));
  return Val_unit;
}

/* ML type: int -> bool */
EXTERNML value mgtk_gtk_icon_size_lookup(value size) { /* ML */
  value result;
  value res;
  int width;
  int height;
  res = Val_bool(gtk_icon_size_lookup(Int_val(size), &width, &height));
  result = alloc_tuple(3);
  Field(result, 0) = res;
  Field(result, 1) = Val_int(width);
  Field(result, 2) = Val_int(height);
  return result;
}

/* ML type: string -> int -> int -> int */
EXTERNML value mgtk_gtk_icon_size_register(value name, value width, value height) { /* ML */
  return Val_int(gtk_icon_size_register(String_val(name), Int_val(width), Int_val(height)));
}

/* ML type: string -> int -> unit */
EXTERNML value mgtk_gtk_icon_size_register_alias(value alias, value target) { /* ML */
  gtk_icon_size_register_alias(String_val(alias), Int_val(target));
  return Val_unit;
}

/* ML type: string -> int */
EXTERNML value mgtk_gtk_icon_size_from_name(value name) { /* ML */
  return Val_int(gtk_icon_size_from_name(String_val(name)));
}

/* ML type: int -> string */
EXTERNML value mgtk_gtk_icon_size_get_name(value size) { /* ML */
  return my_copy_string(gtk_icon_size_get_name(Int_val(size)));
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_icon_set_new(value dummy) { /* ML */
  return Val_GtkIconSet(gtk_icon_set_new());
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_icon_source_get_type(value dummy) { /* ML */
  return Val_int(gtk_icon_source_get_type());
}

/* ML type: int -> int -> int -> string */
EXTERNML value mgtk_gtk_check_version(value required_major, value required_minor, value required_micro) { /* ML */
  return my_copy_string(gtk_check_version(Int_val(required_major), Int_val(required_minor), Int_val(required_micro)));
}

/* ML type: int -> unit */
EXTERNML value mgtk_gtk_exit(value error_code) { /* ML */
  gtk_exit(Int_val(error_code));
  return Val_unit;
}

/* ML type: unit -> unit */
EXTERNML value mgtk_gtk_disable_setlocale(value dummy) { /* ML */
  gtk_disable_setlocale();
  return Val_unit;
}

/* ML type: unit -> string */
EXTERNML value mgtk_gtk_set_locale(value dummy) { /* ML */
  return my_copy_string(gtk_set_locale());
}

/* ML type: unit -> int */
EXTERNML value mgtk_gtk_events_pending(value dummy) { /* ML */
  return Val_int(gtk_events_pending());
}

/* ML type: unit -> unit */
EXTERNML value mgtk_gtk_main(value dummy) { /* ML */
  gtk_main();
  return Val_unit;
}

/* ML type: unit -> int */
EXTERNML value mgtk_gtk_main_level(value dummy) { /* ML */
  return Val_int(gtk_main_level());
}

/* ML type: unit -> unit */
EXTERNML value mgtk_gtk_main_quit(value dummy) { /* ML */
  gtk_main_quit();
  return Val_unit;
}

/* ML type: unit -> bool */
EXTERNML value mgtk_gtk_main_iteration(value dummy) { /* ML */
  return Val_bool(gtk_main_iteration());
}

/* ML type: bool -> bool */
EXTERNML value mgtk_gtk_main_iteration_do(value blocking) { /* ML */
  return Val_bool(gtk_main_iteration_do(Bool_val(blocking)));
}

/* ML type: string -> unit */
EXTERNML value mgtk_gtk_rc_add_default_file(value filename) { /* ML */
  gtk_rc_add_default_file(String_val(filename));
  return Val_unit;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_border_get_type(value dummy) { /* ML */
  return Val_int(gtk_border_get_type());
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_tips_query_get_type(value dummy) { /* ML */
  return Val_int(gtk_tips_query_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_tree_path_new(value dummy) { /* ML */
  return Val_GtkTreePath(gtk_tree_path_new());
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_tree_path_new_from_string(value path) { /* ML */
  return Val_GtkTreePath(gtk_tree_path_new_from_string(String_val(path)));
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_requisition_get_type(value dummy) { /* ML */
  return Val_int(gtk_requisition_get_type());
}



/* *** AccelGroup *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_accel_group_get_type(value dummy) { /* ML */
    return Val_int(gtk_accel_group_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_accel_group_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_accel_group_new());
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_accel_group_lock(value self) { /* ML */
    gtk_accel_group_lock(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_accel_group_unlock(value self) { /* ML */
    gtk_accel_group_unlock(GtkObj_val(self));
    return Val_unit;
}



/* *** IconFactory *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_icon_factory_get_type(value dummy) { /* ML */
    return Val_int(gtk_icon_factory_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_icon_factory_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_icon_factory_new());
}

/* ML type: cptr -> string -> cptr -> unit */
EXTERNML value mgtk_gtk_icon_factory_add(value self, value stock_id, value icon_set) { /* ML */
    gtk_icon_factory_add(GtkObj_val(self), String_val(stock_id), GtkIconSet_val(icon_set));
    return Val_unit;
}

/* ML type: cptr -> string -> cptr */
EXTERNML value mgtk_gtk_icon_factory_lookup(value self, value stock_id) { /* ML */
    return Val_GtkIconSet(gtk_icon_factory_lookup(GtkObj_val(self), String_val(stock_id)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_icon_factory_add_default(value self) { /* ML */
    gtk_icon_factory_add_default(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_icon_factory_remove_default(value self) { /* ML */
    gtk_icon_factory_remove_default(GtkObj_val(self));
    return Val_unit;
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_icon_factory_lookup_default(value stock_id) { /* ML */
    return Val_GtkIconSet(gtk_icon_factory_lookup_default(String_val(stock_id)));
}



/* *** Object *** */
/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_object_flags(value dummy) { /* ML */
    value res = alloc_tuple(4);
    Field(res, 0) = Val_int(GTK_IN_DESTRUCTION);
    Field(res, 1) = Val_int(GTK_FLOATING);
    Field(res, 2) = Val_int(GTK_RESERVED_1);
    Field(res, 3) = Val_int(GTK_RESERVED_2);
    return res;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_object_get_type(value dummy) { /* ML */
    return Val_int(gtk_object_get_type());
}

/* ML type: GType.t -> string -> cptr */
EXTERNML value mgtk_gtk_object_new(value type, value first_property_name) { /* ML */
    return Val_GtkObj(gtk_object_new(Int_val(type), String_val(first_property_name)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_object_sink(value self) { /* ML */
    gtk_object_sink(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_object_destroy(value self) { /* ML */
    gtk_object_destroy(GtkObj_val(self));
    return Val_unit;
}



/* *** Adjustment *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_adjustment_get_type(value dummy) { /* ML */
    return Val_int(gtk_adjustment_get_type());
}

/* ML type: int -> cptr */
EXTERNML value mgtk_gtk_adjustment_new(value mgtk_params) { /* ML */
    value valu = Field(mgtk_params, 0);
    value lower = Field(mgtk_params, 1);
    value upper = Field(mgtk_params, 2);
    value step_incr = Field(mgtk_params, 3);
    value page_incr = Field(mgtk_params, 4);
    value page_size = Field(mgtk_params, 5);
    return Val_GtkObj(gtk_adjustment_new(Double_val(valu), Double_val(lower), Double_val(upper), Double_val(step_incr), Double_val(page_incr), Double_val(page_size)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_adjustment_changed(value self) { /* ML */
    gtk_adjustment_changed(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_adjustment_value_changed(value self) { /* ML */
    gtk_adjustment_value_changed(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> real -> real -> unit */
EXTERNML value mgtk_gtk_adjustment_clamp_page(value self, value lower, value upper) { /* ML */
    gtk_adjustment_clamp_page(GtkObj_val(self), Double_val(lower), Double_val(upper));
    return Val_unit;
}

/* ML type: cptr -> real */
EXTERNML value mgtk_gtk_adjustment_get_value(value self) { /* ML */
    return copy_double(gtk_adjustment_get_value(GtkObj_val(self)));
}

/* ML type: cptr -> real -> unit */
EXTERNML value mgtk_gtk_adjustment_set_value(value self, value valu) { /* ML */
    gtk_adjustment_set_value(GtkObj_val(self), Double_val(valu));
    return Val_unit;
}



/* *** Widget *** */
/* ML type: unit -> int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int * int */
EXTERNML value mgtk_get_gtk_widget_flags(value dummy) { /* ML */
    value res = alloc_tuple(18);
    Field(res, 0) = Val_int(GTK_TOPLEVEL);
    Field(res, 1) = Val_int(GTK_NO_WINDOW);
    Field(res, 2) = Val_int(GTK_REALIZED);
    Field(res, 3) = Val_int(GTK_MAPPED);
    Field(res, 4) = Val_int(GTK_VISIBLE);
    Field(res, 5) = Val_int(GTK_SENSITIVE);
    Field(res, 6) = Val_int(GTK_PARENT_SENSITIVE);
    Field(res, 7) = Val_int(GTK_CAN_FOCUS);
    Field(res, 8) = Val_int(GTK_HAS_FOCUS);
    Field(res, 9) = Val_int(GTK_CAN_DEFAULT);
    Field(res, 10) = Val_int(GTK_HAS_DEFAULT);
    Field(res, 11) = Val_int(GTK_HAS_GRAB);
    Field(res, 12) = Val_int(GTK_RC_STYLE);
    Field(res, 13) = Val_int(GTK_COMPOSITE_CHILD);
    Field(res, 14) = Val_int(GTK_NO_REPARENT);
    Field(res, 15) = Val_int(GTK_APP_PAINTABLE);
    Field(res, 16) = Val_int(GTK_RECEIVES_DEFAULT);
    Field(res, 17) = Val_int(GTK_DOUBLE_BUFFERED);
    return res;
}

/* ML type: unit -> int * int */
EXTERNML value mgtk_get_gtk_widget_helptype(value dummy) { /* ML */
    value res = alloc_tuple(2);
    Field(res, 0) = Val_int(GTK_WIDGET_HELP_TOOLTIP);
    Field(res, 1) = Val_int(GTK_WIDGET_HELP_WHATS_THIS);
    return res;
}

/* ML type: cptr -> int -> int -> int -> int -> bool */
EXTERNML value mgtk_gtk_drag_check_threshold(value self, value start_x, value start_y, value current_x, value current_y) { /* ML */
    return Val_bool(gtk_drag_check_threshold(GtkObj_val(self), Int_val(start_x), Int_val(start_y), Int_val(current_x), Int_val(current_y)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_drag_highlight(value self) { /* ML */
    gtk_drag_highlight(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_drag_unhighlight(value self) { /* ML */
    gtk_drag_unhighlight(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_drag_dest_unset(value self) { /* ML */
    gtk_drag_dest_unset(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_drag_source_unset(value self) { /* ML */
    gtk_drag_source_unset(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_drag_source_set_icon_stock(value self, value stock_id) { /* ML */
    gtk_drag_source_set_icon_stock(GtkObj_val(self), String_val(stock_id));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_grab_add(value self) { /* ML */
    gtk_grab_add(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_grab_remove(value self) { /* ML */
    gtk_grab_remove(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_rc_get_style(value self) { /* ML */
    return Val_GtkObj(gtk_rc_get_style(GtkObj_val(self)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_selection_remove_all(value self) { /* ML */
    gtk_selection_remove_all(GtkObj_val(self));
    return Val_unit;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_widget_get_type(value dummy) { /* ML */
    return Val_int(gtk_widget_get_type());
}

/* ML type: GType.t -> string -> cptr */
EXTERNML value mgtk_gtk_widget_new(value type, value first_property_name) { /* ML */
    return Val_GtkObj(gtk_widget_new(Int_val(type), String_val(first_property_name)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_widget_ref(value self) { /* ML */
    return Val_GtkObj(gtk_widget_ref(GtkObj_val(self)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_unref(value self) { /* ML */
    gtk_widget_unref(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_destroy(value self) { /* ML */
    gtk_widget_destroy(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_widget_destroyed(value self, value widget_pointer) { /* ML */
    gtk_widget_destroyed(GtkObj_val(self), GtkObj_val(widget_pointer));
    return Val_unit;
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_widget_set(value self, value first_property_name) { /* ML */
    gtk_widget_set(GtkObj_val(self), String_val(first_property_name));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_unparent(value self) { /* ML */
    gtk_widget_unparent(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_show(value self) { /* ML */
    gtk_widget_show(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_show_now(value self) { /* ML */
    gtk_widget_show_now(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_hide(value self) { /* ML */
    gtk_widget_hide(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_show_all(value self) { /* ML */
    gtk_widget_show_all(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_hide_all(value self) { /* ML */
    gtk_widget_hide_all(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_map(value self) { /* ML */
    gtk_widget_map(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_unmap(value self) { /* ML */
    gtk_widget_unmap(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_realize(value self) { /* ML */
    gtk_widget_realize(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_unrealize(value self) { /* ML */
    gtk_widget_unrealize(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_queue_draw(value self) { /* ML */
    gtk_widget_queue_draw(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> int -> int -> int -> int -> unit */
EXTERNML value mgtk_gtk_widget_queue_draw_area(value self, value x, value y, value width, value height) { /* ML */
    gtk_widget_queue_draw_area(GtkObj_val(self), Int_val(x), Int_val(y), Int_val(width), Int_val(height));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_queue_clear(value self) { /* ML */
    gtk_widget_queue_clear(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> int -> int -> int -> int -> unit */
EXTERNML value mgtk_gtk_widget_queue_clear_area(value self, value x, value y, value width, value height) { /* ML */
    gtk_widget_queue_clear_area(GtkObj_val(self), Int_val(x), Int_val(y), Int_val(width), Int_val(height));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_queue_resize(value self) { /* ML */
    gtk_widget_queue_resize(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_widget_get_child_requisition(value self, value requisition) { /* ML */
    gtk_widget_get_child_requisition(GtkObj_val(self), GtkRequisition_val(requisition));
    return Val_unit;
}

/* ML type: cptr -> string -> cptr -> unit */
EXTERNML value mgtk_gtk_widget_set_accel_path(value self, value accel_path, value accel_group) { /* ML */
    gtk_widget_set_accel_path(GtkObj_val(self), String_val(accel_path), GtkObj_val(accel_group));
    return Val_unit;
}

/* ML type: cptr -> bool -> bool */
EXTERNML value mgtk_gtk_widget_mnemonic_activate(value self, value group_cycling) { /* ML */
    return Val_bool(gtk_widget_mnemonic_activate(GtkObj_val(self), Bool_val(group_cycling)));
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_widget_activate(value self) { /* ML */
    return Val_bool(gtk_widget_activate(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_widget_set_scroll_adjustments(value self, value hadjustment, value vadjustment) { /* ML */
    return Val_bool(gtk_widget_set_scroll_adjustments(GtkObj_val(self), GtkObj_val(hadjustment), GtkObj_val(vadjustment)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_widget_reparent(value self, value new_parent) { /* ML */
    gtk_widget_reparent(GtkObj_val(self), GtkObj_val(new_parent));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_freeze_child_notify(value self) { /* ML */
    gtk_widget_freeze_child_notify(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_widget_child_notify(value self, value child_property) { /* ML */
    gtk_widget_child_notify(GtkObj_val(self), String_val(child_property));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_thaw_child_notify(value self) { /* ML */
    gtk_widget_thaw_child_notify(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_widget_is_focus(value self) { /* ML */
    return Val_bool(gtk_widget_is_focus(GtkObj_val(self)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_grab_focus(value self) { /* ML */
    gtk_widget_grab_focus(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_grab_default(value self) { /* ML */
    gtk_widget_grab_default(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_widget_set_name(value self, value name) { /* ML */
    gtk_widget_set_name(GtkObj_val(self), String_val(name));
    return Val_unit;
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_widget_get_name(value self) { /* ML */
    return my_copy_string(gtk_widget_get_name(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_widget_set_state(value self, value state) { /* ML */
    gtk_widget_set_state(GtkObj_val(self), Int_val(state));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_widget_set_sensitive(value self, value sensitive) { /* ML */
    gtk_widget_set_sensitive(GtkObj_val(self), Bool_val(sensitive));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_widget_set_app_paintable(value self, value app_paintable) { /* ML */
    gtk_widget_set_app_paintable(GtkObj_val(self), Bool_val(app_paintable));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_widget_set_double_buffered(value self, value double_buffered) { /* ML */
    gtk_widget_set_double_buffered(GtkObj_val(self), Bool_val(double_buffered));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_widget_set_redraw_on_allocate(value self, value redraw_on_allocate) { /* ML */
    gtk_widget_set_redraw_on_allocate(GtkObj_val(self), Bool_val(redraw_on_allocate));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_widget_set_parent(value self, value parent) { /* ML */
    gtk_widget_set_parent(GtkObj_val(self), GtkObj_val(parent));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_widget_set_child_visible(value self, value is_visible) { /* ML */
    gtk_widget_set_child_visible(GtkObj_val(self), Bool_val(is_visible));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_widget_get_child_visible(value self) { /* ML */
    return Val_bool(gtk_widget_get_child_visible(GtkObj_val(self)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_widget_get_parent(value self) { /* ML */
    return Val_GtkObj(gtk_widget_get_parent(GtkObj_val(self)));
}

/* ML type: cptr -> int -> bool */
EXTERNML value mgtk_gtk_widget_child_focus(value self, value direction) { /* ML */
    return Val_bool(gtk_widget_child_focus(GtkObj_val(self), Int_val(direction)));
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_widget_set_size_request(value self, value width, value height) { /* ML */
    gtk_widget_set_size_request(GtkObj_val(self), Int_val(width), Int_val(height));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_get_size_request(value self) { /* ML */
    value result;
    int width;
    int height;
    gtk_widget_get_size_request(GtkObj_val(self), &width, &height);
    result = alloc_tuple(2);
    Field(result, 0) = Val_int(width);
    Field(result, 1) = Val_int(height);
    return result;
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_widget_set_uposition(value self, value x, value y) { /* ML */
    gtk_widget_set_uposition(GtkObj_val(self), Int_val(x), Int_val(y));
    return Val_unit;
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_widget_set_usize(value self, value width, value height) { /* ML */
    gtk_widget_set_usize(GtkObj_val(self), Int_val(width), Int_val(height));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_widget_set_events(value self, value events) { /* ML */
    gtk_widget_set_events(GtkObj_val(self), Int_val(events));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_widget_add_events(value self, value events) { /* ML */
    gtk_widget_add_events(GtkObj_val(self), Int_val(events));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_widget_get_toplevel(value self) { /* ML */
    return Val_GtkObj(gtk_widget_get_toplevel(GtkObj_val(self)));
}

/* ML type: cptr -> GType.t -> cptr */
EXTERNML value mgtk_gtk_widget_get_ancestor(value self, value widget_type) { /* ML */
    return Val_GtkObj(gtk_widget_get_ancestor(GtkObj_val(self), Int_val(widget_type)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_widget_get_settings(value self) { /* ML */
    return Val_GtkObj(gtk_widget_get_settings(GtkObj_val(self)));
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_widget_get_events(value self) { /* ML */
    return Val_int(gtk_widget_get_events(GtkObj_val(self)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_get_pointer(value self) { /* ML */
    value result;
    int x;
    int y;
    gtk_widget_get_pointer(GtkObj_val(self), &x, &y);
    result = alloc_tuple(2);
    Field(result, 0) = Val_int(x);
    Field(result, 1) = Val_int(y);
    return result;
}

/* ML type: cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_widget_is_ancestor(value self, value ancestor) { /* ML */
    return Val_bool(gtk_widget_is_ancestor(GtkObj_val(self), GtkObj_val(ancestor)));
}

/* ML type: cptr -> cptr -> int -> int -> bool */
EXTERNML value mgtk_gtk_widget_translate_coordinates(value self, value dest_widget, value src_x, value src_y) { /* ML */
    value result;
    value res;
    int dest_x;
    int dest_y;
    res = Val_bool(gtk_widget_translate_coordinates(GtkObj_val(self), GtkObj_val(dest_widget), Int_val(src_x), Int_val(src_y), &dest_x, &dest_y));
    result = alloc_tuple(3);
    Field(result, 0) = res;
    Field(result, 1) = Val_int(dest_x);
    Field(result, 2) = Val_int(dest_y);
    return result;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_widget_hide_on_delete(value self) { /* ML */
    return Val_bool(gtk_widget_hide_on_delete(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_widget_set_style(value self, value style) { /* ML */
    gtk_widget_set_style(GtkObj_val(self), GtkObj_val(style));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_ensure_style(value self) { /* ML */
    gtk_widget_ensure_style(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_widget_get_style(value self) { /* ML */
    return Val_GtkObj(gtk_widget_get_style(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_widget_modify_style(value self, value style) { /* ML */
    gtk_widget_modify_style(GtkObj_val(self), GtkObj_val(style));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_widget_get_modifier_style(value self) { /* ML */
    return Val_GtkObj(gtk_widget_get_modifier_style(GtkObj_val(self)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_widget_set_composite_name(value self, value name) { /* ML */
    gtk_widget_set_composite_name(GtkObj_val(self), String_val(name));
    return Val_unit;
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_widget_get_composite_name(value self) { /* ML */
    return my_copy_string(gtk_widget_get_composite_name(GtkObj_val(self)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_reset_rc_styles(value self) { /* ML */
    gtk_widget_reset_rc_styles(GtkObj_val(self));
    return Val_unit;
}

/* ML type: unit -> unit */
EXTERNML value mgtk_gtk_widget_push_composite_child(value dummy) { /* ML */
    gtk_widget_push_composite_child();
    return Val_unit;
}

/* ML type: unit -> unit */
EXTERNML value mgtk_gtk_widget_pop_composite_child(value dummy) { /* ML */
    gtk_widget_pop_composite_child();
    return Val_unit;
}

/* ML type: unit -> unit */
EXTERNML value mgtk_gtk_widget_pop_colormap(value dummy) { /* ML */
    gtk_widget_pop_colormap();
    return Val_unit;
}

/* ML type: cptr -> string -> GValue.GValue -> unit */
EXTERNML value mgtk_gtk_widget_style_get_property(value self, value property_name, value valu) { /* ML */
    gtk_widget_style_get_property(GtkObj_val(self), String_val(property_name), GValue_val(valu));
    return Val_unit;
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_widget_style_get(value self, value first_property_name) { /* ML */
    gtk_widget_style_get(GtkObj_val(self), String_val(first_property_name));
    return Val_unit;
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_widget_get_default_style(value dummy) { /* ML */
    return Val_GtkObj(gtk_widget_get_default_style());
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_widget_set_direction(value self, value dir) { /* ML */
    gtk_widget_set_direction(GtkObj_val(self), Int_val(dir));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_widget_get_direction(value self) { /* ML */
    return Val_int(gtk_widget_get_direction(GtkObj_val(self)));
}

/* ML type: int -> unit */
EXTERNML value mgtk_gtk_widget_set_default_direction(value dir) { /* ML */
    gtk_widget_set_default_direction(Int_val(dir));
    return Val_unit;
}

/* ML type: unit -> int */
EXTERNML value mgtk_gtk_widget_get_default_direction(value dummy) { /* ML */
    return Val_int(gtk_widget_get_default_direction());
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_reset_shapes(value self) { /* ML */
    gtk_widget_reset_shapes(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_path(value self) { /* ML */
    value result;
    int path_length;
    char* path;
    char* path_reversed;
    gtk_widget_path(GtkObj_val(self), &path_length, &path, &path_reversed);
    result = alloc_tuple(3);
    Field(result, 0) = Val_int(path_length);
    Field(result, 1) = my_copy_string(path);
    Field(result, 2) = my_copy_string(path_reversed);
    return result;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_widget_class_path(value self) { /* ML */
    value result;
    int path_length;
    char* path;
    char* path_reversed;
    gtk_widget_class_path(GtkObj_val(self), &path_length, &path, &path_reversed);
    result = alloc_tuple(3);
    Field(result, 0) = Val_int(path_length);
    Field(result, 1) = my_copy_string(path);
    Field(result, 2) = my_copy_string(path_reversed);
    return result;
}



/* *** Editable *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_editable_get_type(value dummy) { /* ML */
    return Val_int(gtk_editable_get_type());
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_editable_select_region(value self, value start, value end) { /* ML */
    gtk_editable_select_region(GtkObj_val(self), Int_val(start), Int_val(end));
    return Val_unit;
}

/* ML type: cptr -> string -> int -> int -> int */
EXTERNML value mgtk_gtk_editable_insert_text(value self, value new_text, value new_text_length, value position_in) { /* ML */
    int position = Int_val(position_in);
    gtk_editable_insert_text(GtkObj_val(self), String_val(new_text), Int_val(new_text_length), &position);
    return Val_int(position);
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_editable_delete_text(value self, value start_pos, value end_pos) { /* ML */
    gtk_editable_delete_text(GtkObj_val(self), Int_val(start_pos), Int_val(end_pos));
    return Val_unit;
}

/* ML type: cptr -> int -> int -> string */
EXTERNML value mgtk_gtk_editable_get_chars(value self, value start_pos, value end_pos) { /* ML */
    return my_copy_string(gtk_editable_get_chars(GtkObj_val(self), Int_val(start_pos), Int_val(end_pos)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_editable_cut_clipboard(value self) { /* ML */
    gtk_editable_cut_clipboard(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_editable_copy_clipboard(value self) { /* ML */
    gtk_editable_copy_clipboard(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_editable_paste_clipboard(value self) { /* ML */
    gtk_editable_paste_clipboard(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_editable_delete_selection(value self) { /* ML */
    gtk_editable_delete_selection(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_editable_set_position(value self, value position) { /* ML */
    gtk_editable_set_position(GtkObj_val(self), Int_val(position));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_editable_get_position(value self) { /* ML */
    return Val_int(gtk_editable_get_position(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_editable_set_editable(value self, value is_editable) { /* ML */
    gtk_editable_set_editable(GtkObj_val(self), Bool_val(is_editable));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_editable_get_editable(value self) { /* ML */
    return Val_bool(gtk_editable_get_editable(GtkObj_val(self)));
}



/* *** ItemFactory *** */
/* ML type: cptr -> string -> cptr */
EXTERNML value mgtk_gtk_item_factory_get_item(value self, value path) { /* ML */
    return Val_GtkObj(gtk_item_factory_get_item(GtkObj_val(self), String_val(path)));
}

/* ML type: cptr -> string -> cptr */
EXTERNML value mgtk_gtk_item_factory_get_widget(value self, value path) { /* ML */
    return Val_GtkObj(gtk_item_factory_get_widget(GtkObj_val(self), String_val(path)));
}

/* ML type: cptr -> int -> cptr */
EXTERNML value mgtk_gtk_item_factory_get_widget_by_action(value self, value action) { /* ML */
    return Val_GtkObj(gtk_item_factory_get_widget_by_action(GtkObj_val(self), Int_val(action)));
}

/* ML type: cptr -> int -> cptr */
EXTERNML value mgtk_gtk_item_factory_get_item_by_action(value self, value action) { /* ML */
    return Val_GtkObj(gtk_item_factory_get_item_by_action(GtkObj_val(self), Int_val(action)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_item_factory_delete_item(value self, value path) { /* ML */
    gtk_item_factory_delete_item(GtkObj_val(self), String_val(path));
    return Val_unit;
}



/* *** IMContext *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_im_context_get_type(value dummy) { /* ML */
    return Val_int(gtk_im_context_get_type());
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_im_context_focus_in(value self) { /* ML */
    gtk_im_context_focus_in(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_im_context_focus_out(value self) { /* ML */
    gtk_im_context_focus_out(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_im_context_reset(value self) { /* ML */
    gtk_im_context_reset(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_im_context_set_use_preedit(value self, value use_preedit) { /* ML */
    gtk_im_context_set_use_preedit(GtkObj_val(self), Bool_val(use_preedit));
    return Val_unit;
}

/* ML type: cptr -> string -> int -> int -> unit */
EXTERNML value mgtk_gtk_im_context_set_surrounding(value self, value text, value len, value cursor_index) { /* ML */
    gtk_im_context_set_surrounding(GtkObj_val(self), String_val(text), Int_val(len), Int_val(cursor_index));
    return Val_unit;
}

/* ML type: cptr -> int -> int -> bool */
EXTERNML value mgtk_gtk_im_context_delete_surrounding(value self, value offset, value n_chars) { /* ML */
    return Val_bool(gtk_im_context_delete_surrounding(GtkObj_val(self), Int_val(offset), Int_val(n_chars)));
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_im_context_simple_get_type(value dummy) { /* ML */
    return Val_int(gtk_im_context_simple_get_type());
}



/* *** IMContextSimple *** */
/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_im_context_simple_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_im_context_simple_new());
}



/* *** IMMulticontext *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_im_multicontext_get_type(value dummy) { /* ML */
    return Val_int(gtk_im_multicontext_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_im_multicontext_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_im_multicontext_new());
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_im_multicontext_append_menuitems(value self, value menushell) { /* ML */
    gtk_im_multicontext_append_menuitems(GtkObj_val(self), GtkObj_val(menushell));
    return Val_unit;
}



/* *** CellRenderer *** */
/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_cellrenderer_state(value dummy) { /* ML */
    value res = alloc_tuple(4);
    Field(res, 0) = Val_int(GTK_CELL_RENDERER_SELECTED);
    Field(res, 1) = Val_int(GTK_CELL_RENDERER_PRELIT);
    Field(res, 2) = Val_int(GTK_CELL_RENDERER_INSENSITIVE);
    Field(res, 3) = Val_int(GTK_CELL_RENDERER_SORTED);
    return res;
}

/* ML type: unit -> int * int * int */
EXTERNML value mgtk_get_gtk_cellrenderer_mode(value dummy) { /* ML */
    value res = alloc_tuple(3);
    Field(res, 0) = Val_int(GTK_CELL_RENDERER_MODE_INERT);
    Field(res, 1) = Val_int(GTK_CELL_RENDERER_MODE_ACTIVATABLE);
    Field(res, 2) = Val_int(GTK_CELL_RENDERER_MODE_EDITABLE);
    return res;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_cell_renderer_get_type(value dummy) { /* ML */
    return Val_int(gtk_cell_renderer_get_type());
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_cell_renderer_set_fixed_size(value self, value width, value height) { /* ML */
    gtk_cell_renderer_set_fixed_size(GtkObj_val(self), Int_val(width), Int_val(height));
    return Val_unit;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_cell_renderer_pixbuf_get_type(value dummy) { /* ML */
    return Val_int(gtk_cell_renderer_pixbuf_get_type());
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_cell_renderer_text_get_type(value dummy) { /* ML */
    return Val_int(gtk_cell_renderer_text_get_type());
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_cell_renderer_toggle_get_type(value dummy) { /* ML */
    return Val_int(gtk_cell_renderer_toggle_get_type());
}



/* *** CellEditable *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_cell_editable_get_type(value dummy) { /* ML */
    return Val_int(gtk_cell_editable_get_type());
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_cell_editable_editing_done(value self) { /* ML */
    gtk_cell_editable_editing_done(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_cell_editable_remove_widget(value self) { /* ML */
    gtk_cell_editable_remove_widget(GtkObj_val(self));
    return Val_unit;
}



/* *** CellRendererToggle *** */
/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_cell_renderer_toggle_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_cell_renderer_toggle_new());
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_cell_renderer_toggle_get_radio(value self) { /* ML */
    return Val_bool(gtk_cell_renderer_toggle_get_radio(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_cell_renderer_toggle_set_radio(value self, value radio) { /* ML */
    gtk_cell_renderer_toggle_set_radio(GtkObj_val(self), Bool_val(radio));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_cell_renderer_toggle_get_active(value self) { /* ML */
    return Val_bool(gtk_cell_renderer_toggle_get_active(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_cell_renderer_toggle_set_active(value self, value setting) { /* ML */
    gtk_cell_renderer_toggle_set_active(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}



/* *** CellRendererText *** */
/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_cell_renderer_text_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_cell_renderer_text_new());
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_cell_renderer_text_set_fixed_height_from_font(value self, value number_of_rows) { /* ML */
    gtk_cell_renderer_text_set_fixed_height_from_font(GtkObj_val(self), Int_val(number_of_rows));
    return Val_unit;
}



/* *** CellRendererPixbuf *** */
/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_cell_renderer_pixbuf_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_cell_renderer_pixbuf_new());
}



/* *** RcStyle *** */
/* ML type: cptr -> string -> string -> GType.t -> cptr */
EXTERNML value mgtk_gtk_rc_get_style_by_paths(value settings, value widget_path, value class_path, value type) { /* ML */
    return Val_GtkObj(gtk_rc_get_style_by_paths(GtkObj_val(settings), String_val(widget_path), String_val(class_path), Int_val(type)));
}

/* ML type: cptr -> bool -> bool */
EXTERNML value mgtk_gtk_rc_reparse_all_for_settings(value settings, value force_load) { /* ML */
    return Val_bool(gtk_rc_reparse_all_for_settings(GtkObj_val(settings), Bool_val(force_load)));
}

/* ML type: string -> unit */
EXTERNML value mgtk_gtk_rc_parse(value filename) { /* ML */
    gtk_rc_parse(String_val(filename));
    return Val_unit;
}

/* ML type: string -> unit */
EXTERNML value mgtk_gtk_rc_parse_string(value rc_string) { /* ML */
    gtk_rc_parse_string(String_val(rc_string));
    return Val_unit;
}

/* ML type: unit -> bool */
EXTERNML value mgtk_gtk_rc_reparse_all(value dummy) { /* ML */
    return Val_bool(gtk_rc_reparse_all());
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_rc_add_widget_name_style(value self, value pattern) { /* ML */
    gtk_rc_add_widget_name_style(GtkObj_val(self), String_val(pattern));
    return Val_unit;
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_rc_add_widget_class_style(value self, value pattern) { /* ML */
    gtk_rc_add_widget_class_style(GtkObj_val(self), String_val(pattern));
    return Val_unit;
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_rc_add_class_style(value self, value pattern) { /* ML */
    gtk_rc_add_class_style(GtkObj_val(self), String_val(pattern));
    return Val_unit;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_rc_style_get_type(value dummy) { /* ML */
    return Val_int(gtk_rc_style_get_type());
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_rc_style_copy(value self) { /* ML */
    return Val_GtkObj(gtk_rc_style_copy(GtkObj_val(self)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_rc_style_ref(value self) { /* ML */
    gtk_rc_style_ref(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_rc_style_unref(value self) { /* ML */
    gtk_rc_style_unref(GtkObj_val(self));
    return Val_unit;
}

/* ML type: string -> string */
EXTERNML value mgtk_gtk_rc_find_module_in_path(value module_file) { /* ML */
    return my_copy_string(gtk_rc_find_module_in_path(String_val(module_file)));
}

/* ML type: unit -> string */
EXTERNML value mgtk_gtk_rc_get_theme_dir(value dummy) { /* ML */
    return my_copy_string(gtk_rc_get_theme_dir());
}

/* ML type: unit -> string */
EXTERNML value mgtk_gtk_rc_get_module_dir(value dummy) { /* ML */
    return my_copy_string(gtk_rc_get_module_dir());
}

/* ML type: unit -> string */
EXTERNML value mgtk_gtk_rc_get_im_module_path(value dummy) { /* ML */
    return my_copy_string(gtk_rc_get_im_module_path());
}

/* ML type: unit -> string */
EXTERNML value mgtk_gtk_rc_get_im_module_file(value dummy) { /* ML */
    return my_copy_string(gtk_rc_get_im_module_file());
}



/* *** Settings *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_settings_get_type(value dummy) { /* ML */
    return Val_int(gtk_settings_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_settings_get_default(value dummy) { /* ML */
    return Val_GtkObj(gtk_settings_get_default());
}

/* ML type: cptr -> string -> string -> string -> unit */
EXTERNML value mgtk_gtk_settings_set_string_property(value self, value name, value v_string, value origin) { /* ML */
    gtk_settings_set_string_property(GtkObj_val(self), String_val(name), String_val(v_string), String_val(origin));
    return Val_unit;
}

/* ML type: cptr -> string -> real -> string -> unit */
EXTERNML value mgtk_gtk_settings_set_double_property(value self, value name, value v_double, value origin) { /* ML */
    gtk_settings_set_double_property(GtkObj_val(self), String_val(name), Double_val(v_double), String_val(origin));
    return Val_unit;
}



/* *** SizeGroup *** */
/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_size_group_mode(value dummy) { /* ML */
    value res = alloc_tuple(4);
    Field(res, 0) = Val_int(GTK_SIZE_GROUP_NONE);
    Field(res, 1) = Val_int(GTK_SIZE_GROUP_HORIZONTAL);
    Field(res, 2) = Val_int(GTK_SIZE_GROUP_VERTICAL);
    Field(res, 3) = Val_int(GTK_SIZE_GROUP_BOTH);
    return res;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_size_group_get_type(value dummy) { /* ML */
    return Val_int(gtk_size_group_get_type());
}

/* ML type: int -> cptr */
EXTERNML value mgtk_gtk_size_group_new(value mode) { /* ML */
    return Val_GtkObj(gtk_size_group_new(Int_val(mode)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_size_group_set_mode(value self, value mode) { /* ML */
    gtk_size_group_set_mode(GtkObj_val(self), Int_val(mode));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_size_group_get_mode(value self) { /* ML */
    return Val_int(gtk_size_group_get_mode(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_size_group_add_widget(value self, value widget) { /* ML */
    gtk_size_group_add_widget(GtkObj_val(self), GtkObj_val(widget));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_size_group_remove_widget(value self, value widget) { /* ML */
    gtk_size_group_remove_widget(GtkObj_val(self), GtkObj_val(widget));
    return Val_unit;
}



/* *** Style *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_style_get_type(value dummy) { /* ML */
    return Val_int(gtk_style_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_style_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_style_new());
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_style_copy(value self) { /* ML */
    return Val_GtkObj(gtk_style_copy(GtkObj_val(self)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_style_detach(value self) { /* ML */
    gtk_style_detach(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_style_ref(value self) { /* ML */
    return Val_GtkObj(gtk_style_ref(GtkObj_val(self)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_style_unref(value self) { /* ML */
    gtk_style_unref(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> string -> cptr */
EXTERNML value mgtk_gtk_style_lookup_icon_set(value self, value stock_id) { /* ML */
    return Val_GtkIconSet(gtk_style_lookup_icon_set(GtkObj_val(self), String_val(stock_id)));
}



/* *** TextBuffer *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_text_buffer_get_type(value dummy) { /* ML */
    return Val_int(gtk_text_buffer_get_type());
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_text_buffer_new(value table) { /* ML */
    return Val_GtkObj(gtk_text_buffer_new(GtkObj_val(table)));
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_text_buffer_get_line_count(value self) { /* ML */
    return Val_int(gtk_text_buffer_get_line_count(GtkObj_val(self)));
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_text_buffer_get_char_count(value self) { /* ML */
    return Val_int(gtk_text_buffer_get_char_count(GtkObj_val(self)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_text_buffer_get_tag_table(value self) { /* ML */
    return Val_GtkObj(gtk_text_buffer_get_tag_table(GtkObj_val(self)));
}

/* ML type: cptr -> string -> int -> unit */
EXTERNML value mgtk_gtk_text_buffer_set_text(value self, value text, value len) { /* ML */
    gtk_text_buffer_set_text(GtkObj_val(self), String_val(text), Int_val(len));
    return Val_unit;
}

/* ML type: cptr -> cptr -> string -> int -> unit */
EXTERNML value mgtk_gtk_text_buffer_insert(value self, value iter, value text, value len) { /* ML */
    gtk_text_buffer_insert(GtkObj_val(self), GtkTextIter_val(iter), String_val(text), Int_val(len));
    return Val_unit;
}

/* ML type: cptr -> string -> int -> unit */
EXTERNML value mgtk_gtk_text_buffer_insert_at_cursor(value self, value text, value len) { /* ML */
    gtk_text_buffer_insert_at_cursor(GtkObj_val(self), String_val(text), Int_val(len));
    return Val_unit;
}

/* ML type: cptr -> cptr -> string -> int -> bool -> bool */
EXTERNML value mgtk_gtk_text_buffer_insert_interactive(value self, value iter, value text, value len, value default_editable) { /* ML */
    return Val_bool(gtk_text_buffer_insert_interactive(GtkObj_val(self), GtkTextIter_val(iter), String_val(text), Int_val(len), Bool_val(default_editable)));
}

/* ML type: cptr -> string -> int -> bool -> bool */
EXTERNML value mgtk_gtk_text_buffer_insert_interactive_at_cursor(value self, value text, value len, value default_editable) { /* ML */
    return Val_bool(gtk_text_buffer_insert_interactive_at_cursor(GtkObj_val(self), String_val(text), Int_val(len), Bool_val(default_editable)));
}

/* ML type: cptr -> cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_insert_range(value self, value iter, value start, value end) { /* ML */
    gtk_text_buffer_insert_range(GtkObj_val(self), GtkTextIter_val(iter), GtkTextIter_val(start), GtkTextIter_val(end));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr -> cptr -> bool -> bool */
EXTERNML value mgtk_gtk_text_buffer_insert_range_interactive(value self, value iter, value start, value end, value default_editable) { /* ML */
    return Val_bool(gtk_text_buffer_insert_range_interactive(GtkObj_val(self), GtkTextIter_val(iter), GtkTextIter_val(start), GtkTextIter_val(end), Bool_val(default_editable)));
}

/* ML type: cptr -> cptr -> string -> int -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_insert_with_tags(value self, value iter, value text, value len, value first_tag) { /* ML */
    gtk_text_buffer_insert_with_tags(GtkObj_val(self), GtkTextIter_val(iter), String_val(text), Int_val(len), GtkObj_val(first_tag));
    return Val_unit;
}

/* ML type: cptr -> cptr -> string -> int -> string -> unit */
EXTERNML value mgtk_gtk_text_buffer_insert_with_tags_by_name(value self, value iter, value text, value len, value first_tag_name) { /* ML */
    gtk_text_buffer_insert_with_tags_by_name(GtkObj_val(self), GtkTextIter_val(iter), String_val(text), Int_val(len), String_val(first_tag_name));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_delete(value self, value start, value end) { /* ML */
    gtk_text_buffer_delete(GtkObj_val(self), GtkTextIter_val(start), GtkTextIter_val(end));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr -> bool -> bool */
EXTERNML value mgtk_gtk_text_buffer_delete_interactive(value self, value start_iter, value end_iter, value default_editable) { /* ML */
    return Val_bool(gtk_text_buffer_delete_interactive(GtkObj_val(self), GtkTextIter_val(start_iter), GtkTextIter_val(end_iter), Bool_val(default_editable)));
}

/* ML type: cptr -> cptr -> cptr -> bool -> string */
EXTERNML value mgtk_gtk_text_buffer_get_text(value self, value start, value end, value include_hidden_chars) { /* ML */
    return my_copy_string(gtk_text_buffer_get_text(GtkObj_val(self), GtkTextIter_val(start), GtkTextIter_val(end), Bool_val(include_hidden_chars)));
}

/* ML type: cptr -> cptr -> cptr -> bool -> string */
EXTERNML value mgtk_gtk_text_buffer_get_slice(value self, value start, value end, value include_hidden_chars) { /* ML */
    return my_copy_string(gtk_text_buffer_get_slice(GtkObj_val(self), GtkTextIter_val(start), GtkTextIter_val(end), Bool_val(include_hidden_chars)));
}

/* ML type: cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_insert_child_anchor(value self, value iter, value anchor) { /* ML */
    gtk_text_buffer_insert_child_anchor(GtkObj_val(self), GtkTextIter_val(iter), GtkObj_val(anchor));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr */
EXTERNML value mgtk_gtk_text_buffer_create_child_anchor(value self, value iter) { /* ML */
    return Val_GtkObj(gtk_text_buffer_create_child_anchor(GtkObj_val(self), GtkTextIter_val(iter)));
}

/* ML type: cptr -> string -> cptr -> bool -> cptr */
EXTERNML value mgtk_gtk_text_buffer_create_mark(value self, value mark_name, value where, value left_gravity) { /* ML */
    return Val_GtkObj(gtk_text_buffer_create_mark(GtkObj_val(self), String_val(mark_name), GtkTextIter_val(where), Bool_val(left_gravity)));
}

/* ML type: cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_move_mark(value self, value mark, value where) { /* ML */
    gtk_text_buffer_move_mark(GtkObj_val(self), GtkObj_val(mark), GtkTextIter_val(where));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_delete_mark(value self, value mark) { /* ML */
    gtk_text_buffer_delete_mark(GtkObj_val(self), GtkObj_val(mark));
    return Val_unit;
}

/* ML type: cptr -> string -> cptr */
EXTERNML value mgtk_gtk_text_buffer_get_mark(value self, value name) { /* ML */
    return Val_GtkObj(gtk_text_buffer_get_mark(GtkObj_val(self), String_val(name)));
}

/* ML type: cptr -> string -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_move_mark_by_name(value self, value name, value where) { /* ML */
    gtk_text_buffer_move_mark_by_name(GtkObj_val(self), String_val(name), GtkTextIter_val(where));
    return Val_unit;
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_text_buffer_delete_mark_by_name(value self, value name) { /* ML */
    gtk_text_buffer_delete_mark_by_name(GtkObj_val(self), String_val(name));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_text_buffer_get_insert(value self) { /* ML */
    return Val_GtkObj(gtk_text_buffer_get_insert(GtkObj_val(self)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_text_buffer_get_selection_bound(value self) { /* ML */
    return Val_GtkObj(gtk_text_buffer_get_selection_bound(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_place_cursor(value self, value where) { /* ML */
    gtk_text_buffer_place_cursor(GtkObj_val(self), GtkTextIter_val(where));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_apply_tag(value self, value tag, value start, value end) { /* ML */
    gtk_text_buffer_apply_tag(GtkObj_val(self), GtkObj_val(tag), GtkTextIter_val(start), GtkTextIter_val(end));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_remove_tag(value self, value tag, value start, value end) { /* ML */
    gtk_text_buffer_remove_tag(GtkObj_val(self), GtkObj_val(tag), GtkTextIter_val(start), GtkTextIter_val(end));
    return Val_unit;
}

/* ML type: cptr -> string -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_apply_tag_by_name(value self, value name, value start, value end) { /* ML */
    gtk_text_buffer_apply_tag_by_name(GtkObj_val(self), String_val(name), GtkTextIter_val(start), GtkTextIter_val(end));
    return Val_unit;
}

/* ML type: cptr -> string -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_remove_tag_by_name(value self, value name, value start, value end) { /* ML */
    gtk_text_buffer_remove_tag_by_name(GtkObj_val(self), String_val(name), GtkTextIter_val(start), GtkTextIter_val(end));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_remove_all_tags(value self, value start, value end) { /* ML */
    gtk_text_buffer_remove_all_tags(GtkObj_val(self), GtkTextIter_val(start), GtkTextIter_val(end));
    return Val_unit;
}

/* ML type: cptr -> string -> string -> cptr */
EXTERNML value mgtk_gtk_text_buffer_create_tag(value self, value tag_name, value first_property_name) { /* ML */
    return Val_GtkObj(gtk_text_buffer_create_tag(GtkObj_val(self), String_val(tag_name), String_val(first_property_name)));
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_text_buffer_get_iter_at_line_offset(value self, value line_number, value char_offset) { /* ML */
    GtkTextIter iter;
    gtk_text_buffer_get_iter_at_line_offset(GtkObj_val(self), &iter, Int_val(line_number), Int_val(char_offset));
    return Val_GtkTextIter(&iter);
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_text_buffer_get_iter_at_line_index(value self, value line_number, value byte_index) { /* ML */
    GtkTextIter iter;
    gtk_text_buffer_get_iter_at_line_index(GtkObj_val(self), &iter, Int_val(line_number), Int_val(byte_index));
    return Val_GtkTextIter(&iter);
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_text_buffer_get_iter_at_offset(value self, value char_offset) { /* ML */
    GtkTextIter iter;
    gtk_text_buffer_get_iter_at_offset(GtkObj_val(self), &iter, Int_val(char_offset));
    return Val_GtkTextIter(&iter);
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_text_buffer_get_iter_at_line(value self, value line_number) { /* ML */
    GtkTextIter iter;
    gtk_text_buffer_get_iter_at_line(GtkObj_val(self), &iter, Int_val(line_number));
    return Val_GtkTextIter(&iter);
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_get_start_iter(value self) { /* ML */
    GtkTextIter iter;
    gtk_text_buffer_get_start_iter(GtkObj_val(self), &iter);
    return Val_GtkTextIter(&iter);
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_get_end_iter(value self) { /* ML */
    GtkTextIter iter;
    gtk_text_buffer_get_end_iter(GtkObj_val(self), &iter);
    return Val_GtkTextIter(&iter);
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_get_bounds(value self) { /* ML */
    value result;
    GtkTextIter start;
    GtkTextIter end;
    gtk_text_buffer_get_bounds(GtkObj_val(self), &start, &end);
    result = alloc_tuple(2);
    Field(result, 0) = Val_GtkTextIter(&start);
    Field(result, 1) = Val_GtkTextIter(&end);
    return result;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_get_iter_at_mark(value self, value mark) { /* ML */
    GtkTextIter iter;
    gtk_text_buffer_get_iter_at_mark(GtkObj_val(self), &iter, GtkObj_val(mark));
    return Val_GtkTextIter(&iter);
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_get_iter_at_child_anchor(value self, value anchor) { /* ML */
    GtkTextIter iter;
    gtk_text_buffer_get_iter_at_child_anchor(GtkObj_val(self), &iter, GtkObj_val(anchor));
    return Val_GtkTextIter(&iter);
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_text_buffer_get_modified(value self) { /* ML */
    return Val_bool(gtk_text_buffer_get_modified(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_text_buffer_set_modified(value self, value setting) { /* ML */
    gtk_text_buffer_set_modified(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_add_selection_clipboard(value self, value clipboard) { /* ML */
    gtk_text_buffer_add_selection_clipboard(GtkObj_val(self), GtkObj_val(clipboard));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_remove_selection_clipboard(value self, value clipboard) { /* ML */
    gtk_text_buffer_remove_selection_clipboard(GtkObj_val(self), GtkObj_val(clipboard));
    return Val_unit;
}

/* ML type: cptr -> cptr -> bool -> unit */
EXTERNML value mgtk_gtk_text_buffer_cut_clipboard(value self, value clipboard, value default_editable) { /* ML */
    gtk_text_buffer_cut_clipboard(GtkObj_val(self), GtkObj_val(clipboard), Bool_val(default_editable));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_copy_clipboard(value self, value clipboard) { /* ML */
    gtk_text_buffer_copy_clipboard(GtkObj_val(self), GtkObj_val(clipboard));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr -> bool -> unit */
EXTERNML value mgtk_gtk_text_buffer_paste_clipboard(value self, value clipboard, value override_location, value default_editable) { /* ML */
    gtk_text_buffer_paste_clipboard(GtkObj_val(self), GtkObj_val(clipboard), GtkTextIter_val(override_location), Bool_val(default_editable));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_text_buffer_get_selection_bounds(value self) { /* ML */
    value result;
    value res;
    GtkTextIter start;
    GtkTextIter end;
    res = Val_bool(gtk_text_buffer_get_selection_bounds(GtkObj_val(self), &start, &end));
    result = alloc_tuple(3);
    Field(result, 0) = res;
    Field(result, 1) = Val_GtkTextIter(&start);
    Field(result, 2) = Val_GtkTextIter(&end);
    return result;
}

/* ML type: cptr -> bool -> bool -> bool */
EXTERNML value mgtk_gtk_text_buffer_delete_selection(value self, value interactive, value default_editable) { /* ML */
    return Val_bool(gtk_text_buffer_delete_selection(GtkObj_val(self), Bool_val(interactive), Bool_val(default_editable)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_begin_user_action(value self) { /* ML */
    gtk_text_buffer_begin_user_action(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_text_buffer_end_user_action(value self) { /* ML */
    gtk_text_buffer_end_user_action(GtkObj_val(self));
    return Val_unit;
}



/* *** TextChildAnchor *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_text_child_anchor_get_type(value dummy) { /* ML */
    return Val_int(gtk_text_child_anchor_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_text_child_anchor_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_text_child_anchor_new());
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_text_child_anchor_get_deleted(value self) { /* ML */
    return Val_bool(gtk_text_child_anchor_get_deleted(GtkObj_val(self)));
}



/* *** TextMark *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_text_mark_get_type(value dummy) { /* ML */
    return Val_int(gtk_text_mark_get_type());
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_text_mark_set_visible(value self, value setting) { /* ML */
    gtk_text_mark_set_visible(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_text_mark_get_visible(value self) { /* ML */
    return Val_bool(gtk_text_mark_get_visible(GtkObj_val(self)));
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_text_mark_get_name(value self) { /* ML */
    return my_copy_string(gtk_text_mark_get_name(GtkObj_val(self)));
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_text_mark_get_deleted(value self) { /* ML */
    return Val_bool(gtk_text_mark_get_deleted(GtkObj_val(self)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_text_mark_get_buffer(value self) { /* ML */
    return Val_GtkObj(gtk_text_mark_get_buffer(GtkObj_val(self)));
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_text_mark_get_left_gravity(value self) { /* ML */
    return Val_bool(gtk_text_mark_get_left_gravity(GtkObj_val(self)));
}



/* *** TextTag *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_text_tag_get_type(value dummy) { /* ML */
    return Val_int(gtk_text_tag_get_type());
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_text_tag_new(value name) { /* ML */
    return Val_GtkObj(gtk_text_tag_new(String_val(name)));
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_text_tag_get_priority(value self) { /* ML */
    return Val_int(gtk_text_tag_get_priority(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_text_tag_set_priority(value self, value priority) { /* ML */
    gtk_text_tag_set_priority(GtkObj_val(self), Int_val(priority));
    return Val_unit;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_text_tag_table_get_type(value dummy) { /* ML */
    return Val_int(gtk_text_tag_table_get_type());
}



/* *** TextTagTable *** */
/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_text_tag_table_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_text_tag_table_new());
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_tag_table_add(value self, value tag) { /* ML */
    gtk_text_tag_table_add(GtkObj_val(self), GtkObj_val(tag));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_tag_table_remove(value self, value tag) { /* ML */
    gtk_text_tag_table_remove(GtkObj_val(self), GtkObj_val(tag));
    return Val_unit;
}

/* ML type: cptr -> string -> cptr */
EXTERNML value mgtk_gtk_text_tag_table_lookup(value self, value name) { /* ML */
    return Val_GtkObj(gtk_text_tag_table_lookup(GtkObj_val(self), String_val(name)));
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_text_tag_table_get_size(value self) { /* ML */
    return Val_int(gtk_text_tag_table_get_size(GtkObj_val(self)));
}



/* *** Tooltips *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_tooltips_get_type(value dummy) { /* ML */
    return Val_int(gtk_tooltips_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_tooltips_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_tooltips_new());
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_tooltips_enable(value self) { /* ML */
    gtk_tooltips_enable(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_tooltips_disable(value self) { /* ML */
    gtk_tooltips_disable(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_tooltips_set_delay(value self, value delay) { /* ML */
    gtk_tooltips_set_delay(GtkObj_val(self), Int_val(delay));
    return Val_unit;
}

/* ML type: cptr -> cptr -> string -> string -> unit */
EXTERNML value mgtk_gtk_tooltips_set_tip(value self, value widget, value tip_text, value tip_private) { /* ML */
    gtk_tooltips_set_tip(GtkObj_val(self), GtkObj_val(widget), String_val(tip_text), String_val(tip_private));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_tooltips_force_window(value self) { /* ML */
    gtk_tooltips_force_window(GtkObj_val(self));
    return Val_unit;
}



/* *** TreeModel *** */
/* ML type: unit -> int * int */
EXTERNML value mgtk_get_gtk_treemodel_flags(value dummy) { /* ML */
    value res = alloc_tuple(2);
    Field(res, 0) = Val_int(GTK_TREE_MODEL_ITERS_PERSIST);
    Field(res, 1) = Val_int(GTK_TREE_MODEL_LIST_ONLY);
    return res;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_tree_model_get_type(value dummy) { /* ML */
    return Val_int(gtk_tree_model_get_type());
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_tree_model_get_flags(value self) { /* ML */
    return Val_int(gtk_tree_model_get_flags(GtkObj_val(self)));
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_tree_model_get_n_columns(value self) { /* ML */
    return Val_int(gtk_tree_model_get_n_columns(GtkObj_val(self)));
}

/* ML type: cptr -> int -> GType.t */
EXTERNML value mgtk_gtk_tree_model_get_column_type(value self, value index) { /* ML */
    return Val_int(gtk_tree_model_get_column_type(GtkObj_val(self), Int_val(index)));
}

/* ML type: cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_tree_model_get_iter(value self, value path) { /* ML */
    value result;
    value res;
    GtkTreeIter iter;
    res = Val_bool(gtk_tree_model_get_iter(GtkObj_val(self), &iter, GtkTreePath_val(path)));
    result = alloc_tuple(2);
    Field(result, 0) = res;
    Field(result, 1) = Val_GtkTreeIter(&iter);
    return result;
}

/* ML type: cptr -> string -> bool */
EXTERNML value mgtk_gtk_tree_model_get_iter_from_string(value self, value path_string) { /* ML */
    value result;
    value res;
    GtkTreeIter iter;
    res = Val_bool(gtk_tree_model_get_iter_from_string(GtkObj_val(self), &iter, String_val(path_string)));
    result = alloc_tuple(2);
    Field(result, 0) = res;
    Field(result, 1) = Val_GtkTreeIter(&iter);
    return result;
}

/* ML type: cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_tree_model_get_iter_root(value self, value iter) { /* ML */
    return Val_bool(gtk_tree_model_get_iter_root(GtkObj_val(self), GtkTreeIter_val(iter)));
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_tree_model_get_iter_first(value self) { /* ML */
    value result;
    value res;
    GtkTreeIter iter;
    res = Val_bool(gtk_tree_model_get_iter_first(GtkObj_val(self), &iter));
    result = alloc_tuple(2);
    Field(result, 0) = res;
    Field(result, 1) = Val_GtkTreeIter(&iter);
    return result;
}

/* ML type: cptr -> cptr -> cptr */
EXTERNML value mgtk_gtk_tree_model_get_path(value self, value iter) { /* ML */
    return Val_GtkTreePath(gtk_tree_model_get_path(GtkObj_val(self), GtkTreeIter_val(iter)));
}

/* ML type: cptr -> cptr -> int -> GValue.GValue -> GValue.GValue */
EXTERNML value mgtk_gtk_tree_model_get_value(value self, value iter, value column, value valu_in) { /* ML */
    GValue* valu = *((GValue**) GValue_val(valu_in));
    gtk_tree_model_get_value(GtkObj_val(self), GtkTreeIter_val(iter), Int_val(column), &valu);
    return Val_GValue(valu);
}

/* ML type: cptr -> cptr -> bool * cptr */
EXTERNML value mgtk_gtk_tree_model_iter_next(value self, value iter_in) { /* ML */
    value result;
    value res;
    GtkTreeIter iter = *((GtkTreeIter*) GtkTreeIter_val(iter_in));
    res = Val_bool(gtk_tree_model_iter_next(GtkObj_val(self), &iter));
    result = alloc_tuple(2);
    Field(result, 0) = res;
    Field(result, 1) = Val_GtkTreeIter(&iter);
    return result;
}

/* ML type: cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_tree_model_iter_children(value self, value parent) { /* ML */
    value result;
    value res;
    GtkTreeIter iter;
    res = Val_bool(gtk_tree_model_iter_children(GtkObj_val(self), &iter, GtkTreeIter_val(parent)));
    result = alloc_tuple(2);
    Field(result, 0) = res;
    Field(result, 1) = Val_GtkTreeIter(&iter);
    return result;
}

/* ML type: cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_tree_model_iter_has_child(value self, value iter) { /* ML */
    return Val_bool(gtk_tree_model_iter_has_child(GtkObj_val(self), GtkTreeIter_val(iter)));
}

/* ML type: cptr -> cptr -> int */
EXTERNML value mgtk_gtk_tree_model_iter_n_children(value self, value iter) { /* ML */
    return Val_int(gtk_tree_model_iter_n_children(GtkObj_val(self), GtkTreeIter_val(iter)));
}

/* ML type: cptr -> cptr -> int -> bool */
EXTERNML value mgtk_gtk_tree_model_iter_nth_child(value self, value parent, value n) { /* ML */
    value result;
    value res;
    GtkTreeIter iter;
    res = Val_bool(gtk_tree_model_iter_nth_child(GtkObj_val(self), &iter, GtkTreeIter_val(parent), Int_val(n)));
    result = alloc_tuple(2);
    Field(result, 0) = res;
    Field(result, 1) = Val_GtkTreeIter(&iter);
    return result;
}

/* ML type: cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_tree_model_iter_parent(value self, value child) { /* ML */
    value result;
    value res;
    GtkTreeIter iter;
    res = Val_bool(gtk_tree_model_iter_parent(GtkObj_val(self), &iter, GtkTreeIter_val(child)));
    result = alloc_tuple(2);
    Field(result, 0) = res;
    Field(result, 1) = Val_GtkTreeIter(&iter);
    return result;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_model_ref_node(value self, value iter) { /* ML */
    gtk_tree_model_ref_node(GtkObj_val(self), GtkTreeIter_val(iter));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_model_unref_node(value self, value iter) { /* ML */
    gtk_tree_model_unref_node(GtkObj_val(self), GtkTreeIter_val(iter));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_model_get(value self, value iter) { /* ML */
    gtk_tree_model_get(GtkObj_val(self), GtkTreeIter_val(iter));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_model_row_changed(value self, value path, value iter) { /* ML */
    gtk_tree_model_row_changed(GtkObj_val(self), GtkTreePath_val(path), GtkTreeIter_val(iter));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_model_row_inserted(value self, value path, value iter) { /* ML */
    gtk_tree_model_row_inserted(GtkObj_val(self), GtkTreePath_val(path), GtkTreeIter_val(iter));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_model_row_has_child_toggled(value self, value path, value iter) { /* ML */
    gtk_tree_model_row_has_child_toggled(GtkObj_val(self), GtkTreePath_val(path), GtkTreeIter_val(iter));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_model_row_deleted(value self, value path) { /* ML */
    gtk_tree_model_row_deleted(GtkObj_val(self), GtkTreePath_val(path));
    return Val_unit;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_tree_model_sort_get_type(value dummy) { /* ML */
    return Val_int(gtk_tree_model_sort_get_type());
}



/* *** TreeDragSource *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_tree_drag_source_get_type(value dummy) { /* ML */
    return Val_int(gtk_tree_drag_source_get_type());
}

/* ML type: cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_tree_drag_source_row_draggable(value self, value path) { /* ML */
    return Val_bool(gtk_tree_drag_source_row_draggable(GtkObj_val(self), GtkTreePath_val(path)));
}

/* ML type: cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_tree_drag_source_drag_data_delete(value self, value path) { /* ML */
    return Val_bool(gtk_tree_drag_source_drag_data_delete(GtkObj_val(self), GtkTreePath_val(path)));
}

/* ML type: cptr -> cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_tree_drag_source_drag_data_get(value self, value path, value selection_data) { /* ML */
    return Val_bool(gtk_tree_drag_source_drag_data_get(GtkObj_val(self), GtkTreePath_val(path), GtkSelectionData_val(selection_data)));
}



/* *** TreeDragDest *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_tree_drag_dest_get_type(value dummy) { /* ML */
    return Val_int(gtk_tree_drag_dest_get_type());
}

/* ML type: cptr -> cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_tree_drag_dest_drag_data_received(value self, value dest, value selection_data) { /* ML */
    return Val_bool(gtk_tree_drag_dest_drag_data_received(GtkObj_val(self), GtkTreePath_val(dest), GtkSelectionData_val(selection_data)));
}

/* ML type: cptr -> cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_tree_drag_dest_row_drop_possible(value self, value dest_path, value selection_data) { /* ML */
    return Val_bool(gtk_tree_drag_dest_row_drop_possible(GtkObj_val(self), GtkTreePath_val(dest_path), GtkSelectionData_val(selection_data)));
}



/* *** TreeSortable *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_tree_sortable_get_type(value dummy) { /* ML */
    return Val_int(gtk_tree_sortable_get_type());
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_tree_sortable_sort_column_changed(value self) { /* ML */
    gtk_tree_sortable_sort_column_changed(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_tree_sortable_set_sort_column_id(value self, value sort_column_id, value order) { /* ML */
    gtk_tree_sortable_set_sort_column_id(GtkObj_val(self), Int_val(sort_column_id), Int_val(order));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_tree_sortable_has_default_sort_func(value self) { /* ML */
    return Val_bool(gtk_tree_sortable_has_default_sort_func(GtkObj_val(self)));
}



/* *** ListStore *** */
/* ML type: int -> cptr */
EXTERNML value mgtk_gtk_list_store_new(value n_columns) { /* ML */
    return Val_GtkObj(gtk_list_store_new(Int_val(n_columns)));
}

/* ML type: int -> GType.t list -> cptr */
EXTERNML value mgtk_gtk_list_store_newv(value n_columns, value types_arr) { /* ML */
    GType* types;
    list_to_array(GType, types, Int_val, types_arr);
    return Val_GtkObj(gtk_list_store_newv(Int_val(n_columns), types));
}

/* ML type: cptr -> int -> GType.t list -> unit */
EXTERNML value mgtk_gtk_list_store_set_column_types(value self, value n_columns, value types_arr) { /* ML */
    GType* types;
    list_to_array(GType, types, Int_val, types_arr);
    gtk_list_store_set_column_types(GtkObj_val(self), Int_val(n_columns), types);
    return Val_unit;
}

/* ML type: cptr -> cptr -> int -> GValue.GValue -> unit */
EXTERNML value mgtk_gtk_list_store_set_value(value self, value iter, value column, value valu) { /* ML */
    gtk_list_store_set_value(GtkObj_val(self), GtkTreeIter_val(iter), Int_val(column), GValue_val(valu));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_list_store_set(value self, value iter) { /* ML */
    gtk_list_store_set(GtkObj_val(self), GtkTreeIter_val(iter));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr */
EXTERNML value mgtk_gtk_list_store_remove(value self, value iter_in) { /* ML */
    GtkTreeIter iter = *((GtkTreeIter*) GtkTreeIter_val(iter_in));
    gtk_list_store_remove(GtkObj_val(self), &iter);
    return Val_GtkTreeIter(&iter);
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_list_store_insert(value self, value position) { /* ML */
    GtkTreeIter iter;
    gtk_list_store_insert(GtkObj_val(self), &iter, Int_val(position));
    return Val_GtkTreeIter(&iter);
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_list_store_insert_before(value self, value sibling) { /* ML */
    GtkTreeIter iter;
    gtk_list_store_insert_before(GtkObj_val(self), &iter, GtkTreeIter_val(sibling));
    return Val_GtkTreeIter(&iter);
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_list_store_insert_after(value self, value sibling) { /* ML */
    GtkTreeIter iter;
    gtk_list_store_insert_after(GtkObj_val(self), &iter, GtkTreeIter_val(sibling));
    return Val_GtkTreeIter(&iter);
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_list_store_prepend(value self) { /* ML */
    GtkTreeIter iter;
    gtk_list_store_prepend(GtkObj_val(self), &iter);
    return Val_GtkTreeIter(&iter);
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_list_store_append(value self) { /* ML */
    GtkTreeIter iter;
    gtk_list_store_append(GtkObj_val(self), &iter);
    return Val_GtkTreeIter(&iter);
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_list_store_clear(value self) { /* ML */
    gtk_list_store_clear(GtkObj_val(self));
    return Val_unit;
}



/* *** TreeModelSort *** */
/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_tree_model_sort_new_with_model(value child_model) { /* ML */
    return Val_GtkObj(gtk_tree_model_sort_new_with_model(GtkObj_val(child_model)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_tree_model_sort_get_model(value self) { /* ML */
    return Val_GtkObj(gtk_tree_model_sort_get_model(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> cptr */
EXTERNML value mgtk_gtk_tree_model_sort_convert_child_path_to_path(value self, value child_path) { /* ML */
    return Val_GtkTreePath(gtk_tree_model_sort_convert_child_path_to_path(GtkObj_val(self), GtkTreePath_val(child_path)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_model_sort_convert_child_iter_to_iter(value self, value child_iter) { /* ML */
    GtkTreeIter sort_iter;
    gtk_tree_model_sort_convert_child_iter_to_iter(GtkObj_val(self), &sort_iter, GtkTreeIter_val(child_iter));
    return Val_GtkTreeIter(&sort_iter);
}

/* ML type: cptr -> cptr -> cptr */
EXTERNML value mgtk_gtk_tree_model_sort_convert_path_to_child_path(value self, value sorted_path) { /* ML */
    return Val_GtkTreePath(gtk_tree_model_sort_convert_path_to_child_path(GtkObj_val(self), GtkTreePath_val(sorted_path)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_model_sort_convert_iter_to_child_iter(value self, value sorted_iter) { /* ML */
    GtkTreeIter child_iter;
    gtk_tree_model_sort_convert_iter_to_child_iter(GtkObj_val(self), &child_iter, GtkTreeIter_val(sorted_iter));
    return Val_GtkTreeIter(&child_iter);
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_tree_model_sort_reset_default_sort_func(value self) { /* ML */
    gtk_tree_model_sort_reset_default_sort_func(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_tree_model_sort_clear_cache(value self) { /* ML */
    gtk_tree_model_sort_clear_cache(GtkObj_val(self));
    return Val_unit;
}



/* *** TreeSelection *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_tree_selection_get_type(value dummy) { /* ML */
    return Val_int(gtk_tree_selection_get_type());
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_tree_selection_set_mode(value self, value type) { /* ML */
    gtk_tree_selection_set_mode(GtkObj_val(self), Int_val(type));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_tree_selection_get_mode(value self) { /* ML */
    return Val_int(gtk_tree_selection_get_mode(GtkObj_val(self)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_tree_selection_get_user_data(value self) { /* ML */
    return (value)(gtk_tree_selection_get_user_data(GtkObj_val(self)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_tree_selection_get_tree_view(value self) { /* ML */
    return Val_GtkObj(gtk_tree_selection_get_tree_view(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_selection_select_path(value self, value path) { /* ML */
    gtk_tree_selection_select_path(GtkObj_val(self), GtkTreePath_val(path));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_selection_unselect_path(value self, value path) { /* ML */
    gtk_tree_selection_unselect_path(GtkObj_val(self), GtkTreePath_val(path));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_selection_select_iter(value self, value iter) { /* ML */
    gtk_tree_selection_select_iter(GtkObj_val(self), GtkTreeIter_val(iter));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_selection_unselect_iter(value self, value iter) { /* ML */
    gtk_tree_selection_unselect_iter(GtkObj_val(self), GtkTreeIter_val(iter));
    return Val_unit;
}

/* ML type: cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_tree_selection_path_is_selected(value self, value path) { /* ML */
    return Val_bool(gtk_tree_selection_path_is_selected(GtkObj_val(self), GtkTreePath_val(path)));
}

/* ML type: cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_tree_selection_iter_is_selected(value self, value iter) { /* ML */
    return Val_bool(gtk_tree_selection_iter_is_selected(GtkObj_val(self), GtkTreeIter_val(iter)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_tree_selection_select_all(value self) { /* ML */
    gtk_tree_selection_select_all(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_tree_selection_unselect_all(value self) { /* ML */
    gtk_tree_selection_unselect_all(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_selection_select_range(value self, value start_path, value end_path) { /* ML */
    gtk_tree_selection_select_range(GtkObj_val(self), GtkTreePath_val(start_path), GtkTreePath_val(end_path));
    return Val_unit;
}



/* *** TreeStore *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_tree_store_get_type(value dummy) { /* ML */
    return Val_int(gtk_tree_store_get_type());
}

/* ML type: int -> cptr */
EXTERNML value mgtk_gtk_tree_store_new(value n_columns) { /* ML */
    return Val_GtkObj(gtk_tree_store_new(Int_val(n_columns)));
}

/* ML type: int -> GType.t list -> cptr */
EXTERNML value mgtk_gtk_tree_store_newv(value n_columns, value types_arr) { /* ML */
    GType* types;
    list_to_array(GType, types, Int_val, types_arr);
    return Val_GtkObj(gtk_tree_store_newv(Int_val(n_columns), types));
}

/* ML type: cptr -> cptr -> int -> GValue.GValue -> unit */
EXTERNML value mgtk_gtk_tree_store_set_value(value self, value iter, value column, value valu) { /* ML */
    gtk_tree_store_set_value(GtkObj_val(self), GtkTreeIter_val(iter), Int_val(column), GValue_val(valu));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_store_set(value self, value iter) { /* ML */
    gtk_tree_store_set(GtkObj_val(self), GtkTreeIter_val(iter));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr */
EXTERNML value mgtk_gtk_tree_store_remove(value self, value iter_in) { /* ML */
    GtkTreeIter iter = *((GtkTreeIter*) GtkTreeIter_val(iter_in));
    gtk_tree_store_remove(GtkObj_val(self), &iter);
    return Val_GtkTreeIter(&iter);
}

/* ML type: cptr -> cptr -> int -> unit */
EXTERNML value mgtk_gtk_tree_store_insert(value self, value parent, value position) { /* ML */
    GtkTreeIter iter;
    gtk_tree_store_insert(GtkObj_val(self), &iter, GtkTreeIter_val(parent), Int_val(position));
    return Val_GtkTreeIter(&iter);
}

/* ML type: cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_store_insert_before(value self, value parent, value sibling) { /* ML */
    GtkTreeIter iter;
    gtk_tree_store_insert_before(GtkObj_val(self), &iter, GtkTreeIter_val(parent), GtkTreeIter_val(sibling));
    return Val_GtkTreeIter(&iter);
}

/* ML type: cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_store_insert_after(value self, value parent, value sibling) { /* ML */
    GtkTreeIter iter;
    gtk_tree_store_insert_after(GtkObj_val(self), &iter, GtkTreeIter_val(parent), GtkTreeIter_val(sibling));
    return Val_GtkTreeIter(&iter);
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_store_prepend(value self, value parent) { /* ML */
    GtkTreeIter iter;
    gtk_tree_store_prepend(GtkObj_val(self), &iter, GtkTreeIter_val(parent));
    return Val_GtkTreeIter(&iter);
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_store_append(value self, value parent) { /* ML */
    GtkTreeIter iter;
    gtk_tree_store_append(GtkObj_val(self), &iter, GtkTreeIter_val(parent));
    return Val_GtkTreeIter(&iter);
}

/* ML type: cptr -> cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_tree_store_is_ancestor(value self, value iter, value descendant) { /* ML */
    return Val_bool(gtk_tree_store_is_ancestor(GtkObj_val(self), GtkTreeIter_val(iter), GtkTreeIter_val(descendant)));
}

/* ML type: cptr -> cptr -> int */
EXTERNML value mgtk_gtk_tree_store_iter_depth(value self, value iter) { /* ML */
    return Val_int(gtk_tree_store_iter_depth(GtkObj_val(self), GtkTreeIter_val(iter)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_tree_store_clear(value self) { /* ML */
    gtk_tree_store_clear(GtkObj_val(self));
    return Val_unit;
}



/* *** TreeViewColumn *** */
/* ML type: unit -> int * int * int */
EXTERNML value mgtk_get_gtk_treeviewcolumn_sizing(value dummy) { /* ML */
    value res = alloc_tuple(3);
    Field(res, 0) = Val_int(GTK_TREE_VIEW_COLUMN_GROW_ONLY);
    Field(res, 1) = Val_int(GTK_TREE_VIEW_COLUMN_AUTOSIZE);
    Field(res, 2) = Val_int(GTK_TREE_VIEW_COLUMN_FIXED);
    return res;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_tree_view_column_get_type(value dummy) { /* ML */
    return Val_int(gtk_tree_view_column_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_tree_view_column_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_tree_view_column_new());
}

/* ML type: string -> cptr -> cptr */
EXTERNML value mgtk_gtk_tree_view_column_new_with_attributes(value title, value cell) { /* ML */
    return Val_GtkObj(gtk_tree_view_column_new_with_attributes(String_val(title), GtkObj_val(cell)));
}

/* ML type: cptr -> cptr -> bool -> unit */
EXTERNML value mgtk_gtk_tree_view_column_pack_start(value self, value cell, value expand) { /* ML */
    gtk_tree_view_column_pack_start(GtkObj_val(self), GtkObj_val(cell), Bool_val(expand));
    return Val_unit;
}

/* ML type: cptr -> cptr -> bool -> unit */
EXTERNML value mgtk_gtk_tree_view_column_pack_end(value self, value cell, value expand) { /* ML */
    gtk_tree_view_column_pack_end(GtkObj_val(self), GtkObj_val(cell), Bool_val(expand));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_tree_view_column_clear(value self) { /* ML */
    gtk_tree_view_column_clear(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> cptr -> string -> int -> unit */
EXTERNML value mgtk_gtk_tree_view_column_add_attribute(value self, value cell_renderer, value attribute, value column) { /* ML */
    gtk_tree_view_column_add_attribute(GtkObj_val(self), GtkObj_val(cell_renderer), String_val(attribute), Int_val(column));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_view_column_set_attributes(value self, value cell_renderer) { /* ML */
    gtk_tree_view_column_set_attributes(GtkObj_val(self), GtkObj_val(cell_renderer));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_view_column_clear_attributes(value self, value cell_renderer) { /* ML */
    gtk_tree_view_column_clear_attributes(GtkObj_val(self), GtkObj_val(cell_renderer));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_tree_view_column_set_spacing(value self, value spacing) { /* ML */
    gtk_tree_view_column_set_spacing(GtkObj_val(self), Int_val(spacing));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_tree_view_column_get_spacing(value self) { /* ML */
    return Val_int(gtk_tree_view_column_get_spacing(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_tree_view_column_set_visible(value self, value visible) { /* ML */
    gtk_tree_view_column_set_visible(GtkObj_val(self), Bool_val(visible));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_tree_view_column_get_visible(value self) { /* ML */
    return Val_bool(gtk_tree_view_column_get_visible(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_tree_view_column_set_resizable(value self, value resizable) { /* ML */
    gtk_tree_view_column_set_resizable(GtkObj_val(self), Bool_val(resizable));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_tree_view_column_get_resizable(value self) { /* ML */
    return Val_bool(gtk_tree_view_column_get_resizable(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_tree_view_column_set_sizing(value self, value type) { /* ML */
    gtk_tree_view_column_set_sizing(GtkObj_val(self), Int_val(type));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_tree_view_column_get_sizing(value self) { /* ML */
    return Val_int(gtk_tree_view_column_get_sizing(GtkObj_val(self)));
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_tree_view_column_get_width(value self) { /* ML */
    return Val_int(gtk_tree_view_column_get_width(GtkObj_val(self)));
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_tree_view_column_get_fixed_width(value self) { /* ML */
    return Val_int(gtk_tree_view_column_get_fixed_width(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_tree_view_column_set_fixed_width(value self, value fixed_width) { /* ML */
    gtk_tree_view_column_set_fixed_width(GtkObj_val(self), Int_val(fixed_width));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_tree_view_column_set_min_width(value self, value min_width) { /* ML */
    gtk_tree_view_column_set_min_width(GtkObj_val(self), Int_val(min_width));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_tree_view_column_get_min_width(value self) { /* ML */
    return Val_int(gtk_tree_view_column_get_min_width(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_tree_view_column_set_max_width(value self, value max_width) { /* ML */
    gtk_tree_view_column_set_max_width(GtkObj_val(self), Int_val(max_width));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_tree_view_column_get_max_width(value self) { /* ML */
    return Val_int(gtk_tree_view_column_get_max_width(GtkObj_val(self)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_tree_view_column_clicked(value self) { /* ML */
    gtk_tree_view_column_clicked(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_tree_view_column_set_title(value self, value title) { /* ML */
    gtk_tree_view_column_set_title(GtkObj_val(self), String_val(title));
    return Val_unit;
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_tree_view_column_get_title(value self) { /* ML */
    return my_copy_string(gtk_tree_view_column_get_title(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_tree_view_column_set_clickable(value self, value active) { /* ML */
    gtk_tree_view_column_set_clickable(GtkObj_val(self), Bool_val(active));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_tree_view_column_get_clickable(value self) { /* ML */
    return Val_bool(gtk_tree_view_column_get_clickable(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_view_column_set_widget(value self, value widget) { /* ML */
    gtk_tree_view_column_set_widget(GtkObj_val(self), GtkObj_val(widget));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_tree_view_column_get_widget(value self) { /* ML */
    return Val_GtkObj(gtk_tree_view_column_get_widget(GtkObj_val(self)));
}

/* ML type: cptr -> real -> unit */
EXTERNML value mgtk_gtk_tree_view_column_set_alignment(value self, value xalign) { /* ML */
    gtk_tree_view_column_set_alignment(GtkObj_val(self), Double_val(xalign));
    return Val_unit;
}

/* ML type: cptr -> real */
EXTERNML value mgtk_gtk_tree_view_column_get_alignment(value self) { /* ML */
    return copy_double(gtk_tree_view_column_get_alignment(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_tree_view_column_set_reorderable(value self, value reorderable) { /* ML */
    gtk_tree_view_column_set_reorderable(GtkObj_val(self), Bool_val(reorderable));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_tree_view_column_get_reorderable(value self) { /* ML */
    return Val_bool(gtk_tree_view_column_get_reorderable(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_tree_view_column_set_sort_column_id(value self, value sort_column_id) { /* ML */
    gtk_tree_view_column_set_sort_column_id(GtkObj_val(self), Int_val(sort_column_id));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_tree_view_column_get_sort_column_id(value self) { /* ML */
    return Val_int(gtk_tree_view_column_get_sort_column_id(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_tree_view_column_set_sort_indicator(value self, value setting) { /* ML */
    gtk_tree_view_column_set_sort_indicator(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_tree_view_column_get_sort_indicator(value self) { /* ML */
    return Val_bool(gtk_tree_view_column_get_sort_indicator(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_tree_view_column_set_sort_order(value self, value order) { /* ML */
    gtk_tree_view_column_set_sort_order(GtkObj_val(self), Int_val(order));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_tree_view_column_get_sort_order(value self) { /* ML */
    return Val_int(gtk_tree_view_column_get_sort_order(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> cptr -> bool -> bool -> unit */
EXTERNML value mgtk_gtk_tree_view_column_cell_set_cell_data(value self, value tree_model, value iter, value is_expander, value is_expanded) { /* ML */
    gtk_tree_view_column_cell_set_cell_data(GtkObj_val(self), GtkObj_val(tree_model), GtkTreeIter_val(iter), Bool_val(is_expander), Bool_val(is_expanded));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_tree_view_column_cell_is_visible(value self) { /* ML */
    return Val_bool(gtk_tree_view_column_cell_is_visible(GtkObj_val(self)));
}



/* *** Separator *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_separator_get_type(value dummy) { /* ML */
    return Val_int(gtk_separator_get_type());
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_separator_menu_item_get_type(value dummy) { /* ML */
    return Val_int(gtk_separator_menu_item_get_type());
}



/* *** VSeparator *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_vseparator_get_type(value dummy) { /* ML */
    return Val_int(gtk_vseparator_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_vseparator_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_vseparator_new());
}



/* *** HSeparator *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_hseparator_get_type(value dummy) { /* ML */
    return Val_int(gtk_hseparator_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_hseparator_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_hseparator_new());
}



/* *** Ruler *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_ruler_get_type(value dummy) { /* ML */
    return Val_int(gtk_ruler_get_type());
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_ruler_set_metric(value self, value metric) { /* ML */
    gtk_ruler_set_metric(GtkObj_val(self), Int_val(metric));
    return Val_unit;
}

/* ML type: cptr -> real -> real -> real -> real -> unit */
EXTERNML value mgtk_gtk_ruler_set_range(value self, value lower, value upper, value position, value max_size) { /* ML */
    gtk_ruler_set_range(GtkObj_val(self), Double_val(lower), Double_val(upper), Double_val(position), Double_val(max_size));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_ruler_draw_ticks(value self) { /* ML */
    gtk_ruler_draw_ticks(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_ruler_draw_pos(value self) { /* ML */
    gtk_ruler_draw_pos(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_ruler_get_metric(value self) { /* ML */
    return Val_int(gtk_ruler_get_metric(GtkObj_val(self)));
}



/* *** VRuler *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_vruler_get_type(value dummy) { /* ML */
    return Val_int(gtk_vruler_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_vruler_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_vruler_new());
}



/* *** HRuler *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_hruler_get_type(value dummy) { /* ML */
    return Val_int(gtk_hruler_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_hruler_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_hruler_new());
}



/* *** Range *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_range_get_type(value dummy) { /* ML */
    return Val_int(gtk_range_get_type());
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_range_set_update_policy(value self, value policy) { /* ML */
    gtk_range_set_update_policy(GtkObj_val(self), Int_val(policy));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_range_get_update_policy(value self) { /* ML */
    return Val_int(gtk_range_get_update_policy(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_range_set_adjustment(value self, value adjustment) { /* ML */
    gtk_range_set_adjustment(GtkObj_val(self), GtkObj_val(adjustment));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_range_get_adjustment(value self) { /* ML */
    return Val_GtkObj(gtk_range_get_adjustment(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_range_set_inverted(value self, value setting) { /* ML */
    gtk_range_set_inverted(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_range_get_inverted(value self) { /* ML */
    return Val_bool(gtk_range_get_inverted(GtkObj_val(self)));
}

/* ML type: cptr -> real -> real -> unit */
EXTERNML value mgtk_gtk_range_set_increments(value self, value step, value page) { /* ML */
    gtk_range_set_increments(GtkObj_val(self), Double_val(step), Double_val(page));
    return Val_unit;
}

/* ML type: cptr -> real -> real -> unit */
EXTERNML value mgtk_gtk_range_set_range(value self, value min, value max) { /* ML */
    gtk_range_set_range(GtkObj_val(self), Double_val(min), Double_val(max));
    return Val_unit;
}

/* ML type: cptr -> real -> unit */
EXTERNML value mgtk_gtk_range_set_value(value self, value valu) { /* ML */
    gtk_range_set_value(GtkObj_val(self), Double_val(valu));
    return Val_unit;
}

/* ML type: cptr -> real */
EXTERNML value mgtk_gtk_range_get_value(value self) { /* ML */
    return copy_double(gtk_range_get_value(GtkObj_val(self)));
}



/* *** Scrollbar *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_scrollbar_get_type(value dummy) { /* ML */
    return Val_int(gtk_scrollbar_get_type());
}



/* *** VScrollbar *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_vscrollbar_get_type(value dummy) { /* ML */
    return Val_int(gtk_vscrollbar_get_type());
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_vscrollbar_new(value adjustment) { /* ML */
    return Val_GtkObj(gtk_vscrollbar_new(GtkObj_val(adjustment)));
}



/* *** HScrollbar *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_hscrollbar_get_type(value dummy) { /* ML */
    return Val_int(gtk_hscrollbar_get_type());
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_hscrollbar_new(value adjustment) { /* ML */
    return Val_GtkObj(gtk_hscrollbar_new(GtkObj_val(adjustment)));
}



/* *** Scale *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_scale_get_type(value dummy) { /* ML */
    return Val_int(gtk_scale_get_type());
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_scale_set_digits(value self, value digits) { /* ML */
    gtk_scale_set_digits(GtkObj_val(self), Int_val(digits));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_scale_get_digits(value self) { /* ML */
    return Val_int(gtk_scale_get_digits(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_scale_set_draw_value(value self, value draw_value) { /* ML */
    gtk_scale_set_draw_value(GtkObj_val(self), Bool_val(draw_value));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_scale_get_draw_value(value self) { /* ML */
    return Val_bool(gtk_scale_get_draw_value(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_scale_set_value_pos(value self, value pos) { /* ML */
    gtk_scale_set_value_pos(GtkObj_val(self), Int_val(pos));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_scale_get_value_pos(value self) { /* ML */
    return Val_int(gtk_scale_get_value_pos(GtkObj_val(self)));
}



/* *** VScale *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_vscale_get_type(value dummy) { /* ML */
    return Val_int(gtk_vscale_get_type());
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_vscale_new(value adjustment) { /* ML */
    return Val_GtkObj(gtk_vscale_new(GtkObj_val(adjustment)));
}

/* ML type: real -> real -> real -> cptr */
EXTERNML value mgtk_gtk_vscale_new_with_range(value min, value max, value step) { /* ML */
    return Val_GtkObj(gtk_vscale_new_with_range(Double_val(min), Double_val(max), Double_val(step)));
}



/* *** HScale *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_hscale_get_type(value dummy) { /* ML */
    return Val_int(gtk_hscale_get_type());
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_hscale_new(value adjustment) { /* ML */
    return Val_GtkObj(gtk_hscale_new(GtkObj_val(adjustment)));
}

/* ML type: real -> real -> real -> cptr */
EXTERNML value mgtk_gtk_hscale_new_with_range(value min, value max, value step) { /* ML */
    return Val_GtkObj(gtk_hscale_new_with_range(Double_val(min), Double_val(max), Double_val(step)));
}



/* *** Progress *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_progress_get_type(value dummy) { /* ML */
    return Val_int(gtk_progress_get_type());
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_progress_set_show_text(value self, value show_text) { /* ML */
    gtk_progress_set_show_text(GtkObj_val(self), Bool_val(show_text));
    return Val_unit;
}

/* ML type: cptr -> real -> real -> unit */
EXTERNML value mgtk_gtk_progress_set_text_alignment(value self, value x_align, value y_align) { /* ML */
    gtk_progress_set_text_alignment(GtkObj_val(self), Double_val(x_align), Double_val(y_align));
    return Val_unit;
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_progress_set_format_string(value self, value format) { /* ML */
    gtk_progress_set_format_string(GtkObj_val(self), String_val(format));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_progress_set_adjustment(value self, value adjustment) { /* ML */
    gtk_progress_set_adjustment(GtkObj_val(self), GtkObj_val(adjustment));
    return Val_unit;
}

/* ML type: cptr -> real -> real -> real -> unit */
EXTERNML value mgtk_gtk_progress_configure(value self, value valu, value min, value max) { /* ML */
    gtk_progress_configure(GtkObj_val(self), Double_val(valu), Double_val(min), Double_val(max));
    return Val_unit;
}

/* ML type: cptr -> real -> unit */
EXTERNML value mgtk_gtk_progress_set_percentage(value self, value percentage) { /* ML */
    gtk_progress_set_percentage(GtkObj_val(self), Double_val(percentage));
    return Val_unit;
}

/* ML type: cptr -> real -> unit */
EXTERNML value mgtk_gtk_progress_set_value(value self, value valu) { /* ML */
    gtk_progress_set_value(GtkObj_val(self), Double_val(valu));
    return Val_unit;
}

/* ML type: cptr -> real */
EXTERNML value mgtk_gtk_progress_get_value(value self) { /* ML */
    return copy_double(gtk_progress_get_value(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_progress_set_activity_mode(value self, value activity_mode) { /* ML */
    gtk_progress_set_activity_mode(GtkObj_val(self), Bool_val(activity_mode));
    return Val_unit;
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_progress_get_current_text(value self) { /* ML */
    return my_copy_string(gtk_progress_get_current_text(GtkObj_val(self)));
}

/* ML type: cptr -> real -> string */
EXTERNML value mgtk_gtk_progress_get_text_from_value(value self, value valu) { /* ML */
    return my_copy_string(gtk_progress_get_text_from_value(GtkObj_val(self), Double_val(valu)));
}

/* ML type: cptr -> real */
EXTERNML value mgtk_gtk_progress_get_current_percentage(value self) { /* ML */
    return copy_double(gtk_progress_get_current_percentage(GtkObj_val(self)));
}

/* ML type: cptr -> real -> real */
EXTERNML value mgtk_gtk_progress_get_percentage_from_value(value self, value valu) { /* ML */
    return copy_double(gtk_progress_get_percentage_from_value(GtkObj_val(self), Double_val(valu)));
}



/* *** ProgressBar *** */
/* ML type: unit -> int * int */
EXTERNML value mgtk_get_gtk_progressbar_style(value dummy) { /* ML */
    value res = alloc_tuple(2);
    Field(res, 0) = Val_int(GTK_PROGRESS_CONTINUOUS);
    Field(res, 1) = Val_int(GTK_PROGRESS_DISCRETE);
    return res;
}

/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_progressbar_orientation(value dummy) { /* ML */
    value res = alloc_tuple(4);
    Field(res, 0) = Val_int(GTK_PROGRESS_LEFT_TO_RIGHT);
    Field(res, 1) = Val_int(GTK_PROGRESS_RIGHT_TO_LEFT);
    Field(res, 2) = Val_int(GTK_PROGRESS_BOTTOM_TO_TOP);
    Field(res, 3) = Val_int(GTK_PROGRESS_TOP_TO_BOTTOM);
    return res;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_progress_bar_get_type(value dummy) { /* ML */
    return Val_int(gtk_progress_bar_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_progress_bar_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_progress_bar_new());
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_progress_bar_pulse(value self) { /* ML */
    gtk_progress_bar_pulse(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_progress_bar_set_text(value self, value text) { /* ML */
    gtk_progress_bar_set_text(GtkObj_val(self), String_val(text));
    return Val_unit;
}

/* ML type: cptr -> real -> unit */
EXTERNML value mgtk_gtk_progress_bar_set_fraction(value self, value fraction) { /* ML */
    gtk_progress_bar_set_fraction(GtkObj_val(self), Double_val(fraction));
    return Val_unit;
}

/* ML type: cptr -> real -> unit */
EXTERNML value mgtk_gtk_progress_bar_set_pulse_step(value self, value fraction) { /* ML */
    gtk_progress_bar_set_pulse_step(GtkObj_val(self), Double_val(fraction));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_progress_bar_set_orientation(value self, value orientation) { /* ML */
    gtk_progress_bar_set_orientation(GtkObj_val(self), Int_val(orientation));
    return Val_unit;
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_progress_bar_get_text(value self) { /* ML */
    return my_copy_string(gtk_progress_bar_get_text(GtkObj_val(self)));
}

/* ML type: cptr -> real */
EXTERNML value mgtk_gtk_progress_bar_get_fraction(value self) { /* ML */
    return copy_double(gtk_progress_bar_get_fraction(GtkObj_val(self)));
}

/* ML type: cptr -> real */
EXTERNML value mgtk_gtk_progress_bar_get_pulse_step(value self) { /* ML */
    return copy_double(gtk_progress_bar_get_pulse_step(GtkObj_val(self)));
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_progress_bar_get_orientation(value self) { /* ML */
    return Val_int(gtk_progress_bar_get_orientation(GtkObj_val(self)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_progress_bar_new_with_adjustment(value adjustment) { /* ML */
    return Val_GtkObj(gtk_progress_bar_new_with_adjustment(GtkObj_val(adjustment)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_progress_bar_set_bar_style(value self, value style) { /* ML */
    gtk_progress_bar_set_bar_style(GtkObj_val(self), Int_val(style));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_progress_bar_set_discrete_blocks(value self, value blocks) { /* ML */
    gtk_progress_bar_set_discrete_blocks(GtkObj_val(self), Int_val(blocks));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_progress_bar_set_activity_step(value self, value step) { /* ML */
    gtk_progress_bar_set_activity_step(GtkObj_val(self), Int_val(step));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_progress_bar_set_activity_blocks(value self, value blocks) { /* ML */
    gtk_progress_bar_set_activity_blocks(GtkObj_val(self), Int_val(blocks));
    return Val_unit;
}

/* ML type: cptr -> real -> unit */
EXTERNML value mgtk_gtk_progress_bar_update(value self, value percentage) { /* ML */
    gtk_progress_bar_update(GtkObj_val(self), Double_val(percentage));
    return Val_unit;
}



/* *** Preview *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_preview_get_type(value dummy) { /* ML */
    return Val_int(gtk_preview_get_type());
}

/* ML type: unit -> unit */
EXTERNML value mgtk_gtk_preview_uninit(value dummy) { /* ML */
    gtk_preview_uninit();
    return Val_unit;
}

/* ML type: int -> cptr */
EXTERNML value mgtk_gtk_preview_new(value type) { /* ML */
    return Val_GtkObj(gtk_preview_new(Int_val(type)));
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_preview_size(value self, value width, value height) { /* ML */
    gtk_preview_size(GtkObj_val(self), Int_val(width), Int_val(height));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_preview_set_expand(value self, value expand) { /* ML */
    gtk_preview_set_expand(GtkObj_val(self), Bool_val(expand));
    return Val_unit;
}

/* ML type: real -> unit */
EXTERNML value mgtk_gtk_preview_set_gamma(value gamma) { /* ML */
    gtk_preview_set_gamma(Double_val(gamma));
    return Val_unit;
}

/* ML type: int -> int -> int -> int -> unit */
EXTERNML value mgtk_gtk_preview_set_color_cube(value nred_shades, value ngreen_shades, value nblue_shades, value ngray_shades) { /* ML */
    gtk_preview_set_color_cube(Int_val(nred_shades), Int_val(ngreen_shades), Int_val(nblue_shades), Int_val(ngray_shades));
    return Val_unit;
}

/* ML type: int -> unit */
EXTERNML value mgtk_gtk_preview_set_install_cmap(value install_cmap) { /* ML */
    gtk_preview_set_install_cmap(Int_val(install_cmap));
    return Val_unit;
}

/* ML type: int -> unit */
EXTERNML value mgtk_gtk_preview_set_reserved(value nreserved) { /* ML */
    gtk_preview_set_reserved(Int_val(nreserved));
    return Val_unit;
}

/* ML type: unit -> unit */
EXTERNML value mgtk_gtk_preview_reset(value dummy) { /* ML */
    gtk_preview_reset();
    return Val_unit;
}



/* *** OldEditable *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_old_editable_get_type(value dummy) { /* ML */
    return Val_int(gtk_old_editable_get_type());
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_old_editable_changed(value self) { /* ML */
    gtk_old_editable_changed(GtkObj_val(self));
    return Val_unit;
}



/* *** Misc *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_misc_get_type(value dummy) { /* ML */
    return Val_int(gtk_misc_get_type());
}

/* ML type: cptr -> real -> real -> unit */
EXTERNML value mgtk_gtk_misc_set_alignment(value self, value xalign, value yalign) { /* ML */
    gtk_misc_set_alignment(GtkObj_val(self), Double_val(xalign), Double_val(yalign));
    return Val_unit;
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_misc_set_padding(value self, value xpad, value ypad) { /* ML */
    gtk_misc_set_padding(GtkObj_val(self), Int_val(xpad), Int_val(ypad));
    return Val_unit;
}



/* *** Pixmap *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_pixmap_get_type(value dummy) { /* ML */
    return Val_int(gtk_pixmap_get_type());
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_pixmap_set_build_insensitive(value self, value build) { /* ML */
    gtk_pixmap_set_build_insensitive(GtkObj_val(self), Bool_val(build));
    return Val_unit;
}



/* *** Arrow *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_arrow_get_type(value dummy) { /* ML */
    return Val_int(gtk_arrow_get_type());
}

/* ML type: int -> int -> cptr */
EXTERNML value mgtk_gtk_arrow_new(value arrow_type, value shadow_type) { /* ML */
    return Val_GtkObj(gtk_arrow_new(Int_val(arrow_type), Int_val(shadow_type)));
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_arrow_set(value self, value arrow_type, value shadow_type) { /* ML */
    gtk_arrow_set(GtkObj_val(self), Int_val(arrow_type), Int_val(shadow_type));
    return Val_unit;
}



/* *** Image *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_image_get_type(value dummy) { /* ML */
    return Val_int(gtk_image_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_image_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_image_new());
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_image_new_from_file(value filename) { /* ML */
    return Val_GtkObj(gtk_image_new_from_file(String_val(filename)));
}

/* ML type: string -> int -> cptr */
EXTERNML value mgtk_gtk_image_new_from_stock(value stock_id, value size) { /* ML */
    return Val_GtkObj(gtk_image_new_from_stock(String_val(stock_id), Int_val(size)));
}

/* ML type: cptr -> int -> cptr */
EXTERNML value mgtk_gtk_image_new_from_icon_set(value icon_set, value size) { /* ML */
    return Val_GtkObj(gtk_image_new_from_icon_set(GtkIconSet_val(icon_set), Int_val(size)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_image_set_from_file(value self, value filename) { /* ML */
    gtk_image_set_from_file(GtkObj_val(self), String_val(filename));
    return Val_unit;
}

/* ML type: cptr -> string -> int -> unit */
EXTERNML value mgtk_gtk_image_set_from_stock(value self, value stock_id, value size) { /* ML */
    gtk_image_set_from_stock(GtkObj_val(self), String_val(stock_id), Int_val(size));
    return Val_unit;
}

/* ML type: cptr -> cptr -> int -> unit */
EXTERNML value mgtk_gtk_image_set_from_icon_set(value self, value icon_set, value size) { /* ML */
    gtk_image_set_from_icon_set(GtkObj_val(self), GtkIconSet_val(icon_set), Int_val(size));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_image_get_storage_type(value self) { /* ML */
    return Val_int(gtk_image_get_storage_type(GtkObj_val(self)));
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_image_menu_item_get_type(value dummy) { /* ML */
    return Val_int(gtk_image_menu_item_get_type());
}



/* *** Label *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_label_get_type(value dummy) { /* ML */
    return Val_int(gtk_label_get_type());
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_label_new(value str) { /* ML */
    return Val_GtkObj(gtk_label_new(String_val(str)));
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_label_new_with_mnemonic(value str) { /* ML */
    return Val_GtkObj(gtk_label_new_with_mnemonic(String_val(str)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_label_set_text(value self, value str) { /* ML */
    gtk_label_set_text(GtkObj_val(self), String_val(str));
    return Val_unit;
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_label_get_text(value self) { /* ML */
    return my_copy_string(gtk_label_get_text(GtkObj_val(self)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_label_set_label(value self, value str) { /* ML */
    gtk_label_set_label(GtkObj_val(self), String_val(str));
    return Val_unit;
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_label_get_label(value self) { /* ML */
    return my_copy_string(gtk_label_get_label(GtkObj_val(self)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_label_set_markup(value self, value str) { /* ML */
    gtk_label_set_markup(GtkObj_val(self), String_val(str));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_label_set_use_markup(value self, value setting) { /* ML */
    gtk_label_set_use_markup(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_label_get_use_markup(value self) { /* ML */
    return Val_bool(gtk_label_get_use_markup(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_label_set_use_underline(value self, value setting) { /* ML */
    gtk_label_set_use_underline(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_label_get_use_underline(value self) { /* ML */
    return Val_bool(gtk_label_get_use_underline(GtkObj_val(self)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_label_set_markup_with_mnemonic(value self, value str) { /* ML */
    gtk_label_set_markup_with_mnemonic(GtkObj_val(self), String_val(str));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_label_get_mnemonic_keyval(value self) { /* ML */
    return Val_int(gtk_label_get_mnemonic_keyval(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_label_set_mnemonic_widget(value self, value widget) { /* ML */
    gtk_label_set_mnemonic_widget(GtkObj_val(self), GtkObj_val(widget));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_label_get_mnemonic_widget(value self) { /* ML */
    return Val_GtkObj(gtk_label_get_mnemonic_widget(GtkObj_val(self)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_label_set_text_with_mnemonic(value self, value str) { /* ML */
    gtk_label_set_text_with_mnemonic(GtkObj_val(self), String_val(str));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_label_set_justify(value self, value jtype) { /* ML */
    gtk_label_set_justify(GtkObj_val(self), Int_val(jtype));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_label_get_justify(value self) { /* ML */
    return Val_int(gtk_label_get_justify(GtkObj_val(self)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_label_set_pattern(value self, value pattern) { /* ML */
    gtk_label_set_pattern(GtkObj_val(self), String_val(pattern));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_label_set_line_wrap(value self, value wrap) { /* ML */
    gtk_label_set_line_wrap(GtkObj_val(self), Bool_val(wrap));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_label_get_line_wrap(value self) { /* ML */
    return Val_bool(gtk_label_get_line_wrap(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_label_set_selectable(value self, value setting) { /* ML */
    gtk_label_set_selectable(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_label_get_selectable(value self) { /* ML */
    return Val_bool(gtk_label_get_selectable(GtkObj_val(self)));
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_label_select_region(value self, value start_offset, value end_offset) { /* ML */
    gtk_label_select_region(GtkObj_val(self), Int_val(start_offset), Int_val(end_offset));
    return Val_unit;
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_label_set(value self, value str) { /* ML */
    gtk_label_set(GtkObj_val(self), String_val(str));
    return Val_unit;
}

/* ML type: cptr -> string -> int */
EXTERNML value mgtk_gtk_label_parse_uline(value self, value string) { /* ML */
    return Val_int(gtk_label_parse_uline(GtkObj_val(self), String_val(string)));
}



/* *** AccelLabel *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_accel_label_get_type(value dummy) { /* ML */
    return Val_int(gtk_accel_label_get_type());
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_accel_label_new(value string) { /* ML */
    return Val_GtkObj(gtk_accel_label_new(String_val(string)));
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_accel_label_accelerator_width(value self) { /* ML */
    return Val_int(gtk_accel_label_accelerator_width(GtkObj_val(self)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_accel_label_get_accel_widget(value self) { /* ML */
    return Val_GtkObj(gtk_accel_label_get_accel_widget(GtkObj_val(self)));
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_accel_label_get_accel_width(value self) { /* ML */
    return Val_int(gtk_accel_label_get_accel_width(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_accel_label_set_accel_widget(value self, value accel_widget) { /* ML */
    gtk_accel_label_set_accel_widget(GtkObj_val(self), GtkObj_val(accel_widget));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_accel_label_refetch(value self) { /* ML */
    return Val_bool(gtk_accel_label_refetch(GtkObj_val(self)));
}



/* *** Invisible *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_invisible_get_type(value dummy) { /* ML */
    return Val_int(gtk_invisible_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_invisible_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_invisible_new());
}



/* *** Entry *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_entry_get_type(value dummy) { /* ML */
    return Val_int(gtk_entry_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_entry_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_entry_new());
}

/* ML type: int -> cptr */
EXTERNML value mgtk_gtk_entry_new_with_max_length(value max) { /* ML */
    return Val_GtkObj(gtk_entry_new_with_max_length(Int_val(max)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_entry_set_visibility(value self, value visible) { /* ML */
    gtk_entry_set_visibility(GtkObj_val(self), Bool_val(visible));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_entry_get_visibility(value self) { /* ML */
    return Val_bool(gtk_entry_get_visibility(GtkObj_val(self)));
}

/* ML type: cptr -> char -> unit */
EXTERNML value mgtk_gtk_entry_set_invisible_char(value self, value ch) { /* ML */
    gtk_entry_set_invisible_char(GtkObj_val(self), Long_val(ch));
    return Val_unit;
}

/* ML type: cptr -> char */
EXTERNML value mgtk_gtk_entry_get_invisible_char(value self) { /* ML */
    return Val_long(gtk_entry_get_invisible_char(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_entry_set_has_frame(value self, value setting) { /* ML */
    gtk_entry_set_has_frame(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_entry_get_has_frame(value self) { /* ML */
    return Val_bool(gtk_entry_get_has_frame(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_entry_set_max_length(value self, value max) { /* ML */
    gtk_entry_set_max_length(GtkObj_val(self), Int_val(max));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_entry_get_max_length(value self) { /* ML */
    return Val_int(gtk_entry_get_max_length(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_entry_set_activates_default(value self, value setting) { /* ML */
    gtk_entry_set_activates_default(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_entry_get_activates_default(value self) { /* ML */
    return Val_bool(gtk_entry_get_activates_default(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_entry_set_width_chars(value self, value n_chars) { /* ML */
    gtk_entry_set_width_chars(GtkObj_val(self), Int_val(n_chars));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_entry_get_width_chars(value self) { /* ML */
    return Val_int(gtk_entry_get_width_chars(GtkObj_val(self)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_entry_set_text(value self, value text) { /* ML */
    gtk_entry_set_text(GtkObj_val(self), String_val(text));
    return Val_unit;
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_entry_get_text(value self) { /* ML */
    return my_copy_string(gtk_entry_get_text(GtkObj_val(self)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_entry_append_text(value self, value text) { /* ML */
    gtk_entry_append_text(GtkObj_val(self), String_val(text));
    return Val_unit;
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_entry_prepend_text(value self, value text) { /* ML */
    gtk_entry_prepend_text(GtkObj_val(self), String_val(text));
    return Val_unit;
}



/* *** SpinButton *** */
/* ML type: unit -> int * int */
EXTERNML value mgtk_get_gtk_spin_button_update_policy(value dummy) { /* ML */
    value res = alloc_tuple(2);
    Field(res, 0) = Val_int(GTK_UPDATE_ALWAYS);
    Field(res, 1) = Val_int(GTK_UPDATE_IF_VALID);
    return res;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_spin_button_get_type(value dummy) { /* ML */
    return Val_int(gtk_spin_button_get_type());
}

/* ML type: cptr -> cptr -> real -> int -> unit */
EXTERNML value mgtk_gtk_spin_button_configure(value self, value adjustment, value climb_rate, value digits) { /* ML */
    gtk_spin_button_configure(GtkObj_val(self), GtkObj_val(adjustment), Double_val(climb_rate), Int_val(digits));
    return Val_unit;
}

/* ML type: cptr -> real -> int -> cptr */
EXTERNML value mgtk_gtk_spin_button_new(value adjustment, value climb_rate, value digits) { /* ML */
    return Val_GtkObj(gtk_spin_button_new(GtkObj_val(adjustment), Double_val(climb_rate), Int_val(digits)));
}

/* ML type: real -> real -> real -> cptr */
EXTERNML value mgtk_gtk_spin_button_new_with_range(value min, value max, value step) { /* ML */
    return Val_GtkObj(gtk_spin_button_new_with_range(Double_val(min), Double_val(max), Double_val(step)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_spin_button_set_adjustment(value self, value adjustment) { /* ML */
    gtk_spin_button_set_adjustment(GtkObj_val(self), GtkObj_val(adjustment));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_spin_button_get_adjustment(value self) { /* ML */
    return Val_GtkObj(gtk_spin_button_get_adjustment(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_spin_button_set_digits(value self, value digits) { /* ML */
    gtk_spin_button_set_digits(GtkObj_val(self), Int_val(digits));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_spin_button_get_digits(value self) { /* ML */
    return Val_int(gtk_spin_button_get_digits(GtkObj_val(self)));
}

/* ML type: cptr -> real -> real -> unit */
EXTERNML value mgtk_gtk_spin_button_set_increments(value self, value step, value page) { /* ML */
    gtk_spin_button_set_increments(GtkObj_val(self), Double_val(step), Double_val(page));
    return Val_unit;
}

/* ML type: cptr -> real -> real -> unit */
EXTERNML value mgtk_gtk_spin_button_set_range(value self, value min, value max) { /* ML */
    gtk_spin_button_set_range(GtkObj_val(self), Double_val(min), Double_val(max));
    return Val_unit;
}

/* ML type: cptr -> real */
EXTERNML value mgtk_gtk_spin_button_get_value(value self) { /* ML */
    return copy_double(gtk_spin_button_get_value(GtkObj_val(self)));
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_spin_button_get_value_as_int(value self) { /* ML */
    return Val_int(gtk_spin_button_get_value_as_int(GtkObj_val(self)));
}

/* ML type: cptr -> real -> unit */
EXTERNML value mgtk_gtk_spin_button_set_value(value self, value valu) { /* ML */
    gtk_spin_button_set_value(GtkObj_val(self), Double_val(valu));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_spin_button_set_update_policy(value self, value policy) { /* ML */
    gtk_spin_button_set_update_policy(GtkObj_val(self), Int_val(policy));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_spin_button_get_update_policy(value self) { /* ML */
    return Val_int(gtk_spin_button_get_update_policy(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_spin_button_set_numeric(value self, value numeric) { /* ML */
    gtk_spin_button_set_numeric(GtkObj_val(self), Bool_val(numeric));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_spin_button_get_numeric(value self) { /* ML */
    return Val_bool(gtk_spin_button_get_numeric(GtkObj_val(self)));
}

/* ML type: cptr -> int -> real -> unit */
EXTERNML value mgtk_gtk_spin_button_spin(value self, value direction, value increment) { /* ML */
    gtk_spin_button_spin(GtkObj_val(self), Int_val(direction), Double_val(increment));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_spin_button_set_wrap(value self, value wrap) { /* ML */
    gtk_spin_button_set_wrap(GtkObj_val(self), Bool_val(wrap));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_spin_button_get_wrap(value self) { /* ML */
    return Val_bool(gtk_spin_button_get_wrap(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_spin_button_set_snap_to_ticks(value self, value snap_to_ticks) { /* ML */
    gtk_spin_button_set_snap_to_ticks(GtkObj_val(self), Bool_val(snap_to_ticks));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_spin_button_get_snap_to_ticks(value self) { /* ML */
    return Val_bool(gtk_spin_button_get_snap_to_ticks(GtkObj_val(self)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_spin_button_update(value self) { /* ML */
    gtk_spin_button_update(GtkObj_val(self));
    return Val_unit;
}



/* *** DrawingArea *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_drawing_area_get_type(value dummy) { /* ML */
    return Val_int(gtk_drawing_area_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_drawing_area_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_drawing_area_new());
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_drawing_area_size(value self, value width, value height) { /* ML */
    gtk_drawing_area_size(GtkObj_val(self), Int_val(width), Int_val(height));
    return Val_unit;
}



/* *** Curve *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_curve_get_type(value dummy) { /* ML */
    return Val_int(gtk_curve_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_curve_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_curve_new());
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_curve_reset(value self) { /* ML */
    gtk_curve_reset(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> real -> unit */
EXTERNML value mgtk_gtk_curve_set_gamma(value self, value gamma) { /* ML */
    gtk_curve_set_gamma(GtkObj_val(self), Double_val(gamma));
    return Val_unit;
}

/* ML type: cptr -> real -> real -> real -> real -> unit */
EXTERNML value mgtk_gtk_curve_set_range(value self, value min_x, value max_x, value min_y, value max_y) { /* ML */
    gtk_curve_set_range(GtkObj_val(self), Double_val(min_x), Double_val(max_x), Double_val(min_y), Double_val(max_y));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_curve_set_curve_type(value self, value type) { /* ML */
    gtk_curve_set_curve_type(GtkObj_val(self), Int_val(type));
    return Val_unit;
}



/* *** Container *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_container_get_type(value dummy) { /* ML */
    return Val_int(gtk_container_get_type());
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_container_set_border_width(value self, value border_width) { /* ML */
    gtk_container_set_border_width(GtkObj_val(self), Int_val(border_width));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_container_get_border_width(value self) { /* ML */
    return Val_int(gtk_container_get_border_width(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_container_add(value self, value widget) { /* ML */
    gtk_container_add(GtkObj_val(self), GtkObj_val(widget));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_container_remove(value self, value widget) { /* ML */
    gtk_container_remove(GtkObj_val(self), GtkObj_val(widget));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_container_set_resize_mode(value self, value resize_mode) { /* ML */
    gtk_container_set_resize_mode(GtkObj_val(self), Int_val(resize_mode));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_container_get_resize_mode(value self) { /* ML */
    return Val_int(gtk_container_get_resize_mode(GtkObj_val(self)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_container_check_resize(value self) { /* ML */
    gtk_container_check_resize(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_container_unset_focus_chain(value self) { /* ML */
    gtk_container_unset_focus_chain(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_container_set_reallocate_redraws(value self, value needs_redraws) { /* ML */
    gtk_container_set_reallocate_redraws(GtkObj_val(self), Bool_val(needs_redraws));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_container_set_focus_child(value self, value child) { /* ML */
    gtk_container_set_focus_child(GtkObj_val(self), GtkObj_val(child));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_container_set_focus_vadjustment(value self, value adjustment) { /* ML */
    gtk_container_set_focus_vadjustment(GtkObj_val(self), GtkObj_val(adjustment));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_container_get_focus_vadjustment(value self) { /* ML */
    return Val_GtkObj(gtk_container_get_focus_vadjustment(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_container_set_focus_hadjustment(value self, value adjustment) { /* ML */
    gtk_container_set_focus_hadjustment(GtkObj_val(self), GtkObj_val(adjustment));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_container_get_focus_hadjustment(value self) { /* ML */
    return Val_GtkObj(gtk_container_get_focus_hadjustment(GtkObj_val(self)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_container_resize_children(value self) { /* ML */
    gtk_container_resize_children(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> GType.t */
EXTERNML value mgtk_gtk_container_child_type(value self) { /* ML */
    return Val_int(gtk_container_child_type(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> string -> unit */
EXTERNML value mgtk_gtk_container_add_with_properties(value self, value widget, value first_prop_name) { /* ML */
    gtk_container_add_with_properties(GtkObj_val(self), GtkObj_val(widget), String_val(first_prop_name));
    return Val_unit;
}

/* ML type: cptr -> cptr -> string -> unit */
EXTERNML value mgtk_gtk_container_child_set(value self, value child, value first_prop_name) { /* ML */
    gtk_container_child_set(GtkObj_val(self), GtkObj_val(child), String_val(first_prop_name));
    return Val_unit;
}

/* ML type: cptr -> cptr -> string -> unit */
EXTERNML value mgtk_gtk_container_child_get(value self, value child, value first_prop_name) { /* ML */
    gtk_container_child_get(GtkObj_val(self), GtkObj_val(child), String_val(first_prop_name));
    return Val_unit;
}

/* ML type: cptr -> cptr -> string -> GValue.GValue -> unit */
EXTERNML value mgtk_gtk_container_child_set_property(value self, value child, value property_name, value valu) { /* ML */
    gtk_container_child_set_property(GtkObj_val(self), GtkObj_val(child), String_val(property_name), GValue_val(valu));
    return Val_unit;
}

/* ML type: cptr -> cptr -> string -> GValue.GValue -> unit */
EXTERNML value mgtk_gtk_container_child_get_property(value self, value child, value property_name, value valu) { /* ML */
    gtk_container_child_get_property(GtkObj_val(self), GtkObj_val(child), String_val(property_name), GValue_val(valu));
    return Val_unit;
}



/* *** TreeView *** */
/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_treeview_drop_position(value dummy) { /* ML */
    value res = alloc_tuple(4);
    Field(res, 0) = Val_int(GTK_TREE_VIEW_DROP_BEFORE);
    Field(res, 1) = Val_int(GTK_TREE_VIEW_DROP_AFTER);
    Field(res, 2) = Val_int(GTK_TREE_VIEW_DROP_INTO_OR_BEFORE);
    Field(res, 3) = Val_int(GTK_TREE_VIEW_DROP_INTO_OR_AFTER);
    return res;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_tree_view_get_type(value dummy) { /* ML */
    return Val_int(gtk_tree_view_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_tree_view_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_tree_view_new());
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_tree_view_new_with_model(value model) { /* ML */
    return Val_GtkObj(gtk_tree_view_new_with_model(GtkObj_val(model)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_tree_view_get_model(value self) { /* ML */
    return Val_GtkObj(gtk_tree_view_get_model(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_view_set_model(value self, value model) { /* ML */
    gtk_tree_view_set_model(GtkObj_val(self), GtkObj_val(model));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_tree_view_get_selection(value self) { /* ML */
    return Val_GtkObj(gtk_tree_view_get_selection(GtkObj_val(self)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_tree_view_get_hadjustment(value self) { /* ML */
    return Val_GtkObj(gtk_tree_view_get_hadjustment(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_view_set_hadjustment(value self, value adjustment) { /* ML */
    gtk_tree_view_set_hadjustment(GtkObj_val(self), GtkObj_val(adjustment));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_tree_view_get_vadjustment(value self) { /* ML */
    return Val_GtkObj(gtk_tree_view_get_vadjustment(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_view_set_vadjustment(value self, value adjustment) { /* ML */
    gtk_tree_view_set_vadjustment(GtkObj_val(self), GtkObj_val(adjustment));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_tree_view_get_headers_visible(value self) { /* ML */
    return Val_bool(gtk_tree_view_get_headers_visible(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_tree_view_set_headers_visible(value self, value headers_visible) { /* ML */
    gtk_tree_view_set_headers_visible(GtkObj_val(self), Bool_val(headers_visible));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_tree_view_columns_autosize(value self) { /* ML */
    gtk_tree_view_columns_autosize(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_tree_view_set_headers_clickable(value self, value active) { /* ML */
    gtk_tree_view_set_headers_clickable(GtkObj_val(self), Bool_val(active));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_tree_view_set_rules_hint(value self, value setting) { /* ML */
    gtk_tree_view_set_rules_hint(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_tree_view_get_rules_hint(value self) { /* ML */
    return Val_bool(gtk_tree_view_get_rules_hint(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> int */
EXTERNML value mgtk_gtk_tree_view_append_column(value self, value column) { /* ML */
    return Val_int(gtk_tree_view_append_column(GtkObj_val(self), GtkObj_val(column)));
}

/* ML type: cptr -> cptr -> int */
EXTERNML value mgtk_gtk_tree_view_remove_column(value self, value column) { /* ML */
    return Val_int(gtk_tree_view_remove_column(GtkObj_val(self), GtkObj_val(column)));
}

/* ML type: cptr -> cptr -> int -> int */
EXTERNML value mgtk_gtk_tree_view_insert_column(value self, value column, value position) { /* ML */
    return Val_int(gtk_tree_view_insert_column(GtkObj_val(self), GtkObj_val(column), Int_val(position)));
}

/* ML type: cptr -> int -> string -> cptr -> int */
EXTERNML value mgtk_gtk_tree_view_insert_column_with_attributes(value self, value position, value title, value cell) { /* ML */
    return Val_int(gtk_tree_view_insert_column_with_attributes(GtkObj_val(self), Int_val(position), String_val(title), GtkObj_val(cell)));
}

/* ML type: cptr -> int -> cptr */
EXTERNML value mgtk_gtk_tree_view_get_column(value self, value n) { /* ML */
    return Val_GtkObj(gtk_tree_view_get_column(GtkObj_val(self), Int_val(n)));
}

/* ML type: cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_view_move_column_after(value self, value column, value base_column) { /* ML */
    gtk_tree_view_move_column_after(GtkObj_val(self), GtkObj_val(column), GtkObj_val(base_column));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_view_set_expander_column(value self, value column) { /* ML */
    gtk_tree_view_set_expander_column(GtkObj_val(self), GtkObj_val(column));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_tree_view_get_expander_column(value self) { /* ML */
    return Val_GtkObj(gtk_tree_view_get_expander_column(GtkObj_val(self)));
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_tree_view_scroll_to_point(value self, value tree_x, value tree_y) { /* ML */
    gtk_tree_view_scroll_to_point(GtkObj_val(self), Int_val(tree_x), Int_val(tree_y));
    return Val_unit;
}

/* ML type: int -> unit */
EXTERNML value mgtk_gtk_tree_view_scroll_to_cell(value mgtk_params) { /* ML */
    value self = Field(mgtk_params, 0);
    value path = Field(mgtk_params, 1);
    value column = Field(mgtk_params, 2);
    value use_align = Field(mgtk_params, 3);
    value row_align = Field(mgtk_params, 4);
    value col_align = Field(mgtk_params, 5);
    gtk_tree_view_scroll_to_cell(GtkObj_val(self), GtkTreePath_val(path), GtkObj_val(column), Bool_val(use_align), Double_val(row_align), Double_val(col_align));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_view_row_activated(value self, value path, value column) { /* ML */
    gtk_tree_view_row_activated(GtkObj_val(self), GtkTreePath_val(path), GtkObj_val(column));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_tree_view_expand_all(value self) { /* ML */
    gtk_tree_view_expand_all(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_tree_view_collapse_all(value self) { /* ML */
    gtk_tree_view_collapse_all(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> cptr -> bool -> unit */
EXTERNML value mgtk_gtk_tree_view_expand_row(value self, value path, value open_all) { /* ML */
    gtk_tree_view_expand_row(GtkObj_val(self), GtkTreePath_val(path), Bool_val(open_all));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_tree_view_collapse_row(value self, value path) { /* ML */
    gtk_tree_view_collapse_row(GtkObj_val(self), GtkTreePath_val(path));
    return Val_unit;
}

/* ML type: cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_tree_view_row_expanded(value self, value path) { /* ML */
    return Val_bool(gtk_tree_view_row_expanded(GtkObj_val(self), GtkTreePath_val(path)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_tree_view_set_reorderable(value self, value reorderable) { /* ML */
    gtk_tree_view_set_reorderable(GtkObj_val(self), Bool_val(reorderable));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_tree_view_get_reorderable(value self) { /* ML */
    return Val_bool(gtk_tree_view_get_reorderable(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> cptr -> bool -> unit */
EXTERNML value mgtk_gtk_tree_view_set_cursor(value self, value path, value focus_column, value start_editing) { /* ML */
    gtk_tree_view_set_cursor(GtkObj_val(self), GtkTreePath_val(path), GtkObj_val(focus_column), Bool_val(start_editing));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_tree_view_unset_rows_drag_source(value self) { /* ML */
    gtk_tree_view_unset_rows_drag_source(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_tree_view_unset_rows_drag_dest(value self) { /* ML */
    gtk_tree_view_unset_rows_drag_dest(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> cptr -> int -> unit */
EXTERNML value mgtk_gtk_tree_view_set_drag_dest_row(value self, value path, value pos) { /* ML */
    gtk_tree_view_set_drag_dest_row(GtkObj_val(self), GtkTreePath_val(path), Int_val(pos));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_tree_view_set_enable_search(value self, value enable_search) { /* ML */
    gtk_tree_view_set_enable_search(GtkObj_val(self), Bool_val(enable_search));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_tree_view_get_enable_search(value self) { /* ML */
    return Val_bool(gtk_tree_view_get_enable_search(GtkObj_val(self)));
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_tree_view_get_search_column(value self) { /* ML */
    return Val_int(gtk_tree_view_get_search_column(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_tree_view_set_search_column(value self, value column) { /* ML */
    gtk_tree_view_set_search_column(GtkObj_val(self), Int_val(column));
    return Val_unit;
}



/* *** Toolbar *** */
/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_toolbar_style(value dummy) { /* ML */
    value res = alloc_tuple(4);
    Field(res, 0) = Val_int(GTK_TOOLBAR_ICONS);
    Field(res, 1) = Val_int(GTK_TOOLBAR_TEXT);
    Field(res, 2) = Val_int(GTK_TOOLBAR_BOTH);
    Field(res, 3) = Val_int(GTK_TOOLBAR_BOTH_HORIZ);
    return res;
}

/* ML type: unit -> int * int * int * int * int */
EXTERNML value mgtk_get_gtk_toolbar_childtype(value dummy) { /* ML */
    value res = alloc_tuple(5);
    Field(res, 0) = Val_int(GTK_TOOLBAR_CHILD_SPACE);
    Field(res, 1) = Val_int(GTK_TOOLBAR_CHILD_BUTTON);
    Field(res, 2) = Val_int(GTK_TOOLBAR_CHILD_TOGGLEBUTTON);
    Field(res, 3) = Val_int(GTK_TOOLBAR_CHILD_RADIOBUTTON);
    Field(res, 4) = Val_int(GTK_TOOLBAR_CHILD_WIDGET);
    return res;
}

/* ML type: unit -> int * int */
EXTERNML value mgtk_get_gtk_toolbar_space_style(value dummy) { /* ML */
    value res = alloc_tuple(2);
    Field(res, 0) = Val_int(GTK_TOOLBAR_SPACE_EMPTY);
    Field(res, 1) = Val_int(GTK_TOOLBAR_SPACE_LINE);
    return res;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_toolbar_get_type(value dummy) { /* ML */
    return Val_int(gtk_toolbar_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_toolbar_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_toolbar_new());
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_toolbar_append_space(value self) { /* ML */
    gtk_toolbar_append_space(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_toolbar_prepend_space(value self) { /* ML */
    gtk_toolbar_prepend_space(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_toolbar_insert_space(value self, value position) { /* ML */
    gtk_toolbar_insert_space(GtkObj_val(self), Int_val(position));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_toolbar_remove_space(value self, value position) { /* ML */
    gtk_toolbar_remove_space(GtkObj_val(self), Int_val(position));
    return Val_unit;
}

/* ML type: cptr -> cptr -> string -> string -> unit */
EXTERNML value mgtk_gtk_toolbar_append_widget(value self, value widget, value tooltip_text, value tooltip_private_text) { /* ML */
    gtk_toolbar_append_widget(GtkObj_val(self), GtkObj_val(widget), String_val(tooltip_text), String_val(tooltip_private_text));
    return Val_unit;
}

/* ML type: cptr -> cptr -> string -> string -> unit */
EXTERNML value mgtk_gtk_toolbar_prepend_widget(value self, value widget, value tooltip_text, value tooltip_private_text) { /* ML */
    gtk_toolbar_prepend_widget(GtkObj_val(self), GtkObj_val(widget), String_val(tooltip_text), String_val(tooltip_private_text));
    return Val_unit;
}

/* ML type: cptr -> cptr -> string -> string -> int -> unit */
EXTERNML value mgtk_gtk_toolbar_insert_widget(value self, value widget, value tooltip_text, value tooltip_private_text, value position) { /* ML */
    gtk_toolbar_insert_widget(GtkObj_val(self), GtkObj_val(widget), String_val(tooltip_text), String_val(tooltip_private_text), Int_val(position));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_toolbar_set_orientation(value self, value orientation) { /* ML */
    gtk_toolbar_set_orientation(GtkObj_val(self), Int_val(orientation));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_toolbar_set_style(value self, value style) { /* ML */
    gtk_toolbar_set_style(GtkObj_val(self), Int_val(style));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_toolbar_set_icon_size(value self, value icon_size) { /* ML */
    gtk_toolbar_set_icon_size(GtkObj_val(self), Int_val(icon_size));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_toolbar_set_tooltips(value self, value enable) { /* ML */
    gtk_toolbar_set_tooltips(GtkObj_val(self), Bool_val(enable));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_toolbar_unset_style(value self) { /* ML */
    gtk_toolbar_unset_style(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_toolbar_unset_icon_size(value self) { /* ML */
    gtk_toolbar_unset_icon_size(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_toolbar_get_orientation(value self) { /* ML */
    return Val_int(gtk_toolbar_get_orientation(GtkObj_val(self)));
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_toolbar_get_style(value self) { /* ML */
    return Val_int(gtk_toolbar_get_style(GtkObj_val(self)));
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_toolbar_get_icon_size(value self) { /* ML */
    return Val_int(gtk_toolbar_get_icon_size(GtkObj_val(self)));
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_toolbar_get_tooltips(value self) { /* ML */
    return Val_bool(gtk_toolbar_get_tooltips(GtkObj_val(self)));
}



/* *** TextView *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_text_view_get_type(value dummy) { /* ML */
    return Val_int(gtk_text_view_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_text_view_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_text_view_new());
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_text_view_new_with_buffer(value buffer) { /* ML */
    return Val_GtkObj(gtk_text_view_new_with_buffer(GtkObj_val(buffer)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_view_set_buffer(value self, value buffer) { /* ML */
    gtk_text_view_set_buffer(GtkObj_val(self), GtkObj_val(buffer));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_text_view_get_buffer(value self) { /* ML */
    return Val_GtkObj(gtk_text_view_get_buffer(GtkObj_val(self)));
}

/* ML type: int -> bool */
EXTERNML value mgtk_gtk_text_view_scroll_to_iter(value mgtk_params) { /* ML */
    value self = Field(mgtk_params, 0);
    value iter = Field(mgtk_params, 1);
    value within_margin = Field(mgtk_params, 2);
    value use_align = Field(mgtk_params, 3);
    value xalign = Field(mgtk_params, 4);
    value yalign = Field(mgtk_params, 5);
    return Val_bool(gtk_text_view_scroll_to_iter(GtkObj_val(self), GtkTextIter_val(iter), Double_val(within_margin), Bool_val(use_align), Double_val(xalign), Double_val(yalign)));
}

/* ML type: int -> unit */
EXTERNML value mgtk_gtk_text_view_scroll_to_mark(value mgtk_params) { /* ML */
    value self = Field(mgtk_params, 0);
    value mark = Field(mgtk_params, 1);
    value within_margin = Field(mgtk_params, 2);
    value use_align = Field(mgtk_params, 3);
    value xalign = Field(mgtk_params, 4);
    value yalign = Field(mgtk_params, 5);
    gtk_text_view_scroll_to_mark(GtkObj_val(self), GtkObj_val(mark), Double_val(within_margin), Bool_val(use_align), Double_val(xalign), Double_val(yalign));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_view_scroll_mark_onscreen(value self, value mark) { /* ML */
    gtk_text_view_scroll_mark_onscreen(GtkObj_val(self), GtkObj_val(mark));
    return Val_unit;
}

/* ML type: cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_text_view_move_mark_onscreen(value self, value mark) { /* ML */
    return Val_bool(gtk_text_view_move_mark_onscreen(GtkObj_val(self), GtkObj_val(mark)));
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_text_view_place_cursor_onscreen(value self) { /* ML */
    return Val_bool(gtk_text_view_place_cursor_onscreen(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_text_view_set_cursor_visible(value self, value setting) { /* ML */
    gtk_text_view_set_cursor_visible(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_text_view_get_cursor_visible(value self) { /* ML */
    return Val_bool(gtk_text_view_get_cursor_visible(GtkObj_val(self)));
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_text_view_get_iter_at_location(value self, value x, value y) { /* ML */
    GtkTextIter iter;
    gtk_text_view_get_iter_at_location(GtkObj_val(self), &iter, Int_val(x), Int_val(y));
    return Val_GtkTextIter(&iter);
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_text_view_set_border_window_size(value self, value type, value size) { /* ML */
    gtk_text_view_set_border_window_size(GtkObj_val(self), Int_val(type), Int_val(size));
    return Val_unit;
}

/* ML type: cptr -> int -> int */
EXTERNML value mgtk_gtk_text_view_get_border_window_size(value self, value type) { /* ML */
    return Val_int(gtk_text_view_get_border_window_size(GtkObj_val(self), Int_val(type)));
}

/* ML type: cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_text_view_forward_display_line(value self, value iter) { /* ML */
    return Val_bool(gtk_text_view_forward_display_line(GtkObj_val(self), GtkTextIter_val(iter)));
}

/* ML type: cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_text_view_backward_display_line(value self, value iter) { /* ML */
    return Val_bool(gtk_text_view_backward_display_line(GtkObj_val(self), GtkTextIter_val(iter)));
}

/* ML type: cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_text_view_forward_display_line_end(value self, value iter) { /* ML */
    return Val_bool(gtk_text_view_forward_display_line_end(GtkObj_val(self), GtkTextIter_val(iter)));
}

/* ML type: cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_text_view_backward_display_line_start(value self, value iter) { /* ML */
    return Val_bool(gtk_text_view_backward_display_line_start(GtkObj_val(self), GtkTextIter_val(iter)));
}

/* ML type: cptr -> cptr -> bool */
EXTERNML value mgtk_gtk_text_view_starts_display_line(value self, value iter) { /* ML */
    return Val_bool(gtk_text_view_starts_display_line(GtkObj_val(self), GtkTextIter_val(iter)));
}

/* ML type: cptr -> cptr -> int -> bool */
EXTERNML value mgtk_gtk_text_view_move_visually(value self, value iter, value count) { /* ML */
    return Val_bool(gtk_text_view_move_visually(GtkObj_val(self), GtkTextIter_val(iter), Int_val(count)));
}

/* ML type: cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_text_view_add_child_at_anchor(value self, value child, value anchor) { /* ML */
    gtk_text_view_add_child_at_anchor(GtkObj_val(self), GtkObj_val(child), GtkObj_val(anchor));
    return Val_unit;
}

/* ML type: cptr -> cptr -> int -> int -> int -> unit */
EXTERNML value mgtk_gtk_text_view_add_child_in_window(value self, value child, value which_window, value xpos, value ypos) { /* ML */
    gtk_text_view_add_child_in_window(GtkObj_val(self), GtkObj_val(child), Int_val(which_window), Int_val(xpos), Int_val(ypos));
    return Val_unit;
}

/* ML type: cptr -> cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_text_view_move_child(value self, value child, value xpos, value ypos) { /* ML */
    gtk_text_view_move_child(GtkObj_val(self), GtkObj_val(child), Int_val(xpos), Int_val(ypos));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_text_view_set_wrap_mode(value self, value wrap_mode) { /* ML */
    gtk_text_view_set_wrap_mode(GtkObj_val(self), Int_val(wrap_mode));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_text_view_get_wrap_mode(value self) { /* ML */
    return Val_int(gtk_text_view_get_wrap_mode(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_text_view_set_editable(value self, value setting) { /* ML */
    gtk_text_view_set_editable(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_text_view_get_editable(value self) { /* ML */
    return Val_bool(gtk_text_view_get_editable(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_text_view_set_pixels_above_lines(value self, value pixels_above_lines) { /* ML */
    gtk_text_view_set_pixels_above_lines(GtkObj_val(self), Int_val(pixels_above_lines));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_text_view_get_pixels_above_lines(value self) { /* ML */
    return Val_int(gtk_text_view_get_pixels_above_lines(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_text_view_set_pixels_below_lines(value self, value pixels_below_lines) { /* ML */
    gtk_text_view_set_pixels_below_lines(GtkObj_val(self), Int_val(pixels_below_lines));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_text_view_get_pixels_below_lines(value self) { /* ML */
    return Val_int(gtk_text_view_get_pixels_below_lines(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_text_view_set_pixels_inside_wrap(value self, value pixels_inside_wrap) { /* ML */
    gtk_text_view_set_pixels_inside_wrap(GtkObj_val(self), Int_val(pixels_inside_wrap));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_text_view_get_pixels_inside_wrap(value self) { /* ML */
    return Val_int(gtk_text_view_get_pixels_inside_wrap(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_text_view_set_justification(value self, value justification) { /* ML */
    gtk_text_view_set_justification(GtkObj_val(self), Int_val(justification));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_text_view_get_justification(value self) { /* ML */
    return Val_int(gtk_text_view_get_justification(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_text_view_set_left_margin(value self, value left_margin) { /* ML */
    gtk_text_view_set_left_margin(GtkObj_val(self), Int_val(left_margin));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_text_view_get_left_margin(value self) { /* ML */
    return Val_int(gtk_text_view_get_left_margin(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_text_view_set_right_margin(value self, value right_margin) { /* ML */
    gtk_text_view_set_right_margin(GtkObj_val(self), Int_val(right_margin));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_text_view_get_right_margin(value self) { /* ML */
    return Val_int(gtk_text_view_get_right_margin(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_text_view_set_indent(value self, value indent) { /* ML */
    gtk_text_view_set_indent(GtkObj_val(self), Int_val(indent));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_text_view_get_indent(value self) { /* ML */
    return Val_int(gtk_text_view_get_indent(GtkObj_val(self)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_text_view_get_default_attributes(value self) { /* ML */
    return Val_GtkTextAttributes(gtk_text_view_get_default_attributes(GtkObj_val(self)));
}



/* *** Table *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_table_get_type(value dummy) { /* ML */
    return Val_int(gtk_table_get_type());
}

/* ML type: int -> int -> bool -> cptr */
EXTERNML value mgtk_gtk_table_new(value rows, value columns, value homogeneous) { /* ML */
    return Val_GtkObj(gtk_table_new(Int_val(rows), Int_val(columns), Bool_val(homogeneous)));
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_table_resize(value self, value rows, value columns) { /* ML */
    gtk_table_resize(GtkObj_val(self), Int_val(rows), Int_val(columns));
    return Val_unit;
}

/* ML type: int -> unit */
EXTERNML value mgtk_gtk_table_attach_defaults(value mgtk_params) { /* ML */
    value self = Field(mgtk_params, 0);
    value widget = Field(mgtk_params, 1);
    value left_attach = Field(mgtk_params, 2);
    value right_attach = Field(mgtk_params, 3);
    value top_attach = Field(mgtk_params, 4);
    value bottom_attach = Field(mgtk_params, 5);
    gtk_table_attach_defaults(GtkObj_val(self), GtkObj_val(widget), Int_val(left_attach), Int_val(right_attach), Int_val(top_attach), Int_val(bottom_attach));
    return Val_unit;
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_table_set_row_spacing(value self, value row, value spacing) { /* ML */
    gtk_table_set_row_spacing(GtkObj_val(self), Int_val(row), Int_val(spacing));
    return Val_unit;
}

/* ML type: cptr -> int -> int */
EXTERNML value mgtk_gtk_table_get_row_spacing(value self, value row) { /* ML */
    return Val_int(gtk_table_get_row_spacing(GtkObj_val(self), Int_val(row)));
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_table_set_col_spacing(value self, value column, value spacing) { /* ML */
    gtk_table_set_col_spacing(GtkObj_val(self), Int_val(column), Int_val(spacing));
    return Val_unit;
}

/* ML type: cptr -> int -> int */
EXTERNML value mgtk_gtk_table_get_col_spacing(value self, value column) { /* ML */
    return Val_int(gtk_table_get_col_spacing(GtkObj_val(self), Int_val(column)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_table_set_row_spacings(value self, value spacing) { /* ML */
    gtk_table_set_row_spacings(GtkObj_val(self), Int_val(spacing));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_table_get_default_row_spacing(value self) { /* ML */
    return Val_int(gtk_table_get_default_row_spacing(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_table_set_col_spacings(value self, value spacing) { /* ML */
    gtk_table_set_col_spacings(GtkObj_val(self), Int_val(spacing));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_table_get_default_col_spacing(value self) { /* ML */
    return Val_int(gtk_table_get_default_col_spacing(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_table_set_homogeneous(value self, value homogeneous) { /* ML */
    gtk_table_set_homogeneous(GtkObj_val(self), Bool_val(homogeneous));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_table_get_homogeneous(value self) { /* ML */
    return Val_bool(gtk_table_get_homogeneous(GtkObj_val(self)));
}



/* *** Socket *** */
/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_socket_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_socket_new());
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_socket_get_type(value dummy) { /* ML */
    return Val_int(gtk_socket_get_type());
}



/* *** Paned *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_paned_get_type(value dummy) { /* ML */
    return Val_int(gtk_paned_get_type());
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_paned_add1(value self, value child) { /* ML */
    gtk_paned_add1(GtkObj_val(self), GtkObj_val(child));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_paned_add2(value self, value child) { /* ML */
    gtk_paned_add2(GtkObj_val(self), GtkObj_val(child));
    return Val_unit;
}

/* ML type: cptr -> cptr -> bool -> bool -> unit */
EXTERNML value mgtk_gtk_paned_pack1(value self, value child, value resize, value shrink) { /* ML */
    gtk_paned_pack1(GtkObj_val(self), GtkObj_val(child), Bool_val(resize), Bool_val(shrink));
    return Val_unit;
}

/* ML type: cptr -> cptr -> bool -> bool -> unit */
EXTERNML value mgtk_gtk_paned_pack2(value self, value child, value resize, value shrink) { /* ML */
    gtk_paned_pack2(GtkObj_val(self), GtkObj_val(child), Bool_val(resize), Bool_val(shrink));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_paned_get_position(value self) { /* ML */
    return Val_int(gtk_paned_get_position(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_paned_set_position(value self, value position) { /* ML */
    gtk_paned_set_position(GtkObj_val(self), Int_val(position));
    return Val_unit;
}

/* ML type: cptr -> int -> int -> int -> unit */
EXTERNML value mgtk_gtk_paned_compute_position(value self, value allocation, value child1_req, value child2_req) { /* ML */
    gtk_paned_compute_position(GtkObj_val(self), Int_val(allocation), Int_val(child1_req), Int_val(child2_req));
    return Val_unit;
}



/* *** VPaned *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_vpaned_get_type(value dummy) { /* ML */
    return Val_int(gtk_vpaned_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_vpaned_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_vpaned_new());
}



/* *** HPaned *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_hpaned_get_type(value dummy) { /* ML */
    return Val_int(gtk_hpaned_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_hpaned_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_hpaned_new());
}



/* *** Notebook *** */
/* ML type: unit -> int * int */
EXTERNML value mgtk_get_gtk_notebook_tab(value dummy) { /* ML */
    value res = alloc_tuple(2);
    Field(res, 0) = Val_int(GTK_NOTEBOOK_TAB_FIRST);
    Field(res, 1) = Val_int(GTK_NOTEBOOK_TAB_LAST);
    return res;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_notebook_get_type(value dummy) { /* ML */
    return Val_int(gtk_notebook_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_notebook_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_notebook_new());
}

/* ML type: cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_notebook_append_page(value self, value child, value tab_label) { /* ML */
    gtk_notebook_append_page(GtkObj_val(self), GtkObj_val(child), GtkObj_val(tab_label));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_notebook_append_page_menu(value self, value child, value tab_label, value menu_label) { /* ML */
    gtk_notebook_append_page_menu(GtkObj_val(self), GtkObj_val(child), GtkObj_val(tab_label), GtkObj_val(menu_label));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_notebook_prepend_page(value self, value child, value tab_label) { /* ML */
    gtk_notebook_prepend_page(GtkObj_val(self), GtkObj_val(child), GtkObj_val(tab_label));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_notebook_prepend_page_menu(value self, value child, value tab_label, value menu_label) { /* ML */
    gtk_notebook_prepend_page_menu(GtkObj_val(self), GtkObj_val(child), GtkObj_val(tab_label), GtkObj_val(menu_label));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr -> int -> unit */
EXTERNML value mgtk_gtk_notebook_insert_page(value self, value child, value tab_label, value position) { /* ML */
    gtk_notebook_insert_page(GtkObj_val(self), GtkObj_val(child), GtkObj_val(tab_label), Int_val(position));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr -> cptr -> int -> unit */
EXTERNML value mgtk_gtk_notebook_insert_page_menu(value self, value child, value tab_label, value menu_label, value position) { /* ML */
    gtk_notebook_insert_page_menu(GtkObj_val(self), GtkObj_val(child), GtkObj_val(tab_label), GtkObj_val(menu_label), Int_val(position));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_notebook_remove_page(value self, value page_num) { /* ML */
    gtk_notebook_remove_page(GtkObj_val(self), Int_val(page_num));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_notebook_get_current_page(value self) { /* ML */
    return Val_int(gtk_notebook_get_current_page(GtkObj_val(self)));
}

/* ML type: cptr -> int -> cptr */
EXTERNML value mgtk_gtk_notebook_get_nth_page(value self, value page_num) { /* ML */
    return Val_GtkObj(gtk_notebook_get_nth_page(GtkObj_val(self), Int_val(page_num)));
}

/* ML type: cptr -> cptr -> int */
EXTERNML value mgtk_gtk_notebook_page_num(value self, value child) { /* ML */
    return Val_int(gtk_notebook_page_num(GtkObj_val(self), GtkObj_val(child)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_notebook_set_current_page(value self, value page_num) { /* ML */
    gtk_notebook_set_current_page(GtkObj_val(self), Int_val(page_num));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_notebook_next_page(value self) { /* ML */
    gtk_notebook_next_page(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_notebook_prev_page(value self) { /* ML */
    gtk_notebook_prev_page(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_notebook_set_show_border(value self, value show_border) { /* ML */
    gtk_notebook_set_show_border(GtkObj_val(self), Bool_val(show_border));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_notebook_get_show_border(value self) { /* ML */
    return Val_bool(gtk_notebook_get_show_border(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_notebook_set_show_tabs(value self, value show_tabs) { /* ML */
    gtk_notebook_set_show_tabs(GtkObj_val(self), Bool_val(show_tabs));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_notebook_get_show_tabs(value self) { /* ML */
    return Val_bool(gtk_notebook_get_show_tabs(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_notebook_set_tab_pos(value self, value pos) { /* ML */
    gtk_notebook_set_tab_pos(GtkObj_val(self), Int_val(pos));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_notebook_get_tab_pos(value self) { /* ML */
    return Val_int(gtk_notebook_get_tab_pos(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_notebook_set_homogeneous_tabs(value self, value homogeneous) { /* ML */
    gtk_notebook_set_homogeneous_tabs(GtkObj_val(self), Bool_val(homogeneous));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_notebook_set_tab_border(value self, value border_width) { /* ML */
    gtk_notebook_set_tab_border(GtkObj_val(self), Int_val(border_width));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_notebook_set_tab_hborder(value self, value tab_hborder) { /* ML */
    gtk_notebook_set_tab_hborder(GtkObj_val(self), Int_val(tab_hborder));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_notebook_set_tab_vborder(value self, value tab_vborder) { /* ML */
    gtk_notebook_set_tab_vborder(GtkObj_val(self), Int_val(tab_vborder));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_notebook_set_scrollable(value self, value scrollable) { /* ML */
    gtk_notebook_set_scrollable(GtkObj_val(self), Bool_val(scrollable));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_notebook_get_scrollable(value self) { /* ML */
    return Val_bool(gtk_notebook_get_scrollable(GtkObj_val(self)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_notebook_popup_enable(value self) { /* ML */
    gtk_notebook_popup_enable(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_notebook_popup_disable(value self) { /* ML */
    gtk_notebook_popup_disable(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> cptr -> cptr */
EXTERNML value mgtk_gtk_notebook_get_tab_label(value self, value child) { /* ML */
    return Val_GtkObj(gtk_notebook_get_tab_label(GtkObj_val(self), GtkObj_val(child)));
}

/* ML type: cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_notebook_set_tab_label(value self, value child, value tab_label) { /* ML */
    gtk_notebook_set_tab_label(GtkObj_val(self), GtkObj_val(child), GtkObj_val(tab_label));
    return Val_unit;
}

/* ML type: cptr -> cptr -> string -> unit */
EXTERNML value mgtk_gtk_notebook_set_tab_label_text(value self, value child, value tab_text) { /* ML */
    gtk_notebook_set_tab_label_text(GtkObj_val(self), GtkObj_val(child), String_val(tab_text));
    return Val_unit;
}

/* ML type: cptr -> cptr -> string */
EXTERNML value mgtk_gtk_notebook_get_tab_label_text(value self, value child) { /* ML */
    return my_copy_string(gtk_notebook_get_tab_label_text(GtkObj_val(self), GtkObj_val(child)));
}

/* ML type: cptr -> cptr -> cptr */
EXTERNML value mgtk_gtk_notebook_get_menu_label(value self, value child) { /* ML */
    return Val_GtkObj(gtk_notebook_get_menu_label(GtkObj_val(self), GtkObj_val(child)));
}

/* ML type: cptr -> cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_notebook_set_menu_label(value self, value child, value menu_label) { /* ML */
    gtk_notebook_set_menu_label(GtkObj_val(self), GtkObj_val(child), GtkObj_val(menu_label));
    return Val_unit;
}

/* ML type: cptr -> cptr -> string -> unit */
EXTERNML value mgtk_gtk_notebook_set_menu_label_text(value self, value child, value menu_text) { /* ML */
    gtk_notebook_set_menu_label_text(GtkObj_val(self), GtkObj_val(child), String_val(menu_text));
    return Val_unit;
}

/* ML type: cptr -> cptr -> string */
EXTERNML value mgtk_gtk_notebook_get_menu_label_text(value self, value child) { /* ML */
    return my_copy_string(gtk_notebook_get_menu_label_text(GtkObj_val(self), GtkObj_val(child)));
}

/* ML type: cptr -> cptr -> bool -> bool -> int -> unit */
EXTERNML value mgtk_gtk_notebook_set_tab_label_packing(value self, value child, value expand, value fill, value pack_type) { /* ML */
    gtk_notebook_set_tab_label_packing(GtkObj_val(self), GtkObj_val(child), Bool_val(expand), Bool_val(fill), Int_val(pack_type));
    return Val_unit;
}

/* ML type: cptr -> cptr -> int -> unit */
EXTERNML value mgtk_gtk_notebook_reorder_child(value self, value child, value position) { /* ML */
    gtk_notebook_reorder_child(GtkObj_val(self), GtkObj_val(child), Int_val(position));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_notebook_current_page(value self) { /* ML */
    return Val_int(gtk_notebook_current_page(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_notebook_set_page(value self, value page_num) { /* ML */
    gtk_notebook_set_page(GtkObj_val(self), Int_val(page_num));
    return Val_unit;
}



/* *** MenuShell *** */
/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_menu_shell_append(value self, value child) { /* ML */
    gtk_menu_shell_append(GtkObj_val(self), GtkObj_val(child));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_menu_shell_prepend(value self, value child) { /* ML */
    gtk_menu_shell_prepend(GtkObj_val(self), GtkObj_val(child));
    return Val_unit;
}

/* ML type: cptr -> cptr -> int -> unit */
EXTERNML value mgtk_gtk_menu_shell_insert(value self, value child, value position) { /* ML */
    gtk_menu_shell_insert(GtkObj_val(self), GtkObj_val(child), Int_val(position));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_menu_shell_deactivate(value self) { /* ML */
    gtk_menu_shell_deactivate(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_menu_shell_select_item(value self, value menu_item) { /* ML */
    gtk_menu_shell_select_item(GtkObj_val(self), GtkObj_val(menu_item));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_menu_shell_deselect(value self) { /* ML */
    gtk_menu_shell_deselect(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> cptr -> bool -> unit */
EXTERNML value mgtk_gtk_menu_shell_activate_item(value self, value menu_item, value force_deactivate) { /* ML */
    gtk_menu_shell_activate_item(GtkObj_val(self), GtkObj_val(menu_item), Bool_val(force_deactivate));
    return Val_unit;
}



/* *** Menu *** */
/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_menu_directiontype(value dummy) { /* ML */
    value res = alloc_tuple(4);
    Field(res, 0) = Val_int(GTK_MENU_DIR_PARENT);
    Field(res, 1) = Val_int(GTK_MENU_DIR_CHILD);
    Field(res, 2) = Val_int(GTK_MENU_DIR_NEXT);
    Field(res, 3) = Val_int(GTK_MENU_DIR_PREV);
    return res;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_menu_get_type(value dummy) { /* ML */
    return Val_int(gtk_menu_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_menu_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_menu_new());
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_menu_reposition(value self) { /* ML */
    gtk_menu_reposition(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_menu_popdown(value self) { /* ML */
    gtk_menu_popdown(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_menu_get_active(value self) { /* ML */
    return Val_GtkObj(gtk_menu_get_active(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_menu_set_active(value self, value index) { /* ML */
    gtk_menu_set_active(GtkObj_val(self), Int_val(index));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_menu_set_accel_group(value self, value accel_group) { /* ML */
    gtk_menu_set_accel_group(GtkObj_val(self), GtkObj_val(accel_group));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_menu_get_accel_group(value self) { /* ML */
    return Val_GtkObj(gtk_menu_get_accel_group(GtkObj_val(self)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_menu_set_accel_path(value self, value accel_path) { /* ML */
    gtk_menu_set_accel_path(GtkObj_val(self), String_val(accel_path));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_menu_detach(value self) { /* ML */
    gtk_menu_detach(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_menu_get_attach_widget(value self) { /* ML */
    return Val_GtkObj(gtk_menu_get_attach_widget(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_menu_set_tearoff_state(value self, value torn_off) { /* ML */
    gtk_menu_set_tearoff_state(GtkObj_val(self), Bool_val(torn_off));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_menu_get_tearoff_state(value self) { /* ML */
    return Val_bool(gtk_menu_get_tearoff_state(GtkObj_val(self)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_menu_set_title(value self, value title) { /* ML */
    gtk_menu_set_title(GtkObj_val(self), String_val(title));
    return Val_unit;
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_menu_get_title(value self) { /* ML */
    return my_copy_string(gtk_menu_get_title(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> int -> unit */
EXTERNML value mgtk_gtk_menu_reorder_child(value self, value child, value position) { /* ML */
    gtk_menu_reorder_child(GtkObj_val(self), GtkObj_val(child), Int_val(position));
    return Val_unit;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_menu_bar_get_type(value dummy) { /* ML */
    return Val_int(gtk_menu_bar_get_type());
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_menu_item_get_type(value dummy) { /* ML */
    return Val_int(gtk_menu_item_get_type());
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_menu_shell_get_type(value dummy) { /* ML */
    return Val_int(gtk_menu_shell_get_type());
}



/* *** MenuBar *** */
/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_menu_bar_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_menu_bar_new());
}



/* *** List *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_list_get_type(value dummy) { /* ML */
    return Val_int(gtk_list_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_list_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_list_new());
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_list_clear_items(value self, value start, value end) { /* ML */
    gtk_list_clear_items(GtkObj_val(self), Int_val(start), Int_val(end));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_list_select_item(value self, value item) { /* ML */
    gtk_list_select_item(GtkObj_val(self), Int_val(item));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_list_unselect_item(value self, value item) { /* ML */
    gtk_list_unselect_item(GtkObj_val(self), Int_val(item));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_list_select_child(value self, value child) { /* ML */
    gtk_list_select_child(GtkObj_val(self), GtkObj_val(child));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_list_unselect_child(value self, value child) { /* ML */
    gtk_list_unselect_child(GtkObj_val(self), GtkObj_val(child));
    return Val_unit;
}

/* ML type: cptr -> cptr -> int */
EXTERNML value mgtk_gtk_list_child_position(value self, value child) { /* ML */
    return Val_int(gtk_list_child_position(GtkObj_val(self), GtkObj_val(child)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_list_set_selection_mode(value self, value mode) { /* ML */
    gtk_list_set_selection_mode(GtkObj_val(self), Int_val(mode));
    return Val_unit;
}

/* ML type: cptr -> int -> real -> bool -> unit */
EXTERNML value mgtk_gtk_list_extend_selection(value self, value scroll_type, value position, value auto_start_selection) { /* ML */
    gtk_list_extend_selection(GtkObj_val(self), Int_val(scroll_type), Double_val(position), Bool_val(auto_start_selection));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_list_start_selection(value self) { /* ML */
    gtk_list_start_selection(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_list_end_selection(value self) { /* ML */
    gtk_list_end_selection(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_list_select_all(value self) { /* ML */
    gtk_list_select_all(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_list_unselect_all(value self) { /* ML */
    gtk_list_unselect_all(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> int -> real -> unit */
EXTERNML value mgtk_gtk_list_scroll_horizontal(value self, value scroll_type, value position) { /* ML */
    gtk_list_scroll_horizontal(GtkObj_val(self), Int_val(scroll_type), Double_val(position));
    return Val_unit;
}

/* ML type: cptr -> int -> real -> unit */
EXTERNML value mgtk_gtk_list_scroll_vertical(value self, value scroll_type, value position) { /* ML */
    gtk_list_scroll_vertical(GtkObj_val(self), Int_val(scroll_type), Double_val(position));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_list_toggle_add_mode(value self) { /* ML */
    gtk_list_toggle_add_mode(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_list_toggle_focus_row(value self) { /* ML */
    gtk_list_toggle_focus_row(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_list_toggle_row(value self, value item) { /* ML */
    gtk_list_toggle_row(GtkObj_val(self), GtkObj_val(item));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_list_undo_selection(value self) { /* ML */
    gtk_list_undo_selection(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_list_end_drag_selection(value self) { /* ML */
    gtk_list_end_drag_selection(GtkObj_val(self));
    return Val_unit;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_list_item_get_type(value dummy) { /* ML */
    return Val_int(gtk_list_item_get_type());
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_list_store_get_type(value dummy) { /* ML */
    return Val_int(gtk_list_store_get_type());
}



/* *** Layout *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_layout_get_type(value dummy) { /* ML */
    return Val_int(gtk_layout_get_type());
}

/* ML type: cptr -> cptr -> cptr */
EXTERNML value mgtk_gtk_layout_new(value hadjustment, value vadjustment) { /* ML */
    return Val_GtkObj(gtk_layout_new(GtkObj_val(hadjustment), GtkObj_val(vadjustment)));
}

/* ML type: cptr -> cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_layout_put(value self, value child_widget, value x, value y) { /* ML */
    gtk_layout_put(GtkObj_val(self), GtkObj_val(child_widget), Int_val(x), Int_val(y));
    return Val_unit;
}

/* ML type: cptr -> cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_layout_move(value self, value child_widget, value x, value y) { /* ML */
    gtk_layout_move(GtkObj_val(self), GtkObj_val(child_widget), Int_val(x), Int_val(y));
    return Val_unit;
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_layout_set_size(value self, value width, value height) { /* ML */
    gtk_layout_set_size(GtkObj_val(self), Int_val(width), Int_val(height));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_layout_set_hadjustment(value self, value adjustment) { /* ML */
    gtk_layout_set_hadjustment(GtkObj_val(self), GtkObj_val(adjustment));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_layout_set_vadjustment(value self, value adjustment) { /* ML */
    gtk_layout_set_vadjustment(GtkObj_val(self), GtkObj_val(adjustment));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_layout_freeze(value self) { /* ML */
    gtk_layout_freeze(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_layout_thaw(value self) { /* ML */
    gtk_layout_thaw(GtkObj_val(self));
    return Val_unit;
}



/* *** Fixed *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_fixed_get_type(value dummy) { /* ML */
    return Val_int(gtk_fixed_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_fixed_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_fixed_new());
}

/* ML type: cptr -> cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_fixed_put(value self, value widget, value x, value y) { /* ML */
    gtk_fixed_put(GtkObj_val(self), GtkObj_val(widget), Int_val(x), Int_val(y));
    return Val_unit;
}

/* ML type: cptr -> cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_fixed_move(value self, value widget, value x, value y) { /* ML */
    gtk_fixed_move(GtkObj_val(self), GtkObj_val(widget), Int_val(x), Int_val(y));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_fixed_set_has_window(value self, value has_window) { /* ML */
    gtk_fixed_set_has_window(GtkObj_val(self), Bool_val(has_window));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_fixed_get_has_window(value self) { /* ML */
    return Val_bool(gtk_fixed_get_has_window(GtkObj_val(self)));
}



/* *** Bin *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_bin_get_type(value dummy) { /* ML */
    return Val_int(gtk_bin_get_type());
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_bin_get_child(value self) { /* ML */
    return Val_GtkObj(gtk_bin_get_child(GtkObj_val(self)));
}



/* *** Viewport *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_viewport_get_type(value dummy) { /* ML */
    return Val_int(gtk_viewport_get_type());
}

/* ML type: cptr -> cptr -> cptr */
EXTERNML value mgtk_gtk_viewport_new(value hadjustment, value vadjustment) { /* ML */
    return Val_GtkObj(gtk_viewport_new(GtkObj_val(hadjustment), GtkObj_val(vadjustment)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_viewport_get_hadjustment(value self) { /* ML */
    return Val_GtkObj(gtk_viewport_get_hadjustment(GtkObj_val(self)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_viewport_get_vadjustment(value self) { /* ML */
    return Val_GtkObj(gtk_viewport_get_vadjustment(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_viewport_set_hadjustment(value self, value adjustment) { /* ML */
    gtk_viewport_set_hadjustment(GtkObj_val(self), GtkObj_val(adjustment));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_viewport_set_vadjustment(value self, value adjustment) { /* ML */
    gtk_viewport_set_vadjustment(GtkObj_val(self), GtkObj_val(adjustment));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_viewport_set_shadow_type(value self, value type) { /* ML */
    gtk_viewport_set_shadow_type(GtkObj_val(self), Int_val(type));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_viewport_get_shadow_type(value self) { /* ML */
    return Val_int(gtk_viewport_get_shadow_type(GtkObj_val(self)));
}



/* *** ScrolledWindow *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_scrolled_window_get_type(value dummy) { /* ML */
    return Val_int(gtk_scrolled_window_get_type());
}

/* ML type: cptr -> cptr -> cptr */
EXTERNML value mgtk_gtk_scrolled_window_new(value hadjustment, value vadjustment) { /* ML */
    return Val_GtkObj(gtk_scrolled_window_new(GtkObj_val(hadjustment), GtkObj_val(vadjustment)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_scrolled_window_set_hadjustment(value self, value hadjustment) { /* ML */
    gtk_scrolled_window_set_hadjustment(GtkObj_val(self), GtkObj_val(hadjustment));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_scrolled_window_set_vadjustment(value self, value hadjustment) { /* ML */
    gtk_scrolled_window_set_vadjustment(GtkObj_val(self), GtkObj_val(hadjustment));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_scrolled_window_get_hadjustment(value self) { /* ML */
    return Val_GtkObj(gtk_scrolled_window_get_hadjustment(GtkObj_val(self)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_scrolled_window_get_vadjustment(value self) { /* ML */
    return Val_GtkObj(gtk_scrolled_window_get_vadjustment(GtkObj_val(self)));
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_scrolled_window_set_policy(value self, value hscrollbar_policy, value vscrollbar_policy) { /* ML */
    gtk_scrolled_window_set_policy(GtkObj_val(self), Int_val(hscrollbar_policy), Int_val(vscrollbar_policy));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_scrolled_window_get_policy(value self) { /* ML */
    value result;
    GtkPolicyType hscrollbar_policy;
    GtkPolicyType vscrollbar_policy;
    gtk_scrolled_window_get_policy(GtkObj_val(self), &hscrollbar_policy, &vscrollbar_policy);
    result = alloc_tuple(2);
    Field(result, 0) = Val_int(&hscrollbar_policy);
    Field(result, 1) = Val_int(&vscrollbar_policy);
    return result;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_scrolled_window_set_placement(value self, value window_placement) { /* ML */
    gtk_scrolled_window_set_placement(GtkObj_val(self), Int_val(window_placement));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_scrolled_window_get_placement(value self) { /* ML */
    return Val_int(gtk_scrolled_window_get_placement(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_scrolled_window_set_shadow_type(value self, value type) { /* ML */
    gtk_scrolled_window_set_shadow_type(GtkObj_val(self), Int_val(type));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_scrolled_window_get_shadow_type(value self) { /* ML */
    return Val_int(gtk_scrolled_window_get_shadow_type(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_scrolled_window_add_with_viewport(value self, value child) { /* ML */
    gtk_scrolled_window_add_with_viewport(GtkObj_val(self), GtkObj_val(child));
    return Val_unit;
}



/* *** Item *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_item_get_type(value dummy) { /* ML */
    return Val_int(gtk_item_get_type());
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_item_select(value self) { /* ML */
    gtk_item_select(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_item_deselect(value self) { /* ML */
    gtk_item_deselect(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_item_toggle(value self) { /* ML */
    gtk_item_toggle(GtkObj_val(self));
    return Val_unit;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_item_factory_get_type(value dummy) { /* ML */
    return Val_int(gtk_item_factory_get_type());
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_item_factory_path_from_widget(value widget) { /* ML */
    return my_copy_string(gtk_item_factory_path_from_widget(GtkObj_val(widget)));
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_item_factory_popup_data_from_widget(value widget) { /* ML */
    return (value)(gtk_item_factory_popup_data_from_widget(GtkObj_val(widget)));
}

/* ML type: string -> string -> unit */
EXTERNML value mgtk_gtk_item_factories_path_delete(value ifactory_path, value path) { /* ML */
    gtk_item_factories_path_delete(String_val(ifactory_path), String_val(path));
    return Val_unit;
}



/* *** MenuItem *** */
/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_menu_item_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_menu_item_new());
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_menu_item_new_with_label(value label) { /* ML */
    return Val_GtkObj(gtk_menu_item_new_with_label(String_val(label)));
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_menu_item_new_with_mnemonic(value label) { /* ML */
    return Val_GtkObj(gtk_menu_item_new_with_mnemonic(String_val(label)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_menu_item_set_submenu(value self, value submenu) { /* ML */
    gtk_menu_item_set_submenu(GtkObj_val(self), GtkObj_val(submenu));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_menu_item_get_submenu(value self) { /* ML */
    return Val_GtkObj(gtk_menu_item_get_submenu(GtkObj_val(self)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_menu_item_remove_submenu(value self) { /* ML */
    gtk_menu_item_remove_submenu(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_menu_item_select(value self) { /* ML */
    gtk_menu_item_select(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_menu_item_deselect(value self) { /* ML */
    gtk_menu_item_deselect(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_menu_item_activate(value self) { /* ML */
    gtk_menu_item_activate(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_menu_item_toggle_size_allocate(value self, value allocation) { /* ML */
    gtk_menu_item_toggle_size_allocate(GtkObj_val(self), Int_val(allocation));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_menu_item_set_right_justified(value self, value right_justified) { /* ML */
    gtk_menu_item_set_right_justified(GtkObj_val(self), Bool_val(right_justified));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_menu_item_get_right_justified(value self) { /* ML */
    return Val_bool(gtk_menu_item_get_right_justified(GtkObj_val(self)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_menu_item_set_accel_path(value self, value accel_path) { /* ML */
    gtk_menu_item_set_accel_path(GtkObj_val(self), String_val(accel_path));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_menu_item_right_justify(value self) { /* ML */
    gtk_menu_item_right_justify(GtkObj_val(self));
    return Val_unit;
}



/* *** TearoffMenuItem *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_tearoff_menu_item_get_type(value dummy) { /* ML */
    return Val_int(gtk_tearoff_menu_item_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_tearoff_menu_item_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_tearoff_menu_item_new());
}



/* *** SeparatorMenuItem *** */
/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_separator_menu_item_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_separator_menu_item_new());
}



/* *** CheckMenuItem *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_check_menu_item_get_type(value dummy) { /* ML */
    return Val_int(gtk_check_menu_item_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_check_menu_item_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_check_menu_item_new());
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_check_menu_item_new_with_label(value label) { /* ML */
    return Val_GtkObj(gtk_check_menu_item_new_with_label(String_val(label)));
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_check_menu_item_new_with_mnemonic(value label) { /* ML */
    return Val_GtkObj(gtk_check_menu_item_new_with_mnemonic(String_val(label)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_check_menu_item_set_active(value self, value is_active) { /* ML */
    gtk_check_menu_item_set_active(GtkObj_val(self), Bool_val(is_active));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_check_menu_item_get_active(value self) { /* ML */
    return Val_bool(gtk_check_menu_item_get_active(GtkObj_val(self)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_check_menu_item_toggled(value self) { /* ML */
    gtk_check_menu_item_toggled(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_check_menu_item_set_inconsistent(value self, value setting) { /* ML */
    gtk_check_menu_item_set_inconsistent(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_check_menu_item_get_inconsistent(value self) { /* ML */
    return Val_bool(gtk_check_menu_item_get_inconsistent(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_check_menu_item_set_show_toggle(value self, value always) { /* ML */
    gtk_check_menu_item_set_show_toggle(GtkObj_val(self), Bool_val(always));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_check_menu_item_set_state(value self, value is_active) { /* ML */
    gtk_check_menu_item_set_state(GtkObj_val(self), Bool_val(is_active));
    return Val_unit;
}



/* *** RadioMenuItem *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_radio_menu_item_get_type(value dummy) { /* ML */
    return Val_int(gtk_radio_menu_item_get_type());
}



/* *** ImageMenuItem *** */
/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_image_menu_item_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_image_menu_item_new());
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_image_menu_item_new_with_label(value label) { /* ML */
    return Val_GtkObj(gtk_image_menu_item_new_with_label(String_val(label)));
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_image_menu_item_new_with_mnemonic(value label) { /* ML */
    return Val_GtkObj(gtk_image_menu_item_new_with_mnemonic(String_val(label)));
}

/* ML type: string -> cptr -> cptr */
EXTERNML value mgtk_gtk_image_menu_item_new_from_stock(value stock_id, value accel_group) { /* ML */
    return Val_GtkObj(gtk_image_menu_item_new_from_stock(String_val(stock_id), GtkObj_val(accel_group)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_image_menu_item_set_image(value self, value image) { /* ML */
    gtk_image_menu_item_set_image(GtkObj_val(self), GtkObj_val(image));
    return Val_unit;
}



/* *** ListItem *** */
/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_list_item_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_list_item_new());
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_list_item_new_with_label(value label) { /* ML */
    return Val_GtkObj(gtk_list_item_new_with_label(String_val(label)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_list_item_select(value self) { /* ML */
    gtk_list_item_select(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_list_item_deselect(value self) { /* ML */
    gtk_list_item_deselect(GtkObj_val(self));
    return Val_unit;
}



/* *** HandleBox *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_handle_box_get_type(value dummy) { /* ML */
    return Val_int(gtk_handle_box_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_handle_box_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_handle_box_new());
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_handle_box_set_shadow_type(value self, value type) { /* ML */
    gtk_handle_box_set_shadow_type(GtkObj_val(self), Int_val(type));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_handle_box_get_shadow_type(value self) { /* ML */
    return Val_int(gtk_handle_box_get_shadow_type(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_handle_box_set_handle_position(value self, value position) { /* ML */
    gtk_handle_box_set_handle_position(GtkObj_val(self), Int_val(position));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_handle_box_get_handle_position(value self) { /* ML */
    return Val_int(gtk_handle_box_get_handle_position(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_handle_box_set_snap_edge(value self, value edge) { /* ML */
    gtk_handle_box_set_snap_edge(GtkObj_val(self), Int_val(edge));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_handle_box_get_snap_edge(value self) { /* ML */
    return Val_int(gtk_handle_box_get_snap_edge(GtkObj_val(self)));
}



/* *** Frame *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_frame_get_type(value dummy) { /* ML */
    return Val_int(gtk_frame_get_type());
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_frame_new(value label) { /* ML */
    return Val_GtkObj(gtk_frame_new(String_val(label)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_frame_set_label(value self, value label) { /* ML */
    gtk_frame_set_label(GtkObj_val(self), String_val(label));
    return Val_unit;
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_frame_get_label(value self) { /* ML */
    return my_copy_string(gtk_frame_get_label(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_frame_set_label_widget(value self, value label_widget) { /* ML */
    gtk_frame_set_label_widget(GtkObj_val(self), GtkObj_val(label_widget));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_frame_get_label_widget(value self) { /* ML */
    return Val_GtkObj(gtk_frame_get_label_widget(GtkObj_val(self)));
}

/* ML type: cptr -> real -> real -> unit */
EXTERNML value mgtk_gtk_frame_set_label_align(value self, value xalign, value yalign) { /* ML */
    gtk_frame_set_label_align(GtkObj_val(self), Double_val(xalign), Double_val(yalign));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_frame_set_shadow_type(value self, value type) { /* ML */
    gtk_frame_set_shadow_type(GtkObj_val(self), Int_val(type));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_frame_get_shadow_type(value self) { /* ML */
    return Val_int(gtk_frame_get_shadow_type(GtkObj_val(self)));
}



/* *** AspectFrame *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_aspect_frame_get_type(value dummy) { /* ML */
    return Val_int(gtk_aspect_frame_get_type());
}

/* ML type: string -> real -> real -> real -> bool -> cptr */
EXTERNML value mgtk_gtk_aspect_frame_new(value label, value xalign, value yalign, value ratio, value obey_child) { /* ML */
    return Val_GtkObj(gtk_aspect_frame_new(String_val(label), Double_val(xalign), Double_val(yalign), Double_val(ratio), Bool_val(obey_child)));
}

/* ML type: cptr -> real -> real -> real -> bool -> unit */
EXTERNML value mgtk_gtk_aspect_frame_set(value self, value xalign, value yalign, value ratio, value obey_child) { /* ML */
    gtk_aspect_frame_set(GtkObj_val(self), Double_val(xalign), Double_val(yalign), Double_val(ratio), Bool_val(obey_child));
    return Val_unit;
}



/* *** EventBox *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_event_box_get_type(value dummy) { /* ML */
    return Val_int(gtk_event_box_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_event_box_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_event_box_new());
}



/* *** Alignment *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_alignment_get_type(value dummy) { /* ML */
    return Val_int(gtk_alignment_get_type());
}

/* ML type: real -> real -> real -> real -> cptr */
EXTERNML value mgtk_gtk_alignment_new(value xalign, value yalign, value xscale, value yscale) { /* ML */
    return Val_GtkObj(gtk_alignment_new(Double_val(xalign), Double_val(yalign), Double_val(xscale), Double_val(yscale)));
}

/* ML type: cptr -> real -> real -> real -> real -> unit */
EXTERNML value mgtk_gtk_alignment_set(value self, value xalign, value yalign, value xscale, value yscale) { /* ML */
    gtk_alignment_set(GtkObj_val(self), Double_val(xalign), Double_val(yalign), Double_val(xscale), Double_val(yscale));
    return Val_unit;
}



/* *** Button *** */
/* ML type: unit -> int * int * int * int */
EXTERNML value mgtk_get_gtk_button_action(value dummy) { /* ML */
    value res = alloc_tuple(4);
    Field(res, 0) = Val_int(GTK_BUTTON_IGNORED);
    Field(res, 1) = Val_int(GTK_BUTTON_SELECTS);
    Field(res, 2) = Val_int(GTK_BUTTON_DRAGS);
    Field(res, 3) = Val_int(GTK_BUTTON_EXPANDS);
    return res;
}

/* ML type: unit -> int * int * int * int * int */
EXTERNML value mgtk_get_gtk_button_box_style(value dummy) { /* ML */
    value res = alloc_tuple(5);
    Field(res, 0) = Val_int(GTK_BUTTONBOX_DEFAULT_STYLE);
    Field(res, 1) = Val_int(GTK_BUTTONBOX_SPREAD);
    Field(res, 2) = Val_int(GTK_BUTTONBOX_EDGE);
    Field(res, 3) = Val_int(GTK_BUTTONBOX_START);
    Field(res, 4) = Val_int(GTK_BUTTONBOX_END);
    return res;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_button_box_get_type(value dummy) { /* ML */
    return Val_int(gtk_button_box_get_type());
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_button_get_type(value dummy) { /* ML */
    return Val_int(gtk_button_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_button_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_button_new());
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_button_new_with_label(value label) { /* ML */
    return Val_GtkObj(gtk_button_new_with_label(String_val(label)));
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_button_new_from_stock(value stock_id) { /* ML */
    return Val_GtkObj(gtk_button_new_from_stock(String_val(stock_id)));
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_button_new_with_mnemonic(value label) { /* ML */
    return Val_GtkObj(gtk_button_new_with_mnemonic(String_val(label)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_button_pressed(value self) { /* ML */
    gtk_button_pressed(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_button_released(value self) { /* ML */
    gtk_button_released(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_button_clicked(value self) { /* ML */
    gtk_button_clicked(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_button_enter(value self) { /* ML */
    gtk_button_enter(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_button_leave(value self) { /* ML */
    gtk_button_leave(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_button_set_relief(value self, value newstyle) { /* ML */
    gtk_button_set_relief(GtkObj_val(self), Int_val(newstyle));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_button_get_relief(value self) { /* ML */
    return Val_int(gtk_button_get_relief(GtkObj_val(self)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_button_set_label(value self, value label) { /* ML */
    gtk_button_set_label(GtkObj_val(self), String_val(label));
    return Val_unit;
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_button_get_label(value self) { /* ML */
    return my_copy_string(gtk_button_get_label(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_button_set_use_underline(value self, value use_underline) { /* ML */
    gtk_button_set_use_underline(GtkObj_val(self), Bool_val(use_underline));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_button_get_use_underline(value self) { /* ML */
    return Val_bool(gtk_button_get_use_underline(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_button_set_use_stock(value self, value use_stock) { /* ML */
    gtk_button_set_use_stock(GtkObj_val(self), Bool_val(use_stock));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_button_get_use_stock(value self) { /* ML */
    return Val_bool(gtk_button_get_use_stock(GtkObj_val(self)));
}



/* *** ToggleButton *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_toggle_button_get_type(value dummy) { /* ML */
    return Val_int(gtk_toggle_button_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_toggle_button_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_toggle_button_new());
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_toggle_button_new_with_label(value label) { /* ML */
    return Val_GtkObj(gtk_toggle_button_new_with_label(String_val(label)));
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_toggle_button_new_with_mnemonic(value label) { /* ML */
    return Val_GtkObj(gtk_toggle_button_new_with_mnemonic(String_val(label)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_toggle_button_set_mode(value self, value draw_indicator) { /* ML */
    gtk_toggle_button_set_mode(GtkObj_val(self), Bool_val(draw_indicator));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_toggle_button_get_mode(value self) { /* ML */
    return Val_bool(gtk_toggle_button_get_mode(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_toggle_button_set_active(value self, value is_active) { /* ML */
    gtk_toggle_button_set_active(GtkObj_val(self), Bool_val(is_active));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_toggle_button_get_active(value self) { /* ML */
    return Val_bool(gtk_toggle_button_get_active(GtkObj_val(self)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_toggle_button_toggled(value self) { /* ML */
    gtk_toggle_button_toggled(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_toggle_button_set_inconsistent(value self, value setting) { /* ML */
    gtk_toggle_button_set_inconsistent(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_toggle_button_get_inconsistent(value self) { /* ML */
    return Val_bool(gtk_toggle_button_get_inconsistent(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_toggle_button_set_state(value self, value is_active) { /* ML */
    gtk_toggle_button_set_state(GtkObj_val(self), Bool_val(is_active));
    return Val_unit;
}



/* *** CheckButton *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_check_button_get_type(value dummy) { /* ML */
    return Val_int(gtk_check_button_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_check_button_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_check_button_new());
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_check_button_new_with_label(value label) { /* ML */
    return Val_GtkObj(gtk_check_button_new_with_label(String_val(label)));
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_check_button_new_with_mnemonic(value label) { /* ML */
    return Val_GtkObj(gtk_check_button_new_with_mnemonic(String_val(label)));
}



/* *** RadioButton *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_radio_button_get_type(value dummy) { /* ML */
    return Val_int(gtk_radio_button_get_type());
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_radio_button_new_from_widget(value group) { /* ML */
    return Val_GtkObj(gtk_radio_button_new_from_widget(GtkObj_val(group)));
}

/* ML type: cptr -> string -> cptr */
EXTERNML value mgtk_gtk_radio_button_new_with_label_from_widget(value group, value label) { /* ML */
    return Val_GtkObj(gtk_radio_button_new_with_label_from_widget(GtkObj_val(group), String_val(label)));
}

/* ML type: cptr -> string -> cptr */
EXTERNML value mgtk_gtk_radio_button_new_with_mnemonic_from_widget(value group, value label) { /* ML */
    return Val_GtkObj(gtk_radio_button_new_with_mnemonic_from_widget(GtkObj_val(group), String_val(label)));
}



/* *** OptionMenu *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_option_menu_get_type(value dummy) { /* ML */
    return Val_int(gtk_option_menu_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_option_menu_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_option_menu_new());
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_option_menu_get_menu(value self) { /* ML */
    return Val_GtkObj(gtk_option_menu_get_menu(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_option_menu_set_menu(value self, value menu) { /* ML */
    gtk_option_menu_set_menu(GtkObj_val(self), GtkObj_val(menu));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_option_menu_remove_menu(value self) { /* ML */
    gtk_option_menu_remove_menu(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_option_menu_get_history(value self) { /* ML */
    return Val_int(gtk_option_menu_get_history(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_option_menu_set_history(value self, value index) { /* ML */
    gtk_option_menu_set_history(GtkObj_val(self), Int_val(index));
    return Val_unit;
}



/* *** Box *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_box_get_type(value dummy) { /* ML */
    return Val_int(gtk_box_get_type());
}

/* ML type: cptr -> cptr -> bool -> bool -> int -> unit */
EXTERNML value mgtk_gtk_box_pack_start(value self, value child, value expand, value fill, value padding) { /* ML */
    gtk_box_pack_start(GtkObj_val(self), GtkObj_val(child), Bool_val(expand), Bool_val(fill), Int_val(padding));
    return Val_unit;
}

/* ML type: cptr -> cptr -> bool -> bool -> int -> unit */
EXTERNML value mgtk_gtk_box_pack_end(value self, value child, value expand, value fill, value padding) { /* ML */
    gtk_box_pack_end(GtkObj_val(self), GtkObj_val(child), Bool_val(expand), Bool_val(fill), Int_val(padding));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_box_pack_start_defaults(value self, value widget) { /* ML */
    gtk_box_pack_start_defaults(GtkObj_val(self), GtkObj_val(widget));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_box_pack_end_defaults(value self, value widget) { /* ML */
    gtk_box_pack_end_defaults(GtkObj_val(self), GtkObj_val(widget));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_box_set_homogeneous(value self, value homogeneous) { /* ML */
    gtk_box_set_homogeneous(GtkObj_val(self), Bool_val(homogeneous));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_box_get_homogeneous(value self) { /* ML */
    return Val_bool(gtk_box_get_homogeneous(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_box_set_spacing(value self, value spacing) { /* ML */
    gtk_box_set_spacing(GtkObj_val(self), Int_val(spacing));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_box_get_spacing(value self) { /* ML */
    return Val_int(gtk_box_get_spacing(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> int -> unit */
EXTERNML value mgtk_gtk_box_reorder_child(value self, value child, value position) { /* ML */
    gtk_box_reorder_child(GtkObj_val(self), GtkObj_val(child), Int_val(position));
    return Val_unit;
}

/* ML type: int -> unit */
EXTERNML value mgtk_gtk_box_set_child_packing(value mgtk_params) { /* ML */
    value self = Field(mgtk_params, 0);
    value child = Field(mgtk_params, 1);
    value expand = Field(mgtk_params, 2);
    value fill = Field(mgtk_params, 3);
    value padding = Field(mgtk_params, 4);
    value pack_type = Field(mgtk_params, 5);
    gtk_box_set_child_packing(GtkObj_val(self), GtkObj_val(child), Bool_val(expand), Bool_val(fill), Int_val(padding), Int_val(pack_type));
    return Val_unit;
}



/* *** VBox *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_vbox_get_type(value dummy) { /* ML */
    return Val_int(gtk_vbox_get_type());
}

/* ML type: bool -> int -> cptr */
EXTERNML value mgtk_gtk_vbox_new(value homogeneous, value spacing) { /* ML */
    return Val_GtkObj(gtk_vbox_new(Bool_val(homogeneous), Int_val(spacing)));
}



/* *** ColorSelection *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_color_selection_get_type(value dummy) { /* ML */
    return Val_int(gtk_color_selection_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_color_selection_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_color_selection_new());
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_color_selection_get_has_opacity_control(value self) { /* ML */
    return Val_bool(gtk_color_selection_get_has_opacity_control(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_color_selection_set_has_opacity_control(value self, value has_opacity) { /* ML */
    gtk_color_selection_set_has_opacity_control(GtkObj_val(self), Bool_val(has_opacity));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_color_selection_get_has_palette(value self) { /* ML */
    return Val_bool(gtk_color_selection_get_has_palette(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_color_selection_set_has_palette(value self, value has_palette) { /* ML */
    gtk_color_selection_set_has_palette(GtkObj_val(self), Bool_val(has_palette));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_color_selection_set_current_alpha(value self, value alpha) { /* ML */
    gtk_color_selection_set_current_alpha(GtkObj_val(self), Int_val(alpha));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_color_selection_get_current_alpha(value self) { /* ML */
    return Val_int(gtk_color_selection_get_current_alpha(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_color_selection_set_previous_alpha(value self, value alpha) { /* ML */
    gtk_color_selection_set_previous_alpha(GtkObj_val(self), Int_val(alpha));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_color_selection_get_previous_alpha(value self) { /* ML */
    return Val_int(gtk_color_selection_get_previous_alpha(GtkObj_val(self)));
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_color_selection_is_adjusting(value self) { /* ML */
    return Val_bool(gtk_color_selection_is_adjusting(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_color_selection_set_update_policy(value self, value policy) { /* ML */
    gtk_color_selection_set_update_policy(GtkObj_val(self), Int_val(policy));
    return Val_unit;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_color_selection_dialog_get_type(value dummy) { /* ML */
    return Val_int(gtk_color_selection_dialog_get_type());
}



/* *** FontSelection *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_font_selection_get_type(value dummy) { /* ML */
    return Val_int(gtk_font_selection_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_font_selection_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_font_selection_new());
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_font_selection_get_font_name(value self) { /* ML */
    return my_copy_string(gtk_font_selection_get_font_name(GtkObj_val(self)));
}

/* ML type: cptr -> string -> bool */
EXTERNML value mgtk_gtk_font_selection_set_font_name(value self, value fontname) { /* ML */
    return Val_bool(gtk_font_selection_set_font_name(GtkObj_val(self), String_val(fontname)));
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_font_selection_get_preview_text(value self) { /* ML */
    return my_copy_string(gtk_font_selection_get_preview_text(GtkObj_val(self)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_font_selection_set_preview_text(value self, value text) { /* ML */
    gtk_font_selection_set_preview_text(GtkObj_val(self), String_val(text));
    return Val_unit;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_font_selection_dialog_get_type(value dummy) { /* ML */
    return Val_int(gtk_font_selection_dialog_get_type());
}



/* *** GammaCurve *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_gamma_curve_get_type(value dummy) { /* ML */
    return Val_int(gtk_gamma_curve_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_gamma_curve_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_gamma_curve_new());
}



/* *** HBox *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_hbox_get_type(value dummy) { /* ML */
    return Val_int(gtk_hbox_get_type());
}

/* ML type: bool -> int -> cptr */
EXTERNML value mgtk_gtk_hbox_new(value homogeneous, value spacing) { /* ML */
    return Val_GtkObj(gtk_hbox_new(Bool_val(homogeneous), Int_val(spacing)));
}



/* *** Statusbar *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_statusbar_get_type(value dummy) { /* ML */
    return Val_int(gtk_statusbar_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_statusbar_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_statusbar_new());
}

/* ML type: cptr -> string -> int */
EXTERNML value mgtk_gtk_statusbar_get_context_id(value self, value context_description) { /* ML */
    return Val_int(gtk_statusbar_get_context_id(GtkObj_val(self), String_val(context_description)));
}

/* ML type: cptr -> int -> string -> int */
EXTERNML value mgtk_gtk_statusbar_push(value self, value context_id, value text) { /* ML */
    return Val_int(gtk_statusbar_push(GtkObj_val(self), Int_val(context_id), String_val(text)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_statusbar_pop(value self, value context_id) { /* ML */
    gtk_statusbar_pop(GtkObj_val(self), Int_val(context_id));
    return Val_unit;
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_statusbar_remove(value self, value context_id, value message_id) { /* ML */
    gtk_statusbar_remove(GtkObj_val(self), Int_val(context_id), Int_val(message_id));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_statusbar_set_has_resize_grip(value self, value setting) { /* ML */
    gtk_statusbar_set_has_resize_grip(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_statusbar_get_has_resize_grip(value self) { /* ML */
    return Val_bool(gtk_statusbar_get_has_resize_grip(GtkObj_val(self)));
}



/* *** Combo *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_combo_get_type(value dummy) { /* ML */
    return Val_int(gtk_combo_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_combo_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_combo_new());
}

/* ML type: cptr -> bool -> bool -> unit */
EXTERNML value mgtk_gtk_combo_set_value_in_list(value self, value val, value ok_if_empty) { /* ML */
    gtk_combo_set_value_in_list(GtkObj_val(self), Bool_val(val), Bool_val(ok_if_empty));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_combo_set_use_arrows(value self, value val) { /* ML */
    gtk_combo_set_use_arrows(GtkObj_val(self), Bool_val(val));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_combo_set_use_arrows_always(value self, value val) { /* ML */
    gtk_combo_set_use_arrows_always(GtkObj_val(self), Bool_val(val));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_combo_set_case_sensitive(value self, value val) { /* ML */
    gtk_combo_set_case_sensitive(GtkObj_val(self), Bool_val(val));
    return Val_unit;
}

/* ML type: cptr -> cptr -> string -> unit */
EXTERNML value mgtk_gtk_combo_set_item_string(value self, value item, value item_value) { /* ML */
    gtk_combo_set_item_string(GtkObj_val(self), GtkObj_val(item), String_val(item_value));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_combo_disable_activate(value self) { /* ML */
    gtk_combo_disable_activate(GtkObj_val(self));
    return Val_unit;
}



/* *** ButtonBox *** */
/* ML type: cptr -> cptr -> bool -> unit */
EXTERNML value mgtk_gtk_button_box_set_child_secondary(value self, value child, value is_secondary) { /* ML */
    gtk_button_box_set_child_secondary(GtkObj_val(self), GtkObj_val(child), Bool_val(is_secondary));
    return Val_unit;
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_button_box_set_child_size(value self, value min_width, value min_height) { /* ML */
    gtk_button_box_set_child_size(GtkObj_val(self), Int_val(min_width), Int_val(min_height));
    return Val_unit;
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_button_box_set_child_ipadding(value self, value ipad_x, value ipad_y) { /* ML */
    gtk_button_box_set_child_ipadding(GtkObj_val(self), Int_val(ipad_x), Int_val(ipad_y));
    return Val_unit;
}



/* *** VButtonBox *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_vbutton_box_get_type(value dummy) { /* ML */
    return Val_int(gtk_vbutton_box_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_vbutton_box_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_vbutton_box_new());
}

/* ML type: unit -> int */
EXTERNML value mgtk_gtk_vbutton_box_get_spacing_default(value dummy) { /* ML */
    return Val_int(gtk_vbutton_box_get_spacing_default());
}

/* ML type: int -> unit */
EXTERNML value mgtk_gtk_vbutton_box_set_spacing_default(value spacing) { /* ML */
    gtk_vbutton_box_set_spacing_default(Int_val(spacing));
    return Val_unit;
}



/* *** HButtonBox *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_hbutton_box_get_type(value dummy) { /* ML */
    return Val_int(gtk_hbutton_box_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_hbutton_box_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_hbutton_box_new());
}

/* ML type: unit -> int */
EXTERNML value mgtk_gtk_hbutton_box_get_spacing_default(value dummy) { /* ML */
    return Val_int(gtk_hbutton_box_get_spacing_default());
}

/* ML type: int -> unit */
EXTERNML value mgtk_gtk_hbutton_box_set_spacing_default(value spacing) { /* ML */
    gtk_hbutton_box_set_spacing_default(Int_val(spacing));
    return Val_unit;
}



/* *** Calendar *** */
/* ML type: unit -> int * int * int * int * int */
EXTERNML value mgtk_get_gtk_calendar_display_options(value dummy) { /* ML */
    value res = alloc_tuple(5);
    Field(res, 0) = Val_int(GTK_CALENDAR_SHOW_HEADING);
    Field(res, 1) = Val_int(GTK_CALENDAR_SHOW_DAY_NAMES);
    Field(res, 2) = Val_int(GTK_CALENDAR_NO_MONTH_CHANGE);
    Field(res, 3) = Val_int(GTK_CALENDAR_SHOW_WEEK_NUMBERS);
    Field(res, 4) = Val_int(GTK_CALENDAR_WEEK_START_MONDAY);
    return res;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_calendar_get_type(value dummy) { /* ML */
    return Val_int(gtk_calendar_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_calendar_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_calendar_new());
}

/* ML type: cptr -> int -> int -> bool */
EXTERNML value mgtk_gtk_calendar_select_month(value self, value month, value year) { /* ML */
    return Val_bool(gtk_calendar_select_month(GtkObj_val(self), Int_val(month), Int_val(year)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_calendar_select_day(value self, value day) { /* ML */
    gtk_calendar_select_day(GtkObj_val(self), Int_val(day));
    return Val_unit;
}

/* ML type: cptr -> int -> bool */
EXTERNML value mgtk_gtk_calendar_mark_day(value self, value day) { /* ML */
    return Val_bool(gtk_calendar_mark_day(GtkObj_val(self), Int_val(day)));
}

/* ML type: cptr -> int -> bool */
EXTERNML value mgtk_gtk_calendar_unmark_day(value self, value day) { /* ML */
    return Val_bool(gtk_calendar_unmark_day(GtkObj_val(self), Int_val(day)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_calendar_clear_marks(value self) { /* ML */
    gtk_calendar_clear_marks(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_calendar_display_options(value self, value flags) { /* ML */
    gtk_calendar_display_options(GtkObj_val(self), Int_val(flags));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_calendar_freeze(value self) { /* ML */
    gtk_calendar_freeze(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_calendar_thaw(value self) { /* ML */
    gtk_calendar_thaw(GtkObj_val(self));
    return Val_unit;
}



/* *** Window *** */
/* ML type: unit -> int * int * int * int * int */
EXTERNML value mgtk_get_gtk_window_position(value dummy) { /* ML */
    value res = alloc_tuple(5);
    Field(res, 0) = Val_int(GTK_WIN_POS_NONE);
    Field(res, 1) = Val_int(GTK_WIN_POS_CENTER);
    Field(res, 2) = Val_int(GTK_WIN_POS_MOUSE);
    Field(res, 3) = Val_int(GTK_WIN_POS_CENTER_ALWAYS);
    Field(res, 4) = Val_int(GTK_WIN_POS_CENTER_ON_PARENT);
    return res;
}

/* ML type: unit -> int * int */
EXTERNML value mgtk_get_gtk_window_type(value dummy) { /* ML */
    value res = alloc_tuple(2);
    Field(res, 0) = Val_int(GTK_WINDOW_TOPLEVEL);
    Field(res, 1) = Val_int(GTK_WINDOW_POPUP);
    return res;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_window_get_type(value dummy) { /* ML */
    return Val_int(gtk_window_get_type());
}

/* ML type: int -> cptr */
EXTERNML value mgtk_gtk_window_new(value type) { /* ML */
    return Val_GtkObj(gtk_window_new(Int_val(type)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_window_set_title(value self, value title) { /* ML */
    gtk_window_set_title(GtkObj_val(self), String_val(title));
    return Val_unit;
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_window_get_title(value self) { /* ML */
    return my_copy_string(gtk_window_get_title(GtkObj_val(self)));
}

/* ML type: cptr -> string -> string -> unit */
EXTERNML value mgtk_gtk_window_set_wmclass(value self, value wmclass_name, value wmclass_class) { /* ML */
    gtk_window_set_wmclass(GtkObj_val(self), String_val(wmclass_name), String_val(wmclass_class));
    return Val_unit;
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_window_set_role(value self, value role) { /* ML */
    gtk_window_set_role(GtkObj_val(self), String_val(role));
    return Val_unit;
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_window_get_role(value self) { /* ML */
    return my_copy_string(gtk_window_get_role(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_window_add_accel_group(value self, value accel_group) { /* ML */
    gtk_window_add_accel_group(GtkObj_val(self), GtkObj_val(accel_group));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_window_remove_accel_group(value self, value accel_group) { /* ML */
    gtk_window_remove_accel_group(GtkObj_val(self), GtkObj_val(accel_group));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_window_set_position(value self, value position) { /* ML */
    gtk_window_set_position(GtkObj_val(self), Int_val(position));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_window_activate_focus(value self) { /* ML */
    return Val_bool(gtk_window_activate_focus(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_window_set_focus(value self, value focus) { /* ML */
    gtk_window_set_focus(GtkObj_val(self), GtkObj_val(focus));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_window_get_focus(value self) { /* ML */
    return Val_GtkObj(gtk_window_get_focus(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_window_set_default(value self, value default_widget) { /* ML */
    gtk_window_set_default(GtkObj_val(self), GtkObj_val(default_widget));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_window_activate_default(value self) { /* ML */
    return Val_bool(gtk_window_activate_default(GtkObj_val(self)));
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_window_set_transient_for(value self, value parent) { /* ML */
    gtk_window_set_transient_for(GtkObj_val(self), GtkObj_val(parent));
    return Val_unit;
}

/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_window_get_transient_for(value self) { /* ML */
    return Val_GtkObj(gtk_window_get_transient_for(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_window_set_destroy_with_parent(value self, value setting) { /* ML */
    gtk_window_set_destroy_with_parent(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_window_get_destroy_with_parent(value self) { /* ML */
    return Val_bool(gtk_window_get_destroy_with_parent(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_window_set_resizable(value self, value resizable) { /* ML */
    gtk_window_set_resizable(GtkObj_val(self), Bool_val(resizable));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_window_get_resizable(value self) { /* ML */
    return Val_bool(gtk_window_get_resizable(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_window_set_has_frame(value self, value setting) { /* ML */
    gtk_window_set_has_frame(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_window_get_has_frame(value self) { /* ML */
    return Val_bool(gtk_window_get_has_frame(GtkObj_val(self)));
}

/* ML type: cptr -> int -> int -> int -> int -> unit */
EXTERNML value mgtk_gtk_window_set_frame_dimensions(value self, value left, value top, value right, value bottom) { /* ML */
    gtk_window_set_frame_dimensions(GtkObj_val(self), Int_val(left), Int_val(top), Int_val(right), Int_val(bottom));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_window_set_decorated(value self, value setting) { /* ML */
    gtk_window_set_decorated(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_window_get_decorated(value self) { /* ML */
    return Val_bool(gtk_window_get_decorated(GtkObj_val(self)));
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_window_set_modal(value self, value modal) { /* ML */
    gtk_window_set_modal(GtkObj_val(self), Bool_val(modal));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_window_get_modal(value self) { /* ML */
    return Val_bool(gtk_window_get_modal(GtkObj_val(self)));
}

/* ML type: cptr -> int -> cptr -> unit */
EXTERNML value mgtk_gtk_window_add_mnemonic(value self, value keyval, value target) { /* ML */
    gtk_window_add_mnemonic(GtkObj_val(self), Int_val(keyval), GtkObj_val(target));
    return Val_unit;
}

/* ML type: cptr -> int -> cptr -> unit */
EXTERNML value mgtk_gtk_window_remove_mnemonic(value self, value keyval, value target) { /* ML */
    gtk_window_remove_mnemonic(GtkObj_val(self), Int_val(keyval), GtkObj_val(target));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_window_present(value self) { /* ML */
    gtk_window_present(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_window_iconify(value self) { /* ML */
    gtk_window_iconify(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_window_deiconify(value self) { /* ML */
    gtk_window_deiconify(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_window_stick(value self) { /* ML */
    gtk_window_stick(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_window_unstick(value self) { /* ML */
    gtk_window_unstick(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_window_maximize(value self) { /* ML */
    gtk_window_maximize(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_window_unmaximize(value self) { /* ML */
    gtk_window_unmaximize(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> int -> int -> int -> int -> unit */
EXTERNML value mgtk_gtk_window_begin_move_drag(value self, value button, value root_x, value root_y, value timestamp) { /* ML */
    gtk_window_begin_move_drag(GtkObj_val(self), Int_val(button), Int_val(root_x), Int_val(root_y), Int_val(timestamp));
    return Val_unit;
}

/* ML type: cptr -> int -> int -> int -> unit */
EXTERNML value mgtk_gtk_window_set_policy(value self, value allow_shrink, value allow_grow, value auto_shrink) { /* ML */
    gtk_window_set_policy(GtkObj_val(self), Int_val(allow_shrink), Int_val(allow_grow), Int_val(auto_shrink));
    return Val_unit;
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_window_set_default_size(value self, value width, value height) { /* ML */
    gtk_window_set_default_size(GtkObj_val(self), Int_val(width), Int_val(height));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_window_get_default_size(value self) { /* ML */
    value result;
    int width;
    int height;
    gtk_window_get_default_size(GtkObj_val(self), &width, &height);
    result = alloc_tuple(2);
    Field(result, 0) = Val_int(width);
    Field(result, 1) = Val_int(height);
    return result;
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_window_resize(value self, value width, value height) { /* ML */
    gtk_window_resize(GtkObj_val(self), Int_val(width), Int_val(height));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_window_get_size(value self) { /* ML */
    value result;
    int width;
    int height;
    gtk_window_get_size(GtkObj_val(self), &width, &height);
    result = alloc_tuple(2);
    Field(result, 0) = Val_int(width);
    Field(result, 1) = Val_int(height);
    return result;
}

/* ML type: cptr -> int -> int -> unit */
EXTERNML value mgtk_gtk_window_move(value self, value x, value y) { /* ML */
    gtk_window_move(GtkObj_val(self), Int_val(x), Int_val(y));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_window_get_position(value self) { /* ML */
    value result;
    int root_x;
    int root_y;
    gtk_window_get_position(GtkObj_val(self), &root_x, &root_y);
    result = alloc_tuple(2);
    Field(result, 0) = Val_int(root_x);
    Field(result, 1) = Val_int(root_y);
    return result;
}

/* ML type: cptr -> string -> bool */
EXTERNML value mgtk_gtk_window_parse_geometry(value self, value geometry) { /* ML */
    return Val_bool(gtk_window_parse_geometry(GtkObj_val(self), String_val(geometry)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_window_reshow_with_initial_size(value self) { /* ML */
    gtk_window_reshow_with_initial_size(GtkObj_val(self));
    return Val_unit;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_window_group_get_type(value dummy) { /* ML */
    return Val_int(gtk_window_group_get_type());
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_window_remove_embedded_xid(value self, value xid) { /* ML */
    gtk_window_remove_embedded_xid(GtkObj_val(self), Int_val(xid));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_window_add_embedded_xid(value self, value xid) { /* ML */
    gtk_window_add_embedded_xid(GtkObj_val(self), Int_val(xid));
    return Val_unit;
}



/* *** Plug *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_plug_get_type(value dummy) { /* ML */
    return Val_int(gtk_plug_get_type());
}



/* *** Dialog *** */
/* ML type: unit -> int * int * int */
EXTERNML value mgtk_get_gtk_dialog_flags(value dummy) { /* ML */
    value res = alloc_tuple(3);
    Field(res, 0) = Val_int(GTK_DIALOG_MODAL);
    Field(res, 1) = Val_int(GTK_DIALOG_DESTROY_WITH_PARENT);
    Field(res, 2) = Val_int(GTK_DIALOG_NO_SEPARATOR);
    return res;
}

/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_dialog_get_type(value dummy) { /* ML */
    return Val_int(gtk_dialog_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_dialog_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_dialog_new());
}

/* ML type: string -> cptr -> int -> string -> cptr */
EXTERNML value mgtk_gtk_dialog_new_with_buttons(value title, value parent, value flags, value first_button_text) { /* ML */
    return Val_GtkObj(gtk_dialog_new_with_buttons(String_val(title), GtkObj_val(parent), Int_val(flags), String_val(first_button_text)));
}

/* ML type: cptr -> cptr -> int -> unit */
EXTERNML value mgtk_gtk_dialog_add_action_widget(value self, value child, value response_id) { /* ML */
    gtk_dialog_add_action_widget(GtkObj_val(self), GtkObj_val(child), Int_val(response_id));
    return Val_unit;
}

/* ML type: cptr -> string -> int -> cptr */
EXTERNML value mgtk_gtk_dialog_add_button(value self, value button_text, value response_id) { /* ML */
    return Val_GtkObj(gtk_dialog_add_button(GtkObj_val(self), String_val(button_text), Int_val(response_id)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_dialog_add_buttons(value self, value first_button_text) { /* ML */
    gtk_dialog_add_buttons(GtkObj_val(self), String_val(first_button_text));
    return Val_unit;
}

/* ML type: cptr -> int -> bool -> unit */
EXTERNML value mgtk_gtk_dialog_set_response_sensitive(value self, value response_id, value setting) { /* ML */
    gtk_dialog_set_response_sensitive(GtkObj_val(self), Int_val(response_id), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_dialog_set_default_response(value self, value response_id) { /* ML */
    gtk_dialog_set_default_response(GtkObj_val(self), Int_val(response_id));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_dialog_set_has_separator(value self, value setting) { /* ML */
    gtk_dialog_set_has_separator(GtkObj_val(self), Bool_val(setting));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_dialog_get_has_separator(value self) { /* ML */
    return Val_bool(gtk_dialog_get_has_separator(GtkObj_val(self)));
}

/* ML type: cptr -> int -> unit */
EXTERNML value mgtk_gtk_dialog_response(value self, value response_id) { /* ML */
    gtk_dialog_response(GtkObj_val(self), Int_val(response_id));
    return Val_unit;
}

/* ML type: cptr -> int */
EXTERNML value mgtk_gtk_dialog_run(value self) { /* ML */
    return Val_int(gtk_dialog_run(GtkObj_val(self)));
}



/* *** MessageDialog *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_message_dialog_get_type(value dummy) { /* ML */
    return Val_int(gtk_message_dialog_get_type());
}



/* *** InputDialog *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_input_dialog_get_type(value dummy) { /* ML */
    return Val_int(gtk_input_dialog_get_type());
}

/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_input_dialog_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_input_dialog_new());
}



/* *** FontSelectionDialog *** */
/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_font_selection_dialog_new(value title) { /* ML */
    return Val_GtkObj(gtk_font_selection_dialog_new(String_val(title)));
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_font_selection_dialog_get_font_name(value self) { /* ML */
    return my_copy_string(gtk_font_selection_dialog_get_font_name(GtkObj_val(self)));
}

/* ML type: cptr -> string -> bool */
EXTERNML value mgtk_gtk_font_selection_dialog_set_font_name(value self, value fontname) { /* ML */
    return Val_bool(gtk_font_selection_dialog_set_font_name(GtkObj_val(self), String_val(fontname)));
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_font_selection_dialog_get_preview_text(value self) { /* ML */
    return my_copy_string(gtk_font_selection_dialog_get_preview_text(GtkObj_val(self)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_font_selection_dialog_set_preview_text(value self, value text) { /* ML */
    gtk_font_selection_dialog_set_preview_text(GtkObj_val(self), String_val(text));
    return Val_unit;
}



/* *** FileSelection *** */
/* ML type: unit -> GType.t */
EXTERNML value mgtk_gtk_file_selection_get_type(value dummy) { /* ML */
    return Val_int(gtk_file_selection_get_type());
}

/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_file_selection_new(value title) { /* ML */
    return Val_GtkObj(gtk_file_selection_new(String_val(title)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_file_selection_set_filename(value self, value filename) { /* ML */
    gtk_file_selection_set_filename(GtkObj_val(self), String_val(filename));
    return Val_unit;
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_file_selection_get_filename(value self) { /* ML */
    return my_copy_string(gtk_file_selection_get_filename(GtkObj_val(self)));
}

/* ML type: cptr -> string -> unit */
EXTERNML value mgtk_gtk_file_selection_complete(value self, value pattern) { /* ML */
    gtk_file_selection_complete(GtkObj_val(self), String_val(pattern));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_file_selection_show_fileop_buttons(value self) { /* ML */
    gtk_file_selection_show_fileop_buttons(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_file_selection_hide_fileop_buttons(value self) { /* ML */
    gtk_file_selection_hide_fileop_buttons(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> bool -> unit */
EXTERNML value mgtk_gtk_file_selection_set_select_multiple(value self, value select_multiple) { /* ML */
    gtk_file_selection_set_select_multiple(GtkObj_val(self), Bool_val(select_multiple));
    return Val_unit;
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_file_selection_get_select_multiple(value self) { /* ML */
    return Val_bool(gtk_file_selection_get_select_multiple(GtkObj_val(self)));
}



/* *** ColorSelectionDialog *** */
/* ML type: string -> cptr */
EXTERNML value mgtk_gtk_color_selection_dialog_new(value title) { /* ML */
    return Val_GtkObj(gtk_color_selection_dialog_new(String_val(title)));
}



/* *** WindowGroup *** */
/* ML type: unit -> cptr */
EXTERNML value mgtk_gtk_window_group_new(value dummy) { /* ML */
    return Val_GtkObj(gtk_window_group_new());
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_window_group_add_window(value self, value window) { /* ML */
    gtk_window_group_add_window(GtkObj_val(self), GtkObj_val(window));
    return Val_unit;
}

/* ML type: cptr -> cptr -> unit */
EXTERNML value mgtk_gtk_window_group_remove_window(value self, value window) { /* ML */
    gtk_window_group_remove_window(GtkObj_val(self), GtkObj_val(window));
    return Val_unit;
}



/* *** Clipboard *** */
/* ML type: cptr -> cptr */
EXTERNML value mgtk_gtk_clipboard_get_owner(value self) { /* ML */
    return Val_GtkObj(gtk_clipboard_get_owner(GtkObj_val(self)));
}

/* ML type: cptr -> unit */
EXTERNML value mgtk_gtk_clipboard_clear(value self) { /* ML */
    gtk_clipboard_clear(GtkObj_val(self));
    return Val_unit;
}

/* ML type: cptr -> string -> int -> unit */
EXTERNML value mgtk_gtk_clipboard_set_text(value self, value text, value len) { /* ML */
    gtk_clipboard_set_text(GtkObj_val(self), String_val(text), Int_val(len));
    return Val_unit;
}

/* ML type: cptr -> string */
EXTERNML value mgtk_gtk_clipboard_wait_for_text(value self) { /* ML */
    return my_copy_string(gtk_clipboard_wait_for_text(GtkObj_val(self)));
}

/* ML type: cptr -> bool */
EXTERNML value mgtk_gtk_clipboard_wait_is_text_available(value self) { /* ML */
    return Val_bool(gtk_clipboard_wait_is_text_available(GtkObj_val(self)));
}

