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

/* *** Nulls and options *** */

/* [GtkOption_nullok(opt)] returns NULL if opt is
   NONE, otherwise the pointer contained in opt.
   THIS ONLY WORKS IF OPT CONTAINS A WIDGET POINTER!
*/
void *GtkObjOption_nullok(value opt) {
  void *res;
  int contag = Tag_val(opt);
  if(contag == SOMEtag) {
      value val;
      val = Field(opt, 0);
      res = GtkObj_val(val);
  } else { /* must be NONE */
      res = NULL;
  };
  return res;
}

/* [StringOption_nullok(opt)] returns NULL if opt is
   NONE, otherwise the pointer contained in opt.
   THIS ONLY WORKS IF OPT CONTAINS A STRING!
*/
char *StringOption_nullok(value opt) {
  void *res;
  int contag = Tag_val(opt);
  if(contag == SOMEtag) {
      value val;
      val = Field(opt, 0);
      res = String_val(val);
  } else { /* must be NONE */
      res = NULL;
  };
  return res;
}

/* *** Signal stuff *** */
void mgtk_callback_dispatch (GtkObject *object, gpointer data, guint nargs, 
			     GtkArg *args) {
  value res;
  valueptr mvp;

  Push_roots(r, 1);  // because both alloc_tuple and Val_GtkObj allocates
    r[0] = alloc_tuple(3);
    Field(r[0],0) = Val_GtkObj(object);
    Field(r[0],1) = (value) args;
    Field(r[0],2) = Val_int(nargs);
    res = r[0];
  Pop_roots();

  mvp = get_valueptr("mgtk_callback_dispatch"); 
  if(mvp == (valueptr) NULL)
    failwith("Cannot find mgtk_callback_dispatch");

  /* printf("callback id = %i\n",(int) data); */

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

/* *** GtkArg stuff *** */

/* ML type : GtkArgs -> int -> bool -> unit */
EXTERNML value mgtk_set_pos_bool (GtkArg *args, value pos, value val) { /* ML */
  int p = Int_val(pos);

  if (GTK_FUNDAMENTAL_TYPE(args[p].type) != GTK_TYPE_BOOL)
    failwith ("mgtk_set_pos_bool: argument type mismatch");
  
  *GTK_RETLOC_BOOL(args[p]) = Bool_val(val);
  return Val_unit;
}


/* *** Convertion from ML to C *** */
#define Mgtk_isCons(x) (Tag_val(x) != 0)
#define Mgtk_head(x) (Field(x, 0))
#define Mgtk_tail(x) (Field(x, 1))


/* CONDITION: conv must not allocate in the mosml heap */
#define Mgtk_SMLARRAY_TO_CARRAY(sarr, carr, conv)               \
{int MGTK_SMLARRAY_I, MGTK_SMLARRAY_SZ;                         \
 sarr = Field(sarr, 0); /* get underlying vector */             \
 MGTK_SMLARRAY_SZ = Wosize_val(sarr);                           \
 carr = malloc(MGTK_SMLARRAY_SZ);                               \
 /* FIXME: check result from malloc */                          \
 for(MGTK_SMLARRAY_I = 0; MGTK_SMLARRAY_I < MGTK_SMLARRAY_SZ;   \
     MGTK_SMLARRAY_I++)                                         \
   carr[MGTK_SMLARRAY_I] = conv(Field(sarr, MGTK_SMLARRAY_I));  \
}


/* *** Glib stuff *** */

/* *** glist stuff *** */

#define Mgtk_SMLLIST_TO_GLIST(sls,gls,conv)                             \
{value MGTK_SMLLIST_TEMP = sls;                                         \
 gls = NULL;                                                            \
 while (Mgtk_isCons(MGTK_SMLLIST_TEMP)){                                \
   value MGTK_SMLLIST_ELEM__TEMP = Mgtk_head(MGTK_SMLLIST_TEMP);        \
   gls = g_list_append (gls,conv(MGTK_SMLLIST_ELEM__TEMP));             \
   MGTK_SMLLIST_TEMP = Mgtk_tail(MGTK_SMLLIST_TEMP);                    \
 }                                                                      \
}
									
/* Shows how Mgtk_SMLLIST_TO_GLIST can be used */
GList* mgtk_smllist_to_glist_string(value smllist) {
  GList* glist;
  Mgtk_SMLLIST_TO_GLIST(smllist,glist,String_val);
  return glist;
}

GList* mgtk_smllist_to_glist_object(value smllist) {
  GList* glist;
  Mgtk_SMLLIST_TO_GLIST(smllist,glist,GtkObj_val);
  return glist;
}

/* *** Access functions to internal widget data *** */

GdkWindow *gtk_widget_get_window (GtkWidget *widget) {
  return widget->window;
}

GtkStateType gtk_widget_get_state (GtkWidget *widget) {
  return widget->state;
}

void gtk_widget_get_allocation (GtkWidget *widget, int *width, int *height,
				int *x, int *y) {
  *width = widget->allocation.width;
  *height = widget->allocation.height;
  *x = widget->allocation.x;
  *y = widget->allocation.y;
}

GdkGC *gtk_widget_get_style_fg_gc (GtkWidget *widget, GtkStateType state) {
  return widget->style->fg_gc[state];
}


