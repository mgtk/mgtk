#include <stdlib.h>

/* GTK stuff */
#include <gtk/gtk.h>
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
                                                                \
  *retloc(args[p]) = mlconv(val);                               \
  return Val_unit;                                              \
}

#define MGTK_MakeGetter(name, gtkvalue, convml)         \
EXTERNML value name (value vargs, value pos) { /* ML */ \
  long p = Long_val(pos);                               \
  GtkArg* args = (GtkArg*) GtkArg_val(vargs);           \
                                                        \
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

  Push_roots(r, 3);  // because both alloc_tuple and Val_GtkObj allocates
    r[0] = object == NULL ? Val_unit : Val_GtkObj(object);
    r[1] = wrap_GtkArg(args);
    res  = alloc_tuple(3);
    Field(res,0) = r[0]; 
    Field(res,1) = r[1];
    Field(res,2) = Val_int(nargs);   
  Pop_roots();

  mvp = get_valueptr("mgtk_callback_dispatch"); 
  if(mvp == (valueptr) NULL)
    failwith("Cannot find mgtk_callback_dispatch");

  /* printf("callback id = %i\n",(int) data); 
   */
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

EXTERNML value mgtk_gtk_timeout_add(value interval, value clb) {
  guint result;
  result = gtk_timeout_add_full (Int_val(interval), NULL,
                                 mgtk_callback_dispatch, (void*) clb,
                                 mgtk_callback_destroy);
  return Val_long(result);
}

EXTERNML value mgtk_gtk_timeout_remove(value id) {
  gtk_timeout_remove (Int_val(id));
  return Val_unit;
}



/* *** Convertion from ML to C *** */
#define Mgtk_isCons(x) (Tag_val(x) != 0)
#define Mgtk_head(x) (Field(x, 0))
#define Mgtk_tail(x) (Field(x, 1))


/* CONDITION: conv must not allocate in the mosml heap */
#define Mgtk_SMLARRAY_TO_CARRAY(sarr, carr, csize, conv)        \
{int MGTK_SMLARRAY_I, MGTK_SMLARRAY_SZ;                         \
 sarr = Field(sarr, 0); /* get underlying vector */             \
 MGTK_SMLARRAY_SZ = Wosize_val(sarr);                           \
 carr = malloc(MGTK_SMLARRAY_SZ*csize);                         \
 /* FIXME: check result from malloc */                          \
 for(MGTK_SMLARRAY_I = 0; MGTK_SMLARRAY_I < MGTK_SMLARRAY_SZ;   \
     MGTK_SMLARRAY_I++)                                         \
   carr[MGTK_SMLARRAY_I] = conv(Field(sarr, MGTK_SMLARRAY_I));  \
}

int mgtk_list_length(value sls) {
  int MGTK_SMLLIST_LEN = 0;
  value MGTK_SMLLIST_TEMP = sls;
  while (Mgtk_isCons(MGTK_SMLLIST_TEMP)) {
    MGTK_SMLLIST_TEMP = Mgtk_tail(MGTK_SMLLIST_TEMP);
    MGTK_SMLLIST_LEN++;
  }
  return MGTK_SMLLIST_LEN;
}

#define Mgtk_SMLLIST_TO_CARRAY(sls, carr, csize, conv)                  \
{int MGTK_SMLLIST_I = 0,                                                \
     MGTK_SMLLIST_SZ = mgtk_list_length(sls);                           \
 value MGTK_SMLLIST_TEMP = sls;                                         \
 carr = malloc(MGTK_SMLLIST_SZ*csize);                                  \
 /* FIXME: check result from malloc */                          \
 while (Mgtk_isCons(MGTK_SMLLIST_TEMP)){                                \
   value MGTK_SMLLIST_ELEM__TEMP = Mgtk_head(MGTK_SMLLIST_TEMP);        \
   carr[MGTK_SMLLIST_I++] = conv(MGTK_SMLLIST_ELEM__TEMP);              \
   MGTK_SMLLIST_TEMP = Mgtk_tail(MGTK_SMLLIST_TEMP);                    \
 }                                                                      \
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

char** mgtk_smllist_to_string_array(value smllist) {
  char **strs;
  Mgtk_SMLLIST_TO_CARRAY(smllist, strs, sizeof(char*), String_val);
  return strs;
}

char** mgtk_smlarray_to_string_array(value smlarray) {
  char **strings;
  Mgtk_SMLARRAY_TO_CARRAY(smlarray, strings, sizeof(char*), String_val);
  return strings;
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

typedef enum {
  GTK_GC_TYPE_FG,
  GTK_GC_TYPE_BG,
  GTK_GC_TYPE_LIGHT,
  GTK_GC_TYPE_DARK,
  GTK_GC_TYPE_MID,
  GTK_GC_TYPE_TEXT,
  GTK_GC_TYPE_BASE,
} GtkGCType;

GdkGC *gtk_widget_get_style_gc (GtkWidget *widget, GtkStateType state,
				GtkGCType typ) {
  GdkGC *rv = widget->style->fg_gc[state];
  switch (typ) {
  case GTK_GC_TYPE_FG: rv = widget->style->fg_gc[state]; break;
  case GTK_GC_TYPE_BG: rv = widget->style->bg_gc[state]; break;
  case GTK_GC_TYPE_LIGHT: rv = widget->style->light_gc[state]; break;
  case GTK_GC_TYPE_DARK: rv = widget->style->dark_gc[state]; break;
  case GTK_GC_TYPE_MID: rv = widget->style->mid_gc[state]; break;
  case GTK_GC_TYPE_TEXT: rv = widget->style->text_gc[state]; break;
  case GTK_GC_TYPE_BASE: rv = widget->style->base_gc[state]; break;
  }
  return rv;
}

typedef enum {
  GTK_COLOR_TYPE_FG,
  GTK_COLOR_TYPE_BG,
  GTK_COLOR_TYPE_LIGHT,
  GTK_COLOR_TYPE_DARK,
  GTK_COLOR_TYPE_MID,
  GTK_COLOR_TYPE_TEXT,
  GTK_COLOR_TYPE_BASE,
} GtkColorType;

void gtk_widget_get_style_color (GtkWidget *widget, GtkStateType state,
				 GtkColorType typ,
				 GdkColor *color) {
  *color = widget->style->fg[state];
  switch (typ) {
  case GTK_COLOR_TYPE_FG: *color = widget->style->fg[state]; break;
  case GTK_COLOR_TYPE_BG: *color = widget->style->bg[state]; break;
  case GTK_COLOR_TYPE_LIGHT: *color = widget->style->light[state]; break;
  case GTK_COLOR_TYPE_DARK: *color = widget->style->dark[state]; break;
  case GTK_COLOR_TYPE_MID: *color = widget->style->mid[state]; break;
  case GTK_COLOR_TYPE_TEXT: *color = widget->style->text[state]; break;
  case GTK_COLOR_TYPE_BASE: *color = widget->style->base[state]; break;
  }
}
