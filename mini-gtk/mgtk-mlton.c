/* mgtk --- an SML binding for GTK.                                          */
/* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       */
/*                                                                           */

/* GTK stuff */
#include <gtk/gtk.h>

/* MLton */
#include "mgtk-mlton.h"

#ifdef WIN32
#define EXTERNML __declspec(dllexport)
#else
#define EXTERNML
#endif

/* FIXME: Do we really want this? */
EXTERNML char mgtk_stringsub(char *s, int i) {
  return s[i];
}


/* FIXME: Does MLton really like this */
EXTERNML void mgtk_init(char** argv, int argc) { /* ML */
  gtk_init(&argc, &argv);
}


/* *** GValue stuff *** */

#define MGTK_MakeGetter(name, rettype, gval_getter)     \
EXTERNML rettype name (GValue* args, long p) { /* ML */ \
  return gval_getter(&args[p]);                         \
}

MGTK_MakeGetter(mgtk_get_pos_bool, Bool, g_value_get_boolean)
MGTK_MakeGetter(mgtk_get_pos_int, long, g_value_get_long)


/* *** Signal stuff *** */
static void mgtk_callback_dispatch (GClosure *closure,
                                    GValue *return_value,
                                    guint n_param_values,
                                    const GValue *param_values,
                                    gpointer invocation_hint,
                                    gpointer marshal_data) {

  mgtk_callback_dispatch_smlside( (long) closure->data
                                , return_value
                                , param_values
                                , n_param_values
                                );
}


static void mgtk_callback_destroy (gpointer data,
                                   GClosure *closure) {

  mgtk_callback_destroy_smlside((long) data);

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
EXTERNML long mgtk_signal_connect ( Pointer object
                                  , Pointer name
                                  , long clb
                                  , Bool after) {  /* ML */
  int res;
  GClosure *closure;


  /*  printf("register id = %i\n",(int) clb); */

  closure = mgtk_closure_new((gpointer) clb);

  res = g_signal_connect_closure (object, 
                                  name, 
                                  closure,
                                  after);

  /*  g_closure_unref(closure);
   */
  return res;
}


/* *** ENUMS *** */
EXTERNML void mgtk_get_window_type (int* x1, int* x2) {
  *x1 = GTK_WINDOW_TOPLEVEL;
  *x2 = GTK_WINDOW_POPUP;
}
