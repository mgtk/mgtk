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
MGTK_MakeGetter(mgtk_get_pos_char, char, g_value_get_char)
MGTK_MakeGetter(mgtk_get_pos_real, double, g_value_get_double)


static inline GValue* create_GValue (GType type) {
  GValue *res = (GValue*) malloc(sizeof(GValue));
  memset(res, 0, sizeof(GValue));
  g_value_init(res, type);
  return res;
}

EXTERNML GValue* mgtk_g_value_set_int (int i){
  GValue* res = create_GValue(G_TYPE_INT);
  g_value_set_int(res, i);
  return res;
}

EXTERNML GValue* mgtk_g_value_set_real (double r){
  GValue* res = create_GValue(G_TYPE_DOUBLE);
  g_value_set_double(res, r);
  return res;
}

EXTERNML GValue* mgtk_g_value_set_string (char *s){
  GValue *res = create_GValue(G_TYPE_STRING);
  g_value_set_string(res, s);
  return res;
}

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


/* GType's */

/* ML type: cptr -> int -> CString.t */
EXTERNML char *mgtk_g_type_name (int typ) { /* ML */
  return g_type_name(typ);
}

/* ML type: unit -> GType.t */
EXTERNML int mgtk_g_type_int (void) { /* ML */
  return G_TYPE_INT;
}

/* ML type: unit -> GType.t */
EXTERNML int mgtk_g_type_real (void) { /* ML */
  return G_TYPE_DOUBLE;
}

/* ML type: unit -> GType.t */
EXTERNML int mgtk_g_type_string (void) { /* ML */
  return G_TYPE_STRING;
}


/* *** Gtk *** */
EXTERNML GtkSelectionData* alloc_GtkSelectionData() {
  GtkSelectionData res;
  return gtk_selection_data_copy(&res);
}

EXTERNML GtkTextIter* alloc_GtkTextIter() {
  GtkTextIter res;
  return gtk_text_iter_copy(&res);
}

EXTERNML GtkTreeIter* alloc_GtkTreeIter() {
  GtkTreeIter res;
  return gtk_tree_iter_copy(&res);
}

/* ML type: int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_accel_flags(int* x0, int* x1, int* x2) {
  *x2 = GTK_ACCEL_MASK;
  *x1 = GTK_ACCEL_LOCKED;
  *x0 = GTK_ACCEL_VISIBLE;
}

/* ML type: int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_celltype(int* x0, int* x1, int* x2, int* x3, int* x4) {
  *x4 = GTK_CELL_WIDGET;
  *x3 = GTK_CELL_PIXTEXT;
  *x2 = GTK_CELL_PIXMAP;
  *x1 = GTK_CELL_TEXT;
  *x0 = GTK_CELL_EMPTY;
}

/* ML type: int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_debug_flag(int* x0, int* x1, int* x2, int* x3, int* x4) {
  *x4 = GTK_DEBUG_UPDATES;
  *x3 = GTK_DEBUG_TREE;
  *x2 = GTK_DEBUG_TEXT;
  *x1 = GTK_DEBUG_PLUGSOCKET;
  *x0 = GTK_DEBUG_MISC;
}

/* ML type: int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_responsetype(int* x0, int* x1, int* x2, int* x3, int* x4, int* x5, int* x6, int* x7, int* x8, int* x9, int* x10) {
  *x10 = GTK_RESPONSE_HELP;
  *x9 = GTK_RESPONSE_APPLY;
  *x8 = GTK_RESPONSE_NO;
  *x7 = GTK_RESPONSE_YES;
  *x6 = GTK_RESPONSE_CLOSE;
  *x5 = GTK_RESPONSE_CANCEL;
  *x4 = GTK_RESPONSE_OK;
  *x3 = GTK_RESPONSE_DELETE_EVENT;
  *x2 = GTK_RESPONSE_ACCEPT;
  *x1 = GTK_RESPONSE_REJECT;
  *x0 = GTK_RESPONSE_NONE;
}

/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_dest_defaults(int* x0, int* x1, int* x2, int* x3) {
  *x3 = GTK_DEST_DEFAULT_ALL;
  *x2 = GTK_DEST_DEFAULT_DROP;
  *x1 = GTK_DEST_DEFAULT_HIGHLIGHT;
  *x0 = GTK_DEST_DEFAULT_MOTION;
}

/* ML type: int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_target_flags(int* x0, int* x1) {
  *x1 = GTK_TARGET_SAME_WIDGET;
  *x0 = GTK_TARGET_SAME_APP;
}

/* ML type: int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_anchortype(int* x0, int* x1, int* x2, int* x3, int* x4, int* x5, int* x6, int* x7, int* x8, int* x9, int* x10, int* x11, int* x12, int* x13, int* x14, int* x15, int* x16) {
  *x16 = GTK_ANCHOR_E;
  *x15 = GTK_ANCHOR_W;
  *x14 = GTK_ANCHOR_SE;
  *x13 = GTK_ANCHOR_SW;
  *x12 = GTK_ANCHOR_S;
  *x11 = GTK_ANCHOR_NE;
  *x10 = GTK_ANCHOR_NW;
  *x9 = GTK_ANCHOR_N;
  *x8 = GTK_ANCHOR_EAST;
  *x7 = GTK_ANCHOR_WEST;
  *x6 = GTK_ANCHOR_SOUTH_EAST;
  *x5 = GTK_ANCHOR_SOUTH_WEST;
  *x4 = GTK_ANCHOR_SOUTH;
  *x3 = GTK_ANCHOR_NORTH_EAST;
  *x2 = GTK_ANCHOR_NORTH_WEST;
  *x1 = GTK_ANCHOR_NORTH;
  *x0 = GTK_ANCHOR_CENTER;
}

/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_arrowtype(int* x0, int* x1, int* x2, int* x3) {
  *x3 = GTK_ARROW_RIGHT;
  *x2 = GTK_ARROW_LEFT;
  *x1 = GTK_ARROW_DOWN;
  *x0 = GTK_ARROW_UP;
}

/* ML type: int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_attach_options(int* x0, int* x1, int* x2) {
  *x2 = GTK_FILL;
  *x1 = GTK_SHRINK;
  *x0 = GTK_EXPAND;
}

/* ML type: int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_curvetype(int* x0, int* x1, int* x2) {
  *x2 = GTK_CURVE_TYPE_FREE;
  *x1 = GTK_CURVE_TYPE_SPLINE;
  *x0 = GTK_CURVE_TYPE_LINEAR;
}

/* ML type: int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_deletetype(int* x0, int* x1, int* x2, int* x3, int* x4, int* x5, int* x6, int* x7) {
  *x7 = GTK_DELETE_WHITESPACE;
  *x6 = GTK_DELETE_PARAGRAPHS;
  *x5 = GTK_DELETE_PARAGRAPH_ENDS;
  *x4 = GTK_DELETE_DISPLAY_LINE_ENDS;
  *x3 = GTK_DELETE_DISPLAY_LINES;
  *x2 = GTK_DELETE_WORDS;
  *x1 = GTK_DELETE_WORD_ENDS;
  *x0 = GTK_DELETE_CHARS;
}

/* ML type: int ref * int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_directiontype(int* x0, int* x1, int* x2, int* x3, int* x4, int* x5) {
  *x5 = GTK_DIR_RIGHT;
  *x4 = GTK_DIR_LEFT;
  *x3 = GTK_DIR_DOWN;
  *x2 = GTK_DIR_UP;
  *x1 = GTK_DIR_TAB_BACKWARD;
  *x0 = GTK_DIR_TAB_FORWARD;
}

/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_expander_style(int* x0, int* x1, int* x2, int* x3) {
  *x3 = GTK_EXPANDER_EXPANDED;
  *x2 = GTK_EXPANDER_SEMI_EXPANDED;
  *x1 = GTK_EXPANDER_SEMI_COLLAPSED;
  *x0 = GTK_EXPANDER_COLLAPSED;
}

/* ML type: int ref * int ref * int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_icon_size(int* x0, int* x1, int* x2, int* x3, int* x4, int* x5, int* x6) {
  *x6 = GTK_ICON_SIZE_DIALOG;
  *x5 = GTK_ICON_SIZE_DND;
  *x4 = GTK_ICON_SIZE_BUTTON;
  *x3 = GTK_ICON_SIZE_LARGE_TOOLBAR;
  *x2 = GTK_ICON_SIZE_SMALL_TOOLBAR;
  *x1 = GTK_ICON_SIZE_MENU;
  *x0 = GTK_ICON_SIZE_INVALID;
}

/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_sidetype(int* x0, int* x1, int* x2, int* x3) {
  *x3 = GTK_SIDE_RIGHT;
  *x2 = GTK_SIDE_LEFT;
  *x1 = GTK_SIDE_BOTTOM;
  *x0 = GTK_SIDE_TOP;
}

/* ML type: int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_text_direction(int* x0, int* x1, int* x2) {
  *x2 = GTK_TEXT_DIR_RTL;
  *x1 = GTK_TEXT_DIR_LTR;
  *x0 = GTK_TEXT_DIR_NONE;
}

/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_justification(int* x0, int* x1, int* x2, int* x3) {
  *x3 = GTK_JUSTIFY_FILL;
  *x2 = GTK_JUSTIFY_CENTER;
  *x1 = GTK_JUSTIFY_RIGHT;
  *x0 = GTK_JUSTIFY_LEFT;
}

/* ML type: int ref * int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_matchtype(int* x0, int* x1, int* x2, int* x3, int* x4, int* x5) {
  *x5 = GTK_MATCH_LAST;
  *x4 = GTK_MATCH_EXACT;
  *x3 = GTK_MATCH_TAIL;
  *x2 = GTK_MATCH_HEAD;
  *x1 = GTK_MATCH_ALL_TAIL;
  *x0 = GTK_MATCH_ALL;
}

/* ML type: int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_metrictype(int* x0, int* x1, int* x2) {
  *x2 = GTK_CENTIMETERS;
  *x1 = GTK_INCHES;
  *x0 = GTK_PIXELS;
}

/* ML type: int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_movement_step(int* x0, int* x1, int* x2, int* x3, int* x4, int* x5, int* x6, int* x7, int* x8) {
  *x8 = GTK_MOVEMENT_BUFFER_ENDS;
  *x7 = GTK_MOVEMENT_PAGES;
  *x6 = GTK_MOVEMENT_PARAGRAPH_ENDS;
  *x5 = GTK_MOVEMENT_PARAGRAPHS;
  *x4 = GTK_MOVEMENT_DISPLAY_LINE_ENDS;
  *x3 = GTK_MOVEMENT_DISPLAY_LINES;
  *x2 = GTK_MOVEMENT_WORDS;
  *x1 = GTK_MOVEMENT_VISUAL_POSITIONS;
  *x0 = GTK_MOVEMENT_LOGICAL_POSITIONS;
}

/* ML type: int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_orientation(int* x0, int* x1) {
  *x1 = GTK_ORIENTATION_VERTICAL;
  *x0 = GTK_ORIENTATION_HORIZONTAL;
}

/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_cornertype(int* x0, int* x1, int* x2, int* x3) {
  *x3 = GTK_CORNER_BOTTOM_RIGHT;
  *x2 = GTK_CORNER_TOP_RIGHT;
  *x1 = GTK_CORNER_BOTTOM_LEFT;
  *x0 = GTK_CORNER_TOP_LEFT;
}

/* ML type: int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_packtype(int* x0, int* x1) {
  *x1 = GTK_PACK_END;
  *x0 = GTK_PACK_START;
}

/* ML type: int ref * int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_path_prioritytype(int* x0, int* x1, int* x2, int* x3, int* x4, int* x5) {
  *x5 = GTK_PATH_PRIO_HIGHEST;
  *x4 = GTK_PATH_PRIO_RC;
  *x3 = GTK_PATH_PRIO_THEME;
  *x2 = GTK_PATH_PRIO_APPLICATION;
  *x1 = GTK_PATH_PRIO_GTK;
  *x0 = GTK_PATH_PRIO_LOWEST;
}

/* ML type: int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_pathtype(int* x0, int* x1, int* x2) {
  *x2 = GTK_PATH_CLASS;
  *x1 = GTK_PATH_WIDGET_CLASS;
  *x0 = GTK_PATH_WIDGET;
}

/* ML type: int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_policytype(int* x0, int* x1, int* x2) {
  *x2 = GTK_POLICY_NEVER;
  *x1 = GTK_POLICY_AUTOMATIC;
  *x0 = GTK_POLICY_ALWAYS;
}

/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_positiontype(int* x0, int* x1, int* x2, int* x3) {
  *x3 = GTK_POS_BOTTOM;
  *x2 = GTK_POS_TOP;
  *x1 = GTK_POS_RIGHT;
  *x0 = GTK_POS_LEFT;
}

/* ML type: int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_previewtype(int* x0, int* x1) {
  *x1 = GTK_PREVIEW_GRAYSCALE;
  *x0 = GTK_PREVIEW_COLOR;
}

/* ML type: int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_relief_style(int* x0, int* x1, int* x2) {
  *x2 = GTK_RELIEF_NONE;
  *x1 = GTK_RELIEF_HALF;
  *x0 = GTK_RELIEF_NORMAL;
}

/* ML type: int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_resize_mode(int* x0, int* x1, int* x2) {
  *x2 = GTK_RESIZE_IMMEDIATE;
  *x1 = GTK_RESIZE_QUEUE;
  *x0 = GTK_RESIZE_PARENT;
}

/* ML type: int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_scrolltype(int* x0, int* x1, int* x2, int* x3, int* x4, int* x5, int* x6, int* x7, int* x8, int* x9, int* x10, int* x11, int* x12, int* x13, int* x14, int* x15) {
  *x15 = GTK_SCROLL_END;
  *x14 = GTK_SCROLL_START;
  *x13 = GTK_SCROLL_PAGE_RIGHT;
  *x12 = GTK_SCROLL_PAGE_LEFT;
  *x11 = GTK_SCROLL_STEP_RIGHT;
  *x10 = GTK_SCROLL_STEP_LEFT;
  *x9 = GTK_SCROLL_PAGE_DOWN;
  *x8 = GTK_SCROLL_PAGE_UP;
  *x7 = GTK_SCROLL_STEP_DOWN;
  *x6 = GTK_SCROLL_STEP_UP;
  *x5 = GTK_SCROLL_PAGE_FORWARD;
  *x4 = GTK_SCROLL_PAGE_BACKWARD;
  *x3 = GTK_SCROLL_STEP_FORWARD;
  *x2 = GTK_SCROLL_STEP_BACKWARD;
  *x1 = GTK_SCROLL_JUMP;
  *x0 = GTK_SCROLL_NONE;
}

/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_selection_mode(int* x0, int* x1, int* x2, int* x3) {
  *x3 = GTK_SELECTION_MULTIPLE;
  *x2 = GTK_SELECTION_BROWSE;
  *x1 = GTK_SELECTION_SINGLE;
  *x0 = GTK_SELECTION_NONE;
}

/* ML type: int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_shadowtype(int* x0, int* x1, int* x2, int* x3, int* x4) {
  *x4 = GTK_SHADOW_ETCHED_OUT;
  *x3 = GTK_SHADOW_ETCHED_IN;
  *x2 = GTK_SHADOW_OUT;
  *x1 = GTK_SHADOW_IN;
  *x0 = GTK_SHADOW_NONE;
}

/* ML type: int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_statetype(int* x0, int* x1, int* x2, int* x3, int* x4) {
  *x4 = GTK_STATE_INSENSITIVE;
  *x3 = GTK_STATE_SELECTED;
  *x2 = GTK_STATE_PRELIGHT;
  *x1 = GTK_STATE_ACTIVE;
  *x0 = GTK_STATE_NORMAL;
}

/* ML type: int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_submenu_direction(int* x0, int* x1) {
  *x1 = GTK_DIRECTION_RIGHT;
  *x0 = GTK_DIRECTION_LEFT;
}

/* ML type: int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_submenu_placement(int* x0, int* x1) {
  *x1 = GTK_LEFT_RIGHT;
  *x0 = GTK_TOP_BOTTOM;
}

/* ML type: int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_updatetype(int* x0, int* x1, int* x2) {
  *x2 = GTK_UPDATE_DELAYED;
  *x1 = GTK_UPDATE_DISCONTINUOUS;
  *x0 = GTK_UPDATE_CONTINUOUS;
}

/* ML type: int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_visibility(int* x0, int* x1, int* x2) {
  *x2 = GTK_VISIBILITY_FULL;
  *x1 = GTK_VISIBILITY_PARTIAL;
  *x0 = GTK_VISIBILITY_NONE;
}

/* ML type: int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_wrap_mode(int* x0, int* x1, int* x2) {
  *x2 = GTK_WRAP_WORD;
  *x1 = GTK_WRAP_CHAR;
  *x0 = GTK_WRAP_NONE;
}

/* ML type: int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_sorttype(int* x0, int* x1) {
  *x1 = GTK_SORT_DESCENDING;
  *x0 = GTK_SORT_ASCENDING;
}

/* ML type: int ref * int ref * int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_imagetype(int* x0, int* x1, int* x2, int* x3, int* x4, int* x5, int* x6) {
  *x6 = GTK_IMAGE_ANIMATION;
  *x5 = GTK_IMAGE_ICON_SET;
  *x4 = GTK_IMAGE_STOCK;
  *x3 = GTK_IMAGE_PIXBUF;
  *x2 = GTK_IMAGE_IMAGE;
  *x1 = GTK_IMAGE_PIXMAP;
  *x0 = GTK_IMAGE_EMPTY;
}

/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_messagetype(int* x0, int* x1, int* x2, int* x3) {
  *x3 = GTK_MESSAGE_ERROR;
  *x2 = GTK_MESSAGE_QUESTION;
  *x1 = GTK_MESSAGE_WARNING;
  *x0 = GTK_MESSAGE_INFO;
}

/* ML type: int ref * int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_buttonstype(int* x0, int* x1, int* x2, int* x3, int* x4, int* x5) {
  *x5 = GTK_BUTTONS_OK_CANCEL;
  *x4 = GTK_BUTTONS_YES_NO;
  *x3 = GTK_BUTTONS_CANCEL;
  *x2 = GTK_BUTTONS_CLOSE;
  *x1 = GTK_BUTTONS_OK;
  *x0 = GTK_BUTTONS_NONE;
}

/* ML type: int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_arg_flags(int* x0, int* x1, int* x2, int* x3, int* x4) {
  *x4 = GTK_ARG_CHILD_ARG;
  *x3 = GTK_ARG_CONSTRUCT_ONLY;
  *x2 = GTK_ARG_CONSTRUCT;
  *x1 = GTK_ARG_WRITABLE;
  *x0 = GTK_ARG_READABLE;
}

/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_rc_flags(int* x0, int* x1, int* x2, int* x3) {
  *x3 = GTK_RC_BASE;
  *x2 = GTK_RC_TEXT;
  *x1 = GTK_RC_BG;
  *x0 = GTK_RC_FG;
}

/* ML type: int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_rc_tokentype(int* x0, int* x1, int* x2, int* x3, int* x4, int* x5, int* x6, int* x7, int* x8, int* x9, int* x10, int* x11, int* x12, int* x13, int* x14, int* x15, int* x16, int* x17, int* x18, int* x19, int* x20, int* x21, int* x22, int* x23, int* x24, int* x25, int* x26, int* x27, int* x28, int* x29, int* x30, int* x31, int* x32, int* x33, int* x34, int* x35, int* x36, int* x37) {
  *x37 = GTK_RC_TOKEN_LAST;
  *x36 = GTK_RC_TOKEN_RTL;
  *x35 = GTK_RC_TOKEN_LTR;
  *x34 = GTK_RC_TOKEN_STOCK;
  *x33 = GTK_RC_TOKEN_IM_MODULE_FILE;
  *x32 = GTK_RC_TOKEN_IM_MODULE_PATH;
  *x31 = GTK_RC_TOKEN_MODULE_PATH;
  *x30 = GTK_RC_TOKEN_ENGINE;
  *x29 = GTK_RC_TOKEN_HIGHEST;
  *x28 = GTK_RC_TOKEN_RC;
  *x27 = GTK_RC_TOKEN_THEME;
  *x26 = GTK_RC_TOKEN_APPLICATION;
  *x25 = GTK_RC_TOKEN_GTK;
  *x24 = GTK_RC_TOKEN_LOWEST;
  *x23 = GTK_RC_TOKEN_CLASS;
  *x22 = GTK_RC_TOKEN_WIDGET_CLASS;
  *x21 = GTK_RC_TOKEN_WIDGET;
  *x20 = GTK_RC_TOKEN_BIND;
  *x19 = GTK_RC_TOKEN_BINDING;
  *x18 = GTK_RC_TOKEN_STYLE;
  *x17 = GTK_RC_TOKEN_PIXMAP_PATH;
  *x16 = GTK_RC_TOKEN_BG_PIXMAP;
  *x15 = GTK_RC_TOKEN_FONT_NAME;
  *x14 = GTK_RC_TOKEN_FONTSET;
  *x13 = GTK_RC_TOKEN_FONT;
  *x12 = GTK_RC_TOKEN_YTHICKNESS;
  *x11 = GTK_RC_TOKEN_XTHICKNESS;
  *x10 = GTK_RC_TOKEN_BASE;
  *x9 = GTK_RC_TOKEN_TEXT;
  *x8 = GTK_RC_TOKEN_BG;
  *x7 = GTK_RC_TOKEN_FG;
  *x6 = GTK_RC_TOKEN_INSENSITIVE;
  *x5 = GTK_RC_TOKEN_SELECTED;
  *x4 = GTK_RC_TOKEN_PRELIGHT;
  *x3 = GTK_RC_TOKEN_ACTIVE;
  *x2 = GTK_RC_TOKEN_NORMAL;
  *x1 = GTK_RC_TOKEN_INCLUDE;
  *x0 = GTK_RC_TOKEN_INVALID;
}

/* ML type: int ref * int ref * int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_spintype(int* x0, int* x1, int* x2, int* x3, int* x4, int* x5, int* x6) {
  *x6 = GTK_SPIN_USER_DEFINED;
  *x5 = GTK_SPIN_END;
  *x4 = GTK_SPIN_HOME;
  *x3 = GTK_SPIN_PAGE_BACKWARD;
  *x2 = GTK_SPIN_PAGE_FORWARD;
  *x1 = GTK_SPIN_STEP_BACKWARD;
  *x0 = GTK_SPIN_STEP_FORWARD;
}

/* ML type: int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_text_search_flags(int* x0, int* x1) {
  *x1 = GTK_TEXT_SEARCH_TEXT_ONLY;
  *x0 = GTK_TEXT_SEARCH_VISIBLE_ONLY;
}

/* ML type: int ref * int ref * int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_text_window_type(int* x0, int* x1, int* x2, int* x3, int* x4, int* x5, int* x6) {
  *x6 = GTK_TEXT_WINDOW_BOTTOM;
  *x5 = GTK_TEXT_WINDOW_TOP;
  *x4 = GTK_TEXT_WINDOW_RIGHT;
  *x3 = GTK_TEXT_WINDOW_LEFT;
  *x2 = GTK_TEXT_WINDOW_TEXT;
  *x1 = GTK_TEXT_WINDOW_WIDGET;
  *x0 = GTK_TEXT_WINDOW_PRIVATE;
}

/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_file_chooser_action(int* x0, int* x1, int* x2, int* x3) {
  *x3 = GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER;
  *x2 = GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER;
  *x1 = GTK_FILE_CHOOSER_ACTION_SAVE;
  *x0 = GTK_FILE_CHOOSER_ACTION_OPEN;
}

/* ML type: int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_file_chooser_error(int* x0, int* x1) {
  *x1 = GTK_FILE_CHOOSER_ERROR_BAD_FILENAME;
  *x0 = GTK_FILE_CHOOSER_ERROR_NONEXISTENT;
}

/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_file_filter_flags(int* x0, int* x1, int* x2, int* x3) {
  *x3 = GTK_FILE_FILTER_MIME_TYPE;
  *x2 = GTK_FILE_FILTER_DISPLAY_NAME;
  *x1 = GTK_FILE_FILTER_URI;
  *x0 = GTK_FILE_FILTER_FILENAME;
}



/* *** AccelGroup *** */


/* *** IconFactory *** */


/* *** Object *** */
/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_object_flags(int* x0, int* x1, int* x2, int* x3) {
    *x3 = GTK_RESERVED_2;
    *x2 = GTK_RESERVED_1;
    *x1 = GTK_FLOATING;
    *x0 = GTK_IN_DESTRUCTION;
}



/* *** Adjustment *** */


/* *** Widget *** */
/* ML type: int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_widget_flags(int* x0, int* x1, int* x2, int* x3, int* x4, int* x5, int* x6, int* x7, int* x8, int* x9, int* x10, int* x11, int* x12, int* x13, int* x14, int* x15, int* x16, int* x17) {
    *x17 = GTK_DOUBLE_BUFFERED;
    *x16 = GTK_RECEIVES_DEFAULT;
    *x15 = GTK_APP_PAINTABLE;
    *x14 = GTK_NO_REPARENT;
    *x13 = GTK_COMPOSITE_CHILD;
    *x12 = GTK_RC_STYLE;
    *x11 = GTK_HAS_GRAB;
    *x10 = GTK_HAS_DEFAULT;
    *x9 = GTK_CAN_DEFAULT;
    *x8 = GTK_HAS_FOCUS;
    *x7 = GTK_CAN_FOCUS;
    *x6 = GTK_PARENT_SENSITIVE;
    *x5 = GTK_SENSITIVE;
    *x4 = GTK_VISIBLE;
    *x3 = GTK_MAPPED;
    *x2 = GTK_REALIZED;
    *x1 = GTK_NO_WINDOW;
    *x0 = GTK_TOPLEVEL;
}

/* ML type: int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_widget_helptype(int* x0, int* x1) {
    *x1 = GTK_WIDGET_HELP_WHATS_THIS;
    *x0 = GTK_WIDGET_HELP_TOOLTIP;
}



/* *** Editable *** */


/* *** ItemFactory *** */


/* *** IMContext *** */


/* *** IMContextSimple *** */


/* *** IMMulticontext *** */


/* *** CellRenderer *** */
/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_cellrenderer_state(int* x0, int* x1, int* x2, int* x3) {
    *x3 = GTK_CELL_RENDERER_SORTED;
    *x2 = GTK_CELL_RENDERER_INSENSITIVE;
    *x1 = GTK_CELL_RENDERER_PRELIT;
    *x0 = GTK_CELL_RENDERER_SELECTED;
}

/* ML type: int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_cellrenderer_mode(int* x0, int* x1, int* x2) {
    *x2 = GTK_CELL_RENDERER_MODE_EDITABLE;
    *x1 = GTK_CELL_RENDERER_MODE_ACTIVATABLE;
    *x0 = GTK_CELL_RENDERER_MODE_INERT;
}



/* *** CellEditable *** */


/* *** CellRendererToggle *** */


/* *** CellRendererText *** */


/* *** CellRendererPixbuf *** */


/* *** RcStyle *** */


/* *** Settings *** */


/* *** SizeGroup *** */
/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_size_group_mode(int* x0, int* x1, int* x2, int* x3) {
    *x3 = GTK_SIZE_GROUP_BOTH;
    *x2 = GTK_SIZE_GROUP_VERTICAL;
    *x1 = GTK_SIZE_GROUP_HORIZONTAL;
    *x0 = GTK_SIZE_GROUP_NONE;
}



/* *** Style *** */


/* *** TextBuffer *** */


/* *** TextChildAnchor *** */


/* *** TextMark *** */


/* *** TextTag *** */


/* *** TextTagTable *** */


/* *** Tooltips *** */


/* *** TreeModel *** */
/* ML type: int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_treemodel_flags(int* x0, int* x1) {
    *x1 = GTK_TREE_MODEL_LIST_ONLY;
    *x0 = GTK_TREE_MODEL_ITERS_PERSIST;
}



/* *** TreeDragSource *** */


/* *** TreeDragDest *** */


/* *** TreeSortable *** */


/* *** ListStore *** */


/* *** TreeModelSort *** */


/* *** TreeSelection *** */


/* *** TreeStore *** */


/* *** TreeViewColumn *** */
/* ML type: int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_treeviewcolumn_sizing(int* x0, int* x1, int* x2) {
    *x2 = GTK_TREE_VIEW_COLUMN_FIXED;
    *x1 = GTK_TREE_VIEW_COLUMN_AUTOSIZE;
    *x0 = GTK_TREE_VIEW_COLUMN_GROW_ONLY;
}



/* *** Separator *** */


/* *** VSeparator *** */


/* *** HSeparator *** */


/* *** Ruler *** */


/* *** VRuler *** */


/* *** HRuler *** */


/* *** Range *** */


/* *** Scrollbar *** */


/* *** VScrollbar *** */


/* *** HScrollbar *** */


/* *** Scale *** */


/* *** VScale *** */


/* *** HScale *** */


/* *** Progress *** */


/* *** ProgressBar *** */
/* ML type: int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_progressbar_style(int* x0, int* x1) {
    *x1 = GTK_PROGRESS_DISCRETE;
    *x0 = GTK_PROGRESS_CONTINUOUS;
}

/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_progressbar_orientation(int* x0, int* x1, int* x2, int* x3) {
    *x3 = GTK_PROGRESS_TOP_TO_BOTTOM;
    *x2 = GTK_PROGRESS_BOTTOM_TO_TOP;
    *x1 = GTK_PROGRESS_RIGHT_TO_LEFT;
    *x0 = GTK_PROGRESS_LEFT_TO_RIGHT;
}



/* *** Preview *** */


/* *** OldEditable *** */


/* *** Misc *** */


/* *** Pixmap *** */


/* *** Arrow *** */


/* *** Image *** */


/* *** Label *** */


/* *** AccelLabel *** */


/* *** Invisible *** */


/* *** Entry *** */


/* *** SpinButton *** */
/* ML type: int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_spin_button_update_policy(int* x0, int* x1) {
    *x1 = GTK_UPDATE_IF_VALID;
    *x0 = GTK_UPDATE_ALWAYS;
}



/* *** DrawingArea *** */


/* *** Curve *** */


/* *** Container *** */


/* *** TreeView *** */
/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_treeview_drop_position(int* x0, int* x1, int* x2, int* x3) {
    *x3 = GTK_TREE_VIEW_DROP_INTO_OR_AFTER;
    *x2 = GTK_TREE_VIEW_DROP_INTO_OR_BEFORE;
    *x1 = GTK_TREE_VIEW_DROP_AFTER;
    *x0 = GTK_TREE_VIEW_DROP_BEFORE;
}



/* *** Toolbar *** */
/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_toolbar_style(int* x0, int* x1, int* x2, int* x3) {
    *x3 = GTK_TOOLBAR_BOTH_HORIZ;
    *x2 = GTK_TOOLBAR_BOTH;
    *x1 = GTK_TOOLBAR_TEXT;
    *x0 = GTK_TOOLBAR_ICONS;
}

/* ML type: int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_toolbar_childtype(int* x0, int* x1, int* x2, int* x3, int* x4) {
    *x4 = GTK_TOOLBAR_CHILD_WIDGET;
    *x3 = GTK_TOOLBAR_CHILD_RADIOBUTTON;
    *x2 = GTK_TOOLBAR_CHILD_TOGGLEBUTTON;
    *x1 = GTK_TOOLBAR_CHILD_BUTTON;
    *x0 = GTK_TOOLBAR_CHILD_SPACE;
}

/* ML type: int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_toolbar_space_style(int* x0, int* x1) {
    *x1 = GTK_TOOLBAR_SPACE_LINE;
    *x0 = GTK_TOOLBAR_SPACE_EMPTY;
}



/* *** TextView *** */


/* *** Table *** */


/* *** Socket *** */


/* *** Paned *** */


/* *** VPaned *** */


/* *** HPaned *** */


/* *** Notebook *** */
/* ML type: int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_notebook_tab(int* x0, int* x1) {
    *x1 = GTK_NOTEBOOK_TAB_LAST;
    *x0 = GTK_NOTEBOOK_TAB_FIRST;
}



/* *** MenuShell *** */


/* *** Menu *** */
/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_menu_directiontype(int* x0, int* x1, int* x2, int* x3) {
    *x3 = GTK_MENU_DIR_PREV;
    *x2 = GTK_MENU_DIR_NEXT;
    *x1 = GTK_MENU_DIR_CHILD;
    *x0 = GTK_MENU_DIR_PARENT;
}



/* *** MenuBar *** */


/* *** List *** */


/* *** Layout *** */


/* *** Fixed *** */


/* *** Bin *** */


/* *** Viewport *** */


/* *** ScrolledWindow *** */


/* *** Item *** */


/* *** MenuItem *** */


/* *** TearoffMenuItem *** */


/* *** SeparatorMenuItem *** */


/* *** CheckMenuItem *** */


/* *** RadioMenuItem *** */


/* *** ImageMenuItem *** */


/* *** ListItem *** */


/* *** HandleBox *** */


/* *** Frame *** */


/* *** AspectFrame *** */


/* *** EventBox *** */


/* *** Alignment *** */


/* *** Button *** */
/* ML type: int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_button_action(int* x0, int* x1, int* x2, int* x3) {
    *x3 = GTK_BUTTON_EXPANDS;
    *x2 = GTK_BUTTON_DRAGS;
    *x1 = GTK_BUTTON_SELECTS;
    *x0 = GTK_BUTTON_IGNORED;
}

/* ML type: int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_button_box_style(int* x0, int* x1, int* x2, int* x3, int* x4) {
    *x4 = GTK_BUTTONBOX_END;
    *x3 = GTK_BUTTONBOX_START;
    *x2 = GTK_BUTTONBOX_EDGE;
    *x1 = GTK_BUTTONBOX_SPREAD;
    *x0 = GTK_BUTTONBOX_DEFAULT_STYLE;
}



/* *** ToggleButton *** */


/* *** CheckButton *** */


/* *** RadioButton *** */


/* *** OptionMenu *** */


/* *** Box *** */


/* *** VBox *** */


/* *** ColorSelection *** */


/* *** FontSelection *** */


/* *** GammaCurve *** */


/* *** HBox *** */


/* *** Statusbar *** */


/* *** Combo *** */


/* *** ButtonBox *** */


/* *** VButtonBox *** */


/* *** HButtonBox *** */


/* *** Calendar *** */
/* ML type: int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_calendar_display_options(int* x0, int* x1, int* x2, int* x3, int* x4) {
    *x4 = GTK_CALENDAR_WEEK_START_MONDAY;
    *x3 = GTK_CALENDAR_SHOW_WEEK_NUMBERS;
    *x2 = GTK_CALENDAR_NO_MONTH_CHANGE;
    *x1 = GTK_CALENDAR_SHOW_DAY_NAMES;
    *x0 = GTK_CALENDAR_SHOW_HEADING;
}



/* *** Window *** */
/* ML type: int ref * int ref * int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_window_position(int* x0, int* x1, int* x2, int* x3, int* x4) {
    *x4 = GTK_WIN_POS_CENTER_ON_PARENT;
    *x3 = GTK_WIN_POS_CENTER_ALWAYS;
    *x2 = GTK_WIN_POS_MOUSE;
    *x1 = GTK_WIN_POS_CENTER;
    *x0 = GTK_WIN_POS_NONE;
}

/* ML type: int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_window_type(int* x0, int* x1) {
    *x1 = GTK_WINDOW_POPUP;
    *x0 = GTK_WINDOW_TOPLEVEL;
}



/* *** Plug *** */


/* *** Dialog *** */
/* ML type: int ref * int ref * int ref -> unit */
EXTERNML void mgtk_get_gtk_dialog_flags(int* x0, int* x1, int* x2) {
    *x2 = GTK_DIALOG_NO_SEPARATOR;
    *x1 = GTK_DIALOG_DESTROY_WITH_PARENT;
    *x0 = GTK_DIALOG_MODAL;
}



/* *** MessageDialog *** */


/* *** InputDialog *** */


/* *** FontSelectionDialog *** */


/* *** ColorSelectionDialog *** */


/* *** WindowGroup *** */


/* *** Clipboard *** */


/* *** FileChooser *** */


/* *** FileChooserDialog *** */


/* *** FileChooserWidget *** */


/* *** FileFilter *** */


/* *** FileSelection *** */
