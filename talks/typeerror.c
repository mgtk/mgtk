#include <gtk/gtk.h>

int main(int argc, char *argv[]) 
{
  GtkLabel* window;
  gtk_init (&argc, &argv);

  /* window = gtk_window_new (GTK_WINDOW_TOPLEVEL); */
  window = GTK_LABEL(gtk_label_new("Text"));
  gtk_window_set_title(window, "New text");
}
