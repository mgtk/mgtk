#include <gtk/gtk.h>

int main(int argc, char *argv[]) 
{
  GtkWidget* window;
  gtk_init (&argc, &argv);

  /* window = gtk_window_new (GTK_WINDOW_TOPLEVEL); */
  window = gtk_label_new("Text");
  gtk_window_set_title(GTK_WINDOW(window), "New text");
}
