(define-method show
  (of-object "GtkWidget")
  (c-name "gtk_widget_show")
  (return-type "none")
)

(include "file3.inc")

(define-method show_now
  (of-object "GtkWidget")
  (c-name "gtk_widget_show_now")
  (return-type "none")
)

