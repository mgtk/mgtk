(* mgtkgen --- generate wrapper code from .defs file.                       *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

signature NameUtil =
sig

    val removePrefix: string -> string
    val remove_prefix: string -> string
    val remove_PREFIX: string -> string

    val separateWords: char -> string -> string

    val toLower: string -> string
    val toUpper: string -> string

end

(*

   [removePrefix s] if s begins with `Gtk' returns s without the
   prefix, otherwise just returns s.

   [remove_prefix s] if s begins with `gtk_' returns s without the
   prefix, otherwise just returns s.

   [remove_PREFIX s] if s begins with `GTK_' returns s without the
   prefix, otherwise just returns s.

   [separateWords sep s] constructs a string from s such that words
   (delimited by upper case letter; a word can consist of all caps)
   are separated by sep. The returned string is all lower case.

   [toLower s] converts s to all lower case.

   [toUpper s] converts s to all upper case.

*)