(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

signature NameUtil =
sig

    type wseq = WSeq.wseq
    type name = string list (* path *) * string list (* base type name *)

    val separate_words: char -> string -> string

    val toLower: string -> string
    val toUpper: string -> string

    val toLower': wseq -> wseq
    val toUpper': wseq -> wseq

    val combine: string -> name -> string

end

(*

   Type [name] represents names occurring in the input file. All names
   have been split into their constituent parts by the parser. A name
   consists of a "path" and a "base". The path specifies in which
   module hierarchy the name resides. The base is a list of words
   making up the name.

   [separate_words sep s] constructs a string from s such that words
   in s are separated by sep. New words a begun with an upper case
   letter; even though a word can consist of entirely upper case
   letters. As special Gtk+ feature words with only one upper case
   letter are not considered single words. Examples

   - separate_words sep "GtkWidgetType" => "Gtk" sep "Widget" sep "Type"
   - separate_words sep "GtkGCType"     => "Gtk" sep "GC" sep "Type"
   - separate_words sep "GtkCList"      => "Gtk" sep "CList"

   [toLower s] converts s to all lower case.
   [toUpper s] converts s to all upper case.

   [toLower' wseq] converts the sequence wseq to all lower case.
   [toUpper wseq] converts the sequence wseq to all lower case.

   [combine sep n] converts the name (a path and a base) to a string
   by separating the parts with sep.

*)