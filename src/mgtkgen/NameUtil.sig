(* defs2sml --- generate wrapper code from .defs file.                      *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000.                        *)

signature NameUtil =
sig

    type name = string list (* path *) * string list (* base type name *)

    val separate_words: char -> string -> string

    val toLower: string -> string
    val toUpper: string -> string

    val nameToString: name -> string

end

(*

   [separate_words sep s] constructs a string from s such that words
   (delimited by upper case letter; a word can consist of all caps)
   are separated by sep. The returned string is all lower case.

   [toLower s] converts s to all lower case.

   [toUpper s] converts s to all upper case.

*)