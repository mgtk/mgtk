structure State =
struct

    datatype target = SIG | SML | C
    val target : target ref = ref C

end (* structure State *)

(*

   Type [target] specifies the kind of target code the user requested.

   [target] is a reference set by a command-line option.

*)