structure MsgUtil = struct

    fun say s = TextIO.output(TextIO.stdErr, s)

    datatype level = DEBUG | TALKATIVE | NORMAL | QUIET
    val level = ref NORMAL
    fun quiet () = level := QUIET
    fun verbose () = level := NORMAL
    fun Verbose () = level := TALKATIVE
    fun Debug () = level := DEBUG

    datatype state = CLOSED | OPEN | BROKEN
    val state = ref CLOSED

    fun print s = 
        if !level = QUIET then ()
	else ( if !state = CLOSED then say "[" else ()
             ; say s
             ; state := OPEN
             )
    fun close s =
	if !level = QUIET then ()
	else if !state <> CLOSED 
	     then ( say (s ^ "]\n") ; state := CLOSED )
   	     else ()

    fun progress s = 
	if !level = TALKATIVE orelse !level = DEBUG
	then say (s^"\n")
	else ()

    fun debug s = 
	if !level = DEBUG then say (s^"\n") else ()

    fun warning s = if !level = QUIET then () else say (s^"\n")

end (* structure MsgUtil *)