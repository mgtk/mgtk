structure MsgUtil = struct

    fun say s = TextIO.output(TextIO.stdErr, s)

    datatype state = CLOSED | OPEN | BROKEN
    val state = ref CLOSED

    fun print s = ( if !state = CLOSED then say "[" else ()
                  ; say s
                  ; state := OPEN
                  )
    fun close s =   if !state <> CLOSED 
		    then ( say (s ^ "]\n") ; state := CLOSED )
		    else ()

    fun warning s = say (s^"\n")

end (* structure MsgUtil *)