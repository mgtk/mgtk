structure MosmlToplevel :> MosmlToplevel =
struct

    type t = {send : string -> string,
	      quit : unit   -> unit}

    open TextIO
    
    (* WARNING Ugly hack ahead *)
    fun getReply is =
	let fun get n acc =
		case input1 is of
		    SOME #"¤" => if n = 2 then (inputN(is, 23);
						List.drop(acc,2))
				 else get (n+1) acc
		  | SOME c     => get 0 (c::acc)
		  | NONE       => acc
	in  String.implode(rev(get 0 []))
	end

    fun fixstring s = 
	let open Substring 
	    val ss = all s
	    val ss = dropr Char.isSpace ss
	in if not (isEmpty ss) andalso sub(ss,size ss - 1) = #";" then
	       concat [ss, all"\n"]
	   else  concat [ss, all";\n"]
	end

    fun dialog is os s = 
	( output(os,fixstring s)
	; flushOut os
        ; output(os,"print\"¤¤¤\";\n")
	; flushOut os
	; getReply is
        )
    	  
    fun newFull name options =
	let val proc = Unix.execute(name, options)
	    val (is,os) = Unix.streamsOf proc
	    val preamble = ( output(os,"print\"¤¤¤\";\n") ; flushOut os
			   ; getReply is)
	in  (preamble,
	     {send = dialog is os,
	      quit = fn() => (output(os, "quit();\n");  flushOut os;
			      Unix.reap proc; ())})
	end

    fun new options = 
	newFull "/bin/sh" ("-c" :: [String.concat("mosml " :: options)]) 
	
end
