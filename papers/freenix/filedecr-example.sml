
(* Simple example, not using modules *)
signature SimpleFileDecr =
sig type read
    type write
    type 'capability fd
    type 'capability access
    val READ   : read access
    val WRITE  : write access
    val openFd : string -> 'cap access -> 'cap fd
    val read   : read fd -> char
    val write  : write fd -> char -> unit
    val close  : 'capability fd -> unit
end

structure SimpleFileDsr :> SimpleFileDsr =
struct
     type 'capability fd = int
     datatype 'capability access = READ | WRITE
     type read = unit
     type write = unit
     fun openFd name access =
         case access of
             READ => ...
           | WRITE => ...
     ...
end



(* Example using, more than one module *)

signature FileDecr =
sig type base
    type 'capacity fd

    val close : 'cap fd -> unit

    (* the part used for inheritance across modules *)
    type raw_fd
    type constructor = unit -> raw_fd
    val repr     : 'a fd -> raw_fd
    val inherit  : 'a -> constructor -> 'a fd
end

structure FileDecr :> FileDecr =
struct
    type raw_fd = int
    type constructor = unit -> raw_fd
    type base = unit

    (* Phantom type, note that 'a does not occur on right-hand side *)
    type 'a fd = {raw_fd : raw_fd,
                  closed : bool ref}

    (* Dummy implementation, should also call the C function close *)
    fun close {raw_fd, closed} = closed := true

    fun repr {raw_fd, closed} = raw_fd

    fun inherit _ constructor = { raw_fd = constructor()
                                , closed = ref false}
end

signature InputDecr =
sig type 'a input_t
    type 'a input_fd = 'a input_t FileDecr.fd

    val openFd : string -> FileDecr.base input_fd
    val read   : FileDecr.base input_fd * int -> string
end

structure InputDecr :> InputDecr =
struct
    type 'a input_t = unit
    type 'a input_fd = 'a input_t FileDecr.fd

    (* Dummy implementation, should call the C function open *)
    fun openFd_ filename = Obj.magic 42                      
    fun openFd filename = FileDecr.inherit () 
                                           (fn _ => openFd_ filename)

    (* Dummy implementation, should call the C function read with a buffer *)
    fun read_ (raw_fd, count) = "Dummy string"    
    fun read (fd, count) = read_ (FileDecr.repr fd, count)
end

signature OutputDecr =
sig type 'a output_t
    type 'a output_fd = 'a output_t FileDecr.fd

    val openFd : string -> FileDecr.base output_fd
    val write  : FileDecr.base output_fd * string -> unit
end

structure OutputDecr :> OutputDecr =
struct
    type 'a output_t = unit
    type 'a output_fd = 'a output_t FileDecr.fd

    (* Dummy implementation, should call the C function open *)
    fun openFd_ filename = Obj.magic 23                      
    fun openFd filename = FileDecr.inherit () 
                                           (fn _ => openFd_ filename)

    (* Dummy implementation, should call the C function write with 
       string and the size of string *)
    fun write_ (raw_fd, string) = ()    
    fun write (fd, count) = write_ (FileDecr.repr fd, count)

end







