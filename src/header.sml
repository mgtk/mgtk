structure Gtk :> Gtk =
struct

    prim_type gtkobj
    prim_type gpointer (* the type of all boxed types *)

    open Dynlib
    local 
	val path = case Process.getEnv "MGTKHOME" of
	               SOME p => Path.concat (p, "mgtk.so")
		     | NONE   => "mgtk.so"
	
	val hdl  = dlopen {lib = path, flag = RTLD_NOW, global = false}
    in
	val symb = dlsym hdl
    end

    (* some helper functions *)
    fun cur2 h (a,b) = app2 h a b
    fun cur3 h (a,b,c) = app3 h a b c
  

    (* Basic GTK stuff *)
    val init_ : string vector -> unit = app1(symb "mgtk_init")
    fun init args = 
	let val args =
	        case args of
		    [] => (print "mGTK Warning: Gtk.init called with empty list\n";
			   [CommandLine.name()])
		  | _  => args
	in
	    init_(vector args)
	end
    val main : unit -> unit = app1(symb "mgtk_main")
    val main_quit : unit -> unit = app1(symb "mgtk_main_quit")
(*    fun main_quit_with e = (main_quit(); raise e)
*)
    (* A litle type cleverness *)
    datatype 'a GtkObject = OBJ of gtkobj

    (* Typecast function *)
    fun super (OBJ obj) = OBJ obj

    fun unwrap (OBJ obj) = obj
    fun unwrapObjOpt NONE = NONE
      | unwrapObjOpt (SOME (OBJ obj)) = SOME obj

    val toWidget = super
    val toObject = super

    local
	prim_type GtkArgs

        type callback_data = gtkobj * GtkArgs * int
	type callback = callback_data -> unit
	type callback_id  = int

	val callbackTable : (int, callback) Polyhash.hash_table
                          = Polyhash.mkPolyTable(401, Domain)
                      
	val add  = Polyhash.insert callbackTable
	val peek = Polyhash.peek callbackTable

	local
	    val intern = ref 0;
	in
	    val localId = fn f => f (!intern before intern := !intern + 1)
	end

	fun dispatch id data =
	    case peek id of
		SOME f => f data(* handle e => main_quit_with e)*)
	      | NONE   => raise Fail("mgtk: Unknown callback function (id: "^
				     Int.toString id^")")
(*
		   main_quit_with(Fail("mgtk: Unknown callback function (id: "^
				       Int.toString id^")"))
*)
	val dummy = Callback.register "mgtk_callback_dispatch" dispatch

	val set_bool_pos : GtkArgs -> int -> bool -> unit 
                         = app3(symb "mgtk_set_pos_bool")

    in
	fun register f = localId(fn id => (add (id, f); id))
	fun reg_unit f = register(fn _ => f())
	fun reg_bool f = register(fn (_,args,pos) => 
				  set_bool_pos args pos (f()))

	val signal_connect : gtkobj -> string -> int -> bool -> int
	                   = app4(symb"mgtk_signal_connect")

    end	

    (* connect a callback with return type unit *)
    fun unit_connect (OBJ wid) sign cb =
	let val id = reg_unit cb
	in  ignore(signal_connect wid sign id false)
	end

    (* connect a callback with return type bool *)
    fun bool_connect (OBJ wid) sign cb =
	let val id = reg_bool cb
	in  ignore(signal_connect wid sign id true)
	end

    type base = unit
    type 'a widget_t = base
    type 'a GtkWidget = 'a widget_t GtkObject
