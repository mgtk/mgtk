structure FunInput :> FunInput =
struct
    type unitcallback = unit -> unit
    type ('input, 'state) statefun = 'input -> 'state -> 'state
    type 'widget connector = 'widget -> unitcallback -> unit

    type ('in, 'wid) connections = ('wid connector * 'wid * 'in) list

    fun apply f init =
	let val state = ref init
	in  fn inp => fn () => state := f inp (!state)
	end

    fun setupInput f init connections =
	let val statefun = apply f init
	    fun setup (connect, w, evt) = connect w (statefun evt)
	in  app setup connections
	  ; statefun
	end
end