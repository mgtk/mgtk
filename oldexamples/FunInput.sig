signature FunInput =
sig
    type unitcallback = unit -> unit
    type ('input, 'state) statefun = 'input -> 'state -> 'state
    type 'widget connector = 'widget -> unitcallback -> unit

    type ('in, 'wid) connections = ('wid connector * 'wid * 'in) list

    val setupInput :  ('i, 's) statefun -> 's -> ('i, 'w) connections ->
                      ('i, unit) statefun

end