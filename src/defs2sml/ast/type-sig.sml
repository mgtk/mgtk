(* mgtk --- an SML binding for GTK.                                          *)
(* (c) Ken Friis Larsen and Henning Niss 1999, 2000, 2001, 2002, 2003.       *)

signature Type = sig

    datatype 'a ty =
	Void
      | Base of 'a
      | Tname of 'a
      | Ptr of 'a ty
      | Const of 'a ty
      | Arr of int option * 'a ty
      | Func of (string * 'a ty) list * 'a ty

    val show: ('n -> string) -> 'n ty -> string
    val map: ('n1 -> 'n2) -> 'n1 ty -> 'n2 ty
    val mapi: (('n1 ty * 'n1) -> 'n2) -> 'n1 ty -> 'n2 ty

    val getParams: 'a ty -> (string * 'a ty) list
    val getRetType: 'a ty -> 'a ty

end (* signature Type *)
