structure Defs = struct

    type type_exp = string
    datatype type_flags = NullOk | Default of string
    type type_name_list = (type_exp * string * type_flags list) list

    type value_list = (string * string) list

    datatype attrib =
	Module of string
      | Parent of string
      | CName of string
      | TypeID of string
      | Fields of type_name_list
      | CopyFunc of string
      | ReleaseFunc of string
      | Constructor of string
      | OfObject of string
      | ReturnType of type_exp
      | Params of type_name_list
      | Values of value_list
      | Deprecated
      | Varargs of bool
      | CallerOwnsReturn of bool

    datatype def_tag =
	Object
      | Boxed
      | Enum
      | Function
      | Method

    type definition = string * def_tag * attrib list

end (* structure Defs *)