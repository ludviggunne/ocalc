
type t

val init : string -> t

val next : t -> t * Token.t option

val peek : t -> t * Token.t option
