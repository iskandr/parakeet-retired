type 'a t = {
  phi_id : ID.t;
  phi_left:  'a;
  phi_right: 'a;
  phi_src : SrcInfo.t option;
}

val id : 'a t -> ID.t
val left : 'a t -> 'a
val right : 'a t -> 'a
val src : 'a t -> SrcInfo.t option

val mk : ?src:SrcInfo.t -> ID.t -> 'a -> 'a -> 'a t
val mk_list : ?src:SrcInfo.t -> ID.t list -> 'a list -> 'a list -> 'a t list

val to_str : ('a -> string) -> 'a t -> string


val collect_phi_values : bool -> 'a t list -> ID.t list * 'a list
