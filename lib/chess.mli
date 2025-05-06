(* File: lib/chess.mli - Module interface *)

type color = White | Black
type piece_type = Pawn | Knight | Bishop | Rook | Queen | King
type piece = { piece_type : piece_type; color : color }
type square = piece option
type position = {
  board : square array array;
  turn : color;
  castling : (bool * bool * bool * bool);
  en_passant : (int * int) option;
  halfmove_clock : int;
  fullmove_number : int;
}

type move = {
  from_row : int;
  from_col : int;
  to_row : int;
  to_col : int;
  promotion : piece_type option;
}

val initial_position : unit -> position
val get_legal_moves : position -> move list
val apply_move : position -> move -> position
val is_in_check : position -> color -> bool
val is_checkmate : position -> bool
val is_stalemate : position -> bool
val position_to_fen : position -> string
val position_to_json : position -> Yojson.Basic.t
val move_to_json : move -> Yojson.Basic.t
val move_of_json : Yojson.Basic.t -> move option

module GameState : sig
  type t = {
    id: string;
    position: position;
    history: position list;
    last_updated: float;
  }
  
  val create : unit -> t
  val get : string -> t option
  val update : string -> position -> t
  val clean_old_games : unit -> unit
end