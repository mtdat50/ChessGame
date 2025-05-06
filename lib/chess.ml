(* File: lib/chess.ml *)
type color = White | Black

type piece_type = Pawn | Knight | Bishop | Rook | Queen | King

type piece = {
  piece_type : piece_type;
  color : color;
}

type square = piece option

type position = {
  board : square array array;  (* 8x8 board *)
  turn : color;
  castling : (bool * bool * bool * bool); (* white kingside, white queenside, black kingside, black queenside *)
  en_passant : (int * int) option;
  halfmove_clock : int;
  fullmove_number : int;
}

let initial_position () =
  let empty_board () = Array.make_matrix 8 8 None in
  let board = empty_board () in
  let place_piece row col piece_type color =
    board.(row).(col) <- Some { piece_type; color }
  in
  (* Place pawns *)
  for col = 0 to 7 do
    place_piece 1 col Pawn Black;
    place_piece 6 col Pawn White;
  done;
  (* Place major pieces *)
  place_piece 0 0 Rook Black;
  place_piece 0 1 Knight Black;
  place_piece 0 2 Bishop Black;
  place_piece 0 3 Queen Black;
  place_piece 0 4 King Black;
  place_piece 0 5 Bishop Black;
  place_piece 0 6 Knight Black;
  place_piece 0 7 Rook Black;
  
  place_piece 7 0 Rook White;
  place_piece 7 1 Knight White;
  place_piece 7 2 Bishop White;
  place_piece 7 3 Queen White;
  place_piece 7 4 King White;
  place_piece 7 5 Bishop White;
  place_piece 7 6 Knight White;
  place_piece 7 7 Rook White;
  
  {
    board;
    turn = White;
    castling = (true, true, true, true);
    en_passant = None;
    halfmove_clock = 0;
    fullmove_number = 1;
  }

let opposite_color = function
  | White -> Black
  | Black -> White

let is_valid_coord row col =
  row >= 0 && row < 8 && col >= 0 && col < 8

let get_piece position row col =
  if is_valid_coord row col then
    position.board.(row).(col)
  else
    None

let string_of_piece piece =
  match piece with
  | { piece_type = Pawn; color = White } -> "P"
  | { piece_type = Knight; color = White } -> "N"
  | { piece_type = Bishop; color = White } -> "B"
  | { piece_type = Rook; color = White } -> "R"
  | { piece_type = Queen; color = White } -> "Q"
  | { piece_type = King; color = White } -> "K"
  | { piece_type = Pawn; color = Black } -> "p"
  | { piece_type = Knight; color = Black } -> "n"
  | { piece_type = Bishop; color = Black } -> "b"
  | { piece_type = Rook; color = Black } -> "r"
  | { piece_type = Queen; color = Black } -> "q"
  | { piece_type = King; color = Black } -> "k"

(* let piece_of_char = function
  | 'P' -> Some { piece_type = Pawn; color = White }
  | 'N' -> Some { piece_type = Knight; color = White }
  | 'B' -> Some { piece_type = Bishop; color = White }
  | 'R' -> Some { piece_type = Rook; color = White }
  | 'Q' -> Some { piece_type = Queen; color = White }
  | 'K' -> Some { piece_type = King; color = White }
  | 'p' -> Some { piece_type = Pawn; color = Black }
  | 'n' -> Some { piece_type = Knight; color = Black }
  | 'b' -> Some { piece_type = Bishop; color = Black }
  | 'r' -> Some { piece_type = Rook; color = Black }
  | 'q' -> Some { piece_type = Queen; color = Black }
  | 'k' -> Some { piece_type = King; color = Black }
  | _ -> None *)

(* Move representation *)
type move = {
  from_row : int;
  from_col : int;
  to_row : int;
  to_col : int;
  promotion : piece_type option;
}

let algebraic_to_coords notation =
  if String.length notation <> 2 then None
  else
    let col = Char.code notation.[0] - Char.code 'a' in
    let row = 8 - (Char.code notation.[1] - Char.code '0') in
    if is_valid_coord row col then Some (row, col) else None

let coords_to_algebraic row col =
  if is_valid_coord row col then
    let file = Char.chr (Char.code 'a' + col) in
    let rank = Char.chr (Char.code '0' + (8 - row)) in
    String.make 1 file ^ String.make 1 rank
  else
    ""

(* Helper functions for move generation *)
let pawn_moves position row col =
  let moves = ref [] in
  (match get_piece position row col with
  | Some { piece_type = Pawn; color } ->
      let direction = if color = White then -1 else 1 in
      let start_row = if color = White then 6 else 1 in
      
      (* Forward move *)
      if get_piece position (row + direction) col = None then begin
        moves := { from_row = row; from_col = col; to_row = row + direction; to_col = col; promotion = None } :: !moves;
        
        (* Double forward move from starting position *)
        if row = start_row && get_piece position (row + 2 * direction) col = None then
          moves := { from_row = row; from_col = col; to_row = row + 2 * direction; to_col = col; promotion = None } :: !moves
      end;
      
      (* Captures *)
      for dc = -1 to 1 do
        if dc <> 0 && is_valid_coord (row + direction) (col + dc) then
          match get_piece position (row + direction) (col + dc) with
          | Some target when target.color <> color ->
              moves := { from_row = row; from_col = col; to_row = row + direction; to_col = col + dc; promotion = None } :: !moves
          | _ -> 
              (* Check for en passant *)
              match position.en_passant with
              | Some (ep_row, ep_col) when ep_row = row + direction && ep_col = col + dc ->
                  moves := { from_row = row; from_col = col; to_row = row + direction; to_col = col + dc; promotion = None } :: !moves
              | _ -> ()
      done;
      
      (* Promotions *)
      let promotion_row = if color = White then 0 else 7 in
      if row + direction = promotion_row then
        (* Replace non-promotion moves with promotion moves *)
        let promotion_moves = List.filter (fun m -> m.to_row = promotion_row) !moves in
        moves := List.filter (fun m -> m.to_row <> promotion_row) !moves;
        
        (* Add all possible promotions *)
        List.iter (fun m ->
          moves := { m with promotion = Some Queen } :: !moves;
          moves := { m with promotion = Some Rook } :: !moves;
          moves := { m with promotion = Some Bishop } :: !moves;
          moves := { m with promotion = Some Knight } :: !moves;
        ) promotion_moves
  | _ -> ());
  !moves  (* Simply return the reference contents *)

let knight_moves position row col =
  let moves = ref [] in
  (match get_piece position row col with
  | Some { piece_type = Knight; color } ->
      let deltas = [(-2, -1); (-2, 1); (-1, -2); (-1, 2); (1, -2); (1, 2); (2, -1); (2, 1)] in
      List.iter (fun (dr, dc) ->
        let new_row, new_col = row + dr, col + dc in
        if is_valid_coord new_row new_col then
          match get_piece position new_row new_col with
          | None -> 
              moves := { from_row = row; from_col = col; to_row = new_row; to_col = new_col; promotion = None } :: !moves
          | Some target when target.color <> color ->
              moves := { from_row = row; from_col = col; to_row = new_row; to_col = new_col; promotion = None } :: !moves
          | _ -> ()
      ) deltas
  | _ -> ());
  !moves

let sliding_moves position row col directions =
  let moves = ref [] in
  (match get_piece position row col with
  | Some { color; _ } ->
      List.iter (fun (dr, dc) ->
        let rec add_moves r c =
          let new_row, new_col = r + dr, c + dc in
          if is_valid_coord new_row new_col then
            match get_piece position new_row new_col with
            | None -> 
                moves := { from_row = row; from_col = col; to_row = new_row; to_col = new_col; promotion = None } :: !moves;
                add_moves new_row new_col
            | Some target when target.color <> color ->
                moves := { from_row = row; from_col = col; to_row = new_row; to_col = new_col; promotion = None } :: !moves
            | _ -> ()
          else ()
        in
        add_moves row col
      ) directions
  | _ -> ());
  !moves

let bishop_moves position row col =
  sliding_moves position row col [(-1, -1); (-1, 1); (1, -1); (1, 1)]

let rook_moves position row col =
  sliding_moves position row col [(-1, 0); (0, -1); (0, 1); (1, 0)]

let queen_moves position row col =
  sliding_moves position row col [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)]

let king_moves position row col =
  let moves = ref [] in
  (match get_piece position row col with
  | Some { piece_type = King; color } ->
      (* Normal king moves *)
      for dr = -1 to 1 do
        for dc = -1 to 1 do
          if not (dr = 0 && dc = 0) then
            let new_row, new_col = row + dr, col + dc in
            if is_valid_coord new_row new_col then
              match get_piece position new_row new_col with
              | None -> 
                  moves := { from_row = row; from_col = col; to_row = new_row; to_col = new_col; promotion = None } :: !moves
              | Some target when target.color <> color ->
                  moves := { from_row = row; from_col = col; to_row = new_row; to_col = new_col; promotion = None } :: !moves
              | _ -> ()
        done
      done;
      
      (* Castling *)
      let (wk, wq, bk, bq) = position.castling in
      let can_castle_kingside = if color = White then wk else bk in
      let can_castle_queenside = if color = White then wq else bq in
      
      let base_row = if color = White then 7 else 0 in
      
      (* Check kingside castling *)
      if can_castle_kingside &&
         get_piece position base_row 5 = None &&
         get_piece position base_row 6 = None then
        moves := { from_row = row; from_col = col; to_row = base_row; to_col = 6; promotion = None } :: !moves;
      
      (* Check queenside castling *)
      if can_castle_queenside &&
         get_piece position base_row 1 = None &&
         get_piece position base_row 2 = None &&
         get_piece position base_row 3 = None then
        moves := { from_row = row; from_col = col; to_row = base_row; to_col = 2; promotion = None } :: !moves
  | _ -> ());
  !moves

let get_all_moves position =
  let moves = ref [] in
  for row = 0 to 7 do
    for col = 0 to 7 do
      match get_piece position row col with
      | Some { piece_type; color } when color = position.turn ->
          let piece_moves = match piece_type with
            | Pawn -> pawn_moves position row col
            | Knight -> knight_moves position row col
            | Bishop -> bishop_moves position row col
            | Rook -> rook_moves position row col
            | Queen -> queen_moves position row col
            | King -> king_moves position row col
          in
          moves := List.append piece_moves !moves
      | _ -> ()
    done
  done;
  !moves

(* Apply a move to a position *)
let apply_move position move =
  let new_board = Array.make_matrix 8 8 None in
  
  (* Copy the board *)
  for r = 0 to 7 do
    for c = 0 to 7 do
      new_board.(r).(c) <- position.board.(r).(c)
    done
  done;
  
  let piece = position.board.(move.from_row).(move.from_col) in
  let is_capture = position.board.(move.to_row).(move.to_col) <> None in
  let is_pawn_move = match piece with Some { piece_type = Pawn; _ } -> true | _ -> false in
  
  (* Handle en passant capture *)
  let en_passant_capture = 
    match (piece, position.en_passant) with
    | (Some { piece_type = Pawn; _ }, Some (ep_row, ep_col)) 
      when move.to_row = ep_row && move.to_col = ep_col ->
        let pawn_row = if position.turn = White then ep_row + 1 else ep_row - 1 in
        new_board.(pawn_row).(ep_col) <- None;
        true
    | _ -> false
  in
  
  (* Move the piece *)
  let moving_piece = match (piece, move.promotion) with
    | (Some p, Some promotion_type) -> Some { p with piece_type = promotion_type }
    | _ -> piece
  in
  new_board.(move.to_row).(move.to_col) <- moving_piece;
  new_board.(move.from_row).(move.from_col) <- None;
  
  (* Handle castling moves *)
  let (wk, wq, bk, bq) = position.castling in
  
  let updated_castling = 
    match piece with
    | Some { piece_type = King; color = White } ->
        (* King moved, lose all castling rights *)
        (false, false, bk, bq)
    | Some { piece_type = King; color = Black } ->
        (* King moved, lose all castling rights *)
        (wk, wq, false, false)
    | Some { piece_type = Rook; color = White } ->
        if move.from_row = 7 && move.from_col = 0 then
          (* Queen's rook moved *)
          (wk, false, bk, bq)
        else if move.from_row = 7 && move.from_col = 7 then
          (* King's rook moved *)
          (false, wq, bk, bq)
        else
          (wk, wq, bk, bq)
    | Some { piece_type = Rook; color = Black } ->
        if move.from_row = 0 && move.from_col = 0 then
          (* Queen's rook moved *)
          (wk, wq, bk, false)
        else if move.from_row = 0 && move.from_col = 7 then
          (* King's rook moved *)
          (wk, wq, false, bq)
        else
          (wk, wq, bk, bq)
    | _ -> (wk, wq, bk, bq)
  in
  
  (* Handle castling rook movement *)
  begin match piece with
  | Some { piece_type = King; color } ->
      let base_row = if color = White then 7 else 0 in
      if move.from_col = 4 && move.to_col = 6 then begin
        (* Kingside castling - move the rook too *)
        new_board.(base_row).(5) <- new_board.(base_row).(7);
        new_board.(base_row).(7) <- None;
      end else if move.from_col = 4 && move.to_col = 2 then begin
        (* Queenside castling - move the rook too *)
        new_board.(base_row).(3) <- new_board.(base_row).(0);
        new_board.(base_row).(0) <- None;
      end
  | _ -> ()
  end;
  
  (* Calculate new en passant square *)
  let new_en_passant =
    match piece with
    | Some { piece_type = Pawn; color } ->
        let direction = if color = White then -1 else 1 in
        if abs (move.to_row - move.from_row) = 2 then
          (* Pawn moved two squares *)
          Some (move.from_row + direction, move.from_col)
        else
          None
    | _ -> None
  in
  
  (* Update halfmove clock *)
  let new_halfmove_clock =
    if is_pawn_move || is_capture || en_passant_capture then
      0  (* Reset on pawn moves and captures *)
    else
      position.halfmove_clock + 1
  in
  
  (* Update fullmove number *)
  let new_fullmove_number =
    if position.turn = Black then
      position.fullmove_number + 1
    else
      position.fullmove_number
  in
  
  {
    board = new_board;
    turn = opposite_color position.turn;
    castling = updated_castling;
    en_passant = new_en_passant;
    halfmove_clock = new_halfmove_clock;
    fullmove_number = new_fullmove_number;
  }

(* Find the king's position *)
let find_king position color =
  let result = ref None in
  for row = 0 to 7 do
    for col = 0 to 7 do
      match position.board.(row).(col) with
      | Some { piece_type = King; color = king_color } when king_color = color ->
          result := Some (row, col)
      | _ -> ()
    done
  done;
  !result

(* Check if a square is under attack *)
let is_square_attacked position row col attacking_color =
  (* Check for attacking pawns *)
  let pawn_directions = if attacking_color = White then [(1, -1); (1, 1)] else [(-1, -1); (-1, 1)] in
  let pawn_attacks = List.exists (fun (dr, dc) ->
    let r, c = row + dr, col + dc in
    if is_valid_coord r c then
      match position.board.(r).(c) with
      | Some { piece_type = Pawn; color } -> color = attacking_color
      | _ -> false
    else false
  ) pawn_directions in
  
  if pawn_attacks then true
  else
    (* Check for attacking knights *)
    let knight_deltas = [(-2, -1); (-2, 1); (-1, -2); (-1, 2); (1, -2); (1, 2); (2, -1); (2, 1)] in
    let knight_attacks = List.exists (fun (dr, dc) ->
      let r, c = row + dr, col + dc in
      if is_valid_coord r c then
        match position.board.(r).(c) with
        | Some { piece_type = Knight; color } -> color = attacking_color
        | _ -> false
      else false
    ) knight_deltas in
    
    if knight_attacks then true
    else
      (* Check for attacking bishops, rooks, queens *)
      let directions = [
        (* Bishop/Queen diagonals *)
        (-1, -1); (-1, 1); (1, -1); (1, 1);
        (* Rook/Queen orthogonals *)
        (-1, 0); (0, -1); (0, 1); (1, 0)
      ] in
      
      let sliding_attacks = List.exists (fun (dr, dc) ->
        let rec check_direction r c =
          let new_r, new_c = r + dr, c + dc in
          if is_valid_coord new_r new_c then
            match position.board.(new_r).(new_c) with
            | Some { piece_type; color } ->
                if color = attacking_color then
                  match piece_type with
                  | Queen -> true
                  | Rook when dr = 0 || dc = 0 -> true
                  | Bishop when dr <> 0 && dc <> 0 -> true
                  | _ -> false
                else
                  false
            | None -> check_direction new_r new_c
          else
            false
        in
        check_direction row col
      ) directions in
      
      if sliding_attacks then true
      else
        (* Check for attacking king *)
        let king_deltas = [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)] in
        List.exists (fun (dr, dc) ->
          let r, c = row + dr, col + dc in
          if is_valid_coord r c then
            match position.board.(r).(c) with
            | Some { piece_type = King; color } -> color = attacking_color
            | _ -> false
          else
            false
        ) king_deltas

(* Check if a player is in check *)
let is_in_check position color =
  match find_king position color with
  | Some (king_row, king_col) ->
      is_square_attacked position king_row king_col (opposite_color color)
  | None -> false

(* Check if a move is legal by verifying it doesn't leave the player in check *)
let is_legal_move position move =
  let new_position = apply_move position move in
  not (is_in_check new_position position.turn)

(* Get all legal moves for the current player *)
let get_legal_moves position =
  List.filter (is_legal_move position) (get_all_moves position)

(* Check for checkmate or stalemate *)
let is_checkmate position =
  is_in_check position position.turn && get_legal_moves position = []

let is_stalemate position =
  not (is_in_check position position.turn) && get_legal_moves position = []

(* Convert position to FEN string *)
let position_to_fen position =
  let fen_board = ref "" in
  
  (* Board representation *)
  for row = 0 to 7 do
    let empty_count = ref 0 in
    for col = 0 to 7 do
      match position.board.(row).(col) with
      | None -> 
          incr empty_count
      | Some piece ->
          if !empty_count > 0 then begin
            fen_board := !fen_board ^ string_of_int !empty_count;
            empty_count := 0
          end;
          fen_board := !fen_board ^ string_of_piece piece
    done;
    
    if !empty_count > 0 then
      fen_board := !fen_board ^ string_of_int !empty_count;
    
    if row < 7 then
      fen_board := !fen_board ^ "/"
  done;
  
  (* Active color *)
  let active_color = match position.turn with White -> "w" | Black -> "b" in
  
  (* Castling availability *)
  let (wk, wq, bk, bq) = position.castling in
  let castling = 
    (if wk then "K" else "") ^
    (if wq then "Q" else "") ^
    (if bk then "k" else "") ^
    (if bq then "q" else "") 
  in
  let castling = if castling = "" then "-" else castling in
  
  (* En passant target square *)
  let en_passant = 
    match position.en_passant with
    | Some (row, col) -> coords_to_algebraic row col
    | None -> "-"
  in
  
  (* Combine all parts *)
  Printf.sprintf "%s %s %s %s %d %d" 
    !fen_board 
    active_color 
    castling 
    en_passant 
    position.halfmove_clock 
    position.fullmove_number

(* Parse a FEN string to create a position
let position_from_fen fen =
  let parts = String.split_on_char ' ' fen in
  if List.length parts < 6 then
    None
  else
    try
      let board_str = List.nth parts 0 in
      let rows = String.split_on_char '/' board_str in
      
      let board = Array.make_matrix 8 8 None in
      
      (* Parse board *)
      List.iteri (fun row_idx row_str ->
        let col = ref 0 in
        String.iter (fun c ->
          if c >= '1' && c <= '8' then
            (* Empty squares *)
            col := !col + (Char.code c - Char.code '0')
          else begin
            (* Piece *)
            board.(row_idx).(!col) <- piece_of_char c;
            incr col
          end
        ) row_str
      ) rows;
      
      (* Parse active color *)
      let turn = if List.nth parts 1 = "w" then White else Black in
      
      (* Parse castling *)
      let castling_str = List.nth parts 2 in
      let wk = String.contains castling_str 'K' in
      let wq = String.contains castling_str 'Q' in
      let bk = String.contains castling_str 'k' in
      let bq = String.contains castling_str 'q' in
      
      (* Parse en passant *)
      let en_passant = 
        let ep_str = List.nth parts 3 in
        if ep_str = "-" then None
        else algebraic_to_coords ep_str
      in
      
      (* Parse halfmove clock and fullmove number *)
      let halfmove_clock = int_of_string (List.nth parts 4) in
      let fullmove_number = int_of_string (List.nth parts 5) in
      
      Some {
        board;
        turn;
        castling = (wk, wq, bk, bq);
        en_passant;
        halfmove_clock;
        fullmove_number;
      }
    with _ -> None *)

(* JSON serialization functions *)
let piece_to_json piece =
  match piece with
  | None -> `Null
  | Some {piece_type; color} ->
      let type_str = match piece_type with
        | Pawn -> "pawn"
        | Knight -> "knight"
        | Bishop -> "bishop"
        | Rook -> "rook"
        | Queen -> "queen"
        | King -> "king"
      in
      let color_str = match color with
        | White -> "white"
        | Black -> "black"
      in
      `Assoc [
        ("piece_type", `String type_str);
        ("color", `String color_str)
      ]

let position_to_json position =
  let board_json = `List (
    Array.to_list (Array.map (fun row ->
      `List (Array.to_list (Array.map piece_to_json row))
    ) position.board)
  ) in
  
  let (wk, wq, bk, bq) = position.castling in
  let castling_json = `Assoc [
    ("white_kingside", `Bool wk);
    ("white_queenside", `Bool wq);
    ("black_kingside", `Bool bk);
    ("black_queenside", `Bool bq)
  ] in
  
  let en_passant_json = match position.en_passant with
    | None -> `Null
    | Some (row, col) -> `Assoc [
        ("row", `Int row);
        ("col", `Int col)
      ]
  in
  
  `Assoc [
    ("board", board_json);
    ("turn", `String (match position.turn with White -> "white" | Black -> "black"));
    ("castling", castling_json);
    ("en_passant", en_passant_json);
    ("halfmove_clock", `Int position.halfmove_clock);
    ("fullmove_number", `Int position.fullmove_number);
    ("check", `Bool (is_in_check position position.turn));
    ("checkmate", `Bool (is_checkmate position));
    ("stalemate", `Bool (is_stalemate position))
  ]

let move_to_json move =
  `Assoc [
    ("from", `String (coords_to_algebraic move.from_row move.from_col));
    ("to", `String (coords_to_algebraic move.to_row move.to_col));
    ("promotion", match move.promotion with
      | None -> `Null
      | Some Queen -> `String "queen"
      | Some Rook -> `String "rook"
      | Some Bishop -> `String "bishop"
      | Some Knight -> `String "knight"
      | _ -> `Null
    )
  ]

(* Convert the JSON representation to a move *)
let move_of_json (json : Yojson.Basic.t) =
  match json with
  | `Assoc props ->
      (* First get all the fields as strings *)
      let from_str = List.assoc_opt "from" props |> function
        | Some (`String s) -> Some s
        | _ -> None
      in
      let to_str = List.assoc_opt "to" props |> function
        | Some (`String s) -> Some s
        | _ -> None
      in
      let promotion_str = List.assoc_opt "promotion" props |> function
        | Some (`String s) -> Some s
        | _ -> None
      in

      (* Then process the promotion type *)
      let promotion_type = match promotion_str with
        | None -> None
        | Some "queen" -> Some Queen
        | Some "rook" -> Some Rook
        | Some "bishop" -> Some Bishop
        | Some "knight" -> Some Knight
        | Some _ -> None
      in

      (* Finally construct the move if coordinates are valid *)
      begin match from_str, to_str with
      | Some from_coord, Some to_coord ->
          begin match algebraic_to_coords from_coord, algebraic_to_coords to_coord with
          | Some (from_row, from_col), Some (to_row, to_col) ->
              Some { from_row; from_col; to_row; to_col; promotion = promotion_type }
          | _ -> None
          end
      | _ -> None
      end
  | _ -> None

(* Game states and persistence *)
module GameState = struct
  type t = {
    id: string;
    position: position;
    history: position list;
    last_updated: float;
  }
  
  let games = Hashtbl.create 100
  
  let create () =
    let id = Printf.sprintf "%08x" (Random.int 0x10000000) in
    let game = {
      id;
      position = initial_position ();
      history = [];
      last_updated = Unix.time ()
    } in
    Hashtbl.add games id game;
    game
  
  let get id =
    Hashtbl.find_opt games id
  
  let update id new_position =
    match Hashtbl.find_opt games id with
    | Some game ->
        let updated = {
          id;
          position = new_position;
          history = game.position :: game.history;
          last_updated = Unix.time ()
        } in
        Hashtbl.replace games id updated;
        updated
    | None -> 
        raise Not_found
  
  let clean_old_games () =
    let current_time = Unix.time () in
    let old_ids = 
      Hashtbl.fold (fun id game acc ->
        if current_time -. game.last_updated > 86400.0 (* 24 hours *) then
          id :: acc
        else
          acc
      ) games []
    in
    List.iter (Hashtbl.remove games) old_ids
end