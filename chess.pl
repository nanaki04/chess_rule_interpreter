:- use_module(library(clpfd)).
:- use_module(library(http/json)).
:- use_module(library(lists)).


% COMMANDS

run_command([error, Error], _, Output) :- Output = [error, Error].
run_command([assess_moves, PieceCoord], GameState, Output) :-
  piece_at(PieceCoord, Piece, GameState),
  piece_movable_coords_without_exposing_king(Piece, Output, GameState).
run_command([check_state, Color], GameState, Output) :-
  check_state(Color, GameState, Output).
run_command([is_upgradable, Coord], GameState, Output) :-
  piece_at(Coord, Piece, GameState),
  (is_upgradable(Piece, GameState) -> Output = true; Output = false).
run_command(no_matches, _, Output) :-
  Output = no_matches.

% RULES

check_state(Color, GameState, CheckState) :-
  is_unable_to_act(Color, GameState),
  (is_check(Color, GameState),
    CheckState = check_mate;
    CheckState = remise
  );
  is_check(Color, GameState),
  CheckState = check;
  CheckState = ok.

is_upgradable(Pawn, GameState) :-
  piece_is_type(Pawn, pawn),
  piece_has_move_count(Pawn, MoveCount),
  is_turn(GameState, MoveCount),
  is_upgradable_(Pawn).

is_upgradable_(Pawn, GameState) :-
  piece_has_color(Pawn, white),
  board_top(GameState, BoardTop),
  piece_is_at_coord(Pawn, [_X, BoardTop]).

is_upgradable_(Pawn, _GameState) :-
  piece_has_color(Pawn, black),
  piece_is_at_coord(Pawn, [_X, 0]).

is_check(white, GameState) :-
  find_white_king(King, GameState),
  is_attackable(King, GameState).

is_check(black, GameState) :-
  find_black_king(King, GameState),
  is_attackable(King, GameState).

is_unable_to_act(white, GameState) :-
  piece_is_alive(Piece),
  setof(Piece, piece_has_color(Piece, white), Pieces),
  is_unable_to_act_(Pieces, GameState).

is_unable_to_act_([], _GameState).

is_unable_to_act_([Piece | Pieces], GameState) :-
  piece_movable_coords_without_exposing_king(Piece, Coords, GameState),
  length(Coords) #= 0,
  is_unable_to_act_(Pieces, GameState).

piece_movable_coords_without_exposing_king(Piece, Coords, GameState) :-
  piece_movable_coords(Piece, MovableCoords, GameState),
  query(ExposedCoord, exposes_king(Piece, ExposedCoord, MovableCoords, GameState), ExposedCoords),
  subtract(MovableCoords, ExposedCoords, Coords).

piece_movable_coords(Piece, Coords, GameState) :-
  piece_is_alive(Piece),
  piece_movable_coords_(Piece, Coords, GameState).

piece_movable_coords_(Rook, Coords, GameState) :-
  piece_is_type(Rook, rook),
  find_straight_lines_until_blocked(Rook, Coords, GameState).

piece_movable_coords_(Knight, Coords, GameState) :-
  piece_is_type(Knight, knight),
  query(Coord, piece_movable_coord(Knight, Coord, GameState), Coords).

piece_movable_coords_(Bishop, Coords, GameState) :-
  piece_is_type(Bishop, bishop),
  find_diagonal_lines_until_blocked(Bishop, Coords, GameState).

piece_movable_coords_(Queen, Coords, GameState) :-
  piece_is_type(Queen, queen),
  find_straight_lines_until_blocked(Queen, StraightCoords, GameState),
  find_diagonal_lines_until_blocked(Queen, DiagonalCoords, GameState),
  append(StraightCoords, DiagonalCoords, Coords).

piece_movable_coords_(King, Coords, GameState) :-
  piece_is_type(King, king),
  query(StandardCoord, piece_movable_coord(King, StandardCoord, GameState), StandardCoords),
  query(RockadeCoord, find_rockade_coord(King, RockadeCoord, GameState), RockadeCoords),
  append(StandardCoords, RockadeCoords, Coords).

piece_movable_coords_(Pawn, Coords, GameState) :-
  piece_is_type(Pawn, pawn),
  query(Coord, find_pawn_coord(Pawn, Coord, GameState), Coords).

piece_movable_coord(Piece, Coord, GameState) :-
  piece_unfiltered_movable_coord(Piece, Coord, GameState),
  not(is_coord_occupied_by_self(Piece, Coord, GameState)).

piece_unfiltered_movable_coord(Piece, Coord, GameState) :-
  coord_on_board(Coord, GameState),
  find_offset(Piece, Coord, Offset),
  piece_movable_offset(Piece, Offset),
  label(Coord).

find_pawn_coord(Pawn, Offset, GameState) :-
  [0, 1] = Offset,
  piece_has_color(Pawn, white),
  is_empty(Pawn, Offset, GameState).

find_pawn_coord(Pawn, Offset, GameState) :-
  [0, -1] = Offset,
  piece_has_color(Pawn, black),
  is_empty(Pawn, Offset, GameState).

find_pawn_coord(Pawn, Offset, GameState) :-
  [0, 2] = Offset,
  piece_has_color(Pawn, white),
  piece_has_move_count(Pawn, 0),
  is_empty(Pawn, Offset, GameState).

find_pawn_coord(Pawn, Offset, GameState) :-
  [0, -2] = Offset,
  piece_has_color(Pawn, black),
  piece_has_move_count(Pawn, 0),
  is_empty(Pawn, Offset, GameState).

find_pawn_coord(Pawn, Offset, GameState) :-
  [X, 1] = Offset,
  X in -1\/1,
  piece_has_color(Pawn, white),
  (is_occupied_by_enemy(Pawn, Offset, GameState);
    is_en_passant(Pawn, Offset, [0, -1], GameState)
  ).

find_pawn_coord(Pawn, Offset, GameState) :-
  [X, -1] = Offset,
  X in -1\/1,
  piece_has_color(Pawn, black),
  (is_occupied_by_enemy(Pawn, Offset, GameState);
    is_en_passant(Pawn, Offset, [0, 1], GameState)
  ).

is_en_passant(Pawn, Offset, EnPassantOffset, GameState) :-
  is_empty(Pawn, Offset, GameState),
  add_coord(Offset, EnPassantOffset, EnPassantCoord),
  piece_at(EnPassantCoord, PassedPawn, GameState),
  pieces_have_different_colors(Pawn, PassedPawn),
  piece_has_move_count(PassedPawn, 1),
  is_turn(GameState, Turn),
  piece_has_moved_at_turn(PassedPawn, Turn - 1),
  piece_last_move_offset(PassedPawn, [_X, Y]),
  Y in -2\/2.

find_rockade_coord(King, Coord, GameState) :-
  find_rockade_coord_(King, Coord, [2, 0], [3, 0], GameState).

find_rockade_coord(King, Coord, GameState) :-
  find_rockade_coord_(King, Coord, [-2, 0], [-4, 0], GameState).

find_rockade_coord_(King, Coord, KingOffset, RookOffset, GameState) :-
  piece_has_move_count(King, 0),
  piece_is_at_coord(King, KingCoord),
  find_offset(King, Coord, KingOffset),
  find_offset(King, RookCoord, RookOffset),
  piece_at(RookCoord, Rook, GameState),
  piece_has_move_count(Rook, 0),
  pieces_have_same_color(King, Rook),
  not(is_piece_inbetween(KingCoord, RookCoord, GameState)),
  not(is_attackable_while_moving_offset(King, KingOffset, GameState)).

find_straight_lines_until_blocked(Piece, FilteredCoords, GameState) :-
  find_until_blocked(Piece, [1, 0], CoordsRight, GameState),
  find_until_blocked(Piece, [-1, 0], CoordsLeft, GameState),
  find_until_blocked(Piece, [0, 1], CoordsUp, GameState),
  find_until_blocked(Piece, [0, -1], CoordsDown, GameState),
  append(CoordsRight, CoordsLeft, CoordsHorizontal),
  append(CoordsUp, CoordsDown, CoordsVertical),
  append(CoordsHorizontal, CoordsVertical, FilteredCoords).

find_diagonal_lines_until_blocked(Piece, FilteredCoords, GameState) :-
  find_until_blocked(Piece, [1, 1], CoordsRightUp, GameState),
  find_until_blocked(Piece, [-1, 1], CoordsLeftUp, GameState),
  find_until_blocked(Piece, [1, -1], CoordsRightDown, GameState),
  find_until_blocked(Piece, [-1, -1], CoordsLeftDown, GameState),
  append(CoordsRightUp, CoordsLeftUp, CoordsUp),
  append(CoordsRightDown, CoordsLeftDown, CoordsDown),
  append(CoordsUp, CoordsDown, FilteredCoords).

find_until_blocked(Piece, Step, Coords, GameState) :-
  [X, 0] = Step,
  X in -1\/1,
  (X #> 0 -> col_length(GameState, LimitX) ; LimitX = -1),
  piece_is_at_coord(Piece, [_, Y]),
  find_until_blocked_(Piece, [X, 0], [LimitX, Y], Coords, GameState).

find_until_blocked(Piece, Step, Coords, GameState) :-
  [0, Y] = Step,
  Y in -1\/1,
  (Y #> 0 -> row_length(GameState, LimitY) ; LimitY = -1),
  piece_is_at_coord(Piece, [X, _]),
  find_until_blocked_(Piece, [0, Y], [X, LimitY], Coords, GameState).

find_until_blocked(Piece, Step, Coords, GameState) :-
  [X, Y] = Step,
  Step ins -1\/1,
  (X #> 0 -> col_length(GameState, LimitX) ; LimitX = -1),
  (Y #> 0 -> row_length(GameState, LimitY) ; LimitY = -1),
  find_until_blocked_(Piece, [X, Y], [LimitX, LimitY], Coords, GameState).

find_until_blocked_(Piece, Step, Limit, Coords, GameState) :-
  find_until_blocked_(Piece, Step, Limit, [], Coords, GameState).

find_until_blocked_(Piece, Step, Limit, Acc, Coords, GameState) :-
  piece_is_at_coord(Piece, Coord),
  add_coord(Coord, Step, Next),
  find_until_blocked_(Piece, Next, Step, Limit, Acc, Coords, GameState).

find_until_blocked_(_Piece, Limit, _Step, Limit, Acc, Coords, _GameState) :-
  Coords = Acc.

find_until_blocked_(Piece, Current, Step, Limit, Acc, Coords, GameState) :-
  not_equal_coords(Current, Limit),
  is_coord_empty(Current, GameState),
  add_coord(Current, Step, Next),
  find_until_blocked_(Piece, Next, Step, Limit, [Current | Acc], Coords, GameState).

find_until_blocked_(Piece, Current, _Step, _Limit, Acc, Coords, GameState) :-
  piece_at(Current, TargetPiece, GameState),
  (pieces_have_same_color(Piece, TargetPiece) -> Coords = Acc ; Coords = [Current | Acc]).

piece_movable_offset(Knight, [X, Y]) :-
  piece_is_type(Knight, knight),
  (abs(X) #= 1,
  abs(Y) #= 2;
  abs(X) #= 2,
  abs(Y) #= 1).

piece_movable_offset(King, Offset) :-
  piece_is_type(King, king),
  Offset ins -1..1.

piece_movable_offset(Rook, [X, Y]) :-
  piece_is_type(Rook, rook),
  X #= 0 #\ Y #= 0.

piece_movable_offset(Bishop, [X, Y]) :-
  piece_is_type(Bishop, bishop),
  abs(X) #= abs(Y).

piece_movable_offset(Queen, [X, Y]) :-
  piece_is_type(Queen, queen),
  (abs(X) #= abs(Y);
  X #= 0 #\ Y #= 0).

piece_movable_offset(Pawn, [0, -1]) :-
  piece_is_type(Pawn, pawn),
  piece_has_color(Pawn, black).

piece_movable_offset(Pawn, [0, 1]) :-
  piece_is_type(Pawn, pawn),
  piece_has_color(Pawn, white).

piece_movable_offset(Pawn, [0, -2]) :-
  piece_is_type(Pawn, pawn),
  piece_has_move_count(Pawn, 0),
  piece_has_color(Pawn, black).

piece_movable_offset(Pawn, [0, 2]) :-
  piece_is_type(Pawn, pawn),
  piece_has_move_count(Pawn, 0),
  piece_has_color(Pawn, white).

is_coord_attackable_by_white(Coord, GameState) :-
  [Pieces | _] = GameState,
  member(Piece, Pieces),
  piece_has_color(Piece, white),
  piece_movable_coords(Piece, Coords, GameState),
  member(Coord, Coords).

is_coord_attackable_by_black(Coord, GameState) :-
  [Pieces | _] = GameState,
  member(Piece, Pieces),
  piece_has_color(Piece, black),
  piece_movable_coords(Piece, Coords, GameState),
  member(Coord, Coords).

is_attackable(Piece, GameState) :-
  piece_is_at_coord(Piece, Coord),
  piece_has_color(Piece, white),
  is_coord_attackable_by_black(Coord, GameState).

is_attackable(Piece, GameState) :-
  piece_is_at_coord(Piece, Coord),
  piece_has_color(Piece, black),
  is_coord_attackable_by_white(Coord, GameState).

is_attackable_while_moving_to(Piece, To, GameState) :-
  find_offset(Piece, To, Offset),
  is_attackable_while_moving_offset(Piece, Offset, GameState).

is_attackable_while_moving_offset(Piece, Offset, GameState) :-
  piece_is_at_coord(Piece, Coord),
  find_offset(Piece, To, Offset),
  sign_coord(Offset, Step),
  is_attackable_while_moving_to_(Piece, Step, Coord, To, GameState).

is_attackable_while_moving_to_(Piece, Step, Current, To, GameState) :-
  not_equal_coords(Current, To),
  piece_has_color(Piece, black),
  (is_coord_attackable_by_white(Current, GameState); 
    add_coord(Current, Step, Next),
    is_attackable_while_moving_to_(Piece, Step, Next, To, GameState)
  ).

is_attackable_while_moving_to_(Piece, Step, Current, To, GameState) :-
  not_equal_coords(Current, To),
  piece_has_color(Piece, white),
  (is_coord_attackable_by_black(Current, GameState); 
    add_coord(Current, Step, Next),
    is_attackable_while_moving_to_(Piece, Step, Next, To, GameState)
  ).

exposes_king(Piece, Coord, MovableCoords, GameState) :-
  member(Coord, MovableCoords),
  update_piece_coord(Piece, Coord, GameState, Simulation),
  find_king(Piece, King, Simulation),
  is_attackable(King, Simulation).

% STATE CHANGES

update_piece_coord(Piece, Coord, GameState, GameStateOut) :-
  [Id | [_Coord | Rest]] = Piece,
  update_piece(Piece, [Id | [Coord | Rest]], GameState, GameStateOut).

update_pieces(NewPieces, [_Pieces | Rest], GameStateOut) :-
  GameStateOut = [NewPieces | Rest].

update_piece(Piece, NewPiece, GameState, GameStateOut) :-
  [Pieces | _] = GameState,
  update_piece_(Piece, NewPiece, Pieces, [], NewPieces),
  update_pieces(NewPieces, GameState, GameStateOut).

update_piece_(Piece, NewPiece, [Piece | Rest], Acc, NewPieces) :-
  reverse([NewPiece | Acc], PartialNewPieces),
  append(PartialNewPieces, Rest, NewPieces).

update_piece_(Piece, NewPiece, [Next | Rest], Acc, NewPieces) :-
  Piece \= Next,
  update_piece_(Piece, NewPiece, Rest, [Next | Acc], NewPieces).

% UTILITY

is_zero_coord([X, Y]) :-
  X #= 0,
  Y #= 0.
is_non_zero_coord([X, Y]) :-
  X #\= 0 ; Y #\= 0.
is_equal_coord([X1, Y1], [X2, Y2]) :-
  X1 #= X2,
  Y1 #= Y2.
subtract_coord([X1, Y1], [X2, Y2], CoordOut) :-
  X #= X1 - X2,
  Y #= Y1 - Y2,
  CoordOut = [X, Y].
add_coord([X1, Y1], [X2, Y2], CoordOut) :-
  X #= X1 + X2,
  Y #= Y1 + Y2,
  CoordOut = [X, Y].
not_equal_coords([X1, Y1], [X2, Y2]) :-
  X1 #\= X2;
  Y1 #\= Y2.
sign_coord([X, Y], CoordOut) :-
  XOut is sign(X),
  YOut is sign(Y),
  CoordOut = [XOut, YOut].
find_offset(Piece, Coord, Offset) :-
  piece_is_at_coord(Piece, PieceCoord),
  subtract_coord(Coord, PieceCoord, Offset),
  is_non_zero_coord(Offset).

limit_offset([0, Y1], [0, Y2]) :-
  abs(Y1) #< abs(Y2) + 1.
limit_offset([X1, 0], [X2, 0]) :-
  abs(X1) #< abs(X2) + 1.
limit_offset([X1, Y1], [X2, Y2]) :-
  abs(X1) #< abs(X2) + 1,
  abs(Y1) #< abs(Y2) + 1.
cap_offset([0, Y1], [0, Y2]) :-
  abs(Y1) #< abs(Y2).
cap_offset([X1, 0], [X2, 0]) :-
  abs(X1) #< abs(X2).
cap_offset([X1, Y1], [X2, Y2]) :-
  abs(X1) #< abs(X2),
  abs(Y1) #< abs(Y2).
min_offset([X, Y], CoordOut) :-
  abs(X) #< abs(Y),
  CoordOut = [X, X].
min_offset([X, Y], CoordOut) :-
  abs(Y) #< abs(X),
  CoordOut = [Y, Y].
min_offset([X, X], [X, X]).

is_straight_direction([0, _]).
is_straight_direction([_, 0]).
is_diagonal_direction([X, Y]) :-
  is_non_zero_coord([X, Y]),
  abs(X) #= abs(Y).
is_straight_or_diagonal_direction(Offset) :-
  is_straight_direction(Offset);
  is_diagonal_direction(Offset).

is_at(Coord, [_ | [Coord | _]]).

find_king(Piece, King, GameState) :-
  piece_has_color(Piece, white),
  find_white_king(King, GameState).

find_king(Piece, King, GameState) :-
  piece_has_color(Piece, black),
  find_black_king(King, GameState).

find_black_king(Piece, GameState) :-
  [Pieces | _] = GameState,
  find_black_king_(Piece, Pieces).

find_black_king_(BlackKing, [BlackKing | Pieces]) :-
  piece_is_black_king(BlackKing);
  find_black_king_(BlackKing, Pieces).

find_white_king(Piece, GameState) :-
  [Pieces | _] = GameState,
  find_white_king_(Piece, Pieces).

find_white_king_(WhiteKing, [WhiteKing | Pieces]) :-
  piece_is_white_king(WhiteKing);
  find_white_king_(WhiteKing, Pieces).

piece_at(Coord, Piece, [Pieces | _]) :-
  piece_at_(Coord, Piece, Pieces).
piece_at_(Coord, Piece, [FirstPiece | Pieces]) :-
  is_at(Coord, FirstPiece),
  Piece = FirstPiece ;
  piece_at_(Coord, Piece, Pieces).

piece_is_white([_Id, _PieceCoord, white, _PieceType, _MoveCount, _Alive, _LastMove]).
piece_is_black([_Id, _PieceCoord, black, _PieceType, _MoveCount, _Alive, _LastMove]).
piece_has_color([_Id, _PieceCoord, PieceColor, _PieceType, _MoveCount, _Alive, _LastMove], PieceColor).
piece_is_type([_Id, _PieceCoord, _PieceColor, PieceType, _MoveCount, _Alive, _LastMove], PieceType).
piece_has_move_count([_Id, _PieceCoord, _PieceColor, _PieceType, MoveCount, _Alive, _LastMove], MoveCount).
piece_has_id([Id | _], Id).
piece_is_alive([_Id, _PieceCoord, _PieceColor, _PieceType, _MoveCount, true, _LastMove]).
piece_is_dead([_Id, _PieceCoord, _PieceColor, _PieceType, _MoveCount, false, _LastMove]).
piece_is_at_coord([_Id, PieceCoord, _PieceColor, _PieceType, _MoveCount, _Alive, _LastMove], PieceCoord).
pieces_have_same_color(Piece1, Piece2) :-
  piece_has_color(Piece1, Color),
  piece_has_color(Piece2, Color).
pieces_have_different_colors(Piece1, Piece2) :-
  piece_has_color(Piece1, Color1),
  piece_has_color(Piece2, Color2),
  Color1 \= Color2.
piece_is_white_king(Piece) :-
  piece_is_white(Piece),
  piece_is_type(Piece, king).
piece_is_black_king(Piece) :-
  piece_is_black(Piece),
  piece_is_type(Piece, king).
piece_has_moved_at_turn([_Id, _Coord, _Color, _Type, _MoveCount, _Alive, [_Offset, MovedAtTurn]], MovedAtTurn).
piece_last_move_offset([_Id, _Coord, _Color, _Type, _MoveCount, _Alive, [Offset, _MovedAtTurn]], Offset).

col_length([_, TileCount, _], ColLength) :-
  ColLength #> 0,
  ColLength * ColLength #= TileCount.
row_length([_, TileCount, _], RowLength) :-
  RowLength #> 0,
  RowLength * RowLength #= TileCount.
board_top([_, TileCount, _], BoardTop) :-
  X #> 0,
  X * X #= TileCount,
  BoardTop #= X - 1.
board_edge([_, TileCount, _], BoardEdge) :-
  X #> 0,
  X * X #= TileCount,
  BoardEdge #= X - 1.
is_turn([_Pieces, _TileCount, TurnNumber], TurnNumber).
coord_on_board(Coord, GameState) :-
  [X, Y] = Coord,
  board_edge(GameState, BoardEdge),
  board_top(GameState, BoardTop),
  X in 0..BoardEdge,
  Y in 0..BoardTop.
is_piece_inbetween(From, To, GameState) :-
  subtract_coord(To, From, Offset),
  is_straight_or_diagonal_direction(Offset),
  sign_coord(Offset, Step),
  is_piece_inbetween_(From, To, Step, GameState).
is_piece_inbetween_(From, To, Step, GameState) :-
  add_coord(From, Step, Next),
  not_equal_coords(Next, To),
  piece_at(Next, _SomePiece, GameState).
is_piece_inbetween_(From, To, Step, GameState) :-
  add_coord(From, Step, Next),
  not_equal_coords(Next, To),
  is_piece_inbetween_(Next, To, Step, GameState).
is_occupied_by_self(Piece, Offset, GameState) :-
  piece_is_at_coord(Piece, Coord),
  add_coord(Coord, Offset, TargetCoord),
  piece_at(TargetCoord, TargetPiece, GameState),
  piece_has_color(Piece, OwnColor),
  piece_has_color(TargetPiece, OwnColor).
is_occupied_by_enemy(Piece, Offset, GameState) :-
  piece_is_at_coord(Piece, Coord),
  add_coord(Coord, Offset, TargetCoord),
  piece_at(TargetCoord, TargetPiece, GameState),
  piece_has_color(Piece, OwnColor),
  piece_has_color(TargetPiece, TargetColor),
  OwnColor \= TargetColor.
is_occupied(Piece, Offset, GameState) :-
  piece_is_at_coord(Piece, Coord),
  add_coord(Coord, Offset, TargetCoord),
  piece_at(TargetCoord, _TargetPiece, GameState).
piece_at_target_tile(Piece, Offset, TargetPiece, GameState) :-
  piece_is_at_coord(Piece, Coord),
  add_coord(Coord, Offset, TargetCoord),
  piece_at(TargetCoord, TargetPiece, GameState).
is_empty(Piece, Offset, GameState) :-
  piece_is_at_coord(Piece, Coord),
  add_coord(Coord, Offset, TargetCoord),
  is_coord_empty(TargetCoord, GameState).
is_coord_occupied(Coord, GameState) :-
  piece_at(Coord, _Piece, GameState).
is_coord_occupied_by_enemy(Piece, Coord, GameState) :-
  piece_at(Coord, TargetPiece, GameState),
  pieces_have_different_colors(Piece, TargetPiece).
is_coord_occupied_by_self(Piece, Coord, GameState) :-
  piece_at(Coord, TargetPiece, GameState),
  pieces_have_same_color(Piece, TargetPiece).
is_coord_empty(Coord, GameState) :-
  [Pieces | _] = GameState,
  not(member([_ | [Coord | _]], Pieces)).

query(Template, Goal, Bag) :-
  setof(Template, Goal, Bag);
  Bag = [].


% CONVERSIONS

convert_game_state([Pieces | [TileCount | [Turn | _]]], GameStateOut) :-
  convert_pieces(Pieces, PiecesOut),
  TileCount #= N * N,
  GameStateOut = [PiecesOut, TileCount, Turn].

convert_pieces([], PiecesOut) :- PiecesOut = [].
convert_pieces([Piece | Pieces], PiecesOut) :-
  convert_piece(Piece, PieceOut),
  convert_pieces(Pieces, RemainingPiecesOut),
  PiecesOut = [PieceOut | RemainingPiecesOut].

convert_piece([Id, Pos, Color, PieceType, MoveCount, Alive, LastMove], PieceOut) :-
  convert_color(Color, ColorOut),
  convert_piece_type(PieceType, PieceTypeOut),
  PieceOut = [Id, Pos, ColorOut, PieceTypeOut, MoveCount, Alive, LastMove].

convert_color(ColorString, ColorAtom) :- atom_string(ColorAtom, ColorString).
convert_piece_type(PieceTypeString, PieceTypeAtom) :- atom_string(PieceTypeAtom, PieceTypeString).

convert_command(CommandName, Payload, CommandOut) :-
  atom_string(CommandNameOut, CommandName),
  atom_json_dict(Payload, PayloadOut, []),
  CommandOut = [CommandNameOut, PayloadOut].
convert_command(CommandIn, CommandOut) :- atom_string(CommandOut, CommandIn).


% PARSING

parse_args([Path, GameState, Command], ConvertedGameState, ConvertedCommand) :-
  parse_args([Path, GameState, Command, []], ConvertedGameState, ConvertedCommand).
parse_args([_, GameState, Command, Payload], ConvertedGameState, ConvertedCommand) :-
  atom_json_dict(GameState, ParsedGameState, []),
  convert_game_state(ParsedGameState, ConvertedGameState),
  convert_command(Command, Payload, ConvertedCommand).
parse_args([_, GameState, _Command, _Payload], GameState, ConvertedCommand) :-
  ConvertedCommand = no_matches.
parse_args(Args, ParsedGameState, ParsedCommand) :-
  write(current_output, Args),
  ParsedGameState = white,
  ParsedCommand = [error, invalid_input].

% ENTRANCE

main :- current_prolog_flag(argv, Input),
  parse_args(Input, GameState, Command),
  run_command(Command, GameState, Output),
  write(current_output, Output).
