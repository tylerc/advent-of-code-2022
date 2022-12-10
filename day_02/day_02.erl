-module(day_02).
-export([day_02_part_1/1, day_02_part_2/1]).

points_for_move(rock) -> 1;
points_for_move(paper) -> 2;
points_for_move(scissors) -> 3.

points_for_outcome(lost) -> 0;
points_for_outcome(draw) -> 3;
points_for_outcome(win) -> 6.

evaluate_round(A, A) -> draw;
evaluate_round(scissors, rock) -> win;
evaluate_round(paper, rock) -> lost;
evaluate_round(scissors, paper) -> lost;
evaluate_round(rock, paper) -> win;
evaluate_round(rock, scissors) -> lost;
evaluate_round(paper, scissors) -> win.

move_to_atom(<<"A">>) -> rock;
move_to_atom(<<"B">>) -> paper;
move_to_atom(<<"C">>) -> scissors;
move_to_atom(<<"X">>) -> rock;
move_to_atom(<<"Y">>) -> paper;
move_to_atom(<<"Z">>) -> scissors.

move_to_atoms(Str) ->
  [OpponentMove, YourMove] = string:split(Str, " ", all),
  {move_to_atom(OpponentMove), move_to_atom(YourMove)}.

day_02_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  MovesRaw = string:lexemes(Text, "\n"),
  MovesAsAtoms = lists:map(fun move_to_atoms/1, MovesRaw),
  lists:foldl(
    fun ({OpponentMove, YourMove}, Accum) ->
      Outcome = evaluate_round(OpponentMove, YourMove),
      Accum + points_for_move(YourMove) + points_for_outcome(Outcome)
    end,
    0,
    MovesAsAtoms
  ).

opponent_move_and_outcome_to_move(A, draw) -> A;
opponent_move_and_outcome_to_move(rock, win) -> paper;
opponent_move_and_outcome_to_move(rock, lost) -> scissors;
opponent_move_and_outcome_to_move(paper, win) -> scissors;
opponent_move_and_outcome_to_move(paper, lost) -> rock;
opponent_move_and_outcome_to_move(scissors, win) -> rock;
opponent_move_and_outcome_to_move(scissors, lost) -> paper.

move_to_atom2(<<"A">>) -> rock;
move_to_atom2(<<"B">>) -> paper;
move_to_atom2(<<"C">>) -> scissors;
move_to_atom2(<<"X">>) -> lost;
move_to_atom2(<<"Y">>) -> draw;
move_to_atom2(<<"Z">>) -> win.

move_to_atoms2(Str) ->
  [OpponentMove, YourMove] = string:split(Str, " ", all),
  {move_to_atom2(OpponentMove), move_to_atom2(YourMove)}.

day_02_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  MovesRaw = string:lexemes(Text, "\n"),
  MovesAsAtoms = lists:map(fun move_to_atoms2/1, MovesRaw),
  lists:foldl(
    fun ({OpponentMove, Outcome}, Accum) ->
      YourMove = opponent_move_and_outcome_to_move(OpponentMove, Outcome),
      Accum + points_for_move(YourMove) + points_for_outcome(Outcome)
    end,
    0,
    MovesAsAtoms
  ).
