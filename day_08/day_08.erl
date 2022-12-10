-module(day_08).
-export([day_08_part_1/1, day_08_part_2/1]).

lines_to_map([Line | Rest], RowIndex, Map) ->
  {NewMap, _} = lists:foldl(
    fun (Char, {MapAccum, ColIndex}) ->
      {
        MapAccum#{
          {RowIndex, ColIndex} => Char - $0
        },
        ColIndex + 1
      }
    end,
    {Map, 0},
    binary_to_list(Line)
  ),
  lines_to_map(Rest, RowIndex + 1, NewMap);
lines_to_map([], _, Map) ->
  Map.

is_visible(Pos, Map) ->
  not is_hidden(Pos, Map).

is_hidden(Pos, Map) ->
  #{ Pos := Height } = Map,
  are_any_same_or_taller(Height, Pos, {  0,  1 }, Map) andalso
  are_any_same_or_taller(Height, Pos, {  0, -1 }, Map) andalso
  are_any_same_or_taller(Height, Pos, {  1,  0 }, Map) andalso
  are_any_same_or_taller(Height, Pos, { -1,  0 }, Map).

are_any_same_or_taller(Height, {Row, Col}, {RowIncr, ColIncr} = Incr, Map) ->
  NextPos = {Row + RowIncr, Col + ColIncr},
  NextTreeExists = maps:is_key(NextPos, Map),
  case NextTreeExists of
    false -> false;
    true ->
      #{NextPos := NextTreeHeight} = Map,
      if
        NextTreeHeight >= Height -> true;
        NextTreeHeight < Height ->
          are_any_same_or_taller(Height, NextPos, Incr, Map)
      end
  end.

day_08_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  Map = lines_to_map(Lines, 0, #{}),
  maps:size(maps:filter(
    fun (Pos, _) ->
      is_visible(Pos, Map)
    end,
    Map
  )).

scenic_score(Pos, Map) ->
  #{ Pos := Height } = Map,
  viewing_distance(Height, Pos, {  0,  1 }, Map) *
  viewing_distance(Height, Pos, {  0, -1 }, Map) *
  viewing_distance(Height, Pos, {  1,  0 }, Map) *
  viewing_distance(Height, Pos, { -1,  0 }, Map).

viewing_distance(Height, {Row, Col}, {RowIncr, ColIncr} = Incr, Map) ->
  NextPos = {Row + RowIncr, Col + ColIncr},
  NextTreeExists = maps:is_key(NextPos, Map),
  case NextTreeExists of
    false -> 0;
    true ->
      #{NextPos := NextTreeHeight} = Map,
      if
        NextTreeHeight >= Height -> 1;
        NextTreeHeight < Height ->
          1 + viewing_distance(Height, NextPos, Incr, Map)
      end
  end.

day_08_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  Map = lines_to_map(Lines, 0, #{}),
  maps:fold(
    fun (Pos, _, HighestScoreSoFar) ->
      Score = scenic_score(Pos, Map),
      if
        Score > HighestScoreSoFar -> Score;
        true -> HighestScoreSoFar
      end
    end,
    0,
    Map
  ).
