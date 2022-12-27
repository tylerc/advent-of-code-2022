-module(day_24).
-export([day_24_part_1/1, day_24_part_2/1]).

map_parse(Str) ->
  map_parse(1, 1, Str, #{}).

map_parse(_, _, <<>>, Map) ->
  Map;
map_parse(Row, Col, <<H:1/binary, Rest/binary>>, #{} = Map) ->
  case H of
    <<"#">> -> map_parse(Row, Col + 1, Rest, Map#{ {Row, Col} => wall });
    <<"^">> -> map_parse(Row, Col + 1, Rest, Map#{ {Row, Col} => [up] });
    <<"v">> -> map_parse(Row, Col + 1, Rest, Map#{ {Row, Col} => [down] });
    <<"<">> -> map_parse(Row, Col + 1, Rest, Map#{ {Row, Col} => [left] });
    <<">">> -> map_parse(Row, Col + 1, Rest, Map#{ {Row, Col} => [right] });
    <<".">> -> map_parse(Row, Col + 1, Rest, Map);
    <<"\n">> -> map_parse(Row + 1, 1, Rest, Map)
  end.

pos_next(up, {Row, Col}) -> {Row - 1, Col};
pos_next(down, {Row, Col}) -> {Row + 1, Col};
pos_next(left, {Row, Col}) -> {Row, Col - 1};
pos_next(right, {Row, Col}) -> {Row, Col + 1}.

map_bounds(Map) ->
  maps:fold(
    fun ({Row, Col}, Value, {RowMin, RowMax, ColMin, ColMax} = Accum) ->
      case Value of
        wall -> {min(Row, RowMin), max(Row, RowMax), min(Col, ColMin), max(Col, ColMax)};
        _ -> Accum
      end
    end,
    {10000, -10000, 10000, -10000},
    Map
  ).

opposite_walls_build(Map) ->
  {RowMin, RowMax, ColMin, ColMax} = map_bounds(Map),
  maps:fold(
    fun (Pos, Value, Accum) ->
      case Value of
        wall ->
          PosOpposite = case Pos of
                          {RowMin, Col} -> {RowMax, Col};
                          {RowMax, Col} -> {RowMin, Col};
                          {Row, ColMin} -> {Row, ColMax};
                          {Row, ColMax} -> {Row, ColMin}
                        end,
          Accum#{ Pos => PosOpposite };
        _ -> Accum
      end
    end,
    #{},
    Map
  ).

pos_next_with_wrap(Direction, Pos, OppositeWalls, Map) ->
  PosNextMaybe = pos_next(Direction, Pos),
  case Map of
    #{ PosNextMaybe := wall } -> pos_next(Direction, maps:get(PosNextMaybe, OppositeWalls));
    _ -> PosNextMaybe
  end.

map_append(Pos, Direction, Map) ->
  case Map of
    #{ Pos := wall } -> error(unexpected_wall);
    #{ Pos := List } -> Map#{ Pos => [Direction | List] };
    _ -> Map#{ Pos => [Direction] }
  end.

map_cache_get(Minute, OppositeWalls, MapCache) ->
  MinutePrev = Minute - 1,
  case MapCache of
    #{ Minute := Map } -> {Map, MapCache};
    #{ MinutePrev := Map } ->
      MapGenerated = map_next(OppositeWalls, Map),
      {MapGenerated, MapCache#{ Minute => MapGenerated }}
  end.

map_next(OppositeWalls, Map) ->
  maps:fold(
    fun (Pos, Value, MapBuilding) ->
      case Value of
        wall -> MapBuilding#{ Pos => wall };
        Blizzards ->
          lists:foldl(
            fun (Direction, MapBuilding2) ->
              PosNext = pos_next_with_wrap(Direction, Pos, OppositeWalls, Map),
              map_append(PosNext, Direction, MapBuilding2)
            end,
            MapBuilding,
            Blizzards
          )
      end
    end,
    #{},
    Map
  ).

goal_pos(Map) ->
  {_, RowMax, _, ColMax} = map_bounds(Map),
  {RowMax, ColMax - 1}.

best_possible_score(Minutes, {Row, Col}, {GoalRow, GoalCol}) ->
  Minutes + abs(Row - GoalRow) + abs(Col - GoalCol).

moves_available(Minute, {Row, Col}, {ExitRow, _}, Map) ->
  lists:filtermap(
    fun ({RowNext, _} = Pos) ->
      case Map of
        #{ Pos := _ } -> false;
        _ -> case RowNext >= 1 andalso Row =< ExitRow of
               false -> false;
               true -> {true, {Minute, Pos}}
             end
      end
    end,
    [
      {Row + 1, Col},
      {Row, Col + 1},
      {Row, Col},
      {Row - 1, Col},
      {Row, Col - 1}
    ]
  ).

fastest(Map) ->
  GoalPos = goal_pos(Map),
  fastest(900, [{1, {1, 2}}], #{}, GoalPos, GoalPos, opposite_walls_build(Map), period(Map), #{ 0 => Map }).

fastest(HighScore, [], _, _, _, _, _, _) ->
  HighScore;
fastest(HighScore, [{Minute, Pos} | Remaining], StateCache, GoalPos, ExitPos, OppositeWalls, Period, MapCache) ->
  case Pos == GoalPos of
    true ->
      Score = Minute - 1,
      fastest(min(HighScore, Score), Remaining, StateCache, GoalPos, ExitPos, OppositeWalls, Period, MapCache);
    false ->
      BestScore = best_possible_score(Minute, Pos, GoalPos),
      MinMinuteThroughPos = maps:get({Minute rem Period, Pos}, StateCache, 10000),
      case BestScore >= HighScore orelse MinMinuteThroughPos =< Minute of
        true -> fastest(HighScore, Remaining, StateCache, GoalPos, ExitPos, OppositeWalls, Period, MapCache);
        false ->
          {Map, MapCacheNext} = map_cache_get(Minute rem Period, OppositeWalls, MapCache),
          MovesNext = moves_available(Minute + 1, Pos, ExitPos, Map) ++ Remaining,
          fastest(HighScore, MovesNext, StateCache#{ {Minute rem Period, Pos} => Minute }, GoalPos, ExitPos, OppositeWalls, Period, MapCacheNext)
      end
  end.

period(Map) ->
  period(1, opposite_walls_build(Map), Map, #{ Map => seen }).

period(N, OppositeWalls, Map, MapCache) ->
  MapNext = map_next(OppositeWalls, Map),
  case MapCache of
    #{ MapNext := seen } -> N;
    _ ->
      MapCacheNext = MapCache#{ MapNext => seen },
      period(N + 1, OppositeWalls, MapNext, MapCacheNext)
  end.

day_24_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  fastest(map_parse(Text)).

fastest2(Map) ->
  OppositeWalls = opposite_walls_build(Map),
  Period = period(Map),
  MapCache = lists:foldl(
    fun (N, MapCacheBuilding) ->
      {_, MapCacheNext} = map_cache_get(N, OppositeWalls, MapCacheBuilding),
      MapCacheNext
    end,
    #{ 0 => Map },
    lists:seq(1, Period)
  ),
  Entrance = {1, 2},
  Exit = goal_pos(Map),
  TripA = fastest(400, [{1, Entrance}], #{}, Exit, Exit, OppositeWalls, Period, MapCache),
  TripB = fastest(400 + TripA, [{TripA, Exit}], #{}, Entrance, Exit, OppositeWalls, Period, MapCache),
  TripC = fastest(400 + TripB, [{TripB, Entrance}], #{}, Exit, Exit, OppositeWalls, Period, MapCache),
  TripC.

day_24_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  fastest2(map_parse(Text)).
