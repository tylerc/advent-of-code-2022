-module(day_14).
-export([day_14_part_1/1, day_14_part_2/1]).

line_parse(LineStr) ->
  CoordinateStrs = string:lexemes(LineStr, " -> "),
  lists:map(
    fun (CoordStr) ->
      [ColStr, RowStr] = string:lexemes(CoordStr, ","),
      {binary_to_integer(RowStr), binary_to_integer(ColStr)}
    end,
    CoordinateStrs
  ).

fill_rocks({Row, Col} = CurrentPoint, EndPoint, DiffRow, DiffCol, #{} = Map) ->
  NextMap = Map#{ CurrentPoint => rock },
  case CurrentPoint == EndPoint of
    true -> NextMap;
    false ->
      NextPoint = { Row + DiffRow, Col + DiffCol },
      fill_rocks(NextPoint, EndPoint, DiffRow, DiffCol, NextMap)
  end.

fill_rocks([{RowA, ColA} = PointA, {RowB, ColB} = PointB | OtherPoints], #{} = Map) ->
  DiffRow = if
              RowA > RowB -> -1;
              RowA == RowB -> 0;
              RowA < RowB -> 1
            end,
  DiffCol = if
              ColA > ColB -> -1;
              ColA == ColB -> 0;
              ColA < ColB -> 1
            end,
  NextMap = fill_rocks(PointA, PointB, DiffRow, DiffCol, Map),
  fill_rocks([PointB | OtherPoints], NextMap);
fill_rocks([_ | _], #{} = Map) ->
  Map.

sand_place(#{} = Map) ->
  NextMap = sand_place({ 0, 500 }, Map),
  case NextMap == Map of
    true -> NextMap;
    false -> sand_place(NextMap)
  end.

sand_place(StartPos, #{} = Map) ->
  FarthestRow = maps:fold(
    fun ({Row, _}, _, Accum) ->
      case Row > Accum of
        true -> Row;
        false -> Accum
      end
    end,
    0,
    Map
  ),
  sand_place(StartPos, FarthestRow, Map).

sand_place({Row, _}, FarthestRow, #{} = Map) when Row > FarthestRow ->
  Map;
sand_place({Row, Col} = Pos, FarthestRow, #{} = Map) ->
  OneBelow = { Row + 1, Col },
  DiagonalLeft = { Row + 1, Col - 1 },
  DiagonalRight = { Row + 1, Col + 1 },
  SpacesAvailable = lists:filter(
    fun (NextPos) ->
      case Map of
        #{ NextPos := _ } -> false;
        #{} -> true
      end
    end,
    [OneBelow, DiagonalLeft, DiagonalRight]
  ),
  case SpacesAvailable of
    [] -> Map#{ Pos => sand };
    [Space | _] -> sand_place(Space, FarthestRow, Map)
  end.

day_14_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  GroupsOfLines = lists:map(fun line_parse/1, string:lexemes(Text, "\n")),
  MapInitial = lists:foldl(
    fun (Lines, Map) ->
      fill_rocks(Lines, Map)
    end,
    #{},
    GroupsOfLines
  ),
  MapWithSand = sand_place(MapInitial),
  maps:fold(
    fun (_, Value, Accum) ->
      case Value of
        sand -> Accum + 1;
        _ -> Accum
      end
    end,
    0,
    MapWithSand
  ).

sand_place2(#{} = Map) ->
  FarthestRow = maps:fold(
    fun ({Row, _}, Value, Accum) ->
      case Value == rock andalso Row > Accum of
        true -> Row;
        false -> Accum
      end
    end,
    0,
    Map
  ),
  FloorRow = FarthestRow + 2,
  sand_place2(FloorRow, Map).

sand_place2(FloorRow, #{} = Map) ->
  NextMap = sand_place2({ 0, 500 }, FloorRow, Map),
  case NextMap of
    #{ { 0, 500 } := sand } -> NextMap;
    #{} -> sand_place2(FloorRow, NextMap)
  end.

sand_place2({Row, Col} = Pos, FloorRow, #{} = Map) ->
  OneBelow = { Row + 1, Col },
  DiagonalLeft = { Row + 1, Col - 1 },
  DiagonalRight = { Row + 1, Col + 1 },
  SpacesAvailable = lists:filter(
    fun ({NextRow, _} = NextPos) ->
      case NextRow >= FloorRow of
        true -> false;
        false ->
          case Map of
            #{ NextPos := _ } -> false;
            #{} -> true
          end
      end
    end,
    [OneBelow, DiagonalLeft, DiagonalRight]
  ),
  case SpacesAvailable of
    [] -> Map#{ Pos => sand };
    [Space | _] -> sand_place2(Space, FloorRow, Map)
  end.

day_14_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  GroupsOfLines = lists:map(fun line_parse/1, string:lexemes(Text, "\n")),
  MapInitial = lists:foldl(
    fun (Lines, Map) ->
      fill_rocks(Lines, Map)
    end,
    #{},
    GroupsOfLines
  ),
  MapWithSand = sand_place2(MapInitial),
  maps:fold(
    fun (_, Value, Accum) ->
      case Value of
        sand -> Accum + 1;
        _ -> Accum
      end
    end,
    0,
    MapWithSand
  ).
