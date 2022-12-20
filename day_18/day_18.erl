-module(day_18).
-export([day_18_part_1/1, day_18_part_2/1]).

cubes_parse_and_populate([], #{} = Map) ->
  Map;
cubes_parse_and_populate([Line | Lines], #{} = Map) ->
  [XStr, YStr, ZStr] = string:split(Line, ",", all),
  Coord = {
    binary_to_integer(XStr),
    binary_to_integer(YStr),
    binary_to_integer(ZStr)
  },
  MapNext = Map#{ Coord => cube },
  cubes_parse_and_populate(Lines, MapNext).

connectedness_to_value(Coord, Cubes) ->
  case Cubes of
    #{ Coord := _ } -> 0;
    #{} -> 1
  end.

count_unconnected_sides(Cubes) ->
  maps:fold(
    fun ({X, Y, Z}, _, Accum) ->
      Accum +
        connectedness_to_value({X + 1, Y, Z}, Cubes) +
        connectedness_to_value({X - 1, Y, Z}, Cubes) +
        connectedness_to_value({X, Y + 1, Z}, Cubes) +
        connectedness_to_value({X, Y - 1, Z}, Cubes) +
        connectedness_to_value({X, Y, Z + 1}, Cubes) +
        connectedness_to_value({X, Y, Z - 1}, Cubes)
    end,
    0,
    Cubes
  ).

day_18_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  Cubes = cubes_parse_and_populate(Lines, #{}),
  count_unconnected_sides(Cubes).

cubes_mark_empties(Cubes) ->
  maps:fold(
    fun ({X, Y, Z}, _, CubesUpdating) ->
      lists:foldl(
        fun (Coord, CubesUpdating2) ->
          case CubesUpdating2 of
            #{ Coord := _ } -> CubesUpdating2;
            #{} -> CubesUpdating2#{ Coord => empty }
          end
        end,
        CubesUpdating,
        [
          {X + 1, Y, Z},
          {X - 1, Y, Z},
          {X, Y + 1, Z},
          {X, Y - 1, Z},
          {X, Y, Z + 1},
          {X, Y, Z - 1}
        ]
      )
    end,
    Cubes,
    Cubes
  ).

cubes_bounds(Cubes) ->
  maps:fold(
    fun ({X, Y, Z}, _, {{MinX, MaxX}, {MinY, MaxY}, {MinZ, MaxZ}}) ->
      {
        {min(X, MinX), max(X, MaxX)},
        {min(Y, MinY), max(Y, MaxY)},
        {min(Z, MinZ), max(Z, MaxZ)}
      }
    end,
    {{0, 0}, {0, 0}, {0, 0}},
    Cubes
  ).

cubes_fill_container(Cubes) ->
  {{MinX, MaxX}, {MinY, MaxY}, {MinZ, MaxZ}} = cubes_bounds(Cubes),
  % Top
  Cubes2 = cubes_fill_container(
    lists:seq(MinX - 2, MaxX + 2),
    lists:seq(MinY - 2, MaxY + 2),
    fun(X, Y) -> {X, Y, MaxZ} end,
    Cubes
  ),
  % Bottom
  Cubes3 = cubes_fill_container(
    lists:seq(MinX - 2, MaxX + 2),
    lists:seq(MinY - 2, MaxY + 2),
    fun(X, Y) -> {X, Y, MinZ} end,
    Cubes2
  ),
  % Front
  Cubes4 = cubes_fill_container(
    lists:seq(MinX - 2, MaxX + 2),
    lists:seq(MinZ - 2, MaxZ + 2),
    fun(X, Z) -> {X, MaxY, Z} end,
    Cubes3
  ),
  % Back
  Cubes5 = cubes_fill_container(
    lists:seq(MinX - 2, MaxX + 2),
    lists:seq(MinZ - 2, MaxZ + 2),
    fun(X, Z) -> {X, MinY, Z} end,
    Cubes4
  ),
  % Left
  Cubes6 = cubes_fill_container(
    lists:seq(MinY - 2, MaxY + 2),
    lists:seq(MinZ - 2, MaxZ + 2),
    fun(Y, Z) -> {MinX, Y, Z} end,
    Cubes5
  ),
  % Right
  cubes_fill_container(
    lists:seq(MinY - 2, MaxY + 2),
    lists:seq(MinZ - 2, MaxZ + 2),
    fun(Y, Z) -> {MaxX, Y, Z} end,
    Cubes6
  ).

% Now I believe if you mark empties and fill the container, container cubes could become "contagious" and transform
% all touching empties (whether marked as such or not) into containers... When the process is done any remaining
% empties are what we need to subtract out.

flood_fill(Cubes) ->
  CubesWithEmpties = cubes_mark_empties(Cubes),
  CubesWithEmptiesAndContainer = cubes_fill_container(CubesWithEmpties),
  Bounds = cubes_bounds(CubesWithEmptiesAndContainer),
  ContainerCoordsInitial = maps:fold(
    fun (Key, Value, Accum) ->
      case Value of
        container -> [Key | Accum];
        _ -> Accum
      end
    end,
    [],
    CubesWithEmptiesAndContainer
  ),
  flood_fill(Bounds, ContainerCoordsInitial, CubesWithEmptiesAndContainer).

flood_fill(_, [], Cubes) ->
  Cubes;
flood_fill(
    {{MinX, MaxX}, {MinY, MaxY}, {MinZ, MaxZ}} = Bounds,
    [{X, Y, Z} | ContainerCoordsRemaining],
    Cubes
) ->
  {NewContainerCoords, CubesNext} = lists:foldl(
    fun ({ThisX, ThisY, ThisZ} = Coord, {CoordsAccum, CubesAccum} = Accum) ->
      IsEmpty = case CubesAccum of
        #{ Coord := cube } -> false;
        #{ Coord := container } -> false;
        #{ Coord := empty } -> true;
        #{} -> true
      end,
      CoordIsInBounds =
        ThisX >= MinX andalso
        ThisX =< MaxX andalso
        ThisY >= MinY andalso
        ThisY =< MaxY andalso
        ThisZ >= MinZ andalso
        ThisZ =< MaxZ,
      case IsEmpty andalso CoordIsInBounds of
        true -> {[Coord | CoordsAccum], CubesAccum#{ Coord => container }};
        false -> Accum
      end
    end,
    {[], Cubes},
    [
      {X + 1, Y, Z},
      {X - 1, Y, Z},
      {X, Y + 1, Z},
      {X, Y - 1, Z},
      {X, Y, Z + 1},
      {X, Y, Z - 1}
    ]
  ),
  flood_fill(Bounds, NewContainerCoords ++ ContainerCoordsRemaining, CubesNext).

cubes_fill_container(ListA, ListB, TransformFun, Cubes) ->
  lists:foldl(
    fun (A, CubesA) ->
      lists:foldl(
        fun (B, CubesB) ->
          Coord = TransformFun(A, B),
          CubesB#{ Coord => container }
        end,
        CubesA,
        ListB
      )
    end,
    Cubes,
    ListA
  ).

connectedness_to_value2(Coord, Cubes) ->
  case Cubes of
    #{ Coord := container } -> 1;
    #{} -> 0
  end.

count_sides_connected_to_container(Cubes) ->
  maps:fold(
    fun ({X, Y, Z}, Value, Accum) ->
      case Value of
        cube ->
          Accum +
            connectedness_to_value2({X + 1, Y, Z}, Cubes) +
            connectedness_to_value2({X - 1, Y, Z}, Cubes) +
            connectedness_to_value2({X, Y + 1, Z}, Cubes) +
            connectedness_to_value2({X, Y - 1, Z}, Cubes) +
            connectedness_to_value2({X, Y, Z + 1}, Cubes) +
            connectedness_to_value2({X, Y, Z - 1}, Cubes);
        _ -> Accum
      end
    end,
    0,
    Cubes
  ).

day_18_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  Cubes = cubes_parse_and_populate(Lines, #{}),
  FloodFilled = flood_fill(Cubes),
  count_sides_connected_to_container(FloodFilled).
