-module(day_22).
-export([day_22_part_1/1, day_22_part_2/1]).

map_parse(<<Str/binary>>) ->
  map_parse(1, 1, #{}, Str).

map_parse(_, _, Map, <<>>) ->
  Map;
map_parse(Row, Col, Map, <<H:1/binary, Rest/binary>>) ->
  case H of
    <<" ">> -> map_parse(Row, Col + 1, Map, Rest);
    <<".">> -> map_parse(Row, Col + 1, Map#{ {Row, Col} => open }, Rest);
    <<"#">> -> map_parse(Row, Col + 1, Map#{ {Row, Col} => wall }, Rest);
    <<"\n">> -> map_parse(Row + 1, 1, Map, Rest)
  end.

path_parse(Str) ->
  path_parse(<<>>, Str).

path_parse(DigitBuilding, <<>>) ->
  [binary_to_integer(DigitBuilding) | []];
path_parse(DigitBuilding, <<H:1/binary, Rest/binary>>) ->
  case H of
    <<"L">> -> [ binary_to_integer(DigitBuilding), left | path_parse(<<>>, Rest) ];
    <<"R">> -> [ binary_to_integer(DigitBuilding), right | path_parse(<<>>, Rest) ];
    _ -> path_parse(<<DigitBuilding/binary, H/binary>>, Rest)
  end.

rotate(right, left) -> up;
rotate(up, left) -> left;
rotate(left, left) -> down;
rotate(down, left) -> right;
rotate(right, right) -> down;
rotate(down, right) -> left;
rotate(left, right) -> up;
rotate(up, right) -> right.

move_diff(right) -> {0, 1};
move_diff(down) -> {1, 0};
move_diff(left) -> {0, -1};
move_diff(up) -> {-1, 0}.

facing_score(right) -> 0;
facing_score(down) -> 1;
facing_score(left) -> 2;
facing_score(up) -> 3.

select_pos(Filter, Sort, Map) ->
  hd(lists:sort(Sort, lists:filter(Filter, maps:keys(Map)))).

wrap(right, {Row, _}, Map) ->
  select_pos(
    fun ({OtherRow, _}) -> Row == OtherRow end,
    fun ({_, ColA}, {_, ColB}) -> ColA =< ColB end,
    Map
  );
wrap(left, {Row, _}, Map) ->
  select_pos(
    fun ({OtherRow, _}) -> Row == OtherRow end,
    fun ({_, ColA}, {_, ColB}) -> ColA > ColB end,
    Map
  );
wrap(up, {_, Col}, Map) ->
  select_pos(
    fun ({_, OtherCol}) -> Col == OtherCol end,
    fun ({RowA, _}, {RowB, _}) -> RowA > RowB end,
    Map
  );
wrap(down, {_, Col}, Map) ->
  select_pos(
    fun ({_, OtherCol}) -> Col == OtherCol end,
    fun ({RowA, _}, {RowB, _}) -> RowA =< RowB end,
    Map
  ).

move(0, _, Pos, _) ->
  Pos;
move(N, Direction, Pos, Map) ->
  move(N - 1, Direction, move(Direction, Pos, Map), Map).

move(Direction, {Row, Col} = Pos, Map) ->
  {RowDiff, ColDiff} = move_diff(Direction),
  PosMaybe = {Row + RowDiff, Col + ColDiff},
  case Map of
    #{ PosMaybe := wall } -> Pos;
    #{ PosMaybe := open } -> PosMaybe;
    #{} ->
      PosWrapped = wrap(Direction, Pos, Map),
      case Map of
        #{ PosWrapped := wall } -> Pos;
        #{ PosWrapped := open } -> PosWrapped
      end
  end.

starting_pos(Map) ->
  maps:fold(
    fun ({Row, Col}, Value, {1, StartCol} = Accum) ->
      case Row == 1 andalso Col < StartCol andalso Value == open of
        true -> {1, Col};
        false -> Accum
      end
    end,
    {1, 10000000},
    Map
  ).

follow_path(Facing, Pos, _, []) ->
  {Facing, Pos};
follow_path(Facing, Pos, Map, [Instr | Path]) ->
  case Instr of
    left -> follow_path(rotate(Facing, left), Pos, Map, Path);
    right -> follow_path(rotate(Facing, right), Pos, Map, Path);
    _ -> follow_path(Facing, move(Instr, Facing, Pos, Map), Map, Path)
  end.

day_22_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  [MapStr, PathStr] = string:split(Text, "\n\n"),
  Map = map_parse(MapStr),
  Path = path_parse(string:trim(PathStr)),
  {Facing, {Row, Col}} = follow_path(right, starting_pos(Map), Map, Path),
  (1000 * Row) + (4 * Col) + facing_score(Facing).

cube_side_size(Map) ->
  floor(math:sqrt(maps:size(Map) div 6)).

pos_to_division({Row, Col}, SideSize) ->
  {(Row - 1) div SideSize, (Col - 1) div SideSize}.

% Given your initial Facing and Division, return your new Facing (with index transformation) and Division.
cube_division_wrap(4, up,    {1, 1}) -> {right, normal, {0, 2}};
cube_division_wrap(4, right, {1, 2}) -> {down, reverse, {2, 3}};
cube_division_wrap(4, down,  {2, 2}) -> {up, reverse, {1, 0}};

cube_division_wrap(50, left, {0, 1}) -> {right, reverse, {2, 0}};
cube_division_wrap(50, up, {0, 1}) -> {right, normal, {3, 0}};
cube_division_wrap(50, down, {0, 2}) -> {left, normal, {1, 1}};
cube_division_wrap(50, right, {0, 2}) -> {left, reverse, {2, 1}};
cube_division_wrap(50, up, {0, 2}) -> {up, normal, {3, 0}};
cube_division_wrap(50, right, {1, 1}) -> {up, normal, {0, 2}};
cube_division_wrap(50, left, {1, 1}) -> {down, normal, {2, 0}};
cube_division_wrap(50, up, {2, 0}) -> {right, normal, {1, 1}};
cube_division_wrap(50, left, {2, 0}) -> {right, reverse, {0, 1}};
cube_division_wrap(50, down, {2, 1}) -> {left, normal, {3, 0}};
cube_division_wrap(50, right, {2, 1}) -> {left, reverse, {0, 2}};
cube_division_wrap(50, left, {3, 0}) -> {down, normal, {0, 1}};
cube_division_wrap(50, right, {3, 0}) -> {up, normal, {2, 1}};
cube_division_wrap(50, down, {3, 0}) -> {down, normal, {0, 2}}.

pos_to_wrap_index(SideSize, {Row, Col}, Facing) ->
  RowIndex = (Row - 1) rem SideSize,
  ColIndex = (Col - 1) rem SideSize,
  case Facing of
    up -> ColIndex;
    down -> ColIndex;
    left -> RowIndex;
    right -> RowIndex
  end.

wrap_index_to_pos(SideSize, Index, Facing, {DivisionRow, DivisionCol}) ->
  case Facing of
    up -> {(DivisionRow + 1) * SideSize, (DivisionCol * SideSize) + Index + 1};
    down -> {(DivisionRow * SideSize) + 1, (DivisionCol * SideSize) + Index + 1};
    left -> {(DivisionRow * SideSize) + Index + 1, (DivisionCol + 1) * SideSize};
    right -> {(DivisionRow * SideSize) + Index + 1, DivisionCol * SideSize + 1}
  end.

index_transform(_, normal, Index) -> Index;
index_transform(SideSize, reverse, Index) -> SideSize - Index - 1.

wrap2(Facing, Pos, Map) ->
  SideSize = cube_side_size(Map),
  Division = pos_to_division(Pos, SideSize),
  {FacingNext, IndexType, DivisionNext} = cube_division_wrap(SideSize, Facing, Division),
  Index = pos_to_wrap_index(SideSize, Pos, Facing),
  IndexTransformed = index_transform(SideSize, IndexType, Index),
  PosNext = wrap_index_to_pos(SideSize, IndexTransformed, FacingNext, DivisionNext),
  {FacingNext, PosNext}.

move2(0, Facing, Pos, _) ->
  {Facing, Pos};
move2(N, Facing, Pos, Map) ->
  {FacingNext, PosNext} = move2(Facing, Pos, Map),
  move2(N - 1, FacingNext, PosNext, Map).

move2(Facing, {Row, Col} = Pos, Map) ->
  {RowDiff, ColDiff} = move_diff(Facing),
  PosMaybe = {Row + RowDiff, Col + ColDiff},
  case Map of
    #{ PosMaybe := wall } -> {Facing, Pos};
    #{ PosMaybe := open } -> {Facing, PosMaybe};
    #{} ->
      {FacingWrapped, PosWrapped} = wrap2(Facing, Pos, Map),
      case Map of
        #{ PosWrapped := wall } -> {Facing, Pos};
        #{ PosWrapped := open } -> {FacingWrapped, PosWrapped}
      end
  end.

follow_path2(Facing, Pos, _, []) ->
  {Facing, Pos};
follow_path2(Facing, Pos, Map, [Instr | Path]) ->
  case Instr of
    left -> follow_path2(rotate(Facing, left), Pos, Map, Path);
    right -> follow_path2(rotate(Facing, right), Pos, Map, Path);
    _ ->
      {FacingNext, PosNext} = move2(Instr, Facing, Pos, Map),
      follow_path2(FacingNext, PosNext, Map, Path)
  end.

day_22_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  [MapStr, PathStr] = string:split(Text, "\n\n"),
  Map = map_parse(MapStr),
  Path = path_parse(string:trim(PathStr)),
  {Facing, {Row, Col}} = follow_path2(right, starting_pos(Map), Map, Path),
  (1000 * Row) + (4 * Col) + facing_score(Facing).
