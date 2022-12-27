-module(day_23).
-export([day_23_part_1/1, day_23_part_2/1]).
-define(DIRECTIONS_START, [[n, ne, nw], [s, se, sw], [w, nw, sw], [e, ne, se]]).

map_parse(Str) ->
  map_parse(1, 1, Str, #{}).

map_parse(_, _, <<>>, Map) ->
  Map;
map_parse(Row, Col, <<H:1/binary, Rest/binary>>, #{} = Map) ->
  case H of
    <<".">> -> map_parse(Row, Col + 1, Rest, Map);
    <<"#">> -> map_parse(Row, Col + 1, Rest, Map#{ {Row, Col} => elf });
    <<"\n">> -> map_parse(Row + 1, 1, Rest, Map)
  end.

direction_diff(n) -> {-1, 0};
direction_diff(ne) -> {-1, 1};
direction_diff(nw) -> {-1, -1};
direction_diff(s) -> {1, 0};
direction_diff(se) -> {1, 1};
direction_diff(sw) -> {1, -1};
direction_diff(w) -> {0, -1};
direction_diff(e) -> {0, 1}.

pos_after_direction({Row, Col}, Dir) ->
  {RowDiff, ColDiff} = direction_diff(Dir),
  {Row + RowDiff, Col + ColDiff}.

directions_rotate([Dir | Rest]) ->
  Rest ++ [Dir].

pos_has_elf(Pos, Map) ->
  case Map of
    #{ Pos := elf } -> true;
    _ -> false
  end.

elf_is_alone(Pos, Map) ->
  lists:all(
    fun (Dir) ->
      not pos_has_elf(pos_after_direction(Pos, Dir), Map)
    end,
    [n, ne, nw, s, se, sw, w, e]
  ).

elf_propose(Pos, [], _) ->
  Pos;
elf_propose(Pos, [Directions | Rest], Map) ->
  OpenInDirection = lists:all(
    fun (Dir) ->
      not pos_has_elf(pos_after_direction(Pos, Dir), Map)
    end,
    Directions
  ),
  case OpenInDirection of
    true -> pos_after_direction(Pos, hd(Directions));
    false -> elf_propose(Pos, Rest, Map)
  end.

round_process(Directions, Map) ->
  Elfs = maps:keys(Map),
  ElfsMoving = lists:filter(
    fun (Pos) ->
      not elf_is_alone(Pos, Map)
    end,
    Elfs
  ),
  ElfProposals = lists:foldl(
    fun (Pos, MapAccum) ->
      PosProposed = elf_propose(Pos, Directions, Map),
      case MapAccum of
        #{ PosProposed := List } -> MapAccum#{ PosProposed => [Pos | List] };
        #{} -> MapAccum#{ PosProposed => [Pos] }
      end
    end,
    #{},
    ElfsMoving
  ),
  maps:fold(
    fun (PosProposed, PosStartList, MapBuilding) ->
      case PosStartList of
        [PosStart] ->
          MapWithoutElf = maps:remove(PosStart, MapBuilding),
          MapWithoutElf#{ PosProposed => elf };
        _ ->
          MapBuilding
      end
    end,
    Map,
    ElfProposals
  ).

rounds_process(0, _, Map) ->
  Map;
rounds_process(N, Directions, Map) ->
  MapUpdated = round_process(Directions, Map),
  DirectionsUpdated = directions_rotate(Directions),
  rounds_process(N - 1, DirectionsUpdated, MapUpdated).

map_bounds(#{} = Map) ->
  maps:fold(
    fun ({Row, Col}, _, {MinRow, MaxRow, MinCol, MaxCol}) ->
      {min(Row, MinRow), max(Row, MaxRow), min(Col, MinCol), max(Col, MaxCol)}
    end,
    {10000, -10000, 10000, -10000},
    Map
  ).

map_count_empty(#{} = Map) ->
  {MinRow, MaxRow, MinCol, MaxCol} = map_bounds(Map),
  lists:foldl(
    fun (Row, Accum) ->
      Accum + lists:foldl(
        fun (Col, Accum2) ->
          case Map of
            #{ {Row, Col} := elf } -> Accum2;
            _ -> Accum2 + 1
          end
        end,
        0,
        lists:seq(MinCol, MaxCol)
      )
    end,
    0,
    lists:seq(MinRow, MaxRow)
  ).

day_23_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  MapInitial = map_parse(Text),
  map_count_empty(rounds_process(10, ?DIRECTIONS_START, MapInitial)).

rounds_process_until_no_movement(N, Directions, Map) ->
  MapUpdated = round_process(Directions, Map),
  case MapUpdated == Map of
    true -> N;
    false ->
      DirectionsUpdated = directions_rotate(Directions),
      rounds_process_until_no_movement(N + 1, DirectionsUpdated, MapUpdated)
  end.

day_23_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  MapInitial = map_parse(Text),
  rounds_process_until_no_movement(1, ?DIRECTIONS_START, MapInitial).
