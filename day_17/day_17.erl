-module(day_17).
-export([day_17_part_1/1, day_17_part_2/1]).

jets_parse(<<H:1/binary, Rest/binary>>) ->
  Term = case H of
           <<">">> -> right;
           <<"<">> -> left
         end,
  [Term | jets_parse(Rest)];
jets_parse(<<>>) ->
  [].

% Chamber is 7 units wide
% Leftmost edge always spawns two units away from the wall (ex: |..S)
% Bottom edge always spawns 3 units above the highest rock or floor.
% Stops if would move on top of rock or floor.
% Falling rock does not move if jet would cause a falling rock to move into walls or other rock.
% Eval order: 1) Pushed by jet; 2) Falls

highest_rock(#{} = Arena) ->
  maps:fold(
    fun ({Row, _}, _, Highest) ->
      case Row > Highest of
        true -> Row;
        false -> Highest
      end
    end,
    0,
    Arena
  ).

shift_for_spawn(Rock, #{} = Arena) ->
  RowShift = highest_rock(Arena) + 3 + 1,
  ColShift = 2 + 1,
  lists:map(
    fun ({Row, Col}) ->
      {Row + RowShift, Col + ColShift}
    end,
    Rock
  ).

colliding_with_wall(Rock) ->
  lists:any(
    fun ({_, Col}) ->
      Col == 0 orelse Col == 8
    end,
    Rock
  ).

colliding_with_floor(Rock) ->
  lists:any(
    fun ({Row, _}) ->
      Row =< 0
    end,
    Rock
  ).

colliding_with_rock(Rock, #{} = Arena) ->
  lists:any(
    fun (Pos) ->
      case Arena of
        #{ Pos := _ } -> true;
        #{} -> false
      end
    end,
    Rock
  ).

shift_col(Diff, Rock) ->
  lists:map(
    fun ({Row, Col}) ->
      {Row, Col + Diff}
    end,
    Rock
  ).

shift_row(Diff, Rock) ->
  lists:map(
    fun ({Row, Col}) ->
      {Row + Diff, Col}
    end,
    Rock
  ).

shift_for_jet(Jet, Rock, Arena) ->
  Diff = case Jet of
           left -> -1;
           right -> 1
         end,
  Shifted = shift_col(Diff, Rock),
  case colliding_with_wall(Shifted) orelse colliding_with_rock(Shifted, Arena) of
    true -> Rock;
    false -> Shifted
  end.

shift_for_gravity(Rock, Arena) ->
  Shifted = shift_row(-1, Rock),
  case colliding_with_floor(Shifted) orelse colliding_with_rock(Shifted, Arena) of
    true -> stopped;
    false -> Shifted
  end.

topology(Arena) ->
  HighestRowForCol = maps:fold(
    fun ({Row, Col}, _, MapBuilding) ->
      case MapBuilding of
        #{ Col := OtherRow } ->
          case Row > OtherRow of
            true -> MapBuilding#{ Col => Row };
            false -> MapBuilding
          end;
        #{} -> MapBuilding#{ Col => Row }
      end
    end,
    #{},
    Arena
  ),
  LowestRowOfHighest = maps:fold(
    fun (_, Row, Lowest) ->
      case Row < Lowest of
        true -> Row;
        false -> Lowest
      end
    end,
    1000000000000,
    HighestRowForCol
  ),
  EvenLowerToBeSafe = LowestRowOfHighest - 5,
  RelevantRocks = maps:fold(
    fun ({Row, _} = Pos, _, Accum) ->
      case Row >= EvenLowerToBeSafe of
        true -> [Pos | Accum];
        false -> Accum
      end
    end,
    [],
    Arena
  ),
  RelevantRocksShifted = lists:map(
    fun ({Row, Col}) ->
      {Row - EvenLowerToBeSafe, Col}
    end,
    RelevantRocks
  ),
  lists:sort(RelevantRocksShifted).

simulate_with_cache(0, _, _, _, _, _, _, _) ->
  0;
simulate_with_cache(Remaining, Period, JetsCurrent, RocksCurrent, Cache, Arena, Jets, Rocks) ->
  Topology = topology(Arena),
  IterationsToRun = min(Remaining, Period),
  io:format("simulate_with_cache remaining: ~p; topology: ~p; period: ~p\n", [Remaining, erlang:phash2(Topology), Period]),
  case Cache of
    #{ {Topology, IterationsToRun, JetsCurrent, RocksCurrent} := {ArenaNext, AddedBuildHeight, JetsCurrentNext, RocksCurrentNext} } ->
      TopologyNext = topology(ArenaNext),
      {RemainingNext, PeriodNext, AddedBuildHeightReturning} = case Topology == TopologyNext of
        true ->
          Repeats = Remaining div IterationsToRun,
          {Remaining - (IterationsToRun * Repeats), Period, AddedBuildHeight * Repeats};
        false -> {Remaining - IterationsToRun, Period, AddedBuildHeight}
      end,
      AddedBuildHeightReturning + simulate_with_cache(RemainingNext, PeriodNext, JetsCurrentNext, RocksCurrentNext, Cache, ArenaNext, Jets, Rocks);
    #{} ->
      io:format("    Topology ~p not cached, simulating ~p falling rocks. (~p/~p ~p/~p)\n", [erlang:phash2(Topology), IterationsToRun, length(JetsCurrent), length(Jets), length(RocksCurrent), length(Rocks)]),
      {JetsCurrentNext, RocksCurrentNext, ArenaNext} = simulate(IterationsToRun, nil, JetsCurrent, RocksCurrent, Arena, Jets, Rocks),
      AddedBuildHeight = highest_rock(ArenaNext) - highest_rock(Arena),
      CacheNext = Cache#{ {Topology, IterationsToRun, JetsCurrent, RocksCurrent} => {ArenaNext, AddedBuildHeight, JetsCurrentNext, RocksCurrentNext} },
      AddedBuildHeight + simulate_with_cache(Remaining - IterationsToRun, Period, JetsCurrentNext, RocksCurrentNext, CacheNext, ArenaNext, Jets, Rocks)
  end.

find_period_multiplier(0, _, _, _, _, _, _, _) ->
  1;
find_period_multiplier(Remaining, Iteration, JetsCurrent, RocksCurrent, Cache, Arena, Jets, Rocks) ->
  Topology = topology(Arena),
  IterationsToRun = min(Remaining, 5),
  case Cache of
    #{ {Topology, IterationsToRun, JetsCurrent, RocksCurrent} := IterationLast } ->
      Iteration - IterationLast;
    #{} ->
      {JetsCurrentNext, RocksCurrentNext, ArenaNext} = simulate(IterationsToRun, nil, JetsCurrent, RocksCurrent, Arena, Jets, Rocks),
      CacheNext = Cache#{ {Topology, IterationsToRun, JetsCurrent, RocksCurrent} => Iteration },
      find_period_multiplier(Remaining - IterationsToRun, Iteration + 1, JetsCurrentNext, RocksCurrentNext, CacheNext, ArenaNext, Jets, Rocks)
  end.

simulate(0, _, JetsCurrent, RocksCurrent, Arena, _, _) ->
  {JetsCurrent, RocksCurrent, Arena};
simulate(Remaining, Falling, [], RocksCurrent, Arena, Jets, Rocks) ->
  simulate(Remaining, Falling, Jets, RocksCurrent, Arena, Jets, Rocks);
simulate(Remaining, Falling, JetsCurrent, [], Arena, Jets, Rocks) ->
  simulate(Remaining, Falling, JetsCurrent, Rocks, Arena, Jets, Rocks);
simulate(Remaining, nil, JetsCurrent, [Rock | RocksCurrent], Arena, Jets, Rocks) ->
  simulate(Remaining, shift_for_spawn(Rock, Arena), JetsCurrent, RocksCurrent, Arena, Jets, Rocks);
simulate(Remaining, Falling, [Jet | JetsCurrent], RocksCurrent, Arena, Jets, Rocks) ->
  JetShifted = shift_for_jet(Jet, Falling, Arena),
  GravityShifted = shift_for_gravity(JetShifted, Arena),
  case GravityShifted of
    stopped ->
      ArenaNext = lists:foldl(
        fun (Pos, ArenaUpdating) ->
          ArenaUpdating#{ Pos => rock }
        end,
        Arena,
        JetShifted
      ),
      simulate(Remaining - 1, nil, JetsCurrent, RocksCurrent, ArenaNext, Jets, Rocks);
    _ -> simulate(Remaining, GravityShifted, JetsCurrent, RocksCurrent, Arena, Jets, Rocks)
  end.

day_17_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Jets = jets_parse(hd(string:lexemes(Text, "\n"))),
  Rocks = [
    [{0, 0}, {0, 1}, {0, 2}, {0, 3}],
    [{0, 1}, {1, 0}, {1, 1}, {1, 2}, {2, 1}],
    [{0, 0}, {0, 1}, {0, 2}, {1, 2}, {2, 2}],
    [{0, 0}, {1, 0}, {2, 0}, {3, 0}],
    [{0, 0}, {0, 1}, {1, 0}, {1, 1}]
  ],
  {_, _, ArenaAfterSimulation} = simulate(2022, nil, [], [], #{}, Jets, Rocks),
  highest_rock(ArenaAfterSimulation).

day_17_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Jets = jets_parse(hd(string:lexemes(Text, "\n"))),
  Rocks = [
    [{0, 0}, {0, 1}, {0, 2}, {0, 3}], % 0-1 (leaning heavily towards 1)
    [{0, 1}, {1, 0}, {1, 1}, {1, 2}, {2, 1}], % 0-3
    [{0, 0}, {0, 1}, {0, 2}, {1, 2}, {2, 2}], % 0-3
    [{0, 0}, {1, 0}, {2, 0}, {3, 0}], % 0-4
    [{0, 0}, {0, 1}, {1, 0}, {1, 1}] % 0-2
  ],
  PeriodMultiplier = find_period_multiplier(1000000000000, 1, [], [], #{}, #{}, Jets, Rocks),
  simulate_with_cache(1000000000000, 5 * PeriodMultiplier, [], [], #{}, #{}, Jets, Rocks).
