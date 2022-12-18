-module(day_16).
-export([day_16_part_1/1, day_16_part_2/1]).

-record(valve, {id, flow_rate, connections, is_open = false}).
-record(world, {valves, location, minutes_remaining, pressure_released = 0, pressure_rate = 0}).
-record(actor, {location, next_location, travel_minutes_remaining}).
-record(sim, {world, actors}).

tunnels_parse(Str) ->
  Words = string:lexemes(Str, " "),
  WordsWithoutCommas = lists:map(
    fun (Word) ->
      string:trim(Word, trailing, ",")
    end,
    Words
  ),
  lists:filter(
    fun (Word) ->
      string:uppercase(Word) == Word
    end,
    WordsWithoutCommas
  ).

valve_parse(<<"Valve ", Name:2/binary, " has flow rate=", Rest/binary>>) ->
  [FlowRateStr, ConnectionsStr] = string:split(Rest, "; "),
  #valve{
    id = Name,
    flow_rate = binary_to_integer(FlowRateStr),
    connections = tunnels_parse(ConnectionsStr)
  }.

valves_compute_pressure_rate(Valves) ->
  maps:fold(
    fun (_, #valve{ is_open = IsOpen, flow_rate = FlowRate }, Pressure) ->
      case IsOpen of
        true -> Pressure + FlowRate;
        false -> Pressure
      end
    end,
    0,
    Valves
  ).

simulate_task(ValveIdToOpen, MinutesTaken, #world{ valves = Valves, minutes_remaining = MinutesRemaining, pressure_released = PressureReleased } = World) ->
  PressureRate = valves_compute_pressure_rate(Valves),
  ValvesNext = case ValveIdToOpen of
    nil -> Valves;
    _ ->
      #{ ValveIdToOpen := Valve } = Valves,
      Valves#{ ValveIdToOpen => Valve#valve{ is_open = true } }
  end,
  World#world{
    minutes_remaining = MinutesRemaining - MinutesTaken,
    pressure_released = PressureReleased + (PressureRate * MinutesTaken),
    pressure_rate = PressureRate,
    valves = ValvesNext,
    location = ValveIdToOpen
  }.

calculate_pathing_costs(#{} = Costs, [], _) ->
  Costs;
calculate_pathing_costs(#{} = Costs, [{ValveId, Cost} | OtherMovesToCheck], #world{ valves = Valves } = World) ->
  CostForNextMove = Cost + 1,
  #{ ValveId := Valve } = Valves,
  ValvesRelevant = lists:filter(
    fun (OtherValveId) ->
      case Costs of
        #{ OtherValveId := ExistingCost } -> CostForNextMove < ExistingCost;
        #{} -> true
      end
    end,
    Valve#valve.connections
  ),
  CostsNext = lists:foldl(
    fun (ValveIdNext, CostsUpdating) ->
      CostsUpdating#{ ValveIdNext => CostForNextMove }
    end,
    Costs,
    ValvesRelevant
  ),
  MovesRelevantWithCosts = lists:map(
    fun (ValveIdNext) ->
      {ValveIdNext, CostForNextMove}
    end,
    ValvesRelevant
  ),
  calculate_pathing_costs(CostsNext, MovesRelevantWithCosts ++ OtherMovesToCheck, World).

valves_closed(#world{ valves = Valves }) ->
  maps:values(maps:filter(
    fun (_, Valve) ->
      not Valve#valve.is_open andalso Valve#valve.flow_rate > 0
    end,
    Valves
  )).

travel_time_cost(ValveIdStart, ValveIdEnd, World) ->
  #{ ValveIdEnd := Cost } = calculate_pathing_costs(#{}, [{ValveIdStart, 0}], World),
  Cost.

travel_times_precompute(#world{} = World) ->
  ValvesClosed = [<<"AA">> | lists:map(fun(Valve) -> Valve#valve.id end, valves_closed(World))],
  lists:foldl(
    fun (ValveIdStart, TravelTimesInProgress) ->
      lists:foldl(
        fun (ValveIdEnd, TravelTimesInProgress2) ->
          case ValveIdStart == ValveIdEnd of
            true -> TravelTimesInProgress2;
            false ->
              TravelTimesInProgress2#{
                { ValveIdStart, ValveIdEnd } => travel_time_cost(ValveIdStart, ValveIdEnd, World)
              }
          end
        end,
        TravelTimesInProgress,
        ValvesClosed
      )
    end,
    #{},
    ValvesClosed
  ).

search_by_task(#world{} = World) ->
  TravelTimes = travel_times_precompute(World),
  search_by_task(0, TravelTimes, [World]).

search_by_task(HighScore, _, []) ->
  HighScore;
search_by_task(HighScore, #{} = TravelTimes, [#world{ location = ValveIdStart, minutes_remaining = MinutesRemaining } = World | WorldsToCheck]) ->
  ValvesClosed = valves_closed(World),
  ValvesClosedWithMinutesToOpen = lists:map(
    fun (Valve) ->
      ValveIdEnd = Valve#valve.id,
      #{ { ValveIdStart, ValveIdEnd } := TravelTime } = TravelTimes,
      {Valve, TravelTime + 1}
    end,
    ValvesClosed
  ),
  ValvesClosedThatCanBeOpened = lists:filter(
    fun ({_, Minutes}) ->
      Minutes =< MinutesRemaining
    end,
    ValvesClosedWithMinutesToOpen
  ),
  WorldsNext = lists:map(
    fun ({Valve, Minutes}) ->
      simulate_task(Valve#valve.id, Minutes, World)
    end,
    ValvesClosedThatCanBeOpened
  ),
  case WorldsNext of
    [] ->
      WorldEnd = simulate_task(nil, MinutesRemaining, World),
      HighScoreNext = case WorldEnd#world.pressure_released > HighScore of
        true -> WorldEnd#world.pressure_released;
        false -> HighScore
      end,
      search_by_task(HighScoreNext, TravelTimes, WorldsToCheck);
    _ ->
      search_by_task(HighScore, TravelTimes, WorldsNext ++ WorldsToCheck)
  end.

day_16_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  ValvesList = lists:map(fun valve_parse/1, Lines),
  ValvesMap = lists:foldl(
    fun (Valve, Accum) ->
      Accum#{
        Valve#valve.id => Valve
      }
    end,
    #{},
    ValvesList
  ),
  WorldInitial = #world{ valves = ValvesMap, location = <<"AA">>,  minutes_remaining = 30 },
  search_by_task(WorldInitial).

tasks_available(ValveIdStart, TakenTasks, #{} = TravelTimes, #world{ minutes_remaining = MinutesRemaining } = World) ->
  ValvesClosed = valves_closed(World),
  ValvesClosedWithMinutesToOpen = lists:map(
    fun (Valve) ->
      ValveIdEnd = Valve#valve.id,
      #{ { ValveIdStart, ValveIdEnd } := TravelTime } = TravelTimes,
      {ValveIdEnd, TravelTime, (MinutesRemaining - TravelTime - 1) * Valve#valve.flow_rate}
    end,
    ValvesClosed
  ),
  Filtered = lists:filter(
    fun ({ValveIdEnd, _, Benefit}) ->
      Benefit > 0 andalso not lists:member(ValveIdEnd, TakenTasks)
    end,
    ValvesClosedWithMinutesToOpen
  ),
  lists:sort(
    fun ({_, _, BenefitA}, {_, _, BenefitB}) ->
      BenefitA >= BenefitB
    end,
    Filtered
  ).

actor_assign_task(#actor{ location = Location, next_location = nil } = Actor, TakenTasks, TravelTimes, #world{} = World) ->
  Tasks = tasks_available(Location, TakenTasks, TravelTimes, World),
  lists:map(
    fun ({Destination, Minutes, _}) ->
      Actor#actor{
        next_location = Destination,
        travel_minutes_remaining = Minutes
      }
    end,
    Tasks
  );
actor_assign_task(#actor{}, _, _, _) ->
  [].

sim_tasks_assign(#sim{ actors = Actors } = Sim, TravelTimes) ->
  lists:foldl(
    fun (N, Sims) ->
      lists:flatmap(
        fun (SimIter) ->
          sim_tasks_assign(N, SimIter, TravelTimes)
        end,
        Sims
      )
    end,
    [Sim],
    lists:seq(1, length(Actors))
  ).

sim_tasks_assign(N, #sim{ actors = Actors, world = World } = Sim, TravelTimes) ->
  Actor = lists:nth(N, Actors),
  TakenTasks = lists:foldl(
    fun (#actor{ next_location = Location }, Accum) ->
      case Location of
        nil -> Accum;
        _ -> [Location | Accum]
      end
    end,
    [],
    Actors
  ),
  FutureActors = actor_assign_task(Actor, TakenTasks, TravelTimes, World),
  case FutureActors of
    [] -> [Sim];
    _ ->
      lists:map(
        fun (FutureActor) ->
          Sim#sim{
            actors = [FutureActor | Actors -- [Actor]]
          }
        end,
        FutureActors
      )
  end.

sim_tick(#sim{
  actors = Actors,
  world = #world{
    valves = Valves,
    minutes_remaining = MinutesRemaining,
    pressure_released = PressureReleased
  } = World
} = Sim) ->
  {ActorsNext, ValveIdsOpened} = lists:mapfoldl(
    fun (#actor{ travel_minutes_remaining = TravelMinutes, next_location = NextLocation } = Actor, Accum) ->
      case TravelMinutes == 0 andalso NextLocation /= nil of
        true -> {Actor#actor{ location = NextLocation, next_location = nil }, [NextLocation | Accum]};
        false -> {Actor#actor{travel_minutes_remaining = TravelMinutes - 1}, Accum}
      end
    end,
    [],
    Actors
  ),
  {ValvesNext, PressureIncreasing} = lists:foldl(
    fun (ValveId, {ValvesUpdating, PressureAccum}) ->
      #{ ValveId := Valve } = ValvesUpdating,
      {ValvesUpdating#{ValveId => Valve#valve{is_open = true}}, PressureAccum + Valve#valve.flow_rate * (MinutesRemaining - 1)}
    end,
    {Valves, 0},
    ValveIdsOpened
  ),
  Sim#sim{
    actors = ActorsNext,
    world = World#world{
      minutes_remaining = MinutesRemaining - 1,
      pressure_released = PressureReleased + PressureIncreasing,
      valves = ValvesNext
    }
  }.

score_if_done(#sim{ world = #world{ minutes_remaining = 0, pressure_released = PressureReleased } }) ->
  PressureReleased;
score_if_done(#sim{}) ->
  0.

% Compute a perfect-world score that assumes you can teleport to the next-largest closed valve. If this score is less
% than the current high score, we know we can discard the sim, because even with the perfect cave layout it will never
% succeed:
heuristic_score(#sim{ world = #world{ pressure_released = PressureReleased, minutes_remaining = MinutesRemaining } = World }) ->
  ValvesClosed = lists:sort(
    fun (A, B) ->
      A#valve.flow_rate >= B#valve.flow_rate
    end,
    valves_closed(World)
  ),
  {RemainingValveScore, _} = lists:foldl(
    fun (Valve, {ScoreAccum, Index}) ->
      Incr = Valve#valve.flow_rate * (MinutesRemaining - Index),
      case Incr =< 0 of
        true -> {ScoreAccum, Index + 2};
        false -> {ScoreAccum + Incr, Index + 2}
      end
    end,
    {0, 1},
    ValvesClosed
  ),
  PressureReleased + RemainingValveScore.

search_by_task2(#world{} = World) ->
  TravelTimes = travel_times_precompute(World),
  Sim = #sim{
    world = World,
    actors = [
      #actor{ location = <<"AA">>, next_location = nil, travel_minutes_remaining = 0 },
      #actor{ location = <<"AA">>, next_location = nil, travel_minutes_remaining = 0 }
    ]
  },
  search_by_task2(0, TravelTimes, sim_tasks_assign(Sim, TravelTimes)).

search_by_task2(HighScore, _, []) ->
  HighScore;
search_by_task2(
    HighScore,
    #{} = TravelTimes,
    [#sim{} = Sim | SimsRemainToCheck]
) ->
  SimUpdated = sim_tick(Sim),
  SimsNext = sim_tasks_assign(SimUpdated, TravelTimes),
  SimsNextFiltered = lists:filter(
    fun (SimNext) ->
      heuristic_score(SimNext) >= HighScore andalso score_if_done(SimNext) == 0
    end,
    SimsNext
  ),
  HighScoreNext = lists:foldl(
    fun (SimNext, ScoreAccum) ->
      ScoreNext = score_if_done(SimNext),
      case ScoreNext > ScoreAccum of
        true -> ScoreNext;
        false -> ScoreAccum
      end
    end,
    HighScore,
    SimsNext
  ),
  search_by_task2(HighScoreNext, TravelTimes, SimsNextFiltered ++ SimsRemainToCheck).

day_16_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  ValvesList = lists:map(fun valve_parse/1, Lines),
  ValvesMap = lists:foldl(
    fun (Valve, Accum) ->
      Accum#{
        Valve#valve.id => Valve
      }
    end,
    #{},
    ValvesList
  ),
  WorldInitial = #world{ valves = ValvesMap, location = <<"AA">>, minutes_remaining = 26 },
  search_by_task2(WorldInitial).
