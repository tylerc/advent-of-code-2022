-module(day_12).
-export([day_12_part_1/1, day_12_part_2/1]).

-record(world, {map = #{}, start = {nil, nil}, goal = {nil, nil}}).

map_parse(<<Binary/binary>>) ->
  map_parse(0, 0, #world{}, Binary).

map_parse(_, _, #world{} = World, <<>>) ->
  World;
map_parse(Row, Col, #world{map = Map} = World, <<Char, Rest/binary>>) ->
  NextWorld = case Char of
    $\n -> World;
    $S -> World#world{
      map = Map#{ { Row, Col } => $a },
      start = { Row, Col }
    };
    $E -> World#world{
      map = Map#{ { Row, Col } => $z },
      goal = { Row, Col }
    };
    Elevation -> World#world{
      map = Map#{ { Row, Col } => Elevation }
    }
  end,
  case World == NextWorld of
    true -> map_parse(Row + 1, 0, NextWorld, Rest);
    false -> map_parse(Row, Col + 1, NextWorld, Rest)
  end.

move_is_possible(From, To, #world{map = Map}) ->
  #{ From := CurrentElevation } = Map,
  NextHigherElevation = CurrentElevation + 1,
  case Map of
    #{ To := Elevation } -> Elevation =< NextHigherElevation;
    #{} -> false
  end.

moves_possible({ Row, Col } = From, #world{} = World) ->
  lists:filter(
    fun (To) ->
      move_is_possible(From, To, World)
    end,
    [
      { Row - 1, Col - 0 },
      { Row - 0, Col - 1 },
      { Row - 0, Col + 1 },
      { Row + 1, Col - 0 }
    ]
  ).

% Algorithm for finding shortest path:
%
% 1. Start at a point A, with cost C
% 2. Enumerate all possible moves from A -> B and compute their costs (C + 1).
% 3. If I've seen the destination before at an equal or lower cost, discard the move.
% 4. Record the remaining costs, and push the destinations onto a stack.
% 5. If items remain on the stack, pop one off and go to step 1.
% 6. Otherwise, we've explored all the paths. The cost for the goal is the answer to part 1.

calculate_pathing_costs(#{} = Costs, [], _) ->
  Costs;
calculate_pathing_costs(#{} = Costs, [{Pos, Cost} | OtherPositionsToCheck], #world{} = World) ->
  CostForNextMove = Cost + 1,
  Moves = moves_possible(Pos, World),
  MovesRelevant = lists:filter(
    fun (Move) ->
      case Costs of
        #{ Move := ExistingCost } -> CostForNextMove < ExistingCost;
        #{} -> true
      end
    end,
    Moves
  ),
  CostsNext = lists:foldl(
    fun (Move, CostsUpdating) ->
      CostsUpdating#{ Move => CostForNextMove }
    end,
    Costs,
    MovesRelevant
  ),
  MovesRelevantWithCosts = lists:map(
    fun (Move) ->
      {Move, CostForNextMove}
    end,
    MovesRelevant
  ),
  calculate_pathing_costs(CostsNext, MovesRelevantWithCosts ++ OtherPositionsToCheck, World).

day_12_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  WorldInitial = map_parse(Text),
  GoalPos = WorldInitial#world.goal,
  #{ GoalPos := GoalCost } = calculate_pathing_costs(#{}, [{WorldInitial#world.start, 0}], WorldInitial),
  GoalCost.

starting_points(#world{ map = Map }) ->
  MapOnlyLowest = maps:filter(
    fun (_, Value) ->
      Value == $a
    end,
    Map
  ),
  maps:keys(MapOnlyLowest).

day_12_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  WorldInitial = map_parse(Text),
  GoalPos = WorldInitial#world.goal,
  StartingPoints = starting_points(WorldInitial),
  Costs = lists:foldl(
    fun (Point, Costs) ->
      calculate_pathing_costs(Costs, [{Point, 0}], WorldInitial)
    end,
    #{},
    StartingPoints
  ),
  #{ GoalPos := GoalCost } = Costs,
  GoalCost.
