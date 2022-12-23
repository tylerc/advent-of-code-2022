-module(day_19).
-export([day_19_part_1/1, day_19_part_2/1]).

-record(blueprint, {id, ore_robot, clay_robot, obsidian_robot, geode_robot}).
-record(inventory, {ore = 0, clay = 0, obsidian = 0, geode = 0, ore_robot = 1, clay_robot = 0, obsidian_robot = 0, geode_robot = 0}).

blueprint_parse(<<"Blueprint ", Rest/binary>>) ->
  [BlueprintIdStr, Rest2] = string:split(Rest, ":"),
  Lexemes = string:lexemes(Rest2, " "),
  #blueprint{
    id = binary_to_integer(BlueprintIdStr),
    ore_robot = [{ore, binary_to_integer(lists:nth(5, Lexemes))}],
    clay_robot = [{ore, binary_to_integer(lists:nth(11, Lexemes))}],
    obsidian_robot = [{ore, binary_to_integer(lists:nth(17, Lexemes))}, {clay, binary_to_integer(lists:nth(20, Lexemes))}],
    geode_robot = [{ore, binary_to_integer(lists:nth(26, Lexemes))}, {obsidian, binary_to_integer(lists:nth(29, Lexemes))}]
  }.

atom_to_inventory_index(ore) -> #inventory.ore;
atom_to_inventory_index(clay) -> #inventory.clay;
atom_to_inventory_index(obsidian) -> #inventory.obsidian;
atom_to_inventory_index(geode) -> #inventory.geode;
atom_to_inventory_index(ore_robot) -> #inventory.ore_robot;
atom_to_inventory_index(clay_robot) -> #inventory.clay_robot;
atom_to_inventory_index(obsidian_robot) -> #inventory.obsidian_robot;
atom_to_inventory_index(geode_robot) -> #inventory.geode_robot.

can_purchase([], _) ->
  true;
can_purchase([{Material, AmountNeeded} | Costs], Inventory) ->
  OnHand = element(atom_to_inventory_index(Material), Inventory),
  case OnHand >= AmountNeeded of
    false -> false;
    true -> can_purchase(Costs, Inventory)
  end.

purchases_available(Blueprint, Inventory) ->
  lists:filter(
    fun ({_, Costs}) ->
      can_purchase(Costs, Inventory)
    end,
    [
      {geode_robot, Blueprint#blueprint.geode_robot},
      {obsidian_robot, Blueprint#blueprint.obsidian_robot},
      {clay_robot, Blueprint#blueprint.clay_robot},
      {ore_robot, Blueprint#blueprint.ore_robot}
    ]
  ).

pay_cost([], Inventory) ->
  Inventory;
pay_cost([{Material, AmountNeeded} | Costs], Inventory) ->
  Index = atom_to_inventory_index(Material),
  OnHand = element(Index, Inventory),
  InventoryNext = setelement(Index, Inventory, OnHand - AmountNeeded),
  pay_cost(Costs, InventoryNext).

purchase_queue_apply([], Inventory) ->
  Inventory;
purchase_queue_apply([Item | Queue], Inventory) ->
  Index = atom_to_inventory_index(Item),
  InventoryNext = setelement(Index, Inventory, element(Index, Inventory) + 1),
  purchase_queue_apply(Queue, InventoryNext).

inventory_tick(#inventory{} = Inventory) ->
  Inventory#inventory{
    ore = Inventory#inventory.ore + Inventory#inventory.ore_robot,
    clay = Inventory#inventory.clay + Inventory#inventory.clay_robot,
    obsidian = Inventory#inventory.obsidian + Inventory#inventory.obsidian_robot,
    geode = Inventory#inventory.geode + Inventory#inventory.geode_robot
  }.

% Computes 1 + 2 + 3 + 4 + ... + N
triangle_number(N) when N =< 0 ->
  0;
triangle_number(N) ->
  ((N * N) + N) div 2.

% Takes a triangle number, and chops off the first part (used by idealized_production to estimate the possible
% production if we just keep building a certain robot each minute, without double-counting what we've already earned).
triangle_number_shifted(N1, N2) ->
  triangle_number(N1 + N2) - triangle_number(N2 - 1).

% Compute an upper limit on geode production by assuming we can just buy an idealized amount of robots. If a scenario's
% idealized production of geodes is equal to or less than the current high score, we know for certain that we can toss
% it.
idealized_production(
    #blueprint{
      geode_robot = [_, {obsidian, GeodeRobotObsidianCost}],
      obsidian_robot = [_, {clay, ObsidianRobotClayCost}],
      clay_robot = [{ore, ClayRobotOreCost}]
    },
    #inventory{
      geode = Geodes,
      geode_robot = GeodeRobots,
      obsidian = Obsidian,
      obsidian_robot = ObsidianRobot,
      clay = Clay,
      clay_robot = ClayRobot,
      ore = Ore,
      ore_robot = OreRobot
    },
    MinutesRemaining
) ->
  IdealOreProduction = Ore + triangle_number_shifted(MinutesRemaining, OreRobot),
  PossibleClayRobotProduction = min(MinutesRemaining, (IdealOreProduction div ClayRobotOreCost)),
  IdealClayProduction = Clay + triangle_number_shifted(PossibleClayRobotProduction, ClayRobot),
  PossibleObsidianRobotProduction = min(MinutesRemaining, (IdealClayProduction div ObsidianRobotClayCost)),
  IdealObsidianProduction = Obsidian + triangle_number_shifted(PossibleObsidianRobotProduction, ObsidianRobot),
  PossibleGeodeRobotProduction = min(MinutesRemaining, (IdealObsidianProduction div GeodeRobotObsidianCost)),
  Geodes + triangle_number_shifted(PossibleGeodeRobotProduction, GeodeRobots).

optimize(Blueprint, Minutes) ->
  optimize(Blueprint, #{}, 0, [{#inventory{}, Minutes, false, [], []}]).

optimize(_, _, HighScore, []) ->
  HighScore;
optimize(Blueprint, Cache, HighScore, [{Inventory, MinutesRemaining, HasDecidedPurchases, PurchaseQueue, PurchaseHistory} | Inventories]) ->
  case HasDecidedPurchases of
    false ->
      InventoriesNew = lists:map(
        fun ({Item, Cost}) ->
          {pay_cost(Cost, Inventory), MinutesRemaining, true, [Item], [Item | PurchaseHistory]}
        end,
        purchases_available(Blueprint, Inventory)
      ),
      optimize(Blueprint, Cache, HighScore, InventoriesNew ++ [{Inventory, MinutesRemaining, true, [], [nil | PurchaseHistory]}] ++ Inventories);
    true ->
      InventoryNext = purchase_queue_apply(PurchaseQueue, inventory_tick(Inventory)),
      MinutesRemainingNext = MinutesRemaining - 1,
      case MinutesRemainingNext == 0 of
        false ->
          case idealized_production(Blueprint, InventoryNext, MinutesRemainingNext) =< HighScore of
            true -> optimize(Blueprint, Cache, HighScore, Inventories);
            false -> optimize(
              Blueprint,
              Cache,
              HighScore,
              [{InventoryNext, MinutesRemainingNext, false, [], PurchaseHistory} | Inventories]
            )
          end;
        true ->
          HighScoreNext = max(HighScore, InventoryNext#inventory.geode),
          optimize(Blueprint, Cache, HighScoreNext, Inventories)
      end
  end.

naive(Blueprints) ->
  Master = self(),
  lists:foreach(
    fun (Blueprint) ->
      spawn(
        fun () ->
          MaxGeodes = optimize(Blueprint, 24),
          Master ! {sum, Blueprint#blueprint.id * MaxGeodes}
        end
      )
    end,
    Blueprints
  ),
  lists:foldl(
    fun (_, Accum) ->
      receive
        {sum, Num} ->
          io:format("."),
          Accum + Num
      end
    end,
    0,
    Blueprints
  ).

day_19_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  Blueprints = lists:map(fun blueprint_parse/1, Lines),
  naive(Blueprints).

naive2(Blueprints) ->
  Master = self(),
  lists:foreach(
    fun (Blueprint) ->
      spawn(
        fun () ->
          MaxGeodes = optimize(Blueprint, 32),
          Master ! {product, MaxGeodes}
        end
      )
    end,
    Blueprints
  ),
  lists:foldl(
    fun (_, Accum) ->
      receive
        {product, Num} ->
          io:format("."),
          Accum * Num
      end
    end,
    1,
    Blueprints
  ).

day_19_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  Blueprints = lists:sublist(lists:map(fun blueprint_parse/1, Lines), 3),
  naive2(Blueprints).
