-module(day_11).
-export([day_11_part_1/1, day_11_part_2/1]).

-record(monkey, {id, items=[], inspection_count, operator, operand, test_value, true_monkey, false_monkey}).
-record(item, {id, worry_level, monkey_id, path}).

monkey_parse(Section) ->
  [
    <<"Monkey ", MonkeyId:1/binary, ":">>,
    <<"  Starting items: ", ItemsStr/binary>>,
    <<"  Operation: new = old ", OperatorStr:1/binary, " ", OperandStr/binary>>,
    <<"  Test: divisible by ", TestValueStr/binary>>,
    <<"    If true: throw to monkey ", TrueMonkeyIdStr/binary>>,
    <<"    If false: throw to monkey ", FalseMonkeyIdStr/binary>>
  ] = string:lexemes(Section, "\n"),
  #monkey{
    id = binary_to_integer(MonkeyId),
    items = lists:map(fun binary_to_integer/1, string:lexemes(ItemsStr, ", ")),
    inspection_count = 0,
    operator = binary_to_atom(OperatorStr),
    operand = case OperandStr of
                <<"old">> -> old;
                _ -> binary_to_integer(OperandStr)
              end,
    test_value = binary_to_integer(TestValueStr),
    true_monkey = binary_to_integer(TrueMonkeyIdStr),
    false_monkey = binary_to_integer(FalseMonkeyIdStr)
  }.

monkey_evaluate(Monkey, Monkeys) ->
  ItemsTransferred = lists:map(
    fun (WorryLevelInitial) ->
      Operand = case Monkey#monkey.operand of
                  old -> WorryLevelInitial;
                  _ -> Monkey#monkey.operand
                end,
      WorryLevelTransformed = case Monkey#monkey.operator of
                                '+' -> WorryLevelInitial + Operand;
                                '*' -> WorryLevelInitial * Operand
                              end,
      WorryLevelFinal = floor(WorryLevelTransformed / 3),
      case (WorryLevelFinal rem Monkey#monkey.test_value) == 0 of
        true -> {Monkey#monkey.true_monkey, WorryLevelFinal};
        false -> {Monkey#monkey.false_monkey, WorryLevelFinal}
      end
    end,
    Monkey#monkey.items
  ),
  MonkeyNext = Monkey#monkey{
    items = [],
    inspection_count = Monkey#monkey.inspection_count + length(Monkey#monkey.items)
  },
  lists:map(
    fun (MonkeyOld) ->
      if
        MonkeyOld == Monkey -> MonkeyNext;
        true ->
          RelevantItems = lists:filtermap(
            fun ({MonkeyId, Item}) ->
              if
                MonkeyId == MonkeyOld#monkey.id -> {true, Item};
                true -> false
              end
            end,
            ItemsTransferred
          ),
          MonkeyOld#monkey{
            items = MonkeyOld#monkey.items ++ RelevantItems
          }
      end
    end,
    Monkeys
  ).

round_evaluate(Monkeys, Evaluator) ->
  lists:foldl(
    fun (#monkey{ id = MonkeyId }, MonkeysLatest) ->
      Monkey = lists:keyfind(MonkeyId, #monkey.id, MonkeysLatest),
      Evaluator(Monkey, MonkeysLatest)
    end,
    Monkeys,
    Monkeys
  ).

rounds_evaluate(0, Monkeys, _) ->
  Monkeys;
rounds_evaluate(N, Monkeys, Evaluator) ->
  rounds_evaluate(N - 1, round_evaluate(Monkeys, Evaluator), Evaluator).

day_11_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Sections = string:split(Text, "\n\n", all),
  Monkeys = lists:map(fun monkey_parse/1, Sections),
  MonkeysAfter20 = rounds_evaluate(20, Monkeys, fun monkey_evaluate/2),
  [M1, M2 | _] = lists:reverse(lists:keysort(#monkey.inspection_count, MonkeysAfter20)),
  M1#monkey.inspection_count * M2#monkey.inspection_count.

monkey_evaluate_part_2(Monkey, Monkeys, Items, Lcm) ->
  {ItemsUpdated, InspectionCount} = lists:mapfoldl(
    fun (Item, Accum) ->
      case Item#item.monkey_id == Monkey#monkey.id of
        false -> {Item, Accum};
        true ->
          WorryLevelInitial = Item#item.worry_level,
          Operand = case Monkey#monkey.operand of
                      old -> WorryLevelInitial;
                      _ -> Monkey#monkey.operand
                    end,
          WorryLevelTransformed = case Monkey#monkey.operator of
                                    '+' -> WorryLevelInitial + Operand;
                                    '*' -> WorryLevelInitial * Operand
                                  end,
          WorryLevelFinal = WorryLevelTransformed,
          if
            WorryLevelFinal > 18446744073709551615 -> throw(number_too_big);
            true -> ok
          end,
          ItemNext = case (WorryLevelFinal rem Monkey#monkey.test_value) == 0 of
            true -> Item#item{
              monkey_id = Monkey#monkey.true_monkey,
              worry_level = WorryLevelFinal rem Lcm,
              path = [ {Monkey#monkey.id, true, WorryLevelFinal, Monkey#monkey.operand} | Item#item.path ]
            };
            false -> Item#item{
              monkey_id = Monkey#monkey.false_monkey,
              worry_level = WorryLevelFinal rem Lcm,
              path = [ {Monkey#monkey.id, false, WorryLevelFinal, Monkey#monkey.operand} | Item#item.path ]
            }
          end,
          {ItemNext, Accum + 1}
      end
    end,
    0,
    Items
  ),
  MonkeyNext = Monkey#monkey{
    items = [],
    inspection_count = Monkey#monkey.inspection_count + InspectionCount
  },
  MonkeysUpdated = lists:map(
    fun (MonkeyOld) ->
      if
        MonkeyOld == Monkey -> MonkeyNext;
        true -> MonkeyOld
      end
    end,
    Monkeys
  ),
  {MonkeysUpdated, ItemsUpdated}.

round_evaluate_part2(Monkeys, Items, Lcm, Evaluator) ->
  lists:foldl(
    fun (#monkey{ id = MonkeyId }, {MonkeysLatest, ItemsLatest}) ->
      Monkey = lists:keyfind(MonkeyId, #monkey.id, MonkeysLatest),
      Evaluator(Monkey, MonkeysLatest, ItemsLatest, Lcm)
    end,
    {Monkeys, Items},
    Monkeys
  ).

rounds_evaluate_part2(0, Monkeys, Items, _, _) ->
  {Monkeys, Items};
rounds_evaluate_part2(N, Monkeys, Items, Lcm, Evaluator) ->
  {MonkeysNext, ItemsNext} = round_evaluate_part2(Monkeys, Items, Lcm, Evaluator),
  rounds_evaluate_part2(N - 1, MonkeysNext, ItemsNext, Lcm, Evaluator).

day_11_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Sections = string:split(Text, "\n\n", all),
  Monkeys = lists:map(fun monkey_parse/1, Sections),
  Items = lists:foldl(
    fun (Monkey, Accum) ->
      Accum ++ lists:map(
        fun (ItemInitialValue) ->
          #item{
            id = ItemInitialValue,
            worry_level = ItemInitialValue,
            monkey_id = Monkey#monkey.id,
            path = []
          }
        end,
        Monkey#monkey.items
      )
    end,
    [],
    Monkeys
  ),
  Lcm = lists:foldl(
    fun (#monkey{ test_value = TestValue }, Accum) ->
      TestValue * Accum
    end,
    1,
    Monkeys
  ),
  {MonkeysAfter10000, _} = rounds_evaluate_part2(10000, Monkeys, Items, Lcm, fun monkey_evaluate_part_2/4),
  [M1, M2 | _] = lists:reverse(lists:keysort(#monkey.inspection_count, MonkeysAfter10000)),
  M1#monkey.inspection_count * M2#monkey.inspection_count.
