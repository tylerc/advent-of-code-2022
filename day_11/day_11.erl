-module(day_11).
-export([day_11_part_1/1, day_11_part_2/1]).

-record(monkey, {id, items=[], inspection_count, operator, operand, test_value, true_monkey, false_monkey}).

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
  io:format("Rounds remaining: ~p\n", [N]),
  rounds_evaluate(N - 1, round_evaluate(Monkeys, Evaluator), Evaluator).

day_11_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Sections = string:split(Text, "\n\n", all),
  Monkeys = lists:map(fun monkey_parse/1, Sections),
  MonkeysAfter20 = rounds_evaluate(20, Monkeys, fun monkey_evaluate/2),
  [M1, M2 | _] = lists:reverse(lists:keysort(#monkey.inspection_count, MonkeysAfter20)),
  M1#monkey.inspection_count * M2#monkey.inspection_count.

monkey_evaluate_faster(Monkey, Monkeys) ->
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
%%      WorryLevelFinal = floor(WorryLevelTransformed / 3),
      WorryLevelFinal = WorryLevelTransformed,
      io:format("~p\n", [WorryLevelFinal rem Monkey#monkey.test_value]),
      case (WorryLevelFinal rem Monkey#monkey.test_value) == 0 of
        true -> {Monkey#monkey.true_monkey, WorryLevelFinal div Monkey#monkey.test_value};
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

day_11_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Sections = string:split(Text, "\n\n", all),
  Monkeys = lists:map(fun monkey_parse/1, Sections),
  MonkeysAfter10000 = rounds_evaluate(10000, Monkeys, fun monkey_evaluate_faster/2),
  [M1, M2 | _] = lists:reverse(lists:keysort(#monkey.inspection_count, MonkeysAfter10000)),
  M1#monkey.inspection_count * M2#monkey.inspection_count.

% Here's what I know:
%
% The monkeys who add are shifting along a possibility space, and the total magnitude of the number
% doesn't matter, only how close it is to the divisor.
%
% The monkeys who are multiplying by primes could in theory be opening things up for future monkeys
% i.e. by then making the item a multiple of 19.
% As far as I can tell they aren't opening up their own possibility space _at all_ (except indirectly,
% through knock-on effects from later monkeys, perhaps).
%
% I'm uncertain what the monkeys who are multiplying by the current value are doing... It almost
% seems as if it should be a no-op, but that isn't borne out by my tests.
