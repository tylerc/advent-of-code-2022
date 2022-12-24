-module(day_21).
-export([day_21_part_1/1, day_21_part_2/1]).

-record(monkey, {id, eval}).

op_to_fun(<<"+">>) -> fun erlang:'+'/2;
op_to_fun(<<"-">>) -> fun erlang:'-'/2;
op_to_fun(<<"*">>) -> fun erlang:'*'/2;
op_to_fun(<<"/">>) -> fun erlang:'div'/2.

monkey_parse([], Lookup, _) ->
  Lookup;
monkey_parse([<<Id:4/binary, ": ", Rest/binary>> | Lines], #{} = LookupBuilding, Part) ->
  Eval = case Rest of
    <<LeftId:4/binary, " ", Op:1/binary, " ", RightId:4/binary>> ->
      case Id == <<"root">> andalso Part == part_2 of
        true ->
          fun (#{ LeftId := #monkey{ eval = LeftEval }, RightId := #monkey{ eval = RightEval } } = Lookup) ->
            Left = LeftEval(Lookup),
            Right = RightEval(Lookup),
            {Left, Right}
          end;
        false ->
          OpFun = op_to_fun(Op),
          fun (#{ LeftId := #monkey{ eval = LeftEval }, RightId := #monkey{ eval = RightEval } } = Lookup) ->
            OpFun(LeftEval(Lookup), RightEval(Lookup))
          end
      end;
    <<NumberStr/binary>> ->
      Number = binary_to_integer(NumberStr),
      fun (_) ->
        Number
      end
  end,
  Monkey = #monkey{ id = Id, eval = Eval },
  LookupNext = LookupBuilding#{ Id => Monkey },
  monkey_parse(Lines, LookupNext, Part).

day_21_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  #{ <<"root">> := RootMonkey } = Monkeys = monkey_parse(Lines, #{}, part_1),
  RootEval = RootMonkey#monkey.eval,
  RootEval(Monkeys).

find_step(Monkeys, RootEval) ->
  MonkeysAttemptA = Monkeys#{ <<"humn">> => #monkey{ id = <<"humn">>, eval = fun (_) -> 0 end } },
  MonkeysAttemptB = Monkeys#{ <<"humn">> => #monkey{ id = <<"humn">>, eval = fun (_) -> 4 end } },
  {A, _} = RootEval(MonkeysAttemptA),
  {B, _} = RootEval(MonkeysAttemptB),
  abs(A - B).

part_2_search(N, Step, Monkeys, RootEval) ->
  MonkeysAttempt = Monkeys#{ <<"humn">> => #monkey{ id = <<"humn">>, eval = fun (_) -> N end } },
  {Left, Right} = RootEval(MonkeysAttempt),
  Diff = Left - Right,
  if
    Diff == 0 -> N;
    true ->
      Increase = case Step of
                   1 -> 1;
                   _ -> (Diff div Step) * 2
                 end,
      part_2_search(N + Increase, Step, Monkeys, RootEval)
  end.

day_21_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  #{ <<"root">> := RootMonkey } = Monkeys = monkey_parse(Lines, #{}, part_2),
  RootEval = RootMonkey#monkey.eval,
  Step = find_step(Monkeys, RootEval),
  part_2_search(0, Step, Monkeys, RootEval).
