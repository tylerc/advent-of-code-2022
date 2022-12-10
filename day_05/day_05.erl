-module(day_05).
-export([day_05_part_1/1, day_05_part_2/1]).

-record(crate, {letter, initial_stack}).
-record(movement, {count, from, to}).

stack_parse(Line) ->
  stack_parse(Line, 1).

stack_parse(<<H:3/binary, $ , T/binary>>, InitialStack) ->
  case H of
    <<"   ">> -> stack_parse(T, InitialStack + 1);
    <<$[, Letter:1/binary, $]>> ->
      [#crate{letter=Letter, initial_stack=InitialStack} | stack_parse(T, InitialStack + 1)]
  end;
% Handles the last stack, which does not have a trailing space:
stack_parse(<<H:3/binary>>, InitialStack) ->
  stack_parse(<<H/binary, $ >>, InitialStack);
stack_parse(<<>>, _) ->
  [].

stacks_initial_parse(LinesRaw) ->
  Lines = lists:droplast(string:lexemes(LinesRaw, "\n")),
  CratesByHeight = lists:reverse(lists:flatmap(fun stack_parse/1, Lines)),
  lists:foldl(
    fun (Crate, Map) ->
      maps:update_with(
        Crate#crate.initial_stack,
        fun (Existing) ->
          [Crate | Existing]
        end,
        [Crate],
        Map
      )
    end,
    #{},
    CratesByHeight).

movement_parse(LineRaw) ->
  <<"move ", Rest/binary>> = LineRaw,
  [MoveCountStr, Rest2] = string:split(Rest, " from "),
  [StackStartStr, StackEndStr] = string:split(Rest2, " to "),
  #movement{
    count=binary_to_integer(MoveCountStr),
    from=binary_to_integer(StackStartStr),
    to=binary_to_integer(StackEndStr)
  }.

movements_parse(LinesRaw) ->
  Lines = string:lexemes(LinesRaw, "\n"),
  lists:map(fun movement_parse/1, Lines).

stack_execute_movement(Movement, Stacks) ->
  From = Movement#movement.from,
  To = Movement#movement.to,
  #{
    From := CratesThatCouldMove,
    To := CratesAlreadyOnDestinationStack
  } = Stacks,
  {CratesMoved, CratesRemaining} = lists:split(Movement#movement.count, CratesThatCouldMove),
  Stacks#{
    From => CratesRemaining,
    To => lists:reverse(CratesMoved) ++ CratesAlreadyOnDestinationStack
  }.

top_of_stack_message(Stacks) ->
  lists:reverse(maps:fold(
    fun (_, Stack, Accum) ->
      [TopCrate | _] = Stack,
      [binary:first(TopCrate#crate.letter) | Accum]
    end,
    "",
    Stacks
  )).

day_05_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  [StacksRaw, MovementsRaw] = string:split(Text, "\n\n"),
  StacksInitial = stacks_initial_parse(StacksRaw),
  Movements = movements_parse(MovementsRaw),
  StacksEnd = lists:foldl(
    fun stack_execute_movement/2,
    StacksInitial,
    Movements
  ),
  top_of_stack_message(StacksEnd).

stack_execute_movement2(Movement, Stacks) ->
  From = Movement#movement.from,
  To = Movement#movement.to,
  #{
    From := CratesThatCouldMove,
    To := CratesAlreadyOnDestinationStack
  } = Stacks,
  {CratesMoved, CratesRemaining} = lists:split(Movement#movement.count, CratesThatCouldMove),
  Stacks#{
    From => CratesRemaining,
    To => CratesMoved ++ CratesAlreadyOnDestinationStack
  }.

day_05_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  [StacksRaw, MovementsRaw] = string:split(Text, "\n\n"),
  StacksInitial = stacks_initial_parse(StacksRaw),
  Movements = movements_parse(MovementsRaw),
  StacksEnd = lists:foldl(
    fun stack_execute_movement2/2,
    StacksInitial,
    Movements
  ),
  top_of_stack_message(StacksEnd).
