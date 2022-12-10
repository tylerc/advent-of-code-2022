-module(day_04).
-export([day_04_part_1/1, day_04_part_2/1]).

assignments_raw_to_ranges(Str) ->
  [Elf1, Elf2] = string:split(Str, ","),
  [Elf1A1, Elf1A2] = string:split(Elf1, "-"),
  [Elf2A1, Elf2A2] = string:split(Elf2, "-"),
  {
    {binary_to_integer(Elf1A1), binary_to_integer(Elf1A2)},
    {binary_to_integer(Elf2A1), binary_to_integer(Elf2A2)}
  }.

fully_contains({{Elf1A1, Elf1A2}, {Elf2A1, Elf2A2}}) ->
  if
    Elf1A1 =< Elf2A1 andalso Elf1A2 >= Elf2A2 -> true;
    Elf2A1 =< Elf1A1 andalso Elf2A2 >= Elf1A2 -> true;
    true -> false
  end.

day_04_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  AssignmentPairsRaw = string:lexemes(Text, "\n"),
  Assignments = lists:map(fun assignments_raw_to_ranges/1, AssignmentPairsRaw),
  lists:foldl(
    fun (AssignmentForPair, Accum) ->
      FullyContains = fully_contains(AssignmentForPair),
      if
        FullyContains -> Accum + 1;
        true -> Accum
      end
    end,
    0,
    Assignments).

overlaps_at_all({{Elf1A1, Elf1A2}, {Elf2A1, Elf2A2}}) ->
  FullyContains = fully_contains({{Elf1A1, Elf1A2}, {Elf2A1, Elf2A2}}),
  if
    FullyContains -> true;
    (Elf1A2 >= Elf2A1 andalso Elf1A2 =< Elf2A2) orelse (Elf1A1 >= Elf2A1 andalso Elf1A1 =< Elf2A2) -> true;
    true -> false
  end.

day_04_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  AssignmentPairsRaw = string:lexemes(Text, "\n"),
  Assignments = lists:map(fun assignments_raw_to_ranges/1, AssignmentPairsRaw),
  lists:foldl(
    fun (AssignmentForPair, Accum) ->
      OverlapsAtAll = overlaps_at_all(AssignmentForPair),
      if
        OverlapsAtAll -> Accum + 1;
        true -> Accum
      end
    end,
    0,
    Assignments).
