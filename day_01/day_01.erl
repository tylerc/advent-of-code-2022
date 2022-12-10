-module(day_01).
-record(elf, {snacks, calorie_total}).
-export([day_01_part_1/1, day_01_part_2/1]).

day_01_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Groups = string:split(Text, "\n\n", all),
  Elves = lists:map(fun binary_group_to_elf/1, Groups),
  ElfWithMost = lists:foldl(
    fun (Elf1, Elf2) ->
      if
        Elf1#elf.calorie_total > Elf2#elf.calorie_total -> Elf1;
        true -> Elf2
      end
    end,
    lists:nth(1, Elves),
    Elves
  ),
  ElfWithMost#elf.calorie_total.

day_01_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Groups = string:split(Text, "\n\n", all),
  Elves = lists:map(fun binary_group_to_elf/1, Groups),
  ElvesSorted = lists:sort(
    fun (Elf1, Elf2) ->
      if
        Elf1#elf.calorie_total >= Elf2#elf.calorie_total -> true;
        true -> false
      end
    end,
    Elves
  ),
  TopThree = lists:sublist(ElvesSorted, 3),
  lists:foldl(fun (Elf, Accum) -> Accum + Elf#elf.calorie_total end, 0, TopThree).

binary_group_to_elf(Group) ->
  Snacks = lists:map(fun binary_to_integer/1, string:lexemes(Group, "\n")),
  CalorieTotal = lists:foldl(fun erlang:'+'/2, 0, Snacks),
  #elf{snacks = Snacks, calorie_total = CalorieTotal}.

