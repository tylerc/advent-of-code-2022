-module(day_03).
-export([day_03_part_1/1, day_03_part_2/1]).

divide_rucksack(Str) ->
  Size = byte_size(Str),
  split_binary(Str, Size div 2).

rucksack_find_common({<<H:1/binary, T/binary>>, RucksackRight}) ->
  case binary:match(RucksackRight, H) of
    nomatch -> rucksack_find_common({T, RucksackRight});
    _ -> binary:at(H, 0)
  end.

char_to_value(Char) when Char >= $a, Char =< $z ->
  Char - $a + 1;
char_to_value(Char) when Char >= $A, Char =< $Z ->
  Char - $A + 27.

day_03_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  RucksacksRaw = string:lexemes(Text, "\n"),
  RucksacksSplit = lists:map(fun divide_rucksack/1, RucksacksRaw),
  RucksackCommonChars = lists:map(fun rucksack_find_common/1, RucksacksSplit),
  RucksackValues = lists:map(fun char_to_value/1, RucksackCommonChars),
  lists:foldl(fun erlang:'+'/2, 0, RucksackValues).

find_common_items_from_groups([A, B, C | RemainingGroups]) ->
  SetA = sets:from_list(binary:bin_to_list(A)),
  SetB = sets:from_list(binary:bin_to_list(B)),
  SetC = sets:from_list(binary:bin_to_list(C)),
  [CommonItem] = sets:to_list(sets:intersection([SetA, SetB, SetC])),
  [CommonItem | find_common_items_from_groups(RemainingGroups)];
find_common_items_from_groups([]) -> [].

day_03_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  RucksacksRaw = string:lexemes(Text, "\n"),
  RucksackCommonChars = find_common_items_from_groups(RucksacksRaw),
  RucksackValues = lists:map(fun char_to_value/1, RucksackCommonChars),
  lists:foldl(fun erlang:'+'/2, 0, RucksackValues).
