-module(day_06).
-export([day_06_part_1_tests/0, day_06_part_1/0, day_06_part_2_tests/0, day_06_part_2/0]).

index_of_4_different(Binary) ->
  index_of_4_different(Binary, 4).

index_of_4_different(<<A:1/binary, B:1/binary, C:1/binary, D:1/binary, Rest/binary>>, Index) ->
  if
    A == B orelse A == C orelse A == D orelse B == C orelse B == D orelse C == D ->
      index_of_4_different(<<B/binary, C/binary, D/binary, Rest/binary>>, Index + 1);
    true -> Index
  end.

day_06_part_1_tests() ->
  7 = index_of_4_different(<<"mjqjpqmgbljsphdztnvjfqwrcgsmlb">>),
  5 = index_of_4_different(<<"bvwbjplbgvbhsrlpgdmjqwftvncz">>),
  6 = index_of_4_different(<<"nppdvjthqldpwncqszvftbrmjlhg">>),
  10 = index_of_4_different(<<"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg">>),
  11 = index_of_4_different(<<"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw">>),
  ok.

index_of_different(Binary, DistinctCountWanted) ->
  index_of_different(Binary, DistinctCountWanted, DistinctCountWanted).

index_of_different(Binary, DistinctCountWanted, Index) ->
  <<Portion:DistinctCountWanted/binary, _/binary>> = Binary,
  DistinctCountFound = sets:size(sets:from_list(binary:bin_to_list(Portion))),
  if
    DistinctCountFound == DistinctCountWanted ->
      Index;
    true ->
      <<_Head:1/binary, Rest/binary>> = Binary,
      index_of_different(Rest, DistinctCountWanted, Index + 1)
  end.

day_06_part_2_tests() ->
  7 = index_of_different(<<"mjqjpqmgbljsphdztnvjfqwrcgsmlb">>, 4),
  5 = index_of_different(<<"bvwbjplbgvbhsrlpgdmjqwftvncz">>, 4),
  6 = index_of_different(<<"nppdvjthqldpwncqszvftbrmjlhg">>, 4),
  10 = index_of_different(<<"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg">>, 4),
  11 = index_of_different(<<"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw">>, 4),

  19 = index_of_different(<<"mjqjpqmgbljsphdztnvjfqwrcgsmlb">>, 14),
  23 = index_of_different(<<"bvwbjplbgvbhsrlpgdmjqwftvncz">>, 14),
  23 = index_of_different(<<"nppdvjthqldpwncqszvftbrmjlhg">>, 14),
  29 = index_of_different(<<"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg">>, 14),
  26 = index_of_different(<<"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw">>, 14),

  ok.

day_06_part_1() ->
  {ok, Text} = file:read_file("./day_06_input.txt"),
  [Chars] = string:lexemes(Text, "\n"),
  index_of_4_different(Chars).

day_06_part_2() ->
  {ok, Text} = file:read_file("./day_06_input.txt"),
  [Chars] = string:lexemes(Text, "\n"),
  index_of_different(Chars, 14).
