-module(day_20).
-export([day_20_part_1/1, day_20_part_2/1]).

-record(number, {value, original_index}).

current_index(OriginalIndex, List) -> current_index(OriginalIndex, List, 1).

current_index(OriginalIndex, [#number{ original_index = OriginalIndex } = Number | _], Index) ->
  {Index, Number};
current_index(OriginalIndex, [_ | Rest], Index) ->
  current_index(OriginalIndex, Rest, Index + 1).

split_index_clean(NextIndex, Max) when NextIndex >= 1 andalso NextIndex =< Max ->
  NextIndex;
split_index_clean(0, Max) ->
  Max;
split_index_clean(NextIndex, Max) when NextIndex > Max ->
  NextIndex rem Max;
split_index_clean(NextIndex, Max) when NextIndex < 0 ->
  split_index_clean(Max + (NextIndex rem Max), Max).

mix(OriginalIndex, Max, Numbers) when OriginalIndex > Max ->
  Numbers;
mix(OriginalIndex, Max, Numbers) ->
  {CurrentIndex, #number{ value = Value, original_index = OriginalIndex } = Number} = current_index(OriginalIndex, Numbers),
  SplitIndex = split_index_clean(CurrentIndex + Value - 1, Max - 1),
  ListWithNumberRemoved = Numbers -- [Number],
  {Left, Right} = lists:split(SplitIndex, ListWithNumberRemoved),
  NumbersNext = Left ++ [ Number | Right ],
  mix(OriginalIndex + 1, Max, NumbersNext).

nth_after_zero(N, Max, Numbers) ->
  #number{ original_index = ZeroOriginalIndex } = Zero = lists:keyfind(0, #number.value, Numbers),
  {CurrentIndex, Zero} = current_index(ZeroOriginalIndex, Numbers),
  WantedIndex = (CurrentIndex + N) rem Max,
  Number = lists:nth(WantedIndex, Numbers),
  Number#number.value.

day_20_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  {Numbers, _} = lists:mapfoldl(
    fun (Number, Accum) ->
      {#number{value = binary_to_integer(Number), original_index = Accum}, Accum + 1}
    end,
    1,
    string:lexemes(Text, "\n")
  ),
  Max = length(Numbers),
  Mixed = mix(1, Max, Numbers),
  nth_after_zero(1000, Max, Mixed) +
    nth_after_zero(2000, Max, Mixed) +
    nth_after_zero(3000, Max, Mixed).

day_20_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  {Numbers, _} = lists:mapfoldl(
    fun (Number, Accum) ->
      {#number{value = binary_to_integer(Number) * 811589153, original_index = Accum}, Accum + 1}
    end,
    1,
    string:lexemes(Text, "\n")
  ),
  Max = length(Numbers),
  Mixed = lists:foldl(
    fun (_, NumbersSoFar) ->
      mix(1, Max, NumbersSoFar)
    end,
    Numbers,
    lists:seq(1, 10)
  ),
  nth_after_zero(1000, Max, Mixed) +
    nth_after_zero(2000, Max, Mixed) +
    nth_after_zero(3000, Max, Mixed).
