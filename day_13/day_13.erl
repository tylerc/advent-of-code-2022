-module(day_13).
-export([day_13_part_1/1, day_13_part_2/1]).

% Thanks to https://stackoverflow.com/a/26350048/11081044
str_to_term(Str) ->
  {ok, S1, _} = erl_scan:string(binary_to_list(<<Str/binary, $.>>)),
  {ok, Term} = erl_parse:parse_term(S1),
  Term.

group_line_parse(GroupLineStr) ->
  lists:map(fun str_to_term/1, string:lexemes(GroupLineStr, "\n")).

custom_compare(A, B) when is_integer(A) andalso is_integer(B) ->
  if
    A == B -> equal;
    A < B -> correct;
    A > B -> incorrect
  end;
custom_compare(A, B) when is_list(A) andalso is_integer(B) ->
  custom_compare(A, [B]);
custom_compare(A, B) when is_integer(A) andalso is_list(B) ->
  custom_compare([A], B);
custom_compare([_ | _], []) ->
  incorrect;
custom_compare([], [_ | _]) ->
  correct;
custom_compare([], []) ->
  equal;
custom_compare([HeadA | TailA], [HeadB | TailB]) ->
  case custom_compare(HeadA, HeadB) of
    incorrect -> incorrect;
    correct -> correct;
    equal -> custom_compare(TailA, TailB)
  end.

day_13_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  GroupLineStrs = string:split(Text, "\n\n", all),
  {_, Sum} = lists:foldl(
    fun ([A, B], {Index, Sum}) ->
      SumNew = case custom_compare(A, B) of
        correct -> Sum + Index;
        incorrect -> Sum
      end,
      {Index + 1, SumNew}
    end,
    {1, 0},
    lists:map(fun group_line_parse/1, GroupLineStrs)
  ),
  Sum.

day_13_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  GroupLineStrs = string:split(<<Text/binary, "\n\n[[2]]\n[[6]]">>, "\n\n", all),
  Parsed = lists:foldl(
    fun (Line, Accum) ->
      group_line_parse(Line) ++ Accum
    end,
    [],
    GroupLineStrs
  ),
  Sorted = lists:sort(
    fun (A, B) ->
      case custom_compare(A, B) of
        correct -> true;
        incorrect -> false
      end
    end,
    Parsed
  ),
  {_, Product} = lists:foldl(
    fun (Item, {Index, Product}) ->
      ProductNew = case Item of
        [[2]] -> Product * Index;
        [[6]] -> Product * Index;
        _ -> Product
      end,
      {Index + 1, ProductNew}
    end,
    {1, 1},
    Sorted
  ),
  Product.
