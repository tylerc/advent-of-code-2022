-module(day_25).
-export([day_25_part_1/1]).

snafu_parse(Str) ->
  floor(snafu_parse(byte_size(Str) - 1, Str)).

snafu_parse(_, <<>>) ->
  0;
snafu_parse(PlaceIndex, <<H:1/binary, Rest/binary>>) ->
  PlaceMultiplier = math:pow(5, PlaceIndex),
  DigitMultiplier = case H of
    <<"2">> -> 2;
    <<"1">> -> 1;
    <<"0">> -> 0;
    <<"-">> -> -1;
    <<"=">> -> -2
  end,
  (PlaceMultiplier * DigitMultiplier) + snafu_parse(PlaceIndex - 1, Rest).

% TODO: 0. Seems like this handles 0 wrong? (or am I missing something?):
max_with_digits(-1) ->
  0;
max_with_digits(Digits) ->
  (math:pow(5, Digits) * 2) + max_with_digits(Digits - 1).

digits_required(Digits, TargetNumber) ->
  case abs(TargetNumber) =< max_with_digits(Digits) of
    true -> Digits;
    false -> digits_required(Digits + 1, TargetNumber)
  end.

decimal_to_snafu(Number) ->
  decimal_to_snafu(digits_required(1, Number), Number).

% Not the most efficient algorithm... But it is reliable!
decimal_to_snafu(-1, 0) ->
  [];
decimal_to_snafu(-1, _) ->
  [failed];
decimal_to_snafu(PlaceIndex, Number) ->
  PlaceMultiplier = floor(math:pow(5, PlaceIndex)),
  Twice = PlaceMultiplier * 2,
  Variance = max_with_digits(PlaceIndex - 1),
  case Number - Twice - Variance =< 0 andalso Number + Twice + Variance >= 0 of
    false -> [failed];
    true ->
      lists:foldl(
        fun ({Change, Digit}, Accum) ->
          case Accum of
            [failed] ->
              case decimal_to_snafu(PlaceIndex - 1, Number - Change) of
                [failed] -> Accum;
                Result ->
                  [Digit | Result]
              end;
            _ -> Accum
          end
        end,
        [failed],
        [
          {Twice, "2"}, {PlaceMultiplier, "1"}, {0, "0"}, {-PlaceMultiplier, "-"}, {-Twice, "="}
        ]
      )
  end.

day_25_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  DecimalNumbers = lists:map(fun snafu_parse/1, string:lexemes(Text, "\n")),
  Sum = lists:foldl(fun erlang:'+'/2, 0, DecimalNumbers),
  io:format("~s\n", [decimal_to_snafu(Sum)]).
