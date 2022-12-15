-module(day_15).
-export([day_15_part_1/2, day_15_part_2/2]).

-record(sensor, {sensor_pos, beacon_pos, beacon_dist}).

manhattan_distance({RowA, ColA}, {RowB, ColB}) ->
  abs(RowA - RowB) + abs(ColA - ColB).

line_parse(<<"Sensor at ", LineStr/binary>>) ->
  [
    FirstHalf,
    <<"closest beacon is at ", SecondHalf/binary>>
  ] = string:split(LineStr, ": "),
  [<<"x=", SensorCol/binary>>, <<"y=", SensorRow/binary>>] = string:split(FirstHalf, ", "),
  [<<"x=", BeaconCol/binary>>, <<"y=", BeaconRow/binary>>] = string:split(SecondHalf, ", "),
  SensorPos = {binary_to_integer(SensorRow), binary_to_integer(SensorCol)},
  BeaconPos = {binary_to_integer(BeaconRow), binary_to_integer(BeaconCol)},
  #sensor{
    sensor_pos = SensorPos,
    beacon_pos = BeaconPos,
    beacon_dist = manhattan_distance(SensorPos, BeaconPos)
  }.

informed_row_bounds(RowToCheck, #sensor{ sensor_pos = {_, SensorCol} = SensorPos, beacon_dist = BeaconDist }) ->
  OtherDist = manhattan_distance({RowToCheck, SensorCol}, SensorPos),
  Diff = BeaconDist - OtherDist,
  case Diff =< 0 of
    true -> [];
    false -> [{RowToCheck, SensorCol - Diff}, {RowToCheck, SensorCol + Diff}]
  end.

day_15_part_1(FilePath, RowToCheck) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  Sensors = lists:map(fun line_parse/1, Lines),
  MapWithSensorsAndBeacons = lists:foldl(
    fun (#sensor{ sensor_pos = SensorPos, beacon_pos = BeaconPos }, MapBeforeSensorAndBeacon) ->
      MapBeforeSensorAndBeacon#{
        SensorPos => sensor,
        BeaconPos => beacon
      }
    end,
    #{},
    Sensors
  ),
  {BoundsLeft, BoundsRight} = lists:foldl(
    fun (#sensor{} = Sensor, Accum) ->
      Bounds = informed_row_bounds(RowToCheck, Sensor),
      case Bounds of
        [] -> Accum;
        [{RowToCheck, ColLeft}, {RowToCheck, ColRight}] ->
          case Accum of
            {} -> {ColLeft, ColRight};
            {ExistingLeft, ExistingRight} ->
              { min(ColLeft, ExistingLeft), max(ColRight, ExistingRight) }
          end
      end
    end,
    {},
    Sensors
  ),
  BoundsSize = (BoundsRight - BoundsLeft) + 1,
  maps:fold(
    fun ({Row, _}, _, Accum) ->
      case Row == RowToCheck of
        false -> Accum;
        true -> Accum - 1
      end
    end,
    BoundsSize,
    MapWithSensorsAndBeacons
  ).

informed_row_bounds2(RowToCheck, #sensor{ sensor_pos = {_, SensorCol} = SensorPos, beacon_dist = BeaconDist }) ->
  OtherDist = manhattan_distance({RowToCheck, SensorCol}, SensorPos),
  Diff = BeaconDist - OtherDist,
  case Diff =< 0 of
    true -> nil;
    false -> {SensorCol - Diff, SensorCol + Diff}
  end.

can_absorb({LeftA, RightA}, {LeftB, _}) ->
  LeftB >= LeftA andalso LeftB =< (RightA + 1).

absorb({LeftA, RightA}, {LeftB, RightB}) ->
  {min(LeftA, LeftB), max(RightA, RightB)}.

find_gaps_in_row(Row, Sensors) ->
  Bounds = lists:filter(
    fun (Bounds) ->
      Bounds /= nil
    end,
    lists:map(fun (Sensor) -> informed_row_bounds2(Row, Sensor) end, Sensors)
  ),
  Sorted = lists:sort(Bounds),
  Result = lists:foldl(
    fun ({NextLeft, NextRight}, Absorbed) ->
      case Absorbed of
        {AbsorbedLeft, AbsorbedRight} ->
          case can_absorb({AbsorbedLeft, AbsorbedRight}, {NextLeft, NextRight}) of
            false -> AbsorbedRight + 1;
            true -> absorb({AbsorbedLeft, AbsorbedRight}, {NextLeft, NextRight})
          end;
        AbsorbedInt -> AbsorbedInt
      end
    end,
    hd(Sorted),
    tl(Sorted)
  ),
  case Result of
    {_, _} -> nil;
    Col -> {Row, Col}
  end.

day_15_part_2(FilePath, MaxRowAndCol) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  Sensors = lists:map(fun line_parse/1, Lines),
  [{Row, Col}] = lists:filtermap(
    fun (Row) ->
      case find_gaps_in_row(Row, Sensors) of
        nil -> false;
        Coords -> {true, Coords}
      end
    end,
    lists:seq(0, MaxRowAndCol)
  ),
  (Col * 4000000) + Row.
