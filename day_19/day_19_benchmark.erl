#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname day_19_benchmark

main(_) ->
  Part1Example = benchmark("Part 1 Example", 33, fun () -> day_19:day_19_part_1("./day_19_example.txt") end),
  Part1Input = benchmark("Part 1 Input", 1199, fun () -> day_19:day_19_part_1("./day_19_input.txt") end),
  Part2Input = benchmark("Part 2 Input", 3510, fun () -> day_19:day_19_part_2("./day_19_input.txt") end),
  Part2Example = benchmark("Part 2 Example", 56 * 62, fun () -> day_19:day_19_part_2("./day_19_example.txt") end),
  1199 = Part1Input,
  3510 = Part2Input,
  33 = Part1Example,
  56 * 62 = Part2Example.

benchmark(Desc, Expected, Fun) ->
  io:format("~s:\n    ", [Desc]),
  Start = erlang:system_time(seconds),
  Result = Fun(),
  End = erlang:system_time(seconds),
  io:format("\n    Took ~p seconds. Result: ~p. Correct result? ~p\n", [End - Start, Result, Result == Expected]),
  Result.
