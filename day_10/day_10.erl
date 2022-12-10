-module(day_10).
-export([day_10_part_1/1, day_10_part_2/1]).

-record(cpu, { cycle = 1, register = 1 }).
-record(instruction, { name, cycles, value }).

instruction_parse(<<"noop">>) ->
  #instruction{ name = noop, cycles = 1, value = 0 };
instruction_parse(<<"addx ", ValueStr/binary>>) ->
  #instruction{ name = addx, cycles = 2, value = binary_to_integer(ValueStr) }.

process([#instruction{ cycles = Cycles, value = Value } = Instr | Remaining], #cpu{ cycle = Cycle, register = Register } = Cpu) ->
  case Cycles of
    1 -> { Remaining, Cpu#cpu{ cycle = Cycle + 1, register = Register + Value } };
    Cycles -> { [ Instr#instruction{ cycles = Cycles - 1 } | Remaining ], Cpu#cpu{ cycle = Cycle + 1 } }
  end.

process_and_accumulate([], _) ->
  0;
process_and_accumulate(Instructions, #cpu{} = Cpu) ->
  { InstructionsNext, CpuNext } = process(Instructions, Cpu),
  IsKeyCycle = sets:is_element(CpuNext#cpu.cycle, sets:from_list([20, 60, 100, 140, 180, 220])),
  if
    IsKeyCycle -> (CpuNext#cpu.register * CpuNext#cpu.cycle) + process_and_accumulate(InstructionsNext, CpuNext);
    true -> process_and_accumulate(InstructionsNext, CpuNext)
  end.

day_10_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  Instructions = lists:map(fun instruction_parse/1, Lines),
  process_and_accumulate(Instructions, #cpu{}).

process_for_crt([], _) ->
  [];
process_for_crt(Instructions, #cpu{} = Cpu) ->
  CrtIndex = (Cpu#cpu.cycle - 1) rem 40,
  IsLit =
    CrtIndex == Cpu#cpu.register - 1 orelse
    CrtIndex == Cpu#cpu.register orelse
    CrtIndex == Cpu#cpu.register + 1,
  Char = if IsLit -> $#; true -> $. end,
  Chars = if CrtIndex == 39 -> [Char, $\n]; true -> [Char] end,
  { InstructionsNext, CpuNext } = process(Instructions, Cpu),
  Chars ++ process_for_crt(InstructionsNext, CpuNext).

day_10_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  Instructions = lists:map(fun instruction_parse/1, Lines),
  io:format(process_for_crt(Instructions, #cpu{}), []).
