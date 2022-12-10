-module(day_09).
-export([day_09_part_1/1, day_09_part_2/1]).

-record(rope, {tail = {0, 0}, head = {0, 0}, tail_visited = #{{0, 0} => 1}}).

rope_update_tail(TailPosNew, #rope{} = Rope) ->
  Rope#rope{
    tail = TailPosNew,
    tail_visited = maps:update_with(
      TailPosNew,
      fun (Count) ->
        Count + 1
      end,
      1,
      Rope#rope.tail_visited
    )
  }.

is_touching(#rope{tail = {TailX, TailY}, head = {HeadX, HeadY}}) ->
  DiffX = TailX - HeadX,
  DiffY = TailY - HeadY,
  abs(DiffX) =< 1 andalso abs(DiffY) =< 1.

tail_follow_head(Rope) ->
  RopeNext = tail_follow_head2(Rope),
  case is_touching(RopeNext) of
    true -> RopeNext;
    false -> tail_follow_head(RopeNext)
  end.

tail_follow_head2(#rope{tail = {TailX, TailY}, head = {HeadX, HeadY}} = Rope) ->
  DiffX = TailX - HeadX,
  DiffY = TailY - HeadY,
  IsTouching = is_touching(Rope),
  if
    % Tail and Head are touching:
    IsTouching -> Rope;
    % Head is left of the tail:
    DiffX > 0 andalso DiffY == 0 -> rope_update_tail({ TailX - 1, TailY }, Rope);
    % Head is right of the tail:
    DiffX < 0 andalso DiffY == 0 -> rope_update_tail({ TailX + 1, TailY }, Rope);
    % Head is above the tail:
    DiffY > 0 andalso DiffX == 0 -> rope_update_tail({ TailX, TailY - 1 }, Rope);
    % Head is below the tail:
    DiffY < 0 andalso DiffX == 0 -> rope_update_tail({ TailX, TailY + 1 }, Rope);

    % Head is diagonally top-left:
    DiffX > 0 andalso DiffY > 0 -> rope_update_tail({ TailX - 1, TailY - 1 }, Rope);
    % Head is diagonally top-right:
    DiffX < 0 andalso DiffY > 0 -> rope_update_tail({ TailX + 1, TailY - 1 }, Rope);
    % Head is diagonally bottom-right:
    DiffX < 0 andalso DiffY < 0 -> rope_update_tail({ TailX + 1, TailY + 1 }, Rope);
    % Head is diagonally bottom-left:
    DiffX > 0 andalso DiffY < 0 -> rope_update_tail({ TailX - 1, TailY + 1 }, Rope);

    true -> throw([Rope, {DiffX, DiffY}])
  end.

move(<<Direction:1/binary, $ , AmountStr/binary>>, Rope) ->
  Amount = binary_to_integer(AmountStr),
  case Direction of
    <<"R">> -> move({1, 0}, Amount, Rope);
    <<"L">> -> move({-1, 0}, Amount, Rope);
    <<"U">> -> move({0, -1}, Amount, Rope);
    <<"D">> -> move({0, 1}, Amount, Rope)
  end.

move(_, 0, Rope) ->
  Rope;
move({DirX, DirY} = Dir, Amount, #rope{head = {HeadX, HeadY}} = Rope) ->
  RopeWithHeadMoved = Rope#rope{head = { HeadX + DirX, HeadY + DirY }},
  RopeWithTailMoved = tail_follow_head(RopeWithHeadMoved),
  move(Dir, Amount - 1, RopeWithTailMoved).

day_09_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  RopeAtEnd = lists:foldl(
    fun move/2,
    #rope{},
    Lines
  ),
  maps:size(RopeAtEnd#rope.tail_visited).

propagate([HeadRope | OtherRopes]) ->
  {RopesNew, _} = lists:mapfoldl(
    fun (KnotCurrent, KnotAhead) ->
      KnotCurrentWithMovedHead = KnotCurrent#rope{
        head = KnotAhead#rope.tail
      },
      KnotCurrentWithMovedTail = tail_follow_head(KnotCurrentWithMovedHead),
      {KnotCurrentWithMovedTail, KnotCurrentWithMovedTail}
    end,
    HeadRope,
    OtherRopes
  ),
  [HeadRope | RopesNew].

move_and_propagate(<<Direction:1/binary, $ , AmountStr/binary>>, Ropes) ->
  Amount = binary_to_integer(AmountStr),
  case Direction of
    <<"R">> -> move_and_propagate({1, 0}, Amount, Ropes);
    <<"L">> -> move_and_propagate({-1, 0}, Amount, Ropes);
    <<"U">> -> move_and_propagate({0, -1}, Amount, Ropes);
    <<"D">> -> move_and_propagate({0, 1}, Amount, Ropes)
  end.

move_and_propagate(_, 0, Ropes) ->
  Ropes;
move_and_propagate({DirX, DirY} = Dir, Amount, [#rope{head = {HeadX, HeadY}} = Rope | OtherRopes]) ->
  RopeWithHeadMoved = Rope#rope{head = { HeadX + DirX, HeadY + DirY }},
  RopeWithTailMoved = tail_follow_head(RopeWithHeadMoved),
  RopesMoved = propagate([RopeWithTailMoved | OtherRopes]),
  move_and_propagate(Dir, Amount - 1, RopesMoved).

day_09_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  RopesAtEnd = lists:foldl(
    fun move_and_propagate/2,
    [#rope{}, #rope{}, #rope{}, #rope{}, #rope{}, #rope{}, #rope{}, #rope{}, #rope{}],
    Lines
  ),
  TailKnot = lists:last(RopesAtEnd),
  maps:size(TailKnot#rope.tail_visited).
