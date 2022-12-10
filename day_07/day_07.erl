-module(day_07).
-export([day_07_part_1/1, day_07_part_2/1]).

-record(entry, {path, type, size}).

line_parse(<<"$ cd ", Path/binary>>) ->
  case Path of
    <<"/">> -> {cd, root};
    <<"..">> -> {cd, up};
    _ -> {cd, Path}
  end;
line_parse(<<"$ ls">>) ->
  {ls};
line_parse(<<"dir ", DirName/binary>>) ->
  {dir, DirName};
line_parse(Line) ->
  [SizeStr, FileName] = string:split(Line, " "),
  {file, FileName, binary_to_integer(SizeStr)}.

listing_build([Line | Rest], Path) ->
  case line_parse(Line) of
    {cd, root} -> listing_build(Rest, []);
    {cd, up} -> listing_build(Rest, lists:droplast(Path));
    {cd, DirNew} -> listing_build(Rest, Path ++ [DirNew]);
    {ls} -> listing_build(Rest, Path);
    {dir, DirName} ->
      [
        #entry{path=Path ++ [DirName], type=dir, size=0}
        | listing_build(Rest, Path)
      ];
    {file, FileName, Size} ->
      [
        #entry{path=Path ++ [FileName], type=file, size=Size}
        | listing_build(Rest, Path)
      ]
  end;
listing_build([], _) ->
  [].

dir_size(DirPath, [#entry{path=Path, size=Size} | Rest]) ->
  case lists:prefix(DirPath, Path) of
    true -> Size + dir_size(DirPath, Rest);
    false -> 0
  end;
dir_size(_, []) ->
  0.

dir_sizes([#entry{type=file} | Rest]) ->
  dir_sizes(Rest);
dir_sizes([#entry{type=dir, path=Path} = Entry | Rest]) ->
  [Entry#entry{size=dir_size(Path, Rest)} | dir_sizes(Rest)];
dir_sizes([]) ->
  [].

day_07_part_1(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  Entries = lists:sort(listing_build(Lines, [])),
  DirSizes = dir_sizes(Entries),
  lists:foldl(
    fun (#entry{size=Size}, Accum) ->
      if
        Size =< 100000 -> Accum + Size;
        true -> Accum
      end
    end,
    0,
    DirSizes
  ).

day_07_part_2(FilePath) ->
  {ok, Text} = file:read_file(FilePath),
  Lines = string:lexemes(Text, "\n"),
  Entries = lists:sort(listing_build(Lines, [])),
  DirSizes = dir_sizes(Entries),
  DirSizesSorted = lists:keysort(#entry.size, DirSizes),
  SpaceInUse = dir_size([], Entries),
  SpaceFree = 70000000 - SpaceInUse,
  SpaceNeeded = 30000000,
  SpaceToDelete = SpaceNeeded - SpaceFree,
  {value, #entry{size=SizeOfDeletedFolder}} = lists:search(
    fun (#entry{size=Size}) ->
      Size > SpaceToDelete
    end,
    DirSizesSorted
  ),
  SizeOfDeletedFolder.
