-module(file_watcher).
-export([start_watching/1, get_events/1, stop_watching/1]).

-include_lib("kernel/include/file.hrl").

% Simple Halloween file watcher that ACTUALLY WORKS! 🎃
start_watching(Directory) ->
    io:format("🎃 Starting SIMPLE Halloween file watcher~n"),

    % Make sure we have a good directory
    Dir = case Directory of
        Bin when is_binary(Bin) -> binary_to_list(Bin);
        List when is_list(List) -> List;
        _ -> "watched/"
    end,

    AbsDir = filename:absname(Dir),
    ensure_directory(AbsDir),

    io:format("👻 Watching: ~s~n", [AbsDir]),

    % Get initial file state
    InitialFiles = get_all_files(AbsDir),
    io:format("📊 Found ~p initial files~n", [map_size(InitialFiles)]),

    % Start simple watcher process
    Pid = spawn_link(fun() ->
        simple_watch_loop(AbsDir, InitialFiles, [])
    end),

    io:format("✅ Simple watcher started!~n"),
    {watcher_ref, pid_to_list(Pid)}.

% Simple watch loop that actually works
simple_watch_loop(Directory, OldFiles, EventBuffer) ->
    % Scan current files
    NewFiles = get_all_files(Directory),

    % Detect changes
    Events = find_changes(OldFiles, NewFiles),

    % Add new events to buffer
    AllEvents = EventBuffer ++ Events,

    % Print events immediately when found
    case Events of
        [] -> ok;
        _ ->
            io:format("~n🔥 DETECTED ~p CHANGES:~n", [length(Events)]),
            lists:foreach(fun print_event/1, Events)
    end,

    % Handle messages
    receive
        {get_events, From} ->
            io:format("📡 Sending ~p events to Gleam~n", [length(AllEvents)]),
            From ! {events, AllEvents},
            simple_watch_loop(Directory, NewFiles, []);  % Clear buffer
        stop ->
            io:format("🛑 Simple watcher stopped~n")
    after 200 ->  % Fast polling for immediate detection
        simple_watch_loop(Directory, NewFiles, AllEvents)
    end.

% Get all files with their info
get_all_files(Directory) ->
    case file:list_dir(Directory) of
        {ok, Files} ->
            maps:from_list([
                {FilePath, {MTime, Size, IsDir}} ||
                File <- Files,
                FilePath <- [filename:join(Directory, File)],
                not is_temp_file(File),  % Skip temp files
                {ok, FileInfo} <- [file:read_file_info(FilePath)],
                MTime <- [FileInfo#file_info.mtime],
                Size <- [FileInfo#file_info.size],
                IsDir <- [FileInfo#file_info.type =:= directory]
            ]);
        _ -> #{}
    end.

% Detect all types of changes
find_changes(OldFiles, NewFiles) ->
    OldPaths = sets:from_list(maps:keys(OldFiles)),
    NewPaths = sets:from_list(maps:keys(NewFiles)),

    % Created files
    Created = sets:to_list(sets:subtract(NewPaths, OldPaths)),

    % Deleted files
    Deleted = sets:to_list(sets:subtract(OldPaths, NewPaths)),

    % Modified files (same path, different mtime or size)
    CommonPaths = sets:to_list(sets:intersection(OldPaths, NewPaths)),
    Modified = [Path || Path <- CommonPaths,
                       maps:get(Path, OldFiles) =/= maps:get(Path, NewFiles),
                       not element(3, maps:get(Path, NewFiles))  % Not directory
               ],

    Timestamp = erlang:system_time(millisecond),

    % Create events
    CreatedEvents = [{"created", Path, element(3, maps:get(Path, NewFiles)), Timestamp} || Path <- Created],
    DeletedEvents = [{"deleted", Path, false, Timestamp} || Path <- Deleted],
    ModifiedEvents = [{"modified", Path, false, Timestamp} || Path <- Modified],

    CreatedEvents ++ DeletedEvents ++ ModifiedEvents.

% Check if file is temporary (skip these)
is_temp_file(Filename) ->
    lists:any(fun(Pattern) ->
        case Pattern of
            "~" -> lists:suffix("~", Filename);
            _ -> string:str(Filename, Pattern) > 0
        end
    end, ["~", ".swp", ".tmp", ".DS_Store", "Thumbs.db", ".#"]).

% Print event nicely
print_event({"created", Path, IsDir, _Time}) ->
    Type = if IsDir -> "DIR "; true -> "FILE" end,
    io:format("  🆕 CREATED ~s: ~s~n", [Type, filename:basename(Path)]);
print_event({"deleted", Path, IsDir, _Time}) ->
    Type = if IsDir -> "DIR "; true -> "FILE" end,
    io:format("  🗑️ DELETED ~s: ~s~n", [Type, filename:basename(Path)]);
print_event({"modified", Path, _IsDir, _Time}) ->
    io:format("  📝 MODIFIED FILE: ~s~n", [filename:basename(Path)]).

% Get events
get_events({watcher_ref, PidStr}) ->
    try
        Pid = list_to_pid(PidStr),
        case is_process_alive(Pid) of
            true ->
                Pid ! {get_events, self()},
                receive
                    {events, Events} -> Events
                after 1000 -> []
                end;
            false -> []
        end
    catch _:_ -> []
    end.

% Stop watching
stop_watching({watcher_ref, PidStr}) ->
    try
        Pid = list_to_pid(PidStr),
        Pid ! stop
    catch _:_ -> ok
    end,
    ok.

% Helper
ensure_directory(Dir) ->
    case filelib:is_dir(Dir) of
        true -> ok;
        false ->
            file:make_dir(Dir),
            io:format("📁 Created directory: ~s~n", [Dir])
    end.
