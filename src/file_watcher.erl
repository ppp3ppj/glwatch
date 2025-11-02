-module(file_watcher).
-export([start_watching/1, get_events/1, stop_watching/1]).

-include_lib("kernel/include/file.hrl").

% Professional file watcher with clean logging
start_watching(Directory) ->
    io:format("[INFO] Starting file watcher~n"),

    % Make sure we have a good directory
    Dir = case Directory of
        Bin when is_binary(Bin) -> binary_to_list(Bin);
        List when is_list(List) -> List;
        _ -> "watched/"
    end,

    AbsDir = filename:absname(Dir),
    ensure_directory(AbsDir),

    io:format("[INFO] Watching directory: ~s~n", [AbsDir]),

    % Get initial file state
    InitialFiles = get_all_files(AbsDir),
    io:format("[INFO] Found ~p initial files~n", [map_size(InitialFiles)]),

    % Start watcher process
    Pid = spawn_link(fun() ->
        watch_loop(AbsDir, InitialFiles, [])
    end),

    io:format("[INFO] File watcher started successfully~n"),
    {watcher_ref, pid_to_list(Pid)}.

% Main watch loop
watch_loop(Directory, OldFiles, EventBuffer) ->
    % Scan current files
    NewFiles = get_all_files(Directory),

    % Detect changes
    Events = find_changes(OldFiles, NewFiles),

    % Add new events to buffer
    AllEvents = EventBuffer ++ Events,

    % Log events when found
    case Events of
        [] -> ok;
        _ ->
            io:format("~n[EVENTS] Detected ~p file system changes:~n", [length(Events)]),
            lists:foreach(fun print_event/1, Events)
    end,

    % Handle messages
    receive
        {get_events, From} ->
            %io:format("[DEBUG] Sending ~p events to client~n", [length(AllEvents)]),
            From ! {events, AllEvents},
            watch_loop(Directory, NewFiles, []);  % Clear buffer
        stop ->
            io:format("[INFO] File watcher stopped~n")
    after 200 ->  % Fast polling for immediate detection
        watch_loop(Directory, NewFiles, AllEvents)
    end.

% Get all files with their metadata
get_all_files(Directory) ->
    case file:list_dir(Directory) of
        {ok, Files} ->
            maps:from_list([
                {FilePath, {MTime, Size, IsDir}} ||
                File <- Files,
                FilePath <- [filename:join(Directory, File)],
                not is_temp_file(File),  % Skip temporary files
                {ok, FileInfo} <- [file:read_file_info(FilePath)],
                MTime <- [FileInfo#file_info.mtime],
                Size <- [FileInfo#file_info.size],
                IsDir <- [FileInfo#file_info.type =:= directory]
            ]);
        {error, Reason} ->
            io:format("[ERROR] Failed to list directory ~s: ~p~n", [Directory, Reason]),
            #{}
    end.

% Detect all types of file system changes
find_changes(OldFiles, NewFiles) ->
    OldPaths = sets:from_list(maps:keys(OldFiles)),
    NewPaths = sets:from_list(maps:keys(NewFiles)),

    % Created files (truly new files)
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

    % Create events with improved logging
    CreatedEvents = [begin
                        io:format("[DEBUG] File truly created: ~s~n", [Path]),
                        {"created", Path, element(3, maps:get(Path, NewFiles)), Timestamp}
                     end || Path <- Created],
    DeletedEvents = [{"deleted", Path, false, Timestamp} || Path <- Deleted],
    ModifiedEvents = [begin
                         io:format("[DEBUG] File modified: ~s~n", [Path]),
                         {"modified", Path, false, Timestamp}
                      end || Path <- Modified],

    CreatedEvents ++ DeletedEvents ++ ModifiedEvents.

% Check if file is temporary (skip these)
is_temp_file(Filename) ->
    lists:any(fun(Pattern) ->
        case Pattern of
            "~" -> lists:suffix("~", Filename);
            _ -> string:str(Filename, Pattern) > 0
        end
    end, ["~", ".swp", ".tmp", ".DS_Store", "Thumbs.db", ".#"]).

% Print event with clean formatting
print_event({"created", Path, IsDir, _Time}) ->
    Type = if IsDir -> "DIRECTORY"; true -> "FILE" end,
    io:format("  [CREATE] ~s: ~s~n", [Type, filename:basename(Path)]);
print_event({"deleted", Path, IsDir, _Time}) ->
    Type = if IsDir -> "DIRECTORY"; true -> "FILE" end,
    io:format("  [DELETE] ~s: ~s~n", [Type, filename:basename(Path)]);
print_event({"modified", Path, _IsDir, _Time}) ->
    io:format("  [MODIFY] FILE: ~s~n", [filename:basename(Path)]).

% Get events from watcher
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
            false ->
                io:format("[WARN] Watcher process is not alive~n"),
                []
        end
    catch _:_ ->
        io:format("[ERROR] Invalid watcher reference~n"),
        []
    end.

% Stop watching
stop_watching({watcher_ref, PidStr}) ->
    try
        Pid = list_to_pid(PidStr),
        Pid ! stop,
        io:format("[INFO] Stop signal sent to watcher~n")
    catch _:_ ->
        io:format("[WARN] Failed to stop watcher~n"),
        ok
    end,
    ok.

% Helper to ensure directory exists
ensure_directory(Dir) ->
    case filelib:is_dir(Dir) of
        true -> ok;
        false ->
            case file:make_dir(Dir) of
                ok -> io:format("[INFO] Created directory: ~s~n", [Dir]);
                {error, Reason} -> io:format("[ERROR] Failed to create directory ~s: ~p~n", [Dir, Reason])
            end
    end.
