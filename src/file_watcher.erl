-module(file_watcher).
-export([start_watching/1, get_events/1, stop_watching/1]).

% Include the file_info record definition
-include_lib("kernel/include/file.hrl").

-record(watcher_state, {
    directory :: string(),
    current_files :: map()
}).

% Simple, clean file watcher
start_watching(Directory) ->
    io:format("🔮 Starting file watcher for: ~s~n", [Directory]),

    % Ensure directory exists
    AbsDir = filename:absname(Directory),
    ensure_directory(AbsDir),

    % Get initial files
    InitialFiles = scan_files(AbsDir),
    io:format("📊 Found ~p initial files~n", [map_size(InitialFiles)]),

    % Start watcher process
    Pid = spawn_link(fun() ->
        watch_loop(#watcher_state{
            directory = AbsDir,
            current_files = InitialFiles
        })
    end),

    {watcher_ref, pid_to_list(Pid)}.

% Get events from watcher
get_events({watcher_ref, PidStr}) ->
    try
        Pid = list_to_pid(PidStr),
        case is_process_alive(Pid) of
            true ->
                Pid ! {get_events, self()},
                receive
                    {events, Events} ->
                        case Events of
                            [] -> [];
                            _ ->
                                io:format("📡 Returning ~p events~n", [length(Events)]),
                                Events
                        end
                after 1000 -> []
                end;
            false -> []
        end
    catch
        _:_ -> []
    end.

% Stop watcher
stop_watching({watcher_ref, PidStr}) ->
    try
        Pid = list_to_pid(PidStr),
        Pid ! stop
    catch
        _:_ -> ok
    end,
    ok.

% Main watch loop - CLEAN VERSION
watch_loop(State) ->
    #watcher_state{directory = Dir, current_files = OldFiles} = State,

    % Scan for new files
    NewFiles = scan_files(Dir),

    % Detect changes
    Events = detect_changes(OldFiles, NewFiles),

    % Print changes if any
    case Events of
        [] -> ok;
        _ ->
            io:format("~n🔥 DETECTED CHANGES:~n"),
            [print_event(E) || E <- Events],
            io:format("~n")
    end,

    % Handle messages
    receive
        {get_events, From} ->
            From ! {events, Events},
            watch_loop(State#watcher_state{current_files = NewFiles});
        stop ->
            io:format("🛑 Watcher stopped~n")
    after 500 ->
        watch_loop(State#watcher_state{current_files = NewFiles})
    end.

% Scan directory for files - SIMPLE VERSION with proper file_info usage
scan_files(Directory) ->
    case file:list_dir(Directory) of
        {ok, Files} ->
            lists:foldl(fun(File, Acc) ->
                FilePath = filename:join(Directory, File),
                case file:read_file_info(FilePath) of
                    {ok, FileInfo} ->
                        % Use the proper file_info record fields
                        IsDir = FileInfo#file_info.type =:= directory,
                        MTime = FileInfo#file_info.mtime,
                        maps:put(FilePath, {MTime, IsDir}, Acc);
                    {error, _Reason} ->
                        % Skip files we can't read
                        Acc
                end
            end, #{}, Files);
        {error, _Reason} ->
            % Return empty map if we can't list directory
            #{}
    end.

% Detect file changes - CLEAN VERSION
detect_changes(OldFiles, NewFiles) ->
    OldPaths = sets:from_list(maps:keys(OldFiles)),
    NewPaths = sets:from_list(maps:keys(NewFiles)),

    % Find created and deleted files
    Created = sets:to_list(sets:subtract(NewPaths, OldPaths)),
    Deleted = sets:to_list(sets:subtract(OldPaths, NewPaths)),

    Timestamp = erlang:system_time(millisecond),

    % Create events as tuples
    CreatedEvents = [{"created", Path, get_is_dir(Path, NewFiles), Timestamp} || Path <- Created],
    DeletedEvents = [{"deleted", Path, get_is_dir(Path, OldFiles), Timestamp} || Path <- Deleted],

    CreatedEvents ++ DeletedEvents.

% Helper functions
get_is_dir(Path, FileMap) ->
    case maps:get(Path, FileMap, undefined) of
        {_MTime, IsDir} -> IsDir;
        _ -> false
    end.

ensure_directory(Dir) ->
    case filelib:is_dir(Dir) of
        true -> ok;
        false ->
            file:make_dir(Dir),
            io:format("📁 Created directory: ~s~n", [Dir])
    end.

print_event({"created", Path, IsDir, _Time}) ->
    Type = case IsDir of true -> "DIR"; false -> "FILE" end,
    io:format("  🆕 CREATED ~s: ~s~n", [Type, filename:basename(Path)]);
print_event({"deleted", Path, IsDir, _Time}) ->
    Type = case IsDir of true -> "DIR"; false -> "FILE" end,
    io:format("  🗑️ DELETED ~s: ~s~n", [Type, filename:basename(Path)]).
