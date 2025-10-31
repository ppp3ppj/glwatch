-module(file_watcher).
-export([start_watching/1, get_events/1, stop_watching/1]).

-include_lib("kernel/include/file.hrl").

-record(watcher_state, {
    directory :: string(),
    current_files :: list(),  % Current snapshot of all files
    processed_events :: list(), % Events we've already reported
    watch_nested :: boolean()   % Whether to watch nested dirs
}).

-record(file_event, {
    type :: atom(),      % created, deleted, modified
    path :: string(),    % Full file path
    is_directory :: boolean(),
    timestamp :: integer()
}).

% Start watching a directory (with nested directory support)
start_watching(Directory) ->
    start_watching(Directory, true). % Default: watch nested dirs

start_watching(Directory, WatchNested) ->
    io:format("🔮 Erlang: Starting enhanced file watcher for ~s~n", [Directory]),
    io:format("📁 Nested directory monitoring: ~p~n", [WatchNested]),

    % Ensure directory exists
    case filelib:is_dir(Directory) of
        true -> ok;
        false ->
            io:format("📁 Creating directory: ~s~n", [Directory]),
            file:make_dir(Directory)
    end,

    % Get initial file snapshot
    InitialFiles = scan_directory(Directory, WatchNested),
    io:format("📊 Initial scan found ~p files~n", [length(InitialFiles)]),

    % Spawn watcher process
    Pid = spawn(fun() ->
        enhanced_watch_loop(#watcher_state{
            directory = Directory,
            current_files = InitialFiles,
            processed_events = [],
            watch_nested = WatchNested
        })
    end),

    % Return watcher reference
    {watcher_ref, Pid}.

% Get all file events (created, deleted, modified)
get_events({watcher_ref, Pid}) ->
    Pid ! {get_events, self()},
    receive
        {events, Events} ->
            case Events of
                [] -> [];
                _ ->
                    io:format("📡 Erlang detected ~p file events~n", [length(Events)]),
                    Events
            end
    after 2000 ->
        []
    end.

% Stop watching
stop_watching({watcher_ref, Pid}) ->
    Pid ! stop,
    ok.

% Enhanced watch loop with delete detection
enhanced_watch_loop(State) ->
    #watcher_state{
        directory = Dir,
        current_files = CurrentFiles,
        processed_events = ProcessedEvents,
        watch_nested = WatchNested
    } = State,

    % Scan directory for current state
    NewScan = scan_directory(Dir, WatchNested),

    % Detect all types of changes
    Events = detect_all_changes(CurrentFiles, NewScan),

    % Filter out already processed events
    NewEvents = Events -- ProcessedEvents,

    % Handle requests
    receive
        {get_events, From} ->
            From ! {events, NewEvents},
            % Mark events as processed and update file list
            UpdatedProcessedEvents = ProcessedEvents ++ NewEvents,
            enhanced_watch_loop(State#watcher_state{
                current_files = NewScan,
                processed_events = UpdatedProcessedEvents
            });
        stop ->
            io:format("🛑 Enhanced Erlang watcher stopped~n"),
            ok
    after 1000 ->
        % Continue watching
        enhanced_watch_loop(State#watcher_state{
            current_files = NewScan
        })
    end.

% Scan directory recursively or non-recursively
scan_directory(Directory, WatchNested) ->
    case WatchNested of
        true -> scan_recursive(Directory);
        false -> scan_single_level(Directory)
    end.

% Scan single directory level only
scan_single_level(Directory) ->
    case file:list_dir(Directory) of
        {ok, Files} ->
            lists:foldl(fun(File, Acc) ->
                FilePath = filename:join(Directory, File),
                case is_supported_file(FilePath) of
                    true ->
                        case file:read_file_info(FilePath) of
                            {ok, FileInfo} ->
                                [{FilePath, FileInfo#file_info.mtime,
                                  FileInfo#file_info.type =:= directory} | Acc];
                            _ -> Acc
                        end;
                    false -> Acc
                end
            end, [], Files);
        {error, _} -> []
    end.

% Scan directory recursively (nested directories)
scan_recursive(Directory) ->
    scan_recursive(Directory, []).

scan_recursive(Directory, Acc) ->
    case file:list_dir(Directory) of
        {ok, Files} ->
            lists:foldl(fun(File, CurrentAcc) ->
                FilePath = filename:join(Directory, File),
                case file:read_file_info(FilePath) of
                    {ok, FileInfo} ->
                        IsDir = FileInfo#file_info.type =:= directory,
                        NewAcc = case is_supported_file(FilePath) orelse IsDir of
                            true ->
                                [{FilePath, FileInfo#file_info.mtime, IsDir} | CurrentAcc];
                            false -> CurrentAcc
                        end,
                        % Recursively scan subdirectories
                        case IsDir of
                            true -> scan_recursive(FilePath, NewAcc);
                            false -> NewAcc
                        end;
                    _ -> CurrentAcc
                end
            end, Acc, Files);
        {error, _} -> Acc
    end.

% Detect all types of file changes
detect_all_changes(OldFiles, NewFiles) ->
    % Convert to maps for easier comparison
    OldMap = lists:foldl(fun({Path, MTime, IsDir}, Acc) ->
        maps:put(Path, {MTime, IsDir}, Acc)
    end, #{}, OldFiles),

    NewMap = lists:foldl(fun({Path, MTime, IsDir}, Acc) ->
        maps:put(Path, {MTime, IsDir}, Acc)
    end, #{}, NewFiles),

    % Get all paths
    AllPaths = sets:to_list(sets:union(
        sets:from_list(maps:keys(OldMap)),
        sets:from_list(maps:keys(NewMap))
    )),

    % Detect changes for each path
    Timestamp = erlang:system_time(millisecond),
    lists:foldl(fun(Path, Events) ->
        case {maps:get(Path, OldMap, undefined),
              maps:get(Path, NewMap, undefined)} of
            {undefined, {_MTime, IsDir}} ->
                % File/directory was created
                Event = #file_event{
                    type = created,
                    path = Path,
                    is_directory = IsDir,
                    timestamp = Timestamp
                },
                [Event | Events];

            {{_OldMTime, IsDir}, undefined} ->
                % File/directory was deleted
                Event = #file_event{
                    type = deleted,
                    path = Path,
                    is_directory = IsDir,
                    timestamp = Timestamp
                },
                [Event | Events];

            {{OldMTime, IsDir}, {NewMTime, IsDir}} when OldMTime =/= NewMTime ->
                % File was modified (but only for files, not directories)
                case IsDir of
                    false ->
                        Event = #file_event{
                            type = modified,
                            path = Path,
                            is_directory = false,
                            timestamp = Timestamp
                        },
                        [Event | Events];
                    true -> Events % Don't report directory modifications
                end;

            _ ->
                % No change
                Events
        end
    end, [], AllPaths).

% Check if file type is supported
is_supported_file(FilePath) ->
    Extension = filename:extension(FilePath),
    SupportedExtensions = [".txt", ".rs", ".gleam", ".py", ".hs", ".js",
                          ".java", ".go", ".c", ".cpp", ".h", ".md",
                          ".json", ".toml", ".yaml", ".yml", ".erl", ".ex"],
    lists:member(Extension, SupportedExtensions).

% Helper function to format file events for debugging
format_event(#file_event{type = Type, path = Path, is_directory = IsDir}) ->
    TypeStr = case Type of
        created -> "CREATED";
        deleted -> "DELETED";
        modified -> "MODIFIED"
    end,
    ItemType = case IsDir of
        true -> "DIR";
        false -> "FILE"
    end,
    io_lib:format("~s ~s: ~s", [TypeStr, ItemType, Path]).
