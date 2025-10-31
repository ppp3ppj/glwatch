-module(file_watcher).
-export([start_watching/1, get_events/1, stop_watching/1]).

-include_lib("kernel/include/file.hrl").

-record(watcher_state, {
    directory :: string(),
    current_files :: map(),     % Use map for better performance
    last_check :: integer(),    % Track last check time
    watch_nested :: boolean()
}).

% File event record that matches Gleam expectations
-record(file_event, {
    event_type :: atom(),       % created, deleted, modified
    path :: string(),
    is_directory :: boolean(),
    timestamp :: integer()
}).

% Start watching a directory
start_watching(Directory) ->
    io:format("🔮 Erlang: Starting file watcher for ~s~n", [Directory]),

    % Ensure directory exists
    case filelib:is_dir(Directory) of
        true -> ok;
        false ->
            io:format("📁 Creating directory: ~s~n", [Directory]),
            file:make_dir(Directory)
    end,

    % Get initial file snapshot
    InitialFiles = scan_directory_to_map(Directory),
    io:format("📊 Initial scan found ~p files~n", [map_size(InitialFiles)]),

    % Spawn watcher process
    Pid = spawn(fun() ->
        watch_loop(#watcher_state{
            directory = Directory,
            current_files = InitialFiles,
            last_check = erlang:system_time(millisecond),
            watch_nested = true
        })
    end),

    % Return watcher reference as string (for Gleam compatibility)
    {watcher_ref, pid_to_list(Pid)}.

% Get all file events
get_events({watcher_ref, PidStr}) when is_list(PidStr) ->
    Pid = list_to_pid(PidStr),
    case is_process_alive(Pid) of
        true ->
            Pid ! {get_events, self()},
            receive
                {events, Events} ->
                    case Events of
                        [] ->
                            io:format("📡 No events detected~n"),
                            [];
                        _ ->
                            io:format("📡 Erlang detected ~p file events~n", [length(Events)]),
                            % Convert events to format Gleam expects
                            convert_events_for_gleam(Events)
                    end
            after 3000 ->
                io:format("⚠️ Timeout waiting for events~n"),
                []
            end;
        false ->
            io:format("⚠️ Watcher process is dead~n"),
            []
    end;
get_events({watcher_ref, Pid}) when is_pid(Pid) ->
    get_events({watcher_ref, pid_to_list(Pid)}).

% Stop watching
stop_watching({watcher_ref, PidStr}) when is_list(PidStr) ->
    Pid = list_to_pid(PidStr),
    case is_process_alive(Pid) of
        true -> Pid ! stop;
        false -> ok
    end,
    ok;
stop_watching({watcher_ref, Pid}) when is_pid(Pid) ->
    stop_watching({watcher_ref, pid_to_list(Pid)}).

% Main watch loop
watch_loop(State) ->
    #watcher_state{
        directory = Dir,
        current_files = CurrentFiles,
        watch_nested = WatchNested
    } = State,

    % Small delay to prevent excessive CPU usage
    timer:sleep(500),

    % Scan directory for current state
    NewFiles = scan_directory_to_map(Dir),

    % Detect changes
    Events = detect_file_changes(CurrentFiles, NewFiles),

    % Handle messages
    NewState = receive
        {get_events, From} ->
            From ! {events, Events},
            State#watcher_state{current_files = NewFiles}
    after 0 ->
        % No message, continue with current state
        State#watcher_state{current_files = NewFiles}
    end,

    % Handle stop message
    receive
        stop ->
            io:format("🛑 Erlang watcher stopped~n"),
            ok
    after 0 ->
        watch_loop(NewState)
    end.

% Scan directory and return as map
scan_directory_to_map(Directory) ->
    Files = scan_recursive(Directory),
    lists:foldl(fun({Path, MTime, IsDir}, Acc) ->
        maps:put(Path, {MTime, IsDir}, Acc)
    end, #{}, Files).

% Scan directory recursively
scan_recursive(Directory) ->
    case file:list_dir(Directory) of
        {ok, Files} ->
            lists:foldl(fun(File, Acc) ->
                FilePath = filename:join(Directory, File),
                case file:read_file_info(FilePath) of
                    {ok, FileInfo} ->
                        IsDir = FileInfo#file_info.type =:= directory,
                        MTime = FileInfo#file_info.mtime,

                        % Add this file/dir to results
                        NewAcc = [{FilePath, MTime, IsDir} | Acc],

                        % If it's a directory, scan recursively
                        case IsDir of
                            true -> scan_recursive(FilePath) ++ NewAcc;
                            false -> NewAcc
                        end;
                    {error, Reason} ->
                        io:format("⚠️ Error reading ~s: ~p~n", [FilePath, Reason]),
                        Acc
                end
            end, [], Files);
        {error, Reason} ->
            io:format("⚠️ Error listing directory ~s: ~p~n", [Directory, Reason]),
            []
    end.

% Detect file changes between old and new snapshots
detect_file_changes(OldFiles, NewFiles) ->
    Timestamp = erlang:system_time(millisecond),

    % Get all unique paths
    OldPaths = sets:from_list(maps:keys(OldFiles)),
    NewPaths = sets:from_list(maps:keys(NewFiles)),
    AllPaths = sets:union(OldPaths, NewPaths),

    Events = sets:fold(fun(Path, Acc) ->
        OldInfo = maps:get(Path, OldFiles, undefined),
        NewInfo = maps:get(Path, NewFiles, undefined),

        case {OldInfo, NewInfo} of
            {undefined, {_MTime, IsDir}} ->
                % File created
                Event = #file_event{
                    event_type = created,
                    path = Path,
                    is_directory = IsDir,
                    timestamp = Timestamp
                },
                [Event | Acc];

            {{_OldMTime, IsDir}, undefined} ->
                % File deleted
                Event = #file_event{
                    event_type = deleted,
                    path = Path,
                    is_directory = IsDir,
                    timestamp = Timestamp
                },
                [Event | Acc];

            {{OldMTime, IsDir}, {NewMTime, IsDir}} when OldMTime =/= NewMTime, IsDir =:= false ->
                % File modified (only report for files, not directories)
                Event = #file_event{
                    event_type = modified,
                    path = Path,
                    is_directory = false,
                    timestamp = Timestamp
                },
                [Event | Acc];

            _ ->
                % No change or directory timestamp change (ignore)
                Acc
        end
    end, [], AllPaths),

    % Debug output
    case Events of
        [] -> ok;
        _ ->
            io:format("🔍 Detected events:~n"),
            [io:format("  ~s~n", [format_event(E)]) || E <- Events]
    end,

    Events.

% Convert events for Gleam compatibility
% Convert events for Gleam compatibility - STRING VERSION
convert_events_for_gleam(Events) ->
    lists:map(fun(#file_event{event_type = Type, path = Path, is_directory = IsDir, timestamp = Timestamp}) ->
        % Convert atom to string
        TypeStr = case Type of
            created -> "created";
            deleted -> "deleted";
            modified -> "modified"
        end,

        % Return tuple with string type
        {TypeStr, Path, IsDir, Timestamp}
    end, Events).

% Helper function to format events for debugging
format_event(#file_event{event_type = Type, path = Path, is_directory = IsDir}) ->
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
