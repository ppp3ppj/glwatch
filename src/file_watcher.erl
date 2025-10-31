-module(file_watcher).
-export([start_watching/1, get_events/1, stop_watching/1]).

% Include the file_info record definition
-include_lib("kernel/include/file.hrl").

-record(watcher_state, {
    directory :: string(),
    current_files :: map()
}).

% Enhanced file watcher with modification detection
start_watching(Directory) ->
    io:format("🔮 Starting enhanced file watcher for: ~s~n", [Directory]),

    % Ensure directory exists
    AbsDir = filename:absname(Directory),
    ensure_directory(AbsDir),

    % Get initial files with detailed info
    InitialFiles = scan_files_detailed(AbsDir),
    io:format("📊 Found ~p initial files~n", [map_size(InitialFiles)]),

    % Start watcher process
    Pid = spawn_link(fun() ->
        enhanced_watch_loop(#watcher_state{
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
                                io:format("📡 Returning ~p events (including modifications)~n", [length(Events)]),
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

% Enhanced watch loop with modification detection
enhanced_watch_loop(State) ->
    #watcher_state{directory = Dir, current_files = OldFiles} = State,

    % Scan for current file state
    NewFiles = scan_files_detailed(Dir),

    % Detect all types of changes (create, delete, modify)
    Events = detect_all_changes(OldFiles, NewFiles),

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
            enhanced_watch_loop(State#watcher_state{current_files = NewFiles});
        stop ->
            io:format("🛑 Enhanced watcher stopped~n")
    after 200 ->  % Faster polling for modification detection
        enhanced_watch_loop(State#watcher_state{current_files = NewFiles})
    end.

% Detailed file scanning that captures modification times and file sizes
scan_files_detailed(Directory) ->
    case file:list_dir(Directory) of
        {ok, Files} ->
            lists:foldl(fun(File, Acc) ->
                FilePath = filename:join(Directory, File),
                case file:read_file_info(FilePath) of
                    {ok, FileInfo} ->
                        IsDir = FileInfo#file_info.type =:= directory,
                        MTime = FileInfo#file_info.mtime,
                        Size = FileInfo#file_info.size,

                        % Store more detailed file info: {MTime, IsDir, Size}
                        maps:put(FilePath, {MTime, IsDir, Size}, Acc);
                    {error, _Reason} ->
                        Acc
                end
            end, #{}, Files);
        {error, _Reason} ->
            #{}
    end.

% Enhanced change detection that includes modifications
detect_all_changes(OldFiles, NewFiles) ->
    OldPaths = sets:from_list(maps:keys(OldFiles)),
    NewPaths = sets:from_list(maps:keys(NewFiles)),

    % Find created and deleted files
    Created = sets:to_list(sets:subtract(NewPaths, OldPaths)),
    Deleted = sets:to_list(sets:subtract(OldPaths, NewPaths)),

    % Find modified files (files that exist in both but have different mtime or size)
    CommonPaths = sets:to_list(sets:intersection(OldPaths, NewPaths)),
    Modified = lists:filter(fun(Path) ->
        file_was_modified(Path, OldFiles, NewFiles)
    end, CommonPaths),

    Timestamp = erlang:system_time(millisecond),

    % Create events
    CreatedEvents = [{"created", Path, get_is_dir_enhanced(Path, NewFiles), Timestamp} || Path <- Created],
    DeletedEvents = [{"deleted", Path, get_is_dir_enhanced(Path, OldFiles), Timestamp} || Path <- Deleted],
    ModifiedEvents = [{"modified", Path, get_is_dir_enhanced(Path, NewFiles), Timestamp} || Path <- Modified],

    % Filter out directory modifications (usually not interesting)
    FilteredModified = lists:filter(fun({"modified", Path, IsDir, _}) ->
        not IsDir  % Only report file modifications, not directory modifications
    end, ModifiedEvents),

    CreatedEvents ++ DeletedEvents ++ FilteredModified.

% Check if a file was modified by comparing mtime and size
file_was_modified(Path, OldFiles, NewFiles) ->
    case {maps:get(Path, OldFiles, undefined), maps:get(Path, NewFiles, undefined)} of
        {{OldMTime, IsDir, OldSize}, {NewMTime, IsDir, NewSize}} ->
            % File was modified if mtime or size changed (and it's not a directory)
            (not IsDir) andalso ((OldMTime =/= NewMTime) orelse (OldSize =/= NewSize));
        _ ->
            false
    end.

% Helper functions
get_is_dir_enhanced(Path, FileMap) ->
    case maps:get(Path, FileMap, undefined) of
        {_MTime, IsDir, _Size} -> IsDir;
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
    io:format("  🗑️ DELETED ~s: ~s~n", [Type, filename:basename(Path)]);
print_event({"modified", Path, IsDir, _Time}) ->
    Type = case IsDir of true -> "DIR"; false -> "FILE" end,
    io:format("  📝 MODIFIED ~s: ~s~n", [Type, filename:basename(Path)]).
