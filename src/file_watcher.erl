-module(file_watcher).
-export([
    start_watching/1,
    start_watching_with_patterns/2,
    start_watching_multiple/1,
    start_watching_multiple_with_patterns/2,
    get_events/1,
    stop_watching/1
]).
-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(glwatch) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppDir = filename:dirname(EbinDir),
            filename:join([AppDir, "priv"]);
        Dir ->
            Dir
    end,
    SoName = filename:join(PrivDir, "rust_watcher"),
    io:format("[Erlang] Loading NIF from: ~s~n", [SoName]),
    case erlang:load_nif(SoName, 0) of
        ok ->
            io:format("[Erlang] ✓ NIF loaded successfully~n"),
            ok;
        {error, {reload, _}} ->
            io:format("[Erlang] ✓ NIF already loaded~n"),
            ok;
        {error, Reason} ->
            io:format("[Erlang] ✗ Failed to load NIF: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Single directory - backward compatible
start_watching(_Directory) ->
    erlang:nif_error(nif_not_loaded).

start_watching_with_patterns(_Directory, _Patterns) ->
    erlang:nif_error(nif_not_loaded).

%% Multiple directories - NEW
start_watching_multiple(_Directories) ->
    erlang:nif_error(nif_not_loaded).

start_watching_multiple_with_patterns(_Directories, _Patterns) ->
    erlang:nif_error(nif_not_loaded).

%% Common functions
get_events(_WatcherRef) ->
    erlang:nif_error(nif_not_loaded).

stop_watching(_WatcherRef) ->
    erlang:nif_error(nif_not_loaded).
