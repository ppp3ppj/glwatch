-module(file_watcher).
-export([start_watching/1, get_events/1, stop_watching/1]).
-on_load(init/0).

%% This function loads the Rust NIF when the module is loaded
init() ->
    PrivDir = case code:priv_dir(glwatch) of
        {error, _} ->
            % Development mode: look relative to the module
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppDir = filename:dirname(EbinDir),
            filename:join([AppDir, "priv"]);
        Dir ->
            Dir
    end,
    % Load the NIF (without .so extension - Erlang adds it automatically)
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

%% NIF stubs - these will be replaced by Rust implementations when NIF loads
start_watching(_Directory) ->
    erlang:nif_error(nif_not_loaded).

get_events(_WatcherRef) ->
    erlang:nif_error(nif_not_loaded).

stop_watching(_WatcherRef) ->
    erlang:nif_error(nif_not_loaded).
