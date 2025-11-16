-module(glwatch_ffi).
-export([exec_command/1]).

%% Execute shell command and return output as string
exec_command(Command) when is_binary(Command) ->
    try
        %% Convert binary to string for os:cmd
        CmdString = binary_to_list(Command),

        %% Execute the command
        Output = os:cmd(CmdString),

        %% os:cmd returns a string (list of integers)
        %% We need to convert it to binary for Gleam
        case unicode:characters_to_binary(Output, unicode, utf8) of
            {error, _Binary, _Rest} ->
                %% Failed to convert, try latin1
                case unicode:characters_to_binary(Output, latin1, utf8) of
                    {error, _, _} -> <<"">>;
                    {incomplete, _, _} -> <<"">>;
                    Binary when is_binary(Binary) -> Binary
                end;
            {incomplete, _Binary, _Rest} ->
                <<"">>;
            Binary when is_binary(Binary) ->
                Binary
        end
    catch
        _:_ ->
            <<"">>
    end;
exec_command(_) ->
    <<"">>.
