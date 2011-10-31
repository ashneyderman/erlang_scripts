-module(sutils).

-export([add_ebin_directory_to_path/1,parse_args/2,parse_args/3,file_ops_error_msg/1]).

add_ebin_directory_to_path(ScriptFile) ->
    Dir = filename:dirname(ScriptFile),
    code:add_path(filename:join(Dir,"ebin")).

parse_args(Args, OptionsSpecs, Command) ->
    case getopt:parse( OptionsSpecs, Args ) of
        {ok, {OptArgs, NonOptArgs}} ->
            [ find_option_value(OptionName,OptArgs) || {OptionName,_,_,_,_} <- OptionsSpecs ] ++ [NonOptArgs];
        {error, {Reason, Data}} ->
            io:format("Unable to parse the arguments. Error occured: ~p ~p~n",[Reason,Data]),
            getopt:usage(OptionsSpecs,Command),
            halt(1)
    end.

file_ops_error_msg( enoent ) -> "The file does not exist.";
file_ops_error_msg( eacces ) -> "Missing permission for reading the file or searching one of the parent directories.";
file_ops_error_msg( eisdir ) -> "The named file is not a regular file. It may be a directory, a fifo, or a device.";
file_ops_error_msg( enotdir ) -> "A component of the file name is not a directory. On some platforms, enoent is returned instead.";
file_ops_error_msg( enospc ) -> "There is a no space left on the device.";
file_ops_error_msg( Other ) -> Other.

find_option_value(OptionName, []) ->
    { OptionName, off };
find_option_value(OptionName, [ OptionName | _Rest ]) ->
    { OptionName, on };
find_option_value(OptionName, [ {OptionName, OptionValue} | _Rest ])  ->
    { OptionName, OptionValue };
find_option_value(OptionName, [ _NonMatch | Rest ]) ->
    find_option_value(OptionName, Rest).

parse_args(Args, OptionsSpecs) ->
    Command = escript:script_name(),
    parse_args(Args, OptionsSpecs, Command).

