#!/usr/bin/env escript

main(Args) ->
    add_ebin_directory_to_path(),
    parse_args(Args).

add_ebin_directory_to_path() ->
    File = escript:script_name(),
    Dir = filename:dirname(File),
    true = code:add_path(filename:join(Dir,"ebin")).

%%
%% options accepted via getopt
%%
option_spec_list() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,     $h, "help",       undefined, "Show the program options"},
     {message,  $m, "message",    undefined, "Show available commands"},
     {dry,      $d, "dry",        undefined, "Only show svn commit command"},
     {verbose,  $v, "verbose",    undefined, "Be verbose about what gets done"}
    ].

%%
%% print help/usage string
%%
help( true ) ->
    OptSpecList = option_spec_list(),
    getopt:usage(OptSpecList, "svnci",
                 "[var=value,...] <command,...>",
                 [{"var=value", "rebar global variables (e.g. force=1)"},
                  {"command", "Command to run (e.g. compile)"}]),
    halt(1);
help( false ) -> _False = false.

%%
%% Parse command line arguments using getopt and also filtering out any
%% key=value pairs. What's left is the list of commands to run
%%
parse_args(Args) ->
    case getopt:parse( option_spec_list(), Args ) of
        {ok, { Options, _NonOptArgs}} ->
            Verbose = get_bool(verbose, Options),
            Dryrun = get_bool(dry, Options),
            DisplayHelp = get_bool(help, Options),
            help( DisplayHelp ),
            runsvn( Verbose, Dryrun, "", "", "" );
        
        {error, {_Reason, _Data}} ->
            help(true),
            halt(1)
    end.

filter_flags([], Commands) ->
    lists:reverse(Commands);
filter_flags([Item | Rest], Commands) ->
    case string:tokens(Item, "=") of
        [Command] ->
            filter_flags(Rest, [Command | Commands]);
        [KeyStr, Value] ->
            Key = list_to_atom(KeyStr),
            rebar_config:set_global(Key, Value),
            filter_flags(Rest, Commands);
        _Other ->
            filter_flags(Rest, Commands)
    end.

get_bool(Key, [P | Ps]) ->
    if is_atom(P), P =:= Key -> 
          true;
       tuple_size(P) >= 1, element(1, P) =:= Key ->
          case P of
             {_, true} -> true;
             _ -> false
          end;
       true -> get_bool(Key, Ps)
    end;
get_bool(_Key, []) ->
    false.

svn_operation( StatusLine ) ->
    [Operation | FileName] = string:tokens( StatusLine, " " ),
    io:format("~p \\~n", FileName),
    { Operation, FileName }.

runsvn( _Verbose, _Dryrun, _Message, _Incident, _Project ) ->
    Pipeline = ["svn status", "grep -v \.[ie]ml$", "grep -v \.settings$", "grep -v out$", "grep -v \.idea$"],
    Result = os:cmd( string:join(Pipeline, " | ") ),
    ResultList = string:tokens( Result, "\n"),
    io:format("svn ci -m " ),
    [ svn_operation(SvnStatusLine) || SvnStatusLine <- ResultList].
