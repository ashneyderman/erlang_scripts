#!/usr/bin/env escript
%%! -pz /Users/sandor/bin/erlang/ebin  

main(Args) ->
    %sutils:add_ebin_directory_to_path(escript:script_name()),
    %%sutils:parse_args(Args),
    show_current_search_path(),
    retrieve_current_working_dir(),
    run_list_experiments().

show_current_search_path() ->
    io:format("~n"),
    io:format("-- ===========================================~n"),
    io:format("-- Output of show_current_search_path~n"),
    io:format("-- ===========================================~n"),
    io:format("~p~n~n",[code:get_path()]).

retrieve_current_working_dir() ->
    io:format("~n"),
    io:format("-- ===========================================~n"),
    io:format("-- Output of retrieve_current_working_dir~n"),
    io:format("-- ===========================================~n"),
    case file:get_cwd() of
        { ok, Dir } ->
            io:format("Retrieved current working directory ~p~n", [ Dir ]);
        { error, Reason } ->
            io:format("Unable to retrieve current working directory. Error reason: ~p~n", [Reason])
    end.

run_list_experiments() ->
    io:format( "          ++ ~p~n", [[{file, "blah.txt"}, {verbose, on}] ++ ["test1", "test2"]] ),
    io:format( "          ++ ~p~n", [[{file, "blah.txt"}, {verbose, on}] ++ [["test1", "test2"]]] ),
    io:format( "lists:append ~p~n", [lists:append([{file, "blah.txt"}, {verbose, on}], ["test1", "test2"])] ),
    io:format( "lists:merge  ~p~n", [lists:merge([{file, "blah.txt"}, {verbose, on}], [["test1", "test2"]])] ).


%%add_ebin_directory_to_path() ->
%%    File = escript:script_name(),
%%    Dir = filename:dirname(File),
%%    true = code:add_path(filename:join(Dir,"ebin")).
