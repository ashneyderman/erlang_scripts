#!/usr/bin/env escript
%%! -pz /Users/sandor/bin/erlang/ebin  

main(_Args) ->
    %%io:format("~p~n", [is_valid([1,2,3,4,5,6,7,8,9])]),
    %%io:format("~p~n", [is_valid([1,2,3,4,5,6,7,9,9])]),
    io:format("~p~n", [lists:append([[1,2,3], [4], []])]),
    io:format("~p~n", [lists:split(0,[1,2,3])]),
    io:format("~p~n", [create_all_row_variations([1,0,0,3,6,9,5,4,0])]).

create_all_row_variations(Row) ->
    Perms = combinatorics:perms(all_sudoku_digits() -- 
                                lists:filter(fun(E) -> E > 0 end, Row)),
    merge_permutations_with_row(Perms, Row).

merge_permutations_with_row([], Row) ->
    Row;
merge_permutations_with_row(Perms, Row) ->
    [merge_permutation_with_row(Perm, Row) || Perm <- Perms].

merge_permutation_with_row([H|T], Row) ->
    {Left,[_Zero|Right]} = lists:splitwith( fun(E) -> E > 0 end, Row ),
    NewRow = lists:append([Left, [H], Right]),
    merge_permutation_with_row(T, NewRow);    
merge_permutation_with_row([], Row) ->
    Row.                                 

all_sudoku_digits() ->
    [1,2,3,4,5,6,7,8,9].
    