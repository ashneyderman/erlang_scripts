#!/usr/bin/env escript
%%! -pz /Users/sandor/bin/erlang/ebin

main(Args) ->
    sutils:add_ebin_directory_to_path(escript:script_name()),
    [ {help,HelpValue},
      {file,FileName},
      {verbose,Verbose},
      _OtherValues ] = sutils:parse_args(Args, option_spec_list(), 'sudoku.erl'),
    help( HelpValue ),
    solve( FileName, Verbose ).

help( off ) ->
    ok;
help( on ) ->
    getopt:usage(option_spec_list(),escript:script_name()),
    halt(1).

message( provided, Msg, MsgParams ) when is_list( MsgParams ) ->
    io:format( Msg, MsgParams );
message( undefined, _Msg, _MSgParams ) ->
    ok.

message( Msg, MsgParams ) when is_list( MsgParams ) ->
    message( provided, Msg, MsgParams ).
    
option_spec_list() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,     $h, "help",       undefined, "Show the program options"},
     {file,     $f, "file",       {'string', 'solve.sudoku'}, "Specifies the input file"},
     {verbose,  $v, "verbose",    undefined, "Be verbose about what gets done"}
    ].

solve( FileName, Verbose ) ->
    case file:consult( FileName ) of
        { ok, [SudokuBoard] } ->
            message( ">>> Input Board:~n~p~n", [SudokuBoard] ),
            message( "<<< Solution:~n~p~n", [solve_board( Verbose, SudokuBoard )] );
        { error, Reason } ->
            message( "Unable to read the board from ~p. Reason - ~p~n", [FileName,sutils:file_ops_error_msg(Reason)] ),
            halt( 2 ) 
    end.

solve_board( Verbose, SudokuBoard ) when is_list(SudokuBoard) ->
    case is_valid_board(SudokuBoard) of
        false -> io:format( "ERROR: Input board is invalid." );
        true -> do_solve_board( Verbose, SudokuBoard, 1 )
    end.

do_solve_board( Verbose, SudokuBoard, PermutatedRow ) when is_list(SudokuBoard) ->
    AllRowVariations = create_all_row_variations( lists:nth(PermutatedRow,SudokuBoard) ),
    do_solve_board_1(Verbose, SudokuBoard, PermutatedRow, AllRowVariations);
do_solve_board( Verbose, SudokuBoard, 10 ) when is_list(SudokuBoard) ->
    message(Verbose, "Reached solution ~p~n", [SudokuBoard]),
    SudokuBoard.

do_solve_board_1( Verbose, SudokuBoard, PermutatedRow, [Variant|Variations] ) ->
    {Prefix, _} = lists:split( PermutatedRow - 1, SudokuBoard ),
    {_, Suffix} = lists:split( PermutatedRow, SudokuBoard ),
    NextBoard = lists:append([Prefix,[Variant],Suffix]),
    case is_valid_board( NextBoard ) of
        true ->
            io:format("valid: ~p~p~n", [PermutatedRow, NextBoard]),
            case do_solve_board(Verbose, NextBoard, PermutatedRow + 1) of
                nil -> do_solve_board_1( Verbose, SudokuBoard, PermutatedRow, Variations );
                Result -> Result
            end;
        false ->
            %%io:format("invalid: ~p~p~n", [PermutatedRow, NextBoard]),
            do_solve_board_1( Verbose, SudokuBoard, PermutatedRow, Variations )
    end;
do_solve_board_1( _Verbose, SudokuBoard, PermutatedRow, [] ) ->
    nil.

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

all_sudoku_digits() -> [1,2,3,4,5,6,7,8,9].

is_valid_board( SudokuBoard ) ->
    case [is_valid_quadrant(SudokuBoard, X, Y) || X <- lists:seq(1,3), Y <- lists:seq(1,3)] of
        [true,true,true,true,true,true,true,true,true] -> true;
        _ -> false
    end and case [is_valid_row(SudokuBoard, X) || X <- lists:seq(1,9)] of
        [true,true,true,true,true,true,true,true,true] -> true;
        _ -> false
    end and case [is_valid_col(SudokuBoard, Y) || Y <- lists:seq(1,9)] of
        [true,true,true,true,true,true,true,true,true] -> true;
        _ -> false
    end.

is_valid_row(SudokuBoard, R) ->
    is_valid(lists:nth(R, SudokuBoard)).

is_valid_col(SudokuBorad, C) ->
    is_valid([lists:nth(C, Row) || Row <- SudokuBorad]).
 
is_valid_quadrant(SudokuBoard, X, Y) ->
    Row1 = lists:nth((Y - 1) * 3 + 1, SudokuBoard),
    Row2 = lists:nth((Y - 1) * 3 + 2, SudokuBoard),
    Row3 = lists:nth((Y - 1) * 3 + 3, SudokuBoard),
    Col1 = lists:sublist(Row1, (X - 1) * 3 + 1, 3),
    Col2 = lists:sublist(Row2, (X - 1) * 3 + 1, 3),
    Col3 = lists:sublist(Row3, (X - 1) * 3 + 1, 3),
    is_valid(lists:append([Col1,Col2,Col3])).
    
is_valid( NumbersList ) when is_list(NumbersList) ->
    GreaterThan0 = fun(E) -> E > 0 end,
    is_empty(lists:filter(GreaterThan0, NumbersList -- lists:seq(1,9))). 

is_empty([]) -> true;
is_empty(_)  -> false.