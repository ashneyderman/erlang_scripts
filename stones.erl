#!/usr/bin/env escript

main(_Args) ->
    % Generate all combinations of weights that add up to 40
    StoneWeights = [ {St1,St2,St3,St4} || St1 <- lists:seq(1,37), 
                                          St2 <- lists:seq(1,37), 
                                          St3 <- lists:seq(1,37), 
                                          St4 <- lists:seq(1,37), 
                                          (St1 + St2 + St3 + St4) == 40 ],

    % Find all the weight combinations that could be our solutions
    Solutions = [ Weights || Weights <- StoneWeights, solves_for_all_X(Weights)],

    io:format("~p~n",[Solutions]).

% Make sure that at least one of the possible equations for 
% X + Signs1 * S1 + Sign2 * S2 + Sign3 * S3 + Sign4 * S4 = 0
% exists for each of the X ranging from 1 to 40 and Sign1..Sign4 
% could take on values -1, 0 and 1. 
solves_for_all_X( { St1, St2, St3, St4 } ) ->
    lists:all( fun(X) ->
                 length( [{Sgn1,Sgn2,Sgn3,Sgn4} || Sgn1 <- [1, 0, -1],
                                                   Sgn2 <- [1, 0, -1],
                                                   Sgn3 <- [1, 0, -1],
                                                   Sgn4 <- [1, 0, -1],
                                                   (X + Sgn1 * St1 + Sgn2 * St2 + Sgn3 * St3 + Sgn4 * St4) == 0 ] ) /= 0
               end, lists:seq(1,40) ).
