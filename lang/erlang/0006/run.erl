%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2016, Mathieu Kerjouan mk@steepath.eu
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(run).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).

sum_of_square_test() ->
    [?assert(sum_of_square(10) =:= 385)].
sum_of_square(Integer) when is_integer(Integer) ->
    Result = lists:sum([ math:pow(X, 2) || X <- lists:seq(1, Integer)]),
    round(Result).

square_of_sum() ->
    [?assert(square_of_sum(10) =:= 3085)].
square_of_sum(Integer) when is_integer(Integer) ->
    Result = math:pow(lists:sum(lists:seq(1,Integer)), 2),
    round(Result).

difference_test() ->
    [?assert(difference(10) =:= 2640)].
difference(Integer) when is_integer(Integer) ->
    Result = square_of_sum(Integer) - sum_of_square(Integer),
    Result.

euler() -> 
    difference(100).
