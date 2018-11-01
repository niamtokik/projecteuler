%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2016, Mathieu Kerjouan mk@steepath.eu
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(run).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

% datastructure:
% {Triangle, TriangleSize, Divisors}
% case ets:match(Ets, {'_', X, '$1'}) of
%   [] -> continue;
%   [[R]] -> R end.


ets() ->
    Ets = ets:new(divisor, [public]),
    Ets.

divisor2_test() ->
    [?assert(divisor2(1) =:= [1])
    ,?assert(divisor2(3) =:= [1,3])
    ,?assert(divisor2(6) =:= [1,2,3,6])
    ,?assert(divisor2(10) =:= [1,2,5,10])
    ,?assert(divisor2(15) =:= [1,3,5,15])
    ,?assert(divisor2(21) =:= [1,3,7,21])
    ,?assert(divisor2(28) =:= [1,2,4,7,14,28])
    ].

% simple divisor -> low memory usage
divisor2(1) ->
    [1];
divisor2(X) ->
    Limit = round(X/math:sqrt(2)),
    divisor2(X, Limit, []).

divisor2(X, 1, Buf) ->
    lists:reverse(Buf ++ [1]) ++ [X];
divisor2(X, T, Buf) ->
    case X rem T =:= 0 of
	true ->
	    divisor2(X, T-1, Buf ++ [T]);
	false ->
	    divisor2(X, T-1, Buf)
    end.

insert(Ets, Divisor) 
  when is_integer(Ets), is_list(Divisor) -> 
    Key = lists:last(Divisor),
    ets:insert(Ets, {Key, Divisor}).

-spec triangle(integer()) -> integer().
triangle_test() ->
    [?assert(triangle(1) =:= 1)
    ,?assert(triangle(2) =:= 3)
    ,?assert(triangle(3) =:= 6)
    ,?assert(triangle(4) =:= 10)
    ,?assert(triangle(5) =:= 15)
    ,?assert(triangle(6) =:= 21)
    ,?assert(triangle(7) =:= 28)
    ].
triangle(X) ->
    trunc((X*(X+1))/2).

euler() ->
    euler(1, 500).

euler(X) ->
    euler(X, 500).

euler_test() ->
    [].
euler(X, Limit) 
  when is_integer(X), is_integer(Limit) ->
    Triangle = triangle(X),
    Divisors = divisor2(Triangle),
    Length = length(Divisors),
    case Length > Limit of
	true -> {X, Triangle, Length};
	false -> euler(X+1, Limit)
    end.
     
     
