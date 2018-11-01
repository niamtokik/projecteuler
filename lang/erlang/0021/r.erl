%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2017, Mathieu Kerjouan <mk [at] steepath.eu>
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(r).
-compile(export_all).

-spec divisor(integer()) -> [integer(), ...].
divisor(X) 
  when (X rem 2) =/= 0 ->
    divisor(X, trunc(X/2), 1, []);
divisor(X) 
  when (X rem 2) =:= 0 ->
    divisor(X, trunc(X/2), 1, []).

divisor(_, 0, _, Buffer) ->
    Buffer;
divisor(X, Y, D, Buffer) ->
    case (X rem Y) of
	0 -> divisor(X, Y-D, D, [Y]++Buffer);
	_ -> divisor(X, Y-D, D, Buffer)
    end.

-spec amicale(integer(), integer()) -> boolean().
amicale(X, Y) 
  when X =/= Y ->
    SumX = lists:sum(divisor(X)),
    SumY = lists:sum(divisor(Y)),
    (SumX =:= Y) andalso (SumY =:= X);
amicale(_, _) ->
    false.

-spec sum_range(integer()) -> [{integer(), integer()}, ...].
sum_range(Range) ->
    [ {lists:sum(r:divisor(X)), X} || X <- lists:seq(1,Range) ].

compare(Range) ->
    List = lists:sort(sum_range(Range)).

primes(Range) ->
    [ X || {1, X} <- lists:sort(sum_range(Range)) ].

euler()	->
    [ {X, Y} || X <- lists:seq(1,1000), 
		Y <- lists:seq(1,1000), 
		r:amicale(X, Y) =:= true ].
