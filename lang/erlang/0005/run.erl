%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2016, Mathieu Kerjouan mk@steepath.eu
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(run).
% -behaviour().
% -export([]).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

% init() -> ok.
% start() -> ok.
% stop() -> ok.

%%
%% @doc
%%   2520 is even and next number need to be even too.
%%   drop all odd number. If we use brute force, need
%%   to increment of 2 and not 1.
%%   so...
%%   Number = 20!
%%   Number = 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 *
%%            .   .   .    .       .       .
%%            10 * 11 * 12 * 13 * 14 * 15 * 16 *
%%            .         .         .         .
%%            17 * 18 * 19 * 20
%%                 .         .
%% 3,5,8
%% [1..1]  =>      1
%% [1..2]  =>      2    *2
%% [1..3]  =>      6    *3
%% [1..4]  =>     12    *2
%% [1..5]  =>     60    *5
%% [1..6]  =>     60    *1
%% [1..7]  =>    420    *7
%% [1..8]  =>    840    *2
%% [1..9]  =>   2520    *3
%% [1..10] =>   2520    *1
%% [1..11] =>  27720    *11
%% [1..12] =>  27720    *1
%% [1..13] => 360360    *143
%% [1..14] => 360360    *1
%% [1..15] => 360360    *1
%% [1..16] => 720720    *2
%% [1..17] => 
%% @end

check(A, B) ->
    T = [ X || X <- [1,2,3,5,7], A rem X =:= 0 ], 
		   %,11,13,17,19], A rem X =:= 0 ],
    if length(T) =:= 5 -> true;
       true -> false
    end.

factorial(0) ->
    1;
factorial(X) ->
    fact(X, 1).

fact(0, N) ->
    N;
fact(X, N) ->
    fact(X-1, X*N).

    
%% parsing(List) ->
%%    parsing(<<>>, List, 0).

printable(<<Char>>) ->
    io:format("~p~n", [Char]),
    if (Char >= 33) and (Char =< 126) ->
	    {ok, Char};
       true ->
	    {error, $\ }
    end.
    
c(<<>>, Buf, Size) ->
    {Buf};
c(<<H:1/binary, T/bitstring>>, <<>>, Size) ->
    c(T, H, Size);
c(<<H:1/binary, T/bitstring>>, <<Buf/bitstring>>, Size) 
  when is_integer(Size) ->
    io:format("~p ~p~n", [Size, size(Buf)]),
    case  size(Buf) rem (Size+1) =:= 0 of
	true ->
	    {ok, Char} = printable(H),
	    Bufs = <<Buf/bitstring, Char, $\n>>,
	    c(T, Bufs, Size);
	false ->
	    case printable(H) of
		{ok, Char} -> Char;
		{error, Char} -> Char
	    end,
	    Bufs = <<Buf/bitstring, Char>>,
	    c(T, Bufs, Size)
    end.




    
    
	     
    


