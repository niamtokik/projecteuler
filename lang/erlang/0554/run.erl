%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2016, Mathieu Kerjouan mk@steepath.eu
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(run).
-include_lib("eunit/include/eunit.hrl").
% -behaviour().
% -export([]).
-compile([export_all]).

init() -> ok.
% start() -> ok.
% stop() -> ok.

-spec square(integer()) -> integer().
square(N) ->
    math:pow(N, 2).

board(N) ->
    2*N*2*N.
board_test() ->
    [?assert(board(2) =:= 16)
    ,?assert(board(5) =:= 100)].

board(graphic, N) ->
    ok.

non_attacking(2) ->
    8.

    
    

%% Board = 2n*2n
%% C(n) => number of way to n² centaure
%% 
-spec c(integer()) -> integer().
c(N) -> 
    Ns = 2*(square(N)),
    Ns*Ns.

c_test() ->
    [?assert(c(1) =:= 4)
    ,?assert(c(2) =:= 25)
    ,?assert(c(10) =:= 1477721)].
    

