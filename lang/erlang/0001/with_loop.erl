%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2016, Mathieu Kerjouan mk@steepath.eu
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(with_loop).
% -behaviour().
% -export([]).
% -compile([]).

% init() -> ok.
% start() -> ok.
% stop() -> ok.

start(List, N) 
  when is_list(List) and is_integer(N) ->
    lists:sum(coupled(List, N)).
start_test() ->
    [?assert(start([3,5], 10) =:= 23)].

multiple(M, N) 
  when is_integer(M) and is_integer(N) ->
    [ X || X <- lists:seq(1, N-1), X rem M =:= 0 ].
multiple_test() ->
    [?assert( multiple(3, 10) =:= [3,6,9] )
    ,?assert( multiple(5, 10) =:= [5] )].

coupled(List, N) 
  when is_list(List) and is_integer(N) ->
    lists:append([ multiple(X, N) || X <- List ]).
coupled_test() ->
    [?assert (coupled([3,5], 10) =:= [3,6,9,5])].
