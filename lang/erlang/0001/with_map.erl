%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2016, Mathieu Kerjouan mk@steepath.eu
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(with_map).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).

% init() -> ok.
% start() -> ok.
% stop() -> ok.

launch() ->
    lists:sum(maps:keys(run:loop([3,5], 1000))).

start(List, N) 
  when is_list(List) and is_integer(N) ->
    lists:sum(maps:keys(run:loop(List, N))).
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
    
loop(List, N) 
  when is_list(List), is_integer(N) ->
    loop(#{}, List, N-1).

loop(Map, [H|T], N) 
  when is_map(Map), is_integer(H), 
       is_list(T), is_integer(N), 
       (H =< N) ->
    Maps = loop_int(Map, [ X || X <- lists:seq(1, N), X rem H =:= 0 ]),
    loop(Maps, T, N);
loop(Map, [], N) 
  when is_integer(N), is_map(Map) ->
    Map.

loop_int(Map, [H|T]) 
  when is_map(Map), is_integer(H), is_list(T) ->
    Maps = maps:put(H, 1, Map),
    loop_int(Maps, T);
loop_int(Map, []) ->
    Map.

    
