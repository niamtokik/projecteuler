%%%-------------------------------------------------------------------
%%% @author <%AUTHOR%>
%%% @copyright (c) <%DATE%>, <%AUTHOR%> <%AUTHOR_MAIL%>
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(run).
% -behaviour().
% -behaviour(gen_server).
% -behaviour(supervisor).
% -behaviour(gen_event).
% -export([]).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

fact_test() ->
    [?assertEqual(fact(0), 1),
     ?assertEqual(fact(1), 1),
     ?assertEqual(fact(2), 2*1),
     ?assertEqual(fact(3), 3*2*1)
    ].

-spec fact(integer()) -> integer().
fact(Integer) ->
    fact(Integer, 1).

fact(1, R) ->
    R;
fact(X, R) ->
    N = X-1,
    fact(N, N*R).

sum(X) ->
    lists:sum([ erlang:list_to_integer([X]) || X <- erlang:integer_to_list(run:fact(X)) ]).

euler() ->
    sum(100).





