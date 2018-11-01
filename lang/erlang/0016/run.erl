%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2016, Mathieu Kerjouan mk@steepath.eu
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
% -compile([]).

% init() -> ok.
% start() -> ok.
% stop() -> ok.

euler() ->
    lists:sum([ list_to_integer(X) || [X] <- integer_to_list(round(math:pow(2,1000))) ]).
