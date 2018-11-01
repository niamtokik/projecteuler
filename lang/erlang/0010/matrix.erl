%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2016, Mathieu Kerjouan mk@steepath.eu
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(matrix).
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).
%% gen_server export
-export([start_link/0, start_link/3, start_link/4]).
-export([start/0, start/3, start/4, init/1]).
-export([stop/1, stop/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3, terminate/2]).

%% api export
-export([create/3]).

-record(matrix_bin, {type :: atom()
		    ,matrix :: list() | iolist()
		    ,pattern_size :: integer()
		    ,row :: integer()
		    ,column :: integer() }).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
start_link() ->
    start_link(?MODULE, [#matrix_bin{}], []).
start_link(Module, Args, Options) ->
    gen_server:start_link({local, Module}, Module, Args, Options).
start_link(ServerName, Module, Args, Options) ->
    gen_server:start_link(ServerName, Module, Args, Options).

start() ->
    start(?MODULE, [#matrix_bin{}], []).
start(Module, Args, Options) ->
    start({local, Module}, Module, Args, Options).
start({local, Module}, Module, Args, Options) ->
    gen_server:start({local, Module}, Module, Args, Options).

stop(ServerRef) ->
    ok.
stop(ServerRef, Reason, Timeout) ->
    ok.

init(State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
%call(ServerRef, Request) ->
%    ok.
%call(ServerRef, Request, Timeout) ->
%    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
%multi_call(Name, Request) ->
%    ok.
%multi_call(Nodes, Name, Request) ->
%    ok.
%multi_call(Nodes, Name, Request, Timeout) ->
%    ok.
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
%cast(ServerRef, Request) ->
%    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_call({set, Matrix}, From, State) 
  when is_record(Matrix, matrix_bin) ->
    {reply, ok, [Matrix]};
handle_call({get, state}, From, State) ->
    {reply, State, State};
handle_call(Request, From, State) ->
    io:format("bad request: ~p~n", [Request]),
    {noreply, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_cast(Request, State) ->
    io:format("~p~n", [Request]),
    {noreply, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
code_change(OldVsn,  State,  Extra) ->
    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec create({atom()}) -> #matrix_bin{}.
create({matrix}) ->
    #matrix_bin{}.

-spec create({atom()}, Matrix :: iolist(), 
	     Options :: [tuple(), ... ]) -> #matrix_bin{}.
create({matrix}, Matrix, Options) 
  when is_list(Options) ->
    PatternSize = proplists:get_value(pattern_size , Options, 8),
    Row = proplists:get_value(row, Options, 1),
    Column = proplists:get_value(column, Options, byte_size(Matrix)),
    #matrix_bin{type = binary
	       ,matrix = Matrix
	       ,pattern_size = PatternSize
	       ,row = Row
	       ,column = Column}.
	
