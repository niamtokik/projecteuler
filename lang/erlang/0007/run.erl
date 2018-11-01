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

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec create_ets() -> integer().
create_ets() ->
    io:format("create ets..."),
    Ets = ets:new(prime, [ordered_set, protected]),
    io:format("~p~n", [Ets]),
    Ets.

-spec init_ets(integer()) -> integer().
init_ets(Ets) 
  when is_integer(Ets) ->
    ets:insert(Ets, {1}),
    ets:insert(Ets, {2}),
    Ets.
    
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
interface() ->
    interface(init_ets(create_ets())).
interface(Ets) ->
    interface(Ets, []).

interface(Ets, Options) ->
    receive 
	{put, Prime} 
	  when is_integer(Prime) ->
	    ets:insert(Ets, {Prime}),
	    interface(Ets, Options);
	{check, Pid, Prime} 
	  when is_pid(Pid), is_integer(Prime) ->
	    interface(Ets, Options);
	{get, Pid} ->
	    interface(Ets, Options);
	{get, Pid, Limit} ->
	    interface(Ets, Options);
	{exit} -> 
	    {exit};
	{exit, Reason} ->
	    {exit, Reason};
	_Else ->
	    io:format("receive: ~p~n", [_Else])
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
is_prime(Integer) ->
    is_prime(Integer, simple).
is_prime(Integer, simple) ->
    is_prime_simple(Integer);
is_prime(Integer, rabin) ->
    ok;
is_prime(Integer, erastosthenes) ->
    ok;
is_prime(Integer, pseudoprime) ->
    ok;
is_prime(Integer, miller) ->
    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
is_prime_simple(Integer) 
  when is_integer(Integer) ->
    Limit = round(math:sqrt(Integer)),
    is_prime_simple(Integer, Limit).

is_prime_simple(Integer, Limit) 
  when is_integer(Integer), is_integer(Limit) ->
    is_prime_simple(Integer, Limit, lists:seq(1, Limit), 0).

is_prime_simple(Integer, Limit, [], 1) ->
    true;
is_prime_simple(Integer, Limit, PrimeList, Acc) 
  when is_integer(Integer), is_integer(Limit), 
       is_list(PrimeList), Acc > 1 ->
    false;
is_prime_simple(Integer, Limit, [H|T], Acc) 
  when is_integer(Integer), is_integer(Limit),
       is_integer(H), is_integer(Acc), H =< Limit ->
    case Integer rem H =:= 0 of
	true -> is_prime_simple(Integer, Limit, T, Acc+1);
	false -> is_prime_simple(Integer, Limit, T, Acc)
    end.

%%--------------------------------------------------------------------
%% elegant euler solution. Just create a recursive function and check
%% every solution until nth prime is found.
%% this method use is_prime_simple. Another method would used 
%% auto-increment prime list. If I have time, I will create it! :)
%%--------------------------------------------------------------------
euler_response(Integer) ->
    euler_response(Integer, 2, 1).
euler_response(Integer, Prime, Acc) 
  when Integer =:= Acc ->
    Prime;
euler_response(Integer, 2, 1) ->
    euler_response(Integer, 3, 2);
euler_response(Integer, Prime, Acc) ->
    P = Prime+2,
    case is_prime_simple(P) of
	true -> euler_response(Integer, P, Acc+1);
	false -> euler_response(Integer, P, Acc)
    end.

euler(Integer) ->
    euler_response(Integer).
	     
    
        





