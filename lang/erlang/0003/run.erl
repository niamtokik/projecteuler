%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2016, Mathieu Kerjouan mk@steepath.eu
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

%% got: [1,71,839,1471,6857,59569,104441,486847,1234169,5753023,10086647,
%% 87625999,716151937,8462696833]

-module(run).
% -behaviour().
% -export([]).
-compile([export_all]).

% init() -> ok.
% start() -> ok.
% stop() -> ok.

local_start() ->
    start({receiver}),
    start({counter, 600851475143, 1000}).

node_start() ->
    start({receiver}),
    start({counter, nodes, 600851475143, 1000}).

integer_split(X, S) 
  when is_integer(X), is_integer(S), S > 0->
    R = X/S,
    [ (round(Y*R)+1) || Y <- lists:seq(0, S) ].

opts(List) when is_list(List) ->
    opts(List, []).

opts([], List) ->
    List;
opts([H,Hs|T], []) ->
    opts([Hs] ++ T, [{H,Hs}]);
opts([H,Hs|T], Buf) ->
    opts([Hs] ++ T, Buf ++ [{H,Hs}]);
opts(_, Buf) ->
    opts([], Buf).
    
start() ->
    ok.

start({receiver}) ->
    Pid = spawn(fun() -> receiver() end),
    register(receiver, Pid),
    Pid;
start({receiver, X}) 
  when is_integer(X), X>0 ->
    Spawner = fun(Name) 
		    when is_atom(Name) ->
		      Run = spawn(fun() -> receiver() end ),
		      register(Name, Run),
		      {Name, Run}
	      end,
    [ Spawner(list_to_atom("receiver" ++ integer_to_list(Y))) || Y <- lists:seq(0, (X-1)) ];

start({counter, {Start, End, Control}}) ->
    Pid = spawn(fun() -> counter(Start, End, Control) end),
    register(counter, Pid),
    Pid;
start({counter, Control, X}) 
  when is_integer(X), X>0, is_integer(Control) ->
    Spawner = fun(Name, _Start, _End, _Control) 
		    when is_atom(Name), is_integer(_Start), 
			 is_integer(_End), is_integer(_Control)->
		      Run = spawn(fun() -> counter(_Start, _End, _Control) end ),
		      register(Name, Run),
		      {Name, Run}
	      end,
    Opts = opts(integer_split(Control, X)),
    io:format("list: ~p~n", [Opts]),
    [ Spawner(list_to_atom("counter" ++ integer_to_list(_Start))
	     , _Start, _End, Control) || {_Start, _End} <- Opts ];
start({counter, nodes, Control, X}) 
  when is_integer(Control),
       is_integer(X) ->
    Spawner = fun( _Start, _End, _Control) 
		    when is_integer(_Start), 
			 is_integer(_End), is_integer(_Control) ->
		      Rand_Node = fun()  -> 
					  List = [node()] ++ nodes(),
					  <<D>> = crypto:rand_bytes(1),
					  Rand = (D rem length(List)+1),
					  lists:nth(Rand, List)
				  end,
		      Run = spawn(Rand_Node(), fun() -> counter(_Start, _End, _Control) end ),
		      {Run}
	      end,
    Opts = opts(integer_split(Control, X)),
    io:format("list: ~p~n", [Opts]),
    [ Spawner( _Start, _End, Control) || {_Start, _End} <- Opts ].


receiver() ->
    receiver(#{}).
receiver(Map) 
  when is_map(Map) ->
	receive
	    {check} ->
		io:format("got: ~p~n", [maps:keys(Map)]),
		receiver(Map);
	    {Pid, export} when is_pid(Pid) ->
		Pid ! Map,
		receiver(Map);
	    {Pid, bye} ->
		io:format("ended: ~p~n", Pid),
		receiver(Map);
	    {found, X} 
	      when is_integer(X) -> 
		io:format("increment~n"),
		Maps = maps:put(X, 1, Map),
		receiver(Maps);
	    {exit} ->
		throw({exit, "bye bye"});
	    _Else -> 
		io:format("bad value ~p~n", [_Else]),
		receiver(Map)
	end.

% End pattern
counter(Start, End, Control) 
  when is_integer(Start),
       is_integer(End),
       is_integer(Control),
       Start =:= End,
       End =< Control ->
    to_receiver(bye);
% Start Pattern
counter(Start, End, Control) 
  when is_integer(Start),
       is_integer(End),
       is_integer(Control),
       Start < End,
       End =< Control ->
    case Control rem Start =:= 0 of 
	true -> to_receiver(Start);
	false -> ok
    end,
    Counter = Start + 2,
    counter(Counter, End, Control).

to_receiver(bye) ->
    { receiver, 'frey@frey'} ! { self(), bye };
to_receiver(X) 
  when is_integer(X) ->
    { receiver, 'frey@frey'} ! { found, X }.


