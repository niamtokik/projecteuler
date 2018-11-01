%%%-------------------------------------------------------------------
%%% @author <%AUTHOR%>
%%% @copyright (c) <%DATE%>, <%AUTHOR%> <%AUTHOR_MAIL%>
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(run).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

table() ->
    [ {[X+64],X} || X <- lists:seq(1,26) ].

letter_test() ->
    [?assertEqual(letter("A"), 1),
     ?assertEqual(letter("Z"), 26)].
-spec letter(list()) -> integer().
letter(X) -> 
    proplists:get_value(X, table()).

-spec read() -> iolist().
read() ->
    {ok, Data} = file:read_file("p022_names.txt"),
    Data.

is_letter(X) 
  when X >= 64, X =< 64+26 ->
    true;
is_letter(_) ->
    false.

parse_test() ->
    [?assertEqual(parse(<<$A>>, [], []), [<<$A>>])
    ].
parse(<<>>, Buf, Result) ->
    case Buf of
	[] -> 
	    Result;
	_Else ->
	    [lists:reverse(Buf)]++Result
    end;
parse(<<Head/integer, Tail/bitstring>>, Buf, Result) ->
    case is_letter(Head) of
	true ->
	    parse(Tail, [Head] ++ Buf, Result);
	false ->
	    case Buf of
		[] -> 
		    parse(Tail, Buf, Result);
		_Else ->
		    parse(Tail, [], [lists:reverse(Buf)]++Result)
	    end
    end.

sort(Data) ->
    lists:sort(parse(Data, [], [])).

score(List) ->
    Fun = fun(Elem, Acc) ->
		  run:letter([Elem]) + Acc 
	  end,
    lists:foldr(Fun, 0, List).

scories(Data) ->
    [ {X, score(X)} || X <- sort(Data) ].

	    
