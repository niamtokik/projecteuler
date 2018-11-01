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

% [{C, 10}, {B, 9, [A, 8, 7, 6...]}]
% [{C, 9}, {


c(Integer) 
  when is_integer(Integer)->
    [{Integer}, b(Integer-1)].

b(Integer) 
  when is_integer(Integer) ->
    b(Integer, []).

b(1, Buf) ->
    Buf;
b(Integer, Buf) ->
    Buf2 = [{Integer, a(Integer-1)}],
    RBuf = Buf ++ Buf2,
    b(Integer-1, RBuf).

a(Integer) 
  when is_integer(Integer) ->
    lists:reverse(lists:seq(1, Integer)).

calcc([{C}, BA]) ->
    calcb(C, BA).
calcb(C, []) ->
    ok;
calcb(C, [{B, A}|T]) ->
    calca(C, B, A),
    calcb(C, T).
calca(C, B, []) ->
    ok;
calca(C, B, [A|T]) ->
    Triplet = triplet(A, B, C),
    case Triplet of
	[_, {_,_,_}, {Y}] when Y >= 999, Y =< 1001 -> 
	    io:format("~p~n", [Triplet]),
	    calca(C, B, T);
	_ ->
	    calca(C, B, T)
    end.
    

triplet(A, B, C) ->
    SQA = math:pow(A, 2),
    SQB = math:pow(B, 2),
    SQC = SQA + SQB,
    [{ A, B, C }, {SQA, SQB, SQC}, {A+B+C}].

is_triplet({A, B, C})
  when A < B andalso B < C ->
    true;
is_triplet(_) ->
    false.

t(A, B) ->
    Result = (math:sqrt(math:pow(A, 2) + math:pow(B,2)) + A + B),
    {A, B, Result, (Result-B-A)}.

tl(X) ->
    [ run:t(A, B) || B <- lists:seq(1, X),
		      A <- lists:seq(1, X) ].

tt (X, List) 
  when is_integer(X) ->
    tt(X+0.0, List);
tt (X, List) 
  when is_float(X), is_list(List)->
    FoldFun = fun({A,B,Xi,R}, Acc) when Xi =:= X ->
		      Acc ++ [{A,B,Xi,R}];
		 (_, Acc) -> Acc
	      end,
    lists:foldl(FoldFun, [], List).

% bruteforce attack
% could be customized without doublon
euler(X) ->
    List = run:tl(1000),
    [H|T] = tt(1000, List),
    {A, B, Result, C} = H,
    io:format("a=~p, b=~p, c=~p~n", [A, B, round(C)]),
    io:format("abc=~p~n", [round(A*B*C)]).
    

