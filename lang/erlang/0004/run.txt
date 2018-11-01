%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2016, Mathieu Kerjouan mk@steepath.eu
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(run).
% -behaviour().
% -export([]).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

%%
%% @name: is_even
%%
-spec is_even(integer()) -> boolean().
is_even(I) 
  when is_integer(I) ->
    if (I rem 2) =:= 0 ->
	    true;
       true -> 
	    false
    end.
is_even_test() ->
    [?assert(is_even(5) =:= false)
    ,?assert(is_even(2) =:= true)
    ].

%%
%% @name: is_odd
%%
-spec is_odd(integer()) -> boolean().
is_odd(I)
  when is_integer(I) ->
    not is_even(I).
is_odd_test() ->
    [?assert(is_odd(5) =:= true)
    ,?assert(is_odd(2) =:= false) 
    ].

%%
%% @doc: return true if integer is palindrom else
%%       return false.
%%
-spec is_palindrom(integer()) -> boolean().
is_palindrom(I) 
  when is_integer(I) ->
    Is = integer_to_list(I),
    Is_Reverse = lists:reverse(Is),
    if Is =:= Is_Reverse -> true;
       true -> false
    end.

%%
%% @doc: check if an integer is an paladrom and return it if its true.
%%
-type return() :: ret | return.
-spec is_palindrom(integer(),return()) -> integer() | nil.
is_palindrom(I, ret) 
  when is_integer(I) ->
    case is_palindrom(I) of
	true -> I;
	false -> nil
    end;
is_palindrom(I, return) 
  when is_integer(I) ->
    is_palindrom(I,ret).
	       	     
is_palindrom_test() ->
    [?assert(is_palindrom(9009) =:= true)
    ,?assert(is_palindrom(1111) =:= true)
    ,?assert(is_palindrom(123)  =:= false)
    ,?assert(is_palindrom(4322) =:= false)
    ,?assert(is_palindrom(9009, ret) =:= 9009)
    ,?assert(is_palindrom(4329, ret) =:= nil)
    ].

%%
%% @name: gen_palindrom
%%
-spec gen_palindrom(integer()) -> [ integer(), ...].
gen_palindrom(I) 
  when is_integer(I), I>0 ->
    List = lists:seq(0, I),
    [ X || X <- List, is_palindrom(X) ].


%%
%% @doc: check palindromic number in simple sequence of number
%%
-spec gen_palindrom(integer(), integer()) -> [ integer(), ...].
gen_palindrom(M, N) 
  when is_integer(M), is_integer(N), N > M ->
    List = [ X || X <- lists:seq(M, N) ],
    [ X || X <- List, is_palindrom(X, ret) =/= nil ].


%%
%% @doc generate palindromic number from list of product of sequence
%%      of integers.
-spec gen_palindrom(integer(), integer(), atom()) -> [ integer(), ... ].
gen_palindrom(M, N, product) 
  when is_integer(M), is_integer(N), N > M ->
    List = [ X || X <- lists:seq(M, N) ],
    Ret = [ X*Y || X <- List, Y <- List, is_palindrom(X*Y, ret) =/= nil ],
    remove_dups_linear(Ret);

%%
%% @doc generate palindromic number from list of sum of numbers
%%
gen_palindrom(M, N, sum) 
  when is_integer(M), is_integer(N), N > M ->
    List = [ X || X <- lists:seq(M, N) ],
    Ret = [ (X+Y) || X <- List, Y <- List, is_palindrom((X+Y), ret) =/= nil ],
    remove_dups_linear(Ret).

gen_palindrom_test() ->
    [?assert(gen_palindrom(20) =:= [0,1,2,3,4,5,6,7,8,9,11])
    ,?assert(gen_palindrom(50) =:= [0,1,2,3,4,5,6,7,8,9,11,22,33,44])
    ,?assert(gen_palindrom(100,200) =:= [101,111,121,131,141,151,161,171,181,191])
    ,?assert(gen_palindrom(10, 20, product) =:= [121,252,272,323])
    ,?assert(gen_palindrom(300, 310, sum) =:= [606,616])
    ].

%%
%% from http://erlang.org/pipermail/erlang-questions/2004-December/013705.html
%%😌
-spec remove_dups_linear(list()) -> list().
remove_dups_linear(L) ->
    remove_dups_linear(L, gb_sets:new()).
remove_dups_linear([], _) ->
    [];
remove_dups_linear([H|T], S) ->
      case gb_sets:is_member(H, S) of
          true  ->
	      remove_dups_linear(T, S);
	  false -> [H|remove_dups_linear(T, gb_sets:insert(H, S))]
      end.
