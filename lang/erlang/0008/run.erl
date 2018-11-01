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

euler_digit() -> 
    <<"73167176531330624919225119674426574742355349194934",
      "96983520312774506326239578318016984801869478851843",
      "85861560789112949495459501737958331952853208805511",
      "12540698747158523863050715693290963295227443043557",
      "66896648950445244523161731856403098711121722383113",
      "62229893423380308135336276614282806444486645238749",
      "30358907296290491560440772390713810515859307960866",
      "70172427121883998797908792274921901699720888093776",
      "65727333001053367881220235421809751254540594752243",
      "52584907711670556013604839586446706324415722155397",
      "53697817977846174064955149290862569321978468622482",
      "83972241375657056057490261407972968652414535100474",
      "82166370484403199890008895243450658541227588666881",
      "16427171479924442928230863465674813919123162824586",
      "17866458359124566529476545682848912883142607690042",
      "24219022671055626321111109370544217506941658960408",
      "07198403850962455444362981230987879927244284909188",
      "84580156166097919133875499200524063689912560717606",
      "05886116467109405077541002256983155200055935729725",
      "71636269561882670428252483600823257530420752963450">>.



shift(Shift, Bitstring) 
  when is_integer(Shift), 
       is_binary(Bitstring) ->
    Shifted = 8*Shift,
    <<Start:(Shifted)/bitstring, Rest/bitstring>> = Bitstring,
    {Start, Rest}.

loop(Shift, Bitstring) ->
    loop(Shift, Bitstring, #{}).

loop(Shift, {Start, <<>>}, Buf) 
  when is_integer(Shift) ->
    Buf2 = maps:put(product(Start), Start, Buf),
    print_product(Start),
    Buf2;
loop(Shift, {Start, Rest}, Buf)
  when is_integer(Shift) ->
    Bitstring = <<Start/bitstring, Rest/bitstring>>,
    Buf2 = maps:put(product(Start), Start, Buf),
    print_product(Start),
    <<_, Next/bitstring>> = Bitstring,
    loop(Shift, shift(Shift, Next), Buf2);
loop(Shift, Bitstring, Buf) 
  when is_integer(Shift) ->
    loop(Shift, shift(Shift, Bitstring), Buf).

product(Binary) when is_binary(Binary) ->
    product(binary_to_list(Binary));
product(List) when is_list(List) ->
    ConvertedList = [ list_to_integer([X]) || X <- List ],
    lists:foldr(fun(Elem, Acc) -> Elem * Acc end, 1, ConvertedList).

print_product(Binary) 
  when is_binary(Binary) ->
    print_product(binary_to_list(Binary));
print_product(List)
  when is_list(List) ->
    io:format("~p, ~p~n", [list_to_binary(List), product(List)]).

handle_product(List, Fun)
  when is_list(List), is_function(Fun) ->
    Fun(List).
				 



