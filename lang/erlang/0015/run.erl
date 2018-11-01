%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2016, Mathieu Kerjouan mk@steepath.eu
%%% @doc documentation about this module
%%%      ...
%%% @end
%%%-------------------------------------------------------------------

-module(run).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

% how to represent a grid?
%
% 1x1 grid = (1*2) + (1*2) = 4 sides
% 2x2 grid = (2*3) + (2*3) = 12 sides
% 3x3 grid = (3*4) + (3*4) = 24 sides
% nxm grid = (n*(n+1)) + (m*(m+1)) 
%  _  _  _  _
% |_||_||_||_|
%  _  _  _  _
% |_||_||_||_|
%  _  _  _  _
% |_||_||_||_|
%  _  _  _  _
% |_||_||_||_|
%
% - 1*1: 2 paths
% { r, d } => { 0, 1 }
% { d, r } => { 1, 0 }

% - 1*2: 3 paths
% { r, r, d } => { 0, 0, 1 }
% { r, d, r } => { 0, 1, 0 }
% { d, r, r } => { 1, 0, 0 }

% - 2*1: 3 paths
% { d, d, r } => { 1, 1, 0 }
% { d, r, d } => { 1, 0, 1 }
% { r, d, d } => { 0, 1, 1 }

% - 1*3: 4 paths
% { r, r, r, d } => { 0, 0, 0, 1 }
% { r, r, d, r } => { 0, 0, 1, 0 }
% { r, d, r, r } => { 0, 1, 0, 0 }
% { d, r, r, r } => { 1, 0, 0, 0 }

% - 3*1: 4 paths
% { d, d, d, r } => { 1, 1, 1, 0 }
% { d, d, r, d } => { 1, 1, 0, 1 }
% { d, r, d, d } => { 1, 0, 1, 1 }
% { r, d, d, d } => { 0, 1, 1, 1 }

% - 2*2: 6 paths
% { r, r, d, d} => { 0, 0, 1, 1 }
% { r, d, r, d} => { 0, 1, 0, 1 }
% { r, d, d, r} => { 0, 1, 1, 0 }
% { d, r, r, d} => { 1, 0, 0, 1 }
% { d, r, d, r} => { 1, 0, 1, 0 }
% { d, d, r, r} => { 1, 1, 0, 0 }

% - 3*2: 
% { r, r, r, d, d}
% { r, r, d, r, d}
% { r, r, d, d, r}
% { r, d, d, r, r}
% ...
% { d, d, r, r, r}

% - 2*3
% { r, r, d, d, d}
% { d, d, d, r, r}

% - 3*3: 20 paths
% { r, r, r, d, d, d } => { 0, 0, 0, 1, 1, 1 }
% { r, r, d, r, d, d } => { 0, 0, 1, 0, 1, 1 }
% { r, r, d, d, r, d } => { 0, 0, 1, 1, 0, 1 }
% { r, r, d, d, d, r } => { 0, 0, 1, 1, 1, 0 }
% { r, d, r, r, d, d } => { 0, 1, 0, 0, 1, 1 }
% { r, d, d, r, r, d } => { 0, 1, 1, 0, 0, 1 }
% { r, d, d, d, r, r } => { 0, 1, 1, 1, 0, 0 }
% { r, d, r, d, r, d } => { 0, 1, 0, 1, 0, 1 }
% ...
% { d, d, d, r ,r ,r } => { 1, 1, 1, 0, 0, 0 }

% - 4*4: 70 paths
% { r, r, r, r, d, d, d, d } => { 0, 0, 0, 0, 1, 1, 1, 1 }
% ...
% { d, d, d, d, r, r, r, r } => { 1, 1, 1, 1, 0, 0, 0, 0 }

% - rules:
%   1. only two possible ways {0, 1}.
%   2. m*n grid is m+n ways

% - notes:
%  {0, [RestZero]}, {1, [RestOne]}
%  0 -> {0, [RestZero-1]}, {1, [RestOne]}.
%  0,1 -> {0, [RestZeo]

way() -> [0,1].

down(X) ->
    Size = lists:seq(1, round(X/2)),
    lists:map(fun(_) -> 0 end, Size).
right(X) ->
    Size = lists:seq(1, round(X/2)),
    lists:map(fun(_) -> 1 end, Size).

-spec generator(integer(), integer()) -> [integer(), ...].
generator_test() ->
    [?assert(generator(1,1) =:= [0,1])
    ,?assert(generator(2,2) =:= [0,0,1,1])
    ,?assert(generator(3,3) =:= [0,0,0,1,1,1])
    ].
generator(M, N) ->
    Ways = M + N,
    down(Ways) ++ right(Ways).



%%--------------------------------------------------------------------
%% bruteforce.
%%--------------------------------------------------------------------
bruteforce(1) ->
    [ { list_to_integer(lists:concat([A]), 2)
      , [A] } || A <- way() ];
bruteforce(2) -> 
    [ { list_to_integer(lists:concat([A, B, C, D]), 2)
      , [A, B, C, D] } 
      || A <- way(), B <- way(), 
	 C <- way(), D <- way(), 
	 lists:sum([A,B,C,D]) =:= 2];
bruteforce(3) -> 
    [ { list_to_integer(lists:concat([A, B, C, D, E, F ]), 2)
      , [A, B, C, D, E, F ] }
      || A <- way(), B <- way(), C <- way(),
	 D <- way(), E <- way(), F <- way(),
	 lists:sum([A,B,C,D,E,F]) =:= 3];
bruteforce(4) ->
    [ { list_to_integer(lists:concat([A, B, C, D
			,E, F, G, H]), 2)
      , [A, B, C, D
	,E, F, G, H ] }
      || A <- way(), B <- way(), C <- way(), D <- way(),
	 E <- way(), F <- way(), G <- way(), H <- way(),
	 lists:sum([A,B,C,D,E,F,G,H]) =:= 4];

bruteforce(wip) -> 
    [ [A, B, C, D, E
      ,F, G, H, I, J
      ,K, L, M, N, O
      ,P, Q, R, S, T] 
      || A <- way(), B <- way(), C <- way(), D <- way(), E <- way(),
	 F <- way(), G <- way(), H <- way(), I <- way(), J <- way(),
	 K <- way(), L <- way(), M <- way(), N <- way(), O <- way(),
	 P <- way(), Q <- way(), R <- way(), S <- way(), T <- way(),
	 lists:sum([A, B, C, D, E, 
		    F, G, H, I, J, 
		    K, L, M, N, O, 
		    P, Q, R, S, T]) =:= 10 ].

fact(N) ->
    Fact = fun(X, Acc) -> X*Acc end,
    Seq = lists:seq(1,N),
    lists:foldr(Fact, 1, Seq).

binomial_coefficient(N, K) ->
    fact(N) / (fact(N-K) * fact(K)).

euler(M) ->
    binomial_coefficient(M*2, M).

euler(M, N) ->
    binomial_coefficient(M+N, trunc((M+N)/2)).
    
solution() ->
    round(euler(20)).
