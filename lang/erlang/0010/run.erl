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

%%--------------------------------------------------------------------
%% Working on Matrix Datastructure:
%%
%% standard list representation:
%%   {list, Matrix::[list(), ...]}.
%%     e.g. {list, [[1,2,3],[4,5,6],[7,8,9]]}
%% 
%% binary representation:
%%   {binary, Matrix::iolist(), Row::integer(), Column::integer()}.
%%     e.g. {binary, <<1,2,3,4,5,6,7,8,9>>, 3, 3}.
%%
%% binary representation v2:
%%   {binary, {Matrix::iolist(), Size::integer()}, 
%%            Row::integer(), Column::integer()}.
%%--------------------------------------------------------------------

grid() ->
    grid(raw).

grid(raw) ->
    <<"0802229738150040007504050778521250779108",
      "4949994017811857608717409843694804566200",
      "8149317355791429937140675388300349133665",
      "5270952304601142692468560132567137023691",
      "2231167151676389419236542240402866331380",
      "2447326099034502447533537836842035171250",
      "3298812864236710263840675954706618386470",
      "6726206802621220956394396308409166499421",
      "2455580566739926971778789683148834896372",
      "2136230975007644204535140061339734313395",
      "7817532822753167159403800462161409535692",
      "1639054296353147555888240017542436298557",
      "8656004835718907054444374460215851541758",
      "1980816805944769287392138652177704895540",
      "0452088397359916079757321626267933279866",
      "8836688757622072034633674655123263935369",
      "0442167338253911249472180846293240627636",
      "2069364172302388346299698267598574043616",
      "2073352978319001743149714886811623570554",
      "0170547183515469169233486143520189196748">>;

grid(converted) ->
    grid({convert, 2*8});
grid({convert, PatternSizeIn}) ->
    grid({convert, PatternSizeIn, PatternSizeIn});
grid({convert, PatternSizeIn, PatternSizeOut}) ->
    grid({convert, PatternSizeIn, PatternSizeIn, grid(raw)});
grid({convert, PatternSizeIn, PatternSizeOut, Grid}) ->
    bit_converter(Grid, PatternSizeIn, PatternSizeOut);
grid(binary) ->
    {binary, grid(), 20, 20}.


bit_converter(Binary, PatternSizeOut, PatternSizeIn) ->
    << <<(X):PatternSizeOut>> || <<X:PatternSizeIn>> <= Binary >>.


-type matrix() :: [[integer()], ...].
% what is an array/matrix?
%
%   0    1  2  3      
% 0 +------------> x
%   |
% 1 |  [[1, 2, 3]
% 2 |  ,[4, 5, 6]
% 3 |  ,[7, 8, 9]]
%  \/
%
% {X,Y} 
% X =< length(line(X))
% Y =< length(colomn(Y))
% X > 0 
% Y > 0
%
% <<1,2,3,4,5,6,7,8,9>>

-spec matrix(Data::iolist(), Line::integer()) -> 
		    {ok, matrix()} | {error, list()}.
matrix(Data, Line) 
  when (byte_size(Data) rem Line) =:= 0 ->
    Column = round(byte_size(Data)/Line),
    {ok, matrix(Data, Line, Column, [])};
matrix(Data, Line)
  when (byte_size(Data) rem Line) =/= 0 ->
    {error, "This matrix could not have this number of line"}.

-spec matrix(Data::iolist(), Line::integer(), integer(), Matrix::matrix()) ->
		    matrix().
matrix(<<>>, 0, _, Matrix) ->
    Matrix;
matrix(Data, Line, Column, Matrix) ->
    Size = 8*Column,
    CharSize = 8*2,
    <<L:(Size)/bitstring, Rest/bitstring>> = Data,
    Elements = [ binary_to_integer(X) 
		 || <<X:CharSize/bitstring>> <= L ],
    matrix(Rest, Line-1, Column, Matrix++[Elements]).

%%--------------------------------------------------------------------
%% LINE:
%% [1,2,3],
%% [4,5,6],
%% [7,8,9]
%% --
%% [{1,1}, {2,1}, {3,1}],
%% [{1,2}, {2,2}, {3,2}],
%% [{1,3}, {2,3}, {3,3}].
%% --
%% {<<1,2,3,4,5,6,7,8,9>>, 3, 3}
%%--------------------------------------------------------------------
line_test() ->
    Data = {binary, <<"010203040506070809">>, 3, 3},    
    Grid = grid(binary),
    [?assertEqual(line(Data,01),[1,2,3])
    ,?assertEqual(line(Data,02),[4,5,6])
    ,?assertEqual(line(Data,03),[7,8,9])
    ,?assertEqual(line(Grid,01),[08,02,22,97,38,15,00,40,00,75
				,04,05,07,78,52,12,50,77,91,08])
    ,?assertEqual(line(Grid,20),[01,70,54,71,83,51,54,69,16,92
				,33,48,61,43,52,01,89,19,67,48])
    ].
-spec line(Matrix :: matrix(), Line :: integer()) -> list().
line(Matrix, Line)
  when is_tuple(Matrix), is_integer(Line) -> 
    {binary, _, _, ColumnSize} = Matrix,
    [ item(Matrix, {X, Line}, 2*8) || X <- lists:seq(1,ColumnSize) ].

%%--------------------------------------------------------------------
%% COLUMN:
%% [1,4,7],
%% [2,5,8],
%% [3,6,9].
%% --
%% [{1,1}, {1,2}, {1,3}],
%% [{2,1}, {2,2}, {2,3}],
%% [{3,1}, {3,2}, {3,3}].
%% --
%% {<<1,4,7,2,5,8,3,6,9>>, 3, 3}
%% --
%% << <<(run:matrix_binary(run:grid(binary), {X,Y}))>> 
%%     || X <- lists:seq(1,20), Y <- lists:seq(1,20) >>
%%--------------------------------------------------------------------
column_test() ->
    Data = {binary, <<"010203040506070809">>, 3, 3},
    Grid = grid(binary),    [?assertEqual(column(Data,1), [1,4,7])
    ,?assertEqual(column(Data,2), [2,5,8])
    ,?assertEqual(column(Data,3), [3,6,9])
    ,?assertEqual(column(Grid,1), [8,49,81,52,22,24,32,67,24,21
				  ,78,16,86,19,4,88,4,20,20,1])
    ,?assertEqual(column(Grid,20), [8,0,65,91,80,50,70,21,72,95
				   ,92,57,58,40,66,69,36,16,54,48])
    ].
-spec column(Matrix :: matrix(), Column :: integer()) -> list().
column(Matrix, Column) 
  when is_tuple(Matrix), is_integer(Column) ->
    {binary, _, _, ColumnSize} = Matrix,
    [ item(Matrix, {Column, X},2*8) || X <- lists:seq(1,ColumnSize) ].

%%--------------------------------------------------------------------
%% DIAGONALE (right to left):
%% [[1],[2,4],[3,5,7],[6,8],[9]].
%% --
%% [{1,1}],
%% [{2,1},{1,2}],
%% [{3,1},{2,2},{1,3}],
%% [{3,2},{2,3}],
%% [{3,3}]
%% --
%% <<1,2,4,3,5,7,6,8,9>>
%%--------------------------------------------------------------------
diagonale_rl_test() ->
    %Data = {binary, <<"010203040506070809">>, 3, 3},
    %Grid = grid(binary),
    [%?assert(diagonale_rl(Data) =:= [[1],[2,4],[3,5,7],[6,8],[9]])
    ].
-spec diagonale_rl(Matrix :: matrix(), Diagonale :: integer()) -> list().
diagonale_rl(_, _) ->
    ok.


%%--------------------------------------------------------------------
%% DIAGONALE (left to right):
%% [[7],[4,8],[1,5,9],[2,6],[3]].
%% --
%% [{1,3}],
%% [{1,2},{2,1}],
%% [{1,1},{2,2},{3,3}],
%% [{3,2},{2,3}],
%% [{3,3}]
%% --
%% <<7,4,8,1,5,9,2,6,3>>
%% -- 
%% upper:
%% [ run:matrix_binary(run:grid(binary), {X,X-2}) || X <- lists:seq(3,20)].
%% lower:
%% [ run:matrix_binary(run:grid(binary), {X-2,X}) || X <- lists:seq(3,20)].
%% -- 
%% {X-0,  X} || X <- lists:seq(01,20)   |\ 20-1 => 19 
%% {X-19, X} || X <- lists:seq(20,20)   |/ 
%% {X,  X-0} || X <- lists:seq(01,20)   |\ 20-1 => 19
%% {X, X-19} || X <- lists:seq(20,20)   |/
%% [ matrix_binary(Matrix, {X-(Shift),X}) 
%%   || X <- lists:seq(Position,Size-1)].
%% 
%%--------------------------------------------------------------------
diagonale_lr_test() ->
    Data = {binary, <<"010203040506070809">>, 3, 3},
    Grid = grid(binary),
    [?assertEqual(diagonale_lr(Data, 1), [4,8])
    ,?assertEqual(diagonale_lr(Data, 2), [7])
    ,?assertEqual(diagonale_lr(Data, 3), [1,5,9])
    ,?assertEqual(diagonale_lr(Data, 5), [3])
    ,?assertEqual(diagonale_lr(Data, 6), [1,5,9])
    ,?assertEqual(diagonale_lr(Grid, 0), [8,49,31,23,51,3,67,20,97,45
					 ,3,24,44,52,26,32,40,4,5,48])
    ,?assertEqual(diagonale_lr(Grid, 19), [1])
    ,?assertEqual(diagonale_lr(Grid, 39), [8])
    ].
-spec diagonale_lr(Matrix :: matrix(), Diagonale :: integer()) -> list().
diagonale_lr(Matrix, Diagonale) 
  when is_integer(Diagonale) ->
    {binary, _, Row, Column} = Matrix,
    Size = round((Row+Column)/2),
    diagonale_lr(Matrix, Diagonale, Size).

-spec diagonale_lr(Matrix :: matrix(), Diagonale :: integer(),
		   Size :: integer()) ->
			  list().
diagonale_lr({binary, Matrix, Row, Column}, Diagonale, Size) 
  when (Diagonale rem Size) =:= 0->
    Struct = {binary, Matrix, Row, Column},
    [ item(Struct, {X,X}, 2*8) 
      || X <- lists:seq(1,Size)];

diagonale_lr({binary, Matrix, Row, Column}, Diagonale, Size) 
  when Diagonale < Size ->
    Struct = {binary, Matrix, Row, Column},
    Shift = (Diagonale rem Size),
    Position = (Diagonale rem Size)+1,
    [ item(Struct, {X-(Shift),X}, 2*8)
      || X <- lists:seq(Position, Size)];

diagonale_lr({binary, Matrix, Row, Column}, Diagonale, Size)
  when Diagonale > Size ->
    Struct = {binary, Matrix, Row, Column},
    Shift = (Diagonale rem Size),
    Position = (Diagonale rem Size)+1,
    [ item(Struct, {X, X-(Shift)}, 2*8)
      || X <- lists:seq(Position, Size)].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
matrix_binary_test() ->
    Grid = grid(binary),
    [?assertEqual(matrix_binary(Grid, {1,1}),    8)
    ,?assertEqual(matrix_binary(Grid, {20,20}), 48)
    ,?assertEqual(matrix_binary(Grid, {1,20}),   1)
    ,?assertEqual(matrix_binary(Grid, {20,1}),   8)
    ,?assertEqual(matrix_binary(Grid, {10,1}),  75)
    ,?assertEqual(matrix_binary(Grid, {10,10}), 45)
    ].
-spec matrix_binary(Matrix :: matrix(), {Row :: integer(), Column :: integer()}) -> 
			   integer().
matrix_binary({binary, Matrix, Row, Column}, {X, Y}) 
  when X>0, Y>0, 
       X=<Row, Y=<Column ->
    PatternSize = 2*8,
    item({binary, Matrix, Row, Column}, {X, Y}, PatternSize).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
item_test() ->
    [].
-spec item(Matrix:: matrix(), {Row :: integer(), Column :: integer()}, 
	   PatternSize::integer()) ->
		  integer().
item({binary, Matrix, _, _}, {1, 1}, PatternSize) ->
    <<Element:(PatternSize)/bitstring, 
      _/bitstring>> = Matrix,
    binary_to_integer(Element);

item({binary, Matrix, Row, Column}, {X, Y}, PatternSize)
  when  X =:= Row, Y =:= Column ->
    Shift = PatternSize*((Row*Column)-1),
    <<_:(Shift)/bitstring, Element:(PatternSize)/bitstring>> = Matrix,
    binary_to_integer(Element);

item({binary, Matrix, Row, _}, {X, Y}, PatternSize) ->
    % thanks to harrynoob@math@freenode
    Shift= PatternSize*((((Y-1)*Row)+X)-1),
    <<_:(Shift)/bitstring, 
      Element:(PatternSize)/bitstring, 
      _/bitstring>> = Matrix,
    binary_to_integer(Element).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
euler_line(list) ->
    [ list_to_binary(run:line(run:grid(binary), X)) || X <- lists:seq(1,20) ];
euler_line(binary) ->
    << <<X/bitstring>> || X <- euler_line(list) >>.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
euler_column(list) ->
    [ list_to_binary(run:column(run:grid(binary), X)) || X <- lists:seq(1,20) ];
euler_column(binary) ->
    << <<X/bitstring>> || X <- euler_column(list) >>.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
euler_diagonale(list) ->
    [ list_to_binary(run:diagonale_lr(run:grid(binary), X)) 
      || X <- lists:seq(1,40) ];
euler_diagonale(binary) ->
    << <<X/bitstring>> || X <- euler_diagonale(list) >>.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
reverse() ->
    Size = 2*8,
    FunColumn = fun(X) ->
			R = run:column(run:grid(binary), X),
			<< <<(integer_to_binary(Y))/bitstring>> || Y <- R, Y<10 >>
		end,
    Grid = << <<(FunColumn(X))/bitstring>> 
		|| X <- lists:seq(1,20) >>, 
    {binary, Grid, 20, 20}.

%%--------------------------------------------------------------------
%%
%% [ run:euler_product(X) || X <- run:euler_line(list) ].
%% [ run:euler_product(X) || X <- run:euler_column(list) ].
%% [ run:euler_product(X) || X <- run:euler_diagonale(list) ].
%%--------------------------------------------------------------------

euler_product({binary, Matrix, Row, Column}) ->
    euler_product(Matrix);
euler_product(Matrix) 
  when is_binary(Matrix) ->
    euler_product(Matrix, #{}).

euler_product(<<>>, Buf) ->
    Buf;
euler_product(<<_:16>>, Buf) ->
    Buf;
euler_product(<<_:16,_:16>>, Buf) ->
    Buf;
euler_product(<<_:16,_:16,_:16>>, Buf) ->
    Buf;
euler_product(Matrix, Buf) ->
    Size = 2*8,
    <<A:(Size)/bitstring, B:(Size)/bitstring, 
      C:(Size)/bitstring, D:(Size)/bitstring, 
      Rest/bitstring>> = Matrix,
    Product = binary_to_integer(A)*binary_to_integer(B)
	*binary_to_integer(C)*binary_to_integer(D),
    euler_product(<<B/bitstring, C/bitstring, D/bitstring, Rest/bitstring>>, maps:put(Product, {A,B,C,D}, Buf)).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
euler_solution() ->
    [ProductLine|_] = lists:reverse(lists:usort(maps:keys(run:euler_product(run:euler_line(binary))))),
    [ProductColumn|_] = lists:reverse(lists:usort(maps:keys(run:euler_product(run:euler_column(binary))))),
    [ProductDiagonale|_] = lists:reverse(lists:usort(maps:keys(run:euler_product(run:euler_diagonale(binary))))),
    {ProductLine, ProductColumn, ProductDiagonale}.
