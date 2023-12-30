%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 12月 2023 15:12
%%%-------------------------------------------------------------------
-module(sudoku).
-author("Administrator").
-export([generate/0]).
-export([generate_fixed/1]).

% 生成数独终盘
generate() ->
  {Board, _} = generate_board(empty_board()),
  Board.
empty_board() ->
  [[0 || _ <- lists:seq(1, 9)] || _ <- lists:seq(1, 9)].

% 生成数独终盘并计算可填数字的数量
generate_board(Board) ->
  case next_empty_cell(Board) of
    {none, _, _} ->
      {Board, count_filled_cells(Board)};
    {Cell, _Cells, FillCells} ->
      AvailableDigits = available_digits(Board, Cell),
      case AvailableDigits of
        [] ->
          backtrack(Board, Cell, FillCells);
        Digits ->
          TmpNum = random:uniform(length(Digits)),
          RandomDigit = lists:nth(TmpNum, Digits),
          FilledBoard = fill_cell(Board, Cell, RandomDigit),
          generate_board(FilledBoard)
      end
  end.

match(false) ->
  ets:insert(global_vars, {count, 1});
match(true) ->
  case ets:lookup(global_vars, count) of
    [{count, Num}] ->
      ets:insert(global_vars, {count, Num + 1});
    _ ->
      ets:insert(global_vars, {count, 1})
  end.

% 寻找下一个待填充的单元格
next_empty_cell(Board) ->
  find_empty_cell(Board, {1, 1}, []).
find_empty_cell(_, {10, _}, FillCells) ->
  {none, [], FillCells};
find_empty_cell(Board, {Row, 10}, FillCells) ->
  find_empty_cell(Board, {Row + 1, 1}, FillCells);
find_empty_cell(Board, {Row, Col}, FillCells) ->
  case cell_value(Board, {Row, Col}) of
    0 ->
      {Cell, Cells, NewCells} = find_empty_cell(Board, {Row, Col + 1}, FillCells),
      {{Row, Col}, [Cell | Cells], NewCells};
    _ ->
      find_empty_cell(Board, {Row, Col + 1}, [{Row, Col}] ++ FillCells)
  end.

% 填充指定单元格
fill_cell(Board, {Row, Col}, Digit) ->
  set_cell_value(Board, {Row, Col}, Digit).

% 回溯
backtrack(Board, _Cell, Cells) ->
  OldFirstRow = lists:sublist(Cells, 9),
  NewFirstRow = lists:sublist(Cells, 9),
  match(OldFirstRow == NewFirstRow),
  case Cells of
    [] ->
      {Board, count_filled_cells(Board)};
    [Cell1 | _Rest] = Cells ->
      {_Row, Col} = Cell1,
      NewNum =
        case ets:lookup(global_vars, count) of
          [{count, Num}] ->
            Num;
          _ ->
            1
        end,
      ClearCells =
        if
          NewNum > 100 ->
            ets:insert(global_vars, {count, 1}),
            lists:sublist(Cells, length(Cells));
          true ->
            lists:sublist(Cells, Col)
        end,
      NewFilledBoard =
        lists:foldl(fun({TmpRow, TmpCol}, Board) ->
          clear_cell(Board, {TmpRow, TmpCol})
                    end, Board, ClearCells),
      generate_board(NewFilledBoard)
  end.

% 清除指定单元格
clear_cell(Board, {Row, Col}) ->
  set_cell_value(Board, {Row, Col}, 0).

set_cell_value(Board, {Row, Col}, Digit) ->
  RowsBefore = lists:sublist(Board, Row - 1),
  CurrentRow = lists:nth(Row, Board),
  RowsAfter = lists:nthtail(Row, Board),
  BeforeCol = lists:sublist(CurrentRow, Col - 1),
  AfterCol = lists:nthtail(Col, CurrentRow),
  RowsBefore ++ [BeforeCol ++ [Digit] ++ AfterCol] ++ RowsAfter.

% 获取指定单元格的值
cell_value(Board, {Row, Col}) ->
  lists:nth(Col, lists:nth(Row, Board)).

% 计算已填充单元格的数量
count_filled_cells(Board) ->
  count_filled_cells(Board, 0).

count_filled_cells([], Count) ->
  Count;
count_filled_cells([Row | Rest], Count) ->
  count_filled_cells(Rest, Count + count_row_cells(Row)).

count_row_cells([]) ->
  0;
count_row_cells([Cell | Rest]) when Cell > 0 ->
  1 + count_row_cells(Rest);
count_row_cells([_ | Rest]) ->
  count_row_cells(Rest).

% 获取指定单元格可用的数字
available_digits(Board, {Row, Col}) ->
  RowDigits = row_digits(Board, Row),
  ColDigits = col_digits(Board, Col),
  BlockDigits = block_digits(Board, {Row, Col}),
  Result = lists:subtract(lists:seq(1, 9), RowDigits ++ ColDigits ++ BlockDigits),
  case length(Result) of
    0 -> [];
    1 -> case istrue(Board, Row, Col, lists:nth(1, Result)) of
           true -> Result;
           _ -> []
         end;
    _ -> Result
  end.

row_digits(Board, Row) ->
  lists:filter(fun(Digit) -> Digit > 0 end, lists:nth(Row, Board)).

col_digits(Board, Col) ->
  lists:filter(fun(Digit) -> Digit > 0 end, [lists:nth(Col, lists:nth(Row, Board)) || Row <- lists:seq(1, 9)]).

block_digits(Board, {Row, Col}) ->
  BlockRow = (Row - 1) div 3 + 1,
  BlockCol = (Col - 1) div 3 + 1,
  BlockRows = lists:seq((BlockRow - 1) * 3 + 1, BlockRow * 3),
  BlockCols = lists:seq((BlockCol - 1) * 3 + 1, BlockCol * 3),
  lists:filter(fun(Digit) ->
    Digit > 0 end, [lists:nth(BCol, lists:nth(BRow, Board)) || BRow <- BlockRows, BCol <- BlockCols]).

istrue(Board, X, Y, Num) ->
  Pred = fun(I) ->
    NewX = lists:nth(I, lists:nth(X, Board)),
    NewY = lists:nth(Y, lists:nth(I, Board)),
    is_not_equal(NewX, Num) and is_not_equal(NewY, Num)
         end,
  IsUni = lists:all(Pred, lists:seq(1, 9)),
  List = config:get({X, Y}),
  Pred1 = fun({TmpX, TmpY}) ->
    TmpNum = lists:nth(TmpY, lists:nth(TmpX, Board)),
    is_not_equal(TmpNum, Num)
          end,
  IsJiu = lists:all(Pred1, List),
  IsUni and IsJiu.

is_not_equal(Num1, Num2) ->
  Num1 /= Num2.

%%生成指定已填数字的格子
generate_fixed(Num) ->
  NewBoard = generate(),
  {AfterBoard, NewBlankCells} = generate_empty_sudoku(NewBoard, 81-Num, []),
  {NewBoard, AfterBoard, NewBlankCells}.

generate_empty_sudoku(AfterBoard, 0, BlankCells) ->
  {AfterBoard, BlankCells};
generate_empty_sudoku(Board, Num, BlankCells) ->
  Row = rand:uniform(9),
  Col = rand:uniform(9),
  NewBoard = set_cell_value(Board, {Row, Col}, 0),
  NewBlankCells = [{Row, Col}]++BlankCells,
  generate_empty_sudoku(NewBoard, Num-1, NewBlankCells).