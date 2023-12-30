%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(toppage_h).
-include("test_pb_pb.hrl").

-export([init/2]).

init(Req0, Opts) ->
	{Answer, ChangeGrid, Cells} = sudoku:generate_fixed(20),
  TupleList = [#tuple_list{row = Row, col = Col}||{Row, Col}<-Cells],
	Res = test_pb_pb:encode_msg(#get_info{cells = TupleList}),
	io:format("Res ==> ~p~n", [Res]),
	ResData = erlang:iolist_to_binary(Res),
	io:format("ResData ==> ~p~n", [ResData]),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain">>
	}, jsx:encode([{"status", [{1,2}, {1,2}]}]), Req0),
	{ok, Req, Opts}.

