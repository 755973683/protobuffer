%%%-------------------------------------------------------------------
%% @doc protobuff public API
%% @end
%%%-------------------------------------------------------------------

-module(protobuff_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    protobuff_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
