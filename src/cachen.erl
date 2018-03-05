%%%-------------------------------------------------------------------
%%% @author Alexander Lindfors
%%% @copyright (C) 2018, Alexander Lindfors
%%% @doc
%%%
%%% @end
%%% Created : 27. Feb 2018 13:14
%%%-------------------------------------------------------------------
-module(cachen).
-author("Alexander Lindfors").

-include("cachen.hrl").

%% API
-export([start/0]).

%% Used by cowboy router
-export([init/2]).

%% Utility functions for cachen
-export([
    items/0,
    purge/0,
    size/0,
    max_size/0,
    set_max_size/1
]).

start() ->
    error_logger:info_msg("Starting cachen.. ~n").

init(Req0, Opts) ->
    Res = hit_or_miss_and_reply(Req0),
    {ok, Res, Opts}.

purge() ->
    lru:purge(?LRU_NAME).

items() ->
    Keys = lru:keys(?LRU_NAME),
    [{X, lru:peek(?LRU_NAME, X)} || X <- Keys].

size() ->
    lru_info(size).

max_size() ->
    lru_info(max_size).

set_max_size(NewSize) ->
    OldSize = max_size(),
    ok = lru:resize(?LRU_NAME, NewSize),
    [{oldsize, OldSize}, {newsize, NewSize}].

%% Internal
hit_or_miss_and_reply(Req0) ->
    ReqPath = cowboy_req:path(Req0),
    case lru:get(?LRU_NAME, ReqPath) of
        undefined ->
            %% Do some work, send back some data
            timer:sleep(2000),
            Body = ReqPath,
            Reply = cowboy_req:reply(200,
                #{<<"content-type">> => <<"text/plain">>},
                <<"Path: ", Body/binary>>,
                Req0),
            lru:add(?LRU_NAME, ReqPath, Body),
            error_logger:info_msg("~p was a MISS~n", [ReqPath]),
            Reply;
        CachedResponseBody ->
            %% The entry existed, reply with cached data
            Reply = cowboy_req:reply(200,
                #{<<"content-type">> => <<"text/plain">>},
                <<"From cache: ", CachedResponseBody/binary>>,
                Req0),
            error_logger:info_msg("~p was a HIT~n", [ReqPath]),
            Reply
    end.

lru_info(Prop) ->
    Info = lru:info(?LRU_NAME),
    proplists:get_value(Prop, Info).