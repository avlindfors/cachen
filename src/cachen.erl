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
-export([init/2, start/0]).

-export([
    add/2,
    get/1,
    exists/1,
    peek/1,
    clear/0,
    items/0,
    max_size/0,
    set_max_size/1
]).

-export([populate/1]).

start() ->
    error_logger:info_msg("Starting cachen.. ~n"),
    application:ensure_all_started(?MODULE).

init(Req0, Opts) ->
    ReqPath = cowboy_req:path(Req0),
    Res =
        case ?MODULE:get(ReqPath) of
            undefined ->
                error_logger:info_msg("Miss~n"),
                Val = cowboy_req:reply(200, #{
                    <<"content-type">> => <<"text/plain">>
                }, <<"Hello world!">>, Req0),
                ?MODULE:add(ReqPath, Val),
                error_logger:info_msg("~p was a MISS~n", [ReqPath]),
                Val;
            Hit ->
                error_logger:info_msg("~p was a HIT~n", [ReqPath]),
                Hit
        end,
    %Req = cowboy_req:reply(200, #{
    %    <<"content-type">> => <<"text/plain">>
    %}, <<"Hello world!">>, Req0),
    %error_logger:info_msg("Sending req: ~p~n", [Opts]),
    {ok, Res, Opts}.

add(K,V) ->
    lru:add(?LRU_NAME, K,V).

get(K) ->
    lru:get(?LRU_NAME, K).

exists(K) ->
    lru:contains(?LRU_NAME, K).

peek(K) ->
    lru:peek(?LRU_NAME, K).

clear() ->
    lru:purge(?LRU_NAME).

items() ->
    info(size).

max_size() ->
    info(max_size).

set_max_size(NewSize) ->
    OldSize = max_size(),
    ok = lru:resize(?LRU_NAME, NewSize),
    [{oldsize, OldSize}, {newsize, NewSize}].

%% Test only
populate(N) ->
    lists:foreach(
        fun(X) ->
            K = list_to_atom("k"++integer_to_list(X)),
            V = list_to_atom("v"++integer_to_list(X)),
            cachen:add(K,V)
        end, lists:seq(1,N)).

%% Internal
info(Prop) ->
    Info = lru:info(?LRU_NAME),
    proplists:get_value(Prop, Info).