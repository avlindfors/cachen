%%%-------------------------------------------------------------------
%% @doc cachen top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cachen_sup).

-behaviour(supervisor).

-include("cachen.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Cache =
        {cachen_master,
            %% Registering the name of the cache so we don't need to keep track of PID
            {lru, start_link, [?LRU_NAME, 4, []]},
            permanent, 5000, worker, [lru]},
    {ok, { {one_for_one, 10, 10}, [Cache]} }.

%%====================================================================
%% Internal functions
%%====================================================================
