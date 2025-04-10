-module(big_kv_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include_lib("bigfile/include/big_sup.hrl").
-include_lib("bigfile/include/big_config.hrl").

%%%===================================================================
%%% Public interface.
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks.
%% ===================================================================

init([]) ->
	big_kv:create_ets(),
	{ok, {{one_for_one, 5, 10}, [?CHILD(big_kv, worker)]}}.
