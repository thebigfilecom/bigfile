-module(big_storage_sup).

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
    ets:new(big_storage, [set, public, named_table, {read_concurrency, true}]),
	ets:new(big_storage_module, [set, public, named_table]),
	{ok, {{one_for_one, 5, 10}, [
		?CHILD(big_storage, worker),
		?CHILD(big_device_lock, worker)
	]}}.
