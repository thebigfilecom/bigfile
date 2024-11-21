-module(big_mining_sup).

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
	%% We'll create workers for all configured parititions - even those partitions that
	%% currently exceed the weave size. Those workers will just lie dormant until the
	%% weave size grows to meet them.
	MiningWorkers = lists:map(
		fun({Partition, _MiningAddr, PackingDifficulty}) ->
			?CHILD_WITH_ARGS(
				big_mining_worker, worker, big_mining_worker:name(Partition, PackingDifficulty),
					[Partition, PackingDifficulty])
		end,
		big_mining_io:get_partitions(infinity)
	),
	Children = MiningWorkers ++ [
		?CHILD(big_mining_server, worker),
		?CHILD(big_mining_hash, worker),
		?CHILD(big_mining_io, worker),
		?CHILD(big_mining_stats, worker)
	],
	{ok, {{one_for_one, 5, 10}, Children}}.
