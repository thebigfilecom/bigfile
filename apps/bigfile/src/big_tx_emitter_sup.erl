-module(big_tx_emitter_sup).

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
	{ok, Config} = application:get_env(bigfile, config),
	Workers = lists:map(
		fun(Num) ->
			Name = list_to_atom("big_tx_emitter_worker_" ++ integer_to_list(Num)),
			{Name, {big_tx_emitter_worker, start_link, [Name]}, permanent, ?SHUTDOWN_TIMEOUT,
					worker, [big_tx_emitter_worker]}
		end,
		lists:seq(1, Config#config.max_emitters)
	),
	WorkerNames = [element(1, El) || El <- Workers],
	Children = [
		?CHILD_WITH_ARGS(big_tx_emitter, worker, big_tx_emitter, [big_tx_emitter, WorkerNames]) | 
		Workers
	],
	{ok, {{one_for_one, 5, 10}, Children}}.
