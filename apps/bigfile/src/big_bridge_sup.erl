-module(big_bridge_sup).

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
	Children = lists:map(
		fun(Num) ->
			Name = list_to_atom("big_block_propagation_worker" ++ integer_to_list(Num)),
			{Name, {big_block_propagation_worker, start_link, [Name]}, permanent,
			 ?SHUTDOWN_TIMEOUT, worker, [big_block_propagation_worker]}
		end,
		lists:seq(1, ?BLOCK_PROPAGATION_PARALLELIZATION)
	),
	Workers = [element(1, El) || El <- Children],
	Children2 = [?CHILD_WITH_ARGS(big_bridge, worker, big_bridge, [big_bridge, Workers]) | Children],
	{ok, {{one_for_one, 5, 10}, Children2}}.
