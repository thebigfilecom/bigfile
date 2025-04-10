-module(big_poller_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include_lib("bigfile/include/big_sup.hrl").
-include_lib("bigfile/include/big_config.hrl").

%%%===================================================================
%%% Public API.
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks.
%% ===================================================================

init([]) ->
	{ok, Config} = application:get_env(bigfile, config),
	Children = lists:map(
		fun(Num) ->
			Name = list_to_atom("big_poller_worker_" ++ integer_to_list(Num)),
			{Name, {big_poller_worker, start_link, [Name]}, permanent, ?SHUTDOWN_TIMEOUT,
					worker, [big_poller_worker]}
		end,
		lists:seq(1, Config#config.block_pollers)
	),
	Workers = [element(1, El) || El <- Children],
	Children2 = [?CHILD_WITH_ARGS(big_poller, worker, big_poller, [big_poller, Workers]) | Children],
	{ok, {{one_for_one, 5, 10}, Children2}}.
