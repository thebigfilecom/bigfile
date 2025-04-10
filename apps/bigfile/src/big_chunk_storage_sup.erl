-module(big_chunk_storage_sup).

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
	ets:new(chunk_storage_file_index, [set, public, named_table, {read_concurrency, true}]),

	Workers = big_chunk_storage:register_workers() ++
		big_entropy_gen:register_workers(big_entropy_gen) ++
		big_entropy_gen:register_workers(big_entropy_storage),
	{ok, {{one_for_one, 5, 10}, Workers}}.
